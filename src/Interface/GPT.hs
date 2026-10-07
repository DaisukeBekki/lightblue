{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- {-|
-- Module      : GPT
-- Licence     : LGPL
-- Copyright   : Asa Tomita
-- Stability   : beta
-- Filtering Function using KWJA
-- -}

module Interface.GPT (
    callGPT
) where

import System.Environment (lookupEnv, getArgs)
import Network.HTTP.Simple
import Configuration.Dotenv (loadFile, defaultConfig)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.List (find)
import Data.Time.Clock (getCurrentTime)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T


modelName :: T.Text
modelName = "gpt-5.4-2026-03-05"


data ReplayRecord = ReplayRecord
  { replayJsemId :: T.Text
  , replayRawResponse :: T.Text
  }


instance FromJSON ReplayRecord where
  parseJSON = withObject "ReplayRecord" $ \value ->
    ReplayRecord <$> value .: "jsem_id" <*> value .: "raw_response"

-- テスト用のmain関数
main :: IO ()
main = do
  -- 環境変数からAPIキーを取得
  -- _ <- loadFile defaultConfig
  -- mApiKey <- lookupEnv "API_KEY"
  -- prompt:_ <- getArgs
  -- case mApiKey of
    -- Nothing -> putStrLn "環境変数 API_KEY が設定されていません。"
    -- Just apiKey -> do
      let prompt = "Haskellはどんな言語ですか？"
      response <- callGPT (T.pack prompt)
      putStrLn "===== GPTの応答 ====="
      T.putStrLn response

-- GPT APIを呼び出す関数
callGPT :: T.Text -> IO T.Text
callGPT prompt = do
  replay <- lookupReplayResponse
  case replay of
    Just savedResponse -> return savedResponse
    Nothing -> callGPTApi prompt


callGPTApi :: T.Text -> IO T.Text
callGPTApi prompt = do
  _ <- loadFile defaultConfig
  mApiKey <- lookupEnv "API_KEY"
  case mApiKey of
    Nothing -> error "環境変数 API_KEY が設定されていません。"
    Just key -> do
      let url = "https://api.openai.com/v1/chat/completions"
      initReq <- parseRequest url
      let body = object
            [ "model" .= modelName
            , "reasoning_effort" .= String "none"
            , "temperature" .= (0 :: Int)
            , "messages" .=
                [ object
                    [ "role" .= String "user"
                    , "content" .= prompt
                    ]
                ]
            , "response_format" .= lexicalRelationResponseFormat
            ]
          request = setRequestMethod "POST"
                  $ setRequestHeader "Authorization" ["Bearer " <> BS.pack key]
                  $ setRequestHeader "Content-Type" ["application/json"]
                  $ setRequestBodyJSON body
                  $ initReq
      response <- httpLBS request
      let responseBody = getResponseBody response
          statusCode = getResponseStatusCode response
          apiContent = extractText responseBody
          content = normalizeLexicalContent apiContent
      appendApiLog prompt statusCode responseBody apiContent content
      if statusCode >= 200 && statusCode < 300
        then return content
        else error $ "OpenAI API returned HTTP " ++ show statusCode


lexicalRelationResponseFormat :: Value
lexicalRelationResponseFormat = object
  [ "type" .= String "json_schema"
  , "json_schema" .= object
      [ "name" .= String "lexical_relation_labels"
      , "strict" .= True
      , "schema" .= object
          [ "type" .= String "object"
          , "properties" .= object
              [ "labels" .= object
                  [ "type" .= String "array"
                  , "items" .= object
                      [ "type" .= String "array"
                      , "items" .= object
                          [ "type" .= String "string"
                          , "enum" .=
                              [ "synonym" :: T.Text
                              , "hypernym"
                              , "hyponym"
                              , "similar"
                              , "inflection"
                              , "antonym"
                              , "derivation"
                              ]
                          ]
                      ]
                  ]
              ]
          , "required" .= ["labels" :: T.Text]
          , "additionalProperties" .= False
          ]
      ]
  ]


-- Structured Outputs requires an object at the schema root.  FeedBack's
-- existing parser expects the historical top-level array, so unwrap only the
-- labels field before returning from callGPT.
normalizeLexicalContent :: T.Text -> T.Text
normalizeLexicalContent content =
  case eitherDecode (LBS.pack $ T.unpack content) of
    Right (Object value) ->
      case parseMaybe (.: "labels") value :: Maybe [[T.Text]] of
        Just labels -> T.pack $ LBS.unpack $ encode labels
        Nothing -> content
    _ -> content


lookupReplayResponse :: IO (Maybe T.Text)
lookupReplayResponse = do
  replayPath <- lookupEnv "LIGHTBLUE_GPT_REPLAY_JSONL"
  currentJsemId <- fmap (T.pack . maybe "" id) $ lookupEnv "LIGHTBLUE_JSEM_ID"
  case replayPath of
    Nothing -> return Nothing
    Just path -> do
      contents <- LBS.readFile path
      let records = [record | line <- LBS.lines contents, Just record <- [decode line]]
      case find (\record -> replayJsemId record == currentJsemId) records of
        Just record -> return $ Just $ replayRawResponse record
        Nothing -> error $ "No saved GPT response for JSeM ID " ++ T.unpack currentJsemId


appendApiLog :: T.Text -> Int -> LBS.ByteString -> T.Text -> T.Text -> IO ()
appendApiLog prompt statusCode responseBody apiContent content = do
  maybePath <- lookupEnv "LIGHTBLUE_GPT_API_LOG_JSONL"
  case maybePath of
    Nothing -> return ()
    Just path -> do
      timestamp <- getCurrentTime
      runId <- envText "LIGHTBLUE_RUN_ID"
      splitName <- envText "LIGHTBLUE_SPLIT"
      jsemId <- envText "LIGHTBLUE_JSEM_ID"
      pairId <- envText "LIGHTBLUE_PAIR_ID"
      let responseJson = case eitherDecode responseBody of
            Right value -> value
            Left _ -> String $ T.pack $ LBS.unpack responseBody
          record = object
            [ "timestamp" .= show timestamp
            , "run_id" .= runId
            , "split" .= splitName
            , "jsem_id" .= jsemId
            , "pair_id" .= pairId
            , "model_requested" .= modelName
            , "reasoning_effort" .= String "none"
            , "temperature" .= (0 :: Int)
            , "prompt" .= prompt
            , "http_status" .= statusCode
            , "api_content" .= apiContent
            , "content" .= content
            , "response_json" .= responseJson
            ]
      LBS.appendFile path $ encode record `LBS.append` "\n"
  where
    envText name = fmap (T.pack . maybe "" id) $ lookupEnv name

-- JSONレスポンスから応答文だけを抽出
extractText :: LBS.ByteString -> T.Text
extractText body =
  case eitherDecode body of
    Left err -> T.pack ("JSON decode error: " ++ err)
    Right (Object v) ->
      case parseMaybe parser v of
        Just content -> content
        Nothing -> "応答の解析に失敗しました"
    _ -> "Unexpected JSON"

-- メッセージ本文を取り出すパーサー
parser :: Object -> Parser T.Text
parser v = do
  choices <- v .: "choices"
  case choices of
    (Object o : _) -> do
      msg <- o .: "message"
      msg .: "content"
    _ -> fail "Unexpected format in choices"
