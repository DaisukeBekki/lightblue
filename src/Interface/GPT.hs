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
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T

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
  _ <- loadFile defaultConfig
  mApiKey <- lookupEnv "API_KEY"
  case mApiKey of
    Nothing -> error "環境変数 API_KEY が設定されていません。"
    Just key -> do
      let url = "https://api.openai.com/v1/chat/completions"
      initReq <- parseRequest url
      let body = object
            [ "model" .= String "gpt-4o-mini"
            , "messages" .=
                [ object
                    [ "role" .= String "user"
                    , "content" .= prompt
                    ]
                ]
            ]
          request = setRequestMethod "POST"
                  $ setRequestHeader "Authorization" ["Bearer " <> BS.pack key]
                  $ setRequestHeader "Content-Type" ["application/json"]
                  $ setRequestBodyJSON body
                  $ initReq
      response <- httpLBS request
      let responseBody = getResponseBody response
      -- putStrLn "===== APIレスポンス ====="
      -- LBS.putStrLn responseBody
      return $ extractText responseBody

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

