{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Parser.Japanese.CallJuman
Copyright   : (c) Daisuke Bekki, 2024
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

A module for compound nouns in Japanese.
-}
module Parser.Language.Japanese.Juman.CallJuman (
  MorphAnalyzerName(..)
  , findCompoundNouns
  ) where

import Prelude as P
import qualified Data.Char as C           --base
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified Shelly as S              -- shelly
import Parser.CCG
import qualified Parser.Language.Japanese.Templates as TPL
  
data MorphAnalyzerName = JUMAN | JUMANPP | KWJA deriving (Eq,Show)
instance Read MorphAnalyzerName where
  readsPrec _ r =
    [(JUMAN,s) | (x,s) <- lex r, map C.toLower x == "juman"]
    ++ [(JUMANPP,s) | (x,s) <- lex r, map C.toLower x == "jumanpp"]
    ++ [(KWJA,s) | (x,s) <- lex r, map C.toLower x == "kwja"]

-- | Main function: jumaCompoundNouns
-- |   given a sentence, returns a list of compound nouns
findCompoundNouns :: MorphAnalyzerName -> T.Text -> IO [Node]
findCompoundNouns morphaName sentence = do
  jumanData <- callMorphologicalAnalyzer morphaName sentence
  -- mapM_ printJumanData jumanData
  return $ map jumanNoun2Node $ findCompNouns [] jumanData

type PhoneticForm = T.Text
type POS = T.Text
type POSDetail = T.Text
type ConjugationForm = T.Text
type JumanData = (PhoneticForm, POS, POSDetail, ConjugationForm) -- (表層,品詞,品詞細分,（動詞なら）活用形)

printJumanData :: JumanData -> IO()
printJumanData (ph,pos,posd,conj) = do
  T.putStrLn $ T.concat [ph, "/", pos, "/", posd, "/", conj]

callMorphologicalAnalyzer :: MorphAnalyzerName -> T.Text -> IO [JumanData]
callMorphologicalAnalyzer morphaName sentence = do
  let command = case morphaName of
        JUMAN   -> \s -> T.concat ["echo ", s, " | juman"]
        JUMANPP -> \s -> T.concat ["echo ", s, " | jumanpp"]
        KWJA    -> \s -> T.concat ["kwja --text ", s]
  maOutput <- S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ T.toStrict $ command sentence
  return $ map toJumanData $ excludeCommentLines morphaName $ T.lines $ T.fromStrict maOutput
  where 
    -- | EOSまでの行を取得し、@+*から始まる行を除外する
    excludeCommentLines :: MorphAnalyzerName -> [T.Text] -> [T.Text] 
    excludeCommentLines morphanName = 
      filter (\x -> not (isCommentLineOf morphanName x)) . P.takeWhile (/= "EOS") 
    -- | Check if the given text is a prefix of any of the given prefixes
    isCommentLineOf :: MorphAnalyzerName -> T.Text -> Bool
    isCommentLineOf morphanName x = any (`T.isPrefixOf` x) $ case morphanName of
      JUMAN   -> ["@"]
      JUMANPP -> ["@"]
      KWJA    -> ["@","*","+","#"]
    -- | (表層,品詞,品詞細分,（動詞なら）活用形)の4つ組を取得
    toJumanData :: T.Text -> JumanData 
    toJumanData = (\l -> ((l!!0),(l!!3),(l!!5),(l!!9))) . T.split (== ' ')

data JumanCompNoun = 
  JumanCompCN [T.Text] 
  -- | JumanCompNP [T.Text] 
  deriving (Eq,Show)

isCNcomponent :: JumanData -> Bool
isCNcomponent (_,j2,j3,j4) 
  | j2 == "名詞" = True
  | j2 == "副詞" = True
  | j3 == "名詞接頭辞" = True
  | "名詞性名詞" `T.isPrefixOf` j3 = True
  | "形容詞性名詞" `T.isPrefixOf` j3 = True
  | j2 == "動詞" && j4 == "基本連用形" = True
  | j2 == "特殊" && j3 == "記号" = True
  | j2 == "未定義語" = True
  | otherwise = False

isCNsuffix :: JumanData -> Bool
isCNsuffix (_,j2,j3,j4) 
  | j3 == "名詞接頭辞" = True
  | otherwise = False

isCNpostfix :: JumanData -> Bool
isCNpostfix (_,j2,j3,j4) 
  | j2 == "接尾辞" = True
  | otherwise = False

-- | usage: findCompNouns jumanData [] 
findCompNouns :: [JumanCompNoun] -> [JumanData] -> [JumanCompNoun]
findCompNouns compNouns jumanData =
  case jumanData of
    [] -> compNouns
    j@(j1,_,_,_):js | isCNsuffix j    -> acceptedSuffix compNouns js j1
                    | isCNcomponent j -> processingCompNoun compNouns js [j1]
                    | otherwise       -> findCompNouns compNouns js

acceptedSuffix :: [JumanCompNoun] -> [JumanData] -> T.Text -> [JumanCompNoun]
acceptedSuffix compNouns jumanData suffix = 
  case jumanData of
    [] -> compNouns
    j@(j1,_,_,_):js | isCNcomponent j -> processingCompNoun compNouns js [j1,suffix]
                    | otherwise       -> findCompNouns compNouns js

processingCompNoun :: [JumanCompNoun] -> [JumanData] -> [T.Text] -> [JumanCompNoun]
processingCompNoun compNouns jumanData compNounStack = 
  case jumanData of 
    [] -> (JumanCompCN compNounStack):compNouns
    j@(j1,_,_,_):js | isCNcomponent j -> processingCompNoun compNouns js (j1:compNounStack)
                    | isCNpostfix   j -> findCompNouns ((JumanCompCN (j1:compNounStack)):compNouns) js
                    | otherwise       -> findCompNouns ((JumanCompCN compNounStack):compNouns) js

-- | Transforming juman pos-tags obtained from the given sentence into a compound nouns
jumanNoun2Node :: JumanCompNoun -> Node
jumanNoun2Node (JumanCompCN j) = 
  let revj = reverse j in
  TPL.lexicalitem (T.concat revj) "(CompN)" 98 (N) (TPL.commonNounSR $ T.intercalate "~" revj)
                              
