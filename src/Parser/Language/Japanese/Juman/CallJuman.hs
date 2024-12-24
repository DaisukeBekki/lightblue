{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Parser.Japanese.CallJuman
Copyright   : (c) Daisuke Bekki, 2016
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
  return $ jumanNouns2nodes $ findCompNouns [] jumanData

type PhoneticForm = T.Text
type POS = T.Text
type POSDetail = T.Text
type ConjugationForm = T.Text
type JumanData = (PhoneticForm, POS, POSDetail, ConjugationForm) -- (表層,品詞,品詞細分,（動詞なら）活用形)

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
  | JumanCompNP [T.Text] 
  deriving (Eq,Show)

isCNcomponent :: JumanData -> Bool
isCNcomponent (_,j2,j3,j4) 
  | j2 == "名詞" = True
  | j2 == "副詞" = True
  | j3 == "名詞接頭辞" = True
  | "名詞性名詞" `T.isPrefixOf` j3 = True
  | "形容詞性名詞" `T.isPrefixOf` j3 = True
  | j2 == "特殊" && j3 == "記号" = True
  | j2 == "動詞" && j4 == "基本連用形" = True
  | j2 == "未定義語" = True
  | otherwise = False

isCNsuffix :: JumanData -> Bool
isCNsuffix (_,j2,j3,j4) 
  | j3 == "名詞接頭辞" = True
  | otherwise = False

isCNpostfix :: JumanData -> Bool
isCNpostfix (_,j2,j3,j4) 
  | j3 == "名詞接頭辞" = True
  | otherwise = False

-- | usage: findCompNouns jumanPairs [] 
-- |   returns the list of pair (hyoso, predname)  
findCompNouns :: [JumanCompNoun] -> [JumanData] -> [JumanCompNoun]
findCompNouns compNouns jumanPairs =
  case jumanPairs of
    [] -> compNouns
    (j1,j2,j3,j4):js | (j2 == "名詞" && j3 == "数詞") -> processingCompNoun compNouns js [j1] j3
                     | (j2 == "動詞" && j4 == "基本連用形") -> processingCompNoun compNouns js [j1] j3
                     | (j2 == "未定義語") -> processingCompNoun compNouns js [j1] j3
                     | isCNcomponent (j1,j2,j3,j4) -> accepted1Noun compNouns js j1
                     | otherwise -> findCompNouns compNouns js

accepted1Noun :: [JumanCompNoun] -> [JumanData] -> T.Text -> [JumanCompNoun]
accepted1Noun compNouns jumanPairs oneNoun = 
  case jumanPairs of
    [] -> compNouns
    (j1,j2,j3,j4):js -> if isCNcomponent (j1,j2,j3,j4) || j2 == "未定義語"
                           then processingCompNoun compNouns js [j1,oneNoun] j3
                           else findCompNouns compNouns js

processingCompNoun :: [JumanCompNoun] -> [JumanData] -> [T.Text] -> T.Text -> [JumanCompNoun]
processingCompNoun compNouns jumanPairs compNoun compNounHead = 
  let newCompNoun = if "名詞" `T.isSuffixOf` compNounHead || "名詞" `T.isPrefixOf` compNounHead || "*" ==  compNounHead
                    then (JumanCompCN compNoun)
                    else (JumanCompNP compNoun) in
  case jumanPairs of 
    [] -> newCompNoun:compNouns
    (j1,j2,j3,j4):js -> if isCNcomponent (j1,j2,j3,j4) || j2 == "未定義語"
                           then processingCompNoun (newCompNoun:compNouns) js (j1:compNoun) j3
                           else findCompNouns (newCompNoun:compNouns) js

-- | Transforming juman pos-tags obtained from the given sentence 
-- | into a list of (possible) compound nouns
jumanNouns2nodes :: [JumanCompNoun] -> [Node]
jumanNouns2nodes jumancompnouns = 
  let name = \j -> T.intercalate "~" $ reverse j in
  case jumancompnouns of
    [] -> []
    ((JumanCompNP j):js) -> (TPL.lexicalitem (T.concat $ reverse j) "(CompN)" 97 (T True 1 TPL.modifiableS `SL` (T True 1 TPL.modifiableS `BS` NP [F [Nc]])) (TPL.properNameSR (name j)))
                            :((TPL.lexicalitem (T.concat $ reverse j) "(CompN)" 95 (N) (TPL.commonNounSR (name j)))
                              :(jumanNouns2nodes js))
    ((JumanCompCN j):js) -> (TPL.lexicalitem (T.concat $ reverse j) "(CompN)" 97 (N) (TPL.commonNounSR (name j)))
                              :(jumanNouns2nodes js)

