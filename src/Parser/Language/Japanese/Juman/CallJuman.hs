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
  -- jumanCompoundNouns,
  --, kwjaCompoundNouns
  , compoundNouns
  ) where

import Prelude as P
import qualified Data.Char as C           --base
import qualified Data.Text.Lazy as T      --text
--import qualified Data.Text.Lazy.IO as T   --text
--import qualified System.Process as S      --process
import qualified Shelly as S              -- shelly
import Parser.CCG
import qualified Parser.Language.Japanese.Templates as TPL
--import Debug.Trace

data MorphAnalyzerName = JUMAN | JUMANPP | KWJA deriving (Eq,Show)
instance Read MorphAnalyzerName where
  readsPrec _ r =
    [(JUMAN,s) | (x,s) <- lex r, map C.toLower x == "juman"]
    ++ [(JUMANPP,s) | (x,s) <- lex r, map C.toLower x == "jumanpp"]
    ++ [(KWJA,s) | (x,s) <- lex r, map C.toLower x == "kwja"]

-- | Main function: jumaCompoundNouns
-- |   given a sentence, returns a list of compound nouns
compoundNouns :: MorphAnalyzerName -> T.Text -> IO([Node])
compoundNouns JUMAN = fmap jumanNouns2nodes . callJuman 
compoundNouns JUMANPP = fmap jumanNouns2nodes . callJumanpp 
compoundNouns KWJA = fmap jumanNouns2nodes . callKWJA

-- jumanCompoundNouns :: T.Text -> IO([Node])
-- jumanCompoundNouns sentence = do
--   fmap jumanNouns2nodes $ callJuman sentence

--   -- | Main function: kwjaCompoundNouns
-- -- |   given a sentence, returns a list of compound nouns
-- kwjaCompoundNouns :: T.Text -> IO([Node])
-- kwjaCompoundNouns sentence = do
--   fmap jumanNouns2nodes $ callKWJA sentence

data JumanCompNoun = 
  JumanCompCN [T.Text] 
  | JumanCompNP [T.Text] 
  deriving (Eq,Show)

type PhoneticForm = T.Text
type POS = T.Text
type POSDetail = T.Text
type ConjugationForm = T.Text

type JumanPair = (PhoneticForm, POS, POSDetail, ConjugationForm) -- (表層,品詞,品詞細分,（動詞なら）活用形)

-- | Call Juman as an external process and returns a list of juman pos-tags
callJuman :: T.Text -> IO [JumanCompNoun]
callJuman = callMorphologicalAnalyzer (\s -> T.concat ["echo ", s, " | juman"]) ["@"]

-- | Call Juman as an external process and returns a list of juman pos-tags
callJumanpp :: T.Text -> IO [JumanCompNoun]
callJumanpp = callMorphologicalAnalyzer (\s -> T.concat ["echo ", s, " | jumanpp"]) ["@"]

-- | Call KWJA as an external process and returns a list of juman pos-tags
callKWJA :: T.Text -> IO [JumanCompNoun]
callKWJA = callMorphologicalAnalyzer (\s -> T.concat ["kwja --text ", s]) ["@","*","+","#"]

callMorphologicalAnalyzer :: (T.Text -> T.Text) -> [T.Text] -> T.Text -> IO [JumanCompNoun]
callMorphologicalAnalyzer command prefixes sentence = do
  output <- S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ T.toStrict $ command sentence
  return $ findCompNouns [] $ getJumanPairs $ excludeSpecialPrefixes $ T.lines $ T.fromStrict output
  where 
    -- EOSまでの行を取得し、@+*から始まる行を除外する
    excludeSpecialPrefixes :: [T.Text] -> [T.Text]
    excludeSpecialPrefixes = 
      filter (\x -> not (isPrefixOfAny prefixes x)) . P.takeWhile (/= "EOS") 

-- (表層,品詞,品詞細分,（動詞なら）活用形)の4つ組を取得
getJumanPairs :: [T.Text] -> [JumanPair]
getJumanPairs = map ((\l -> ((l!!0),(l!!3),(l!!5),(l!!9))) . (T.split (==' '))) 

-- | Check if the given text is a prefix of any of the given prefixes
isPrefixOfAny :: [T.Text] -> T.Text -> Bool
isPrefixOfAny prefixes x = any (`T.isPrefixOf` x) prefixes

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

isCNcomponent :: JumanPair -> Bool
isCNcomponent (_,j2,j3,j4) =
  if j2 == "名詞" ||
     j2 == "副詞" ||
     j3 == "名詞接頭辞" ||
     "名詞性名詞" `T.isPrefixOf` j3 ||
     "形容詞性名詞" `T.isPrefixOf` j3 ||
     (j2 == "特殊" && j3 == "記号") ||
     (j2 == "動詞" && j4 == "基本連用形")
     then True
     else False

-- | usage: findCompNouns jumanPairs [] 
-- |   returns the list of pair (hyoso, predname)  
findCompNouns :: [JumanCompNoun] -> [JumanPair] -> [JumanCompNoun]
findCompNouns compNouns jumanPairs =
  case jumanPairs of
    [] -> compNouns
    (j1,j2,j3,j4):js | (j2 == "名詞" && j3 == "数詞") -> processingCompNoun compNouns js [j1] j3
                     | (j2 == "動詞" && j4 == "基本連用形") -> processingCompNoun compNouns js [j1] j3
                     | (j2 == "未定義語") -> processingCompNoun compNouns js [j1] j3
                     | isCNcomponent (j1,j2,j3,j4) -> accepted1Noun compNouns js j1
                     | otherwise -> findCompNouns compNouns js

accepted1Noun :: [JumanCompNoun] -> [JumanPair] -> T.Text -> [JumanCompNoun]
accepted1Noun compNouns jumanPairs oneNoun = 
  case jumanPairs of
    [] -> compNouns
    (j1,j2,j3,j4):js -> if isCNcomponent (j1,j2,j3,j4) || j2 == "未定義語"
                           then processingCompNoun compNouns js [j1,oneNoun] j3
                           else findCompNouns compNouns js

processingCompNoun :: [JumanCompNoun] -> [JumanPair] -> [T.Text] -> T.Text -> [JumanCompNoun]
processingCompNoun compNouns jumanPairs compNoun compNounHead = 
  let newCompNoun = if "名詞" `T.isSuffixOf` compNounHead || "名詞" `T.isPrefixOf` compNounHead || "*" ==  compNounHead
                    then (JumanCompCN compNoun)
                    else (JumanCompNP compNoun) in
  case jumanPairs of 
    [] -> newCompNoun:compNouns
    (j1,j2,j3,j4):js -> if isCNcomponent (j1,j2,j3,j4) || j2 == "未定義語"
                           then processingCompNoun (newCompNoun:compNouns) js (j1:compNoun) j3
                           else findCompNouns (newCompNoun:compNouns) js

