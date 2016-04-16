{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : CallJuman
Description : A module for compound nouns in Japanese
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}
module Parser.Japanese.CallJuman (
  jumanCompoundNouns
  ) where
  
import Prelude as P
import qualified Data.Text.Lazy as T                 --text
import qualified Data.Text.Lazy.IO as T              --text
import qualified System.Process as S                 --process
import Parser.CombinatoryCategorialGrammar
import qualified Parser.Japanese.Templates as TPL

--import qualified Data.List as L
--import qualified System.IO as S

-- | Main function: jumaCompoundNouns
-- |   given a sentence, returns a list of compound nouns
jumanCompoundNouns :: T.Text -> IO([Node])
jumanCompoundNouns sentence = do
  nounlist <- callJuman sentence
  return $ jumanNouns2nodes nounlist

data JumanCompNoun = JumanCompCN [T.Text] | JumanCompNP [T.Text]
type JumanPair = (T.Text, T.Text, T.Text, T.Text) -- (表層,品詞,品詞細分,（動詞なら）活用形)

-- | Call Juman as an external process and returns a list of juman pos-tags
callJuman :: T.Text -> IO([JumanCompNoun])
callJuman sentence = do
  (_, stdout, _, _) <- S.runInteractiveCommand $ T.unpack $ T.concat ["echo ", sentence, " | nkf -e | juman | nkf -w"]
  t <- T.hGetContents stdout
  return $ findCompNouns [] $ map ((\l -> ((l!!0),(l!!3),(l!!5),(l!!9))) . (T.split (==' '))) $ filter (\x -> not (T.isPrefixOf "@" x)) $ P.takeWhile (/= "EOS") $ T.lines t 
           

-- | Transforming juman pos-tags obtained from the given sentence 
-- | into a list of (possible) compound nouns
jumanNouns2nodes :: [JumanCompNoun] -> [Node]
jumanNouns2nodes jumancompnouns = 
  let name = \j -> T.intercalate "~" $ reverse j in
  case jumancompnouns of
    [] -> []
    ((JumanCompNP j):js) -> (TPL.lexicalitem (T.concat $ reverse j) "(CompN)" 95 (T True 1 TPL.modifiableS `SL` (T True 1 TPL.modifiableS `BS` NP [F [Nc]])) (TPL.properNameSR (name j))):(jumanNouns2nodes js)
    ((JumanCompCN j):js) -> (TPL.lexicalitem (T.concat $ reverse j) "(CompN)" 95 (N) (TPL.commonNounSR (name j))):(jumanNouns2nodes js)

-- | usage: findCompNouns jumanPairs [] 
-- |   returns the list of pair (hyoso, predname)  
findCompNouns :: [JumanCompNoun] -> [JumanPair] -> [JumanCompNoun]
findCompNouns compNouns jumanPairs =
  case jumanPairs of
    [] -> compNouns
    (j1,j2,j3,j4):js | (j2 == "名詞" || j3 == "名詞接頭辞" || "名詞性名詞" `T.isPrefixOf` j3) || (j2 == "動詞" && j4 == "基本連用形") -> accepted1Noun compNouns js j1
                     | (j2 == "未定義語") -> processingCompNoun compNouns js [j1] j3
                     | otherwise -> findCompNouns compNouns js

accepted1Noun :: [JumanCompNoun] -> [JumanPair] -> T.Text -> [JumanCompNoun]
accepted1Noun compNouns jumanPairs oneNoun = case jumanPairs of           
  [] -> compNouns
  (j1,j2,j3,j4):js -> if j2 == "名詞" || j3 == "名詞接頭辞" || "名詞性名詞" `T.isPrefixOf` j3 || j2 == "未定義語" || (j2 == "動詞" && j4 == "基本連用形")
                   then processingCompNoun compNouns js [j1,oneNoun] j3
                   else findCompNouns compNouns js

processingCompNoun :: [JumanCompNoun] -> [JumanPair] -> [T.Text] -> T.Text -> [JumanCompNoun]
processingCompNoun compNouns jumanPairs compNoun compNounHead = 
  let newCompNoun = if "名詞" `T.isSuffixOf` compNounHead || "名詞" `T.isPrefixOf` compNounHead || "*" ==  compNounHead
                    then (JumanCompCN compNoun)
                    else (JumanCompNP compNoun) in
  case jumanPairs of 
  [] -> newCompNoun:compNouns
  (j1,j2,j3,j4):js -> if j2 == "名詞" || j3 == "名詞接頭辞" || "名詞性名詞" `T.isPrefixOf` j3 || j2 == "未定義語" || (j2 == "動詞" && j4 == "基本連用形")
                   then processingCompNoun (newCompNoun:compNouns) js (j1:compNoun) j3
                   else findCompNouns (newCompNoun:compNouns) js
