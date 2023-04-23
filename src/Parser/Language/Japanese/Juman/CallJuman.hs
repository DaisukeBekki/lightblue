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
  jumanCompoundNouns
  ) where

import Prelude as P
import qualified Data.Text.Lazy as T                 --text
import qualified Data.Text.Lazy.IO as T              --text
import qualified System.Process as S                 --process
import Parser.CCG
import qualified Parser.Language.Japanese.Templates as TPL

{-
see :: [JumanPair] -> IO([JumanPair])
see = mapM see2

see2 :: JumanPair -> IO(JumanPair)
see2 noun@(j1,j2,j3,j4) = do
  T.putStr j1
  T.putStr ","
  T.putStr j2
  T.putStr ","
  T.putStr j3
  T.putStr ","
  T.putStrLn j4
  return noun
-}

-- | Main function: jumaCompoundNouns
-- |   given a sentence, returns a list of compound nouns
jumanCompoundNouns :: T.Text -> IO([Node])
jumanCompoundNouns sentence = do
  fmap jumanNouns2nodes $ callJuman sentence

data JumanCompNoun = JumanCompCN [T.Text] | JumanCompNP [T.Text] deriving (Eq,Show)
type JumanPair = (T.Text, T.Text, T.Text, T.Text) -- (表層,品詞,品詞細分,（動詞なら）活用形)

-- | Call Juman as an external process and returns a list of juman pos-tags
callJuman :: T.Text -> IO([JumanCompNoun])
callJuman sentence = do
  (_, stdout, _, _) <- S.runInteractiveCommand $ T.unpack $ T.concat ["echo ", sentence, " | jumanpp"]
  --(_, stdout, _, procHandle) <- S.runInteractiveCommand $ T.unpack $ T.concat ["echo ", sentence, " | juman"]
  --_ <- S.waitForProcess procHandle
  t <- T.hGetContents stdout
  --terminateProcess processhandle
  --mapped <- see $ map ((\l -> ((l!!0),(l!!3),(l!!5),(l!!9))) . (T.split (==' '))) $ filter (\x -> not (T.isPrefixOf "@" x)) $ P.takeWhile (/= "EOS") $ T.lines t 
  --return $ findCompNouns [] mapped
  return $ findCompNouns [] $ map ((\l -> ((l!!0),(l!!3),(l!!5),(l!!9))) . (T.split (==' '))) $ filter (\x -> not (T.isPrefixOf "@" x)) $ P.takeWhile (/= "EOS") $ T.lines t 

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

