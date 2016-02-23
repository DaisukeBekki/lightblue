{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module CallJuman (
  jumanCompoundNouns
  ) where
  
import Prelude as P
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified System.Process as S
import qualified CombinatoryCategorialGrammar as CCG
import DependentTypes 
import qualified Data.Ratio as R
--import qualified Data.List as L
--import qualified System.IO as S

-- | Main function: jumaCompoundNouns
-- |   given a sentence, returns a list of compound nouns
jumanCompoundNouns :: T.Text -> IO([CCG.Node])
jumanCompoundNouns sentence = do
  nounlist <- callJuman sentence
  return $ jumanNouns2nodes nounlist

data JumanCompNoun = JumanCompCN [T.Text] | JumanCompNP [T.Text]
type JumanPair = (T.Text, T.Text, T.Text) -- (表層,品詞,品詞細分)

-- | Call Juman as an external process and returns a list of juman pos-tags
callJuman :: T.Text -> IO([JumanCompNoun])
callJuman sentence = do
  (_, stdout, _, _) <- S.runInteractiveCommand $ T.unpack $ T.concat ["echo ", sentence, " | nkf -e | juman | nkf -w"]
  t <- T.hGetContents stdout
  return $ findCompNouns [] $ map ((\l -> ((l!!0),(l!!3),(l!!5))) . (T.split (==' '))) $ filter (\x -> not (T.isPrefixOf "@" x)) $ P.takeWhile (/= "EOS") $ T.lines t 
           

-- | Transforming juman pos-tags obtained from the given sentence 
-- | into a list of (possible) compound nouns
jumanNouns2nodes :: [JumanCompNoun] -> [CCG.Node]
jumanNouns2nodes jumancompnouns = case jumancompnouns of
  [] -> []
  ((JumanCompNP j):js) -> (lexicalitem (T.concat $ reverse j) "(JC)" (92 R.% 100) (CCG.NP [CCG.Nc]) (Con (T.intercalate ";" $ reverse j))):(jumanNouns2nodes js)
  ((JumanCompCN j):js) -> (lexicalitem (T.concat $ reverse j) "(JC)" (92 R.% 100) (CCG.N) (Lam (App (Con (T.intercalate ";" $ reverse j)) (Var 0)))):(jumanNouns2nodes js)
  where lexicalitem word m r c s = CCG.Node CCG.LEX word c s [] r m

-- | usage: findCompNouns jumanPairs [] 
-- |   returns the list of pair (hyoso, predname)  
findCompNouns :: [JumanCompNoun] -> [JumanPair] -> [JumanCompNoun]
findCompNouns compNouns jumanPairs =
  case jumanPairs of
    [] -> compNouns
    (j1,j2,j3):js -> case () of
                       _ | (j2 == "名詞" || j3 == "名詞接頭辞" || j3 == "名詞性名詞接尾辞") -> accepted1Noun compNouns js j1
                         | (j2 == "未定義語") -> processingCompNoun compNouns js [j1] j3
                         | otherwise -> findCompNouns compNouns js

accepted1Noun :: [JumanCompNoun] -> [JumanPair] -> T.Text -> [JumanCompNoun]
accepted1Noun compNouns jumanPairs oneNoun = case jumanPairs of           
  [] -> compNouns
  (j1,j2,j3):js -> if j2 == "名詞" || j3 == "名詞接頭辞" || j3 == "名詞性名詞接尾辞" || j2 == "未定義語"
                   then processingCompNoun compNouns js [j1,oneNoun] j3
                   else findCompNouns compNouns js

processingCompNoun :: [JumanCompNoun] -> [JumanPair] -> [T.Text] -> T.Text -> [JumanCompNoun]
processingCompNoun compNouns jumanPairs compNoun compNounHead = 
  let newCompNoun = if "普通名詞" `T.isPrefixOf` compNounHead || "名詞性" `T.isPrefixOf` compNounHead
                    then (JumanCompCN compNoun)
                    else (JumanCompNP compNoun) in
  case jumanPairs of 
  [] -> newCompNoun:compNouns
  (j1,j2,j3):js -> if j2 == "名詞" || j3 == "名詞接頭辞" || j3 == "名詞性名詞接尾辞" || j2 == "未定義語"
                   then processingCompNoun (newCompNoun:compNouns) js (j1:compNoun) j3
                   else findCompNouns (newCompNoun:compNouns) js
