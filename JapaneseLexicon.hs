{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures #-}

{-|
Module      : JapaneseLexicon
Description : A lexicon of Japanese
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : alpha
-}
module JapaneseLexicon (
  --Node(..),
  Lexicon,
  --isCONJ,
  lookupLexicon,
  setupLexicon,
  LEX.emptyCategories
  ) where

import Prelude hiding (id)
import qualified Data.Text.Lazy as T    --text
import qualified Data.Text.Lazy.IO as T --text
--import Data.Ratio as R                  --base
--import qualified Data.Maybe as M
import qualified CallJuman as JU
import qualified MyLexicon as LEX
import CombinatoryCategorialGrammar
import DependentTypes

-- | Lexicon consists of a set of CCG Nodes
type Lexicon = [Node]

-- | This function takes a word and a lexicon and returns a set of CCG lexical entries whose PF is that word.
lookupLexicon :: T.Text -> Lexicon -> [Node]
lookupLexicon word lexicon = filter (\l -> (pf l) == word) lexicon

-- | This function takes a sentence and returns a numeration needed to parse that sentence, i.e., a union of 
setupLexicon :: T.Text -> IO(Lexicon)
setupLexicon sentence = do
  jumandic <- T.readFile "Juman.dic"
  jumanCN <- JU.jumanCompoundNouns sentence
  return $ (concat $ (map parseJumanLine $ filter (\l -> (head l) `T.isInfixOf` sentence) $ map (T.split (=='\t')) (T.lines jumandic)))
           ++ (filter (\l -> T.isInfixOf (pf l) sentence) LEX.myLexicon)
           ++ jumanCN

-- | Read each line in "Juman.dic" and convert it to a CCG lexical item
parseJumanLine :: [T.Text] -> [Node]
parseJumanLine jumanline = 
  case jumanline of
    (hyoki:(score':(cat':(daihyo':(source:(caseframe:_)))))) -> 
      let catsemlist = jumanPos2Cat daihyo' cat' caseframe in
      [(lexicalitem hyoki (T.concat ["(J",T.take 3 source,")"]) (read (T.unpack score')::Integer) cat2 sem2) | (cat2,sem2) <- catsemlist] --(T.concat [daihyo',":",cat']))
    _ -> []

-- | Main function 1 "jumanPos2Cat" that converts Juman entries to lexical items
jumanPos2Cat :: T.Text -> T.Text -> T.Text -> [(Cat,Preterm)]
jumanPos2Cat daihyo ct caseframe 
  | T.isPrefixOf "名詞:普通名詞" ct    = [(N, commonNounSR daihyo)]
  | T.isPrefixOf "名詞:人名"    ct    = constructProperName daihyo
  | T.isPrefixOf "名詞:地名"    ct    = constructProperName daihyo
  | T.isPrefixOf "名詞:組織名"  ct    = constructProperName daihyo
  | T.isPrefixOf "名詞:副詞的名詞" ct  = [((anySExStem`SL` anySExStem) `BS` (S anyPos [Attr]), (Lam (Var 0)))]
  | T.isPrefixOf "名詞:時相名詞" ct      = constructPredicate daihyo [Nda,Nna,Nno]
  | T.isPrefixOf "動詞:子音動詞カ行促音便形"  ct  = constructVerb daihyo caseframe [V5IKU,V5YUK]
  | T.isPrefixOf "動詞:子音動詞カ行"  ct  = constructVerb daihyo caseframe [V5k]
  | T.isPrefixOf "動詞:子音動詞サ行"  ct  = constructVerb daihyo caseframe [V5s]
  | T.isPrefixOf "動詞:子音動詞タ行"  ct  = constructVerb daihyo caseframe [V5t]
  | T.isPrefixOf "動詞:子音動詞ナ行"  ct  = constructVerb daihyo caseframe [V5n]
  | T.isPrefixOf "動詞:子音動詞マ行"  ct  = constructVerb daihyo caseframe [V5m]
  | T.isPrefixOf "動詞:子音動詞ラ行イ形"  ct  = constructVerb daihyo caseframe [V5NAS]
  | T.isPrefixOf "動詞:子音動詞ラ行"  ct  = constructVerb daihyo caseframe [V5r]
  | T.isPrefixOf "動詞:子音動詞ワ行文語音便形"  ct  = constructVerb daihyo caseframe [V5TOW]
  | T.isPrefixOf "動詞:子音動詞ワ行"  ct  = constructVerb daihyo caseframe [V5w]
  | T.isPrefixOf "動詞:子音動詞ガ行"  ct  = constructVerb daihyo caseframe [V5g]
  | T.isPrefixOf "動詞:子音動詞バ行"  ct  = constructVerb daihyo caseframe [V5b]
  | T.isPrefixOf "動詞:母音動詞"     ct  = constructVerb daihyo caseframe [V1]
  | T.isPrefixOf "動詞:カ変動詞"     ct  = constructVerb daihyo caseframe [VK]
  | T.isPrefixOf "名詞:サ変名詞"     ct  = [(N, commonNounSR daihyo)] ++ (constructVerb daihyo caseframe [VS])
  | T.isPrefixOf "動詞:サ変動詞"     ct  = constructVerb daihyo caseframe [VS]
  | T.isPrefixOf "動詞:ザ変動詞"     ct  = constructVerb daihyo caseframe [VZ]
  | T.isPrefixOf "形容詞:イ形容詞アウオ段" ct = constructPredicate daihyo [Aauo]
  | T.isPrefixOf "形容詞:イ形容詞イ段"    ct = constructPredicate daihyo [Ai]
  | T.isPrefixOf "形容詞:ナ形容詞"       ct = constructPredicate daihyo [Nda,Nna,Nni]
  | T.isPrefixOf "形容詞:ナノ形容詞"     ct = constructPredicate daihyo [Nda,Nna,Nno]
  | T.isPrefixOf "副詞"  ct  = constructPredicate daihyo [Nda,Nna,Nno,Nni,Nemp]
  | T.isPrefixOf "連体詞" ct  = [(N `SL` N, (Lam (Lam (Lam (Sigma (App (App (Var 2) (Var 1)) (Var 0)) (App (Con daihyo) (Var 0)))))))]
  | T.isPrefixOf "接続詞" ct = [(((S anyPos [Term,Pre,Imper] `SL` S anyPos [Term,Pre,Imper]) `BS` S anyPos [Term]), (Lam (Lam (Sigma (Var 1) (Var 0)))))]
  | T.isPrefixOf "接頭辞:イ形容詞" ct   = [((S [Aauo] [Stem] `BS` NP [Ga]) `SL` (S [Aauo] [Stem] `BS` NP [Ga]), id)]
  | T.isPrefixOf "接頭辞:ナ形容詞" ct   = [((S [Nda] [Stem] `BS` NP [Ga]) `SL` (S [Nda] [Stem] `BS` NP [Ga]), id)]
  | T.isPrefixOf "名詞性名詞助数辞" ct  = [(N,id)]
  | T.isPrefixOf "名詞性特殊接尾辞" ct  = [(N,id)]
  | T.isPrefixOf "名詞性名詞接尾辞" ct  = [(N,id)]
  | T.isPrefixOf "数詞"           ct = [(Nat,id)]
  -- T.isPrefixOf "接頭辞:名詞" ct   = [(N `SL` N, (Lam (Lam (Sigma (App (Var 1) (Var 0)) (App (Con daihyo) (Var 1))))))]
  -- T.isPrefixOf "接尾辞" t   = [(BS (T True 1 anySExStem) (T True 1 anySExStem), id)]
  -- T.isPrefixOf "感動詞" t   = [(S [Exp] [Term], id)]
  | otherwise      = [(S [Error] [Term], (Con $ T.concat [T.pack "Juman Error: ", ct]))]

constructProperName :: T.Text -> [(Cat,Preterm)]
constructProperName daihyo = [((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])), properNameSR daihyo)]

constructPredicate :: T.Text -> [CatPos] -> [(Cat,Preterm)]
constructPredicate daihyo cpos = [(S cpos [Stem] `BS` NP [Ga], predSR daihyo)]

constructVerb :: T.Text -> T.Text -> [CatPos] -> [(Cat, Preterm)]
constructVerb daihyo caseframe cpos =
  if caseframe == T.empty
    then [((S cpos [Stem] `BS` NP [Ga]) `BS` NP [Ni,O], verbSR 2 daihyo)]
    else let caseframelist = map (T.split (==',')) $ T.split (=='#') caseframe in
         [(verbCat cf (S cpos [Stem]), (verbSR (length cf) daihyo)) | cf <- caseframelist]

verbCat :: [T.Text] -> Cat -> Cat
verbCat cf ct = case cf of
  [] -> ct
  (c:cs) | c == "ガ格" -> (verbCat cs ct) `BS` NP [Ga]
         | c == "ヲ格" -> (verbCat cs ct) `BS` NP [O]
         | c == "ニ格" -> (verbCat cs ct) `BS` NP [Ni]
         | c == "ト格" -> (verbCat cs ct) `BS` NP [To]
         | c == "によって" -> (verbCat cs ct) `BS` NP [Niyotte]
         | otherwise -> (verbCat cs ct)

