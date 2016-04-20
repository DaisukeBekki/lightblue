{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures #-}

{-|
Module      : JapaneseLexicon
Description : A Japanese CCG lexicon
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}
module Parser.Japanese.Lexicon (
  --Node(..),
  LexicalItems,
  --isCONJ,
  lookupLexicon,
  setupLexicon,
  LEX.emptyCategories,
  LEX.myLexicon
  ) where

import Prelude hiding (id)
import qualified Data.Text.Lazy as T    --text
import qualified Data.Text.Lazy.IO as T --text
import qualified Data.List as L         -- base
import qualified Data.Map as M          -- base
import qualified System.Environment as E -- base
--import qualified Control.Parallel.Strategies as P  --parallel
--import Data.Ratio as R                  --base
--import qualified Data.Maybe as M
import Parser.CombinatoryCategorialGrammar
import qualified Parser.Japanese.CallJuman as JU
import qualified Parser.Japanese.MyLexicon as LEX
import Parser.Japanese.Templates
import Logic.DependentTypes

---- | The location of @Juman.dic@
--jumandicpath :: String
--jumandicpath = "/home/bekki/dropbox/MyProgram/Haskell/CCG06/Parser/Japanese/Juman.dic"

-- | Lexicon consists of a set of CCG Nodes
type LexicalItems = [Node]

-- | This function takes a word and a lexicon and returns a set of CCG lexical entries whose PF is that word.
lookupLexicon :: T.Text -> LexicalItems -> [Node]
lookupLexicon word lexicon = filter (\l -> (pf l) == word) lexicon

-- | This function takes a sentence and returns a numeration needed to parse that sentence, i.e., a union of 
setupLexicon :: T.Text -> IO(LexicalItems)
setupLexicon sentence = do
  --  1. Setting up lexical items provided by JUMAN++
  jumandicpath <- E.getEnv "LIGHTBLUE"
  jumandic <- T.readFile $ jumandicpath ++ "Parser/Japanese/Juman.dic"
  let jumandicFiltered = filter (\l -> (head l) `T.isInfixOf` sentence) $ map (T.split (=='\t')) (T.lines jumandic)
  let (jumandicParsed,(f2,f3)) = L.foldl' parseJumanLine ([],(M.empty,M.empty)) $ jumandicFiltered
  --  2. Setting up private lexicon
  let mylexiconFiltered = filter (\l -> T.isInfixOf (pf l) sentence) LEX.myLexicon
  --  3. Setting up compound nouns (returned from an execution of JUMAN)
  jumanCN <- JU.jumanCompoundNouns (T.replace "―" "、" sentence)
  -- 
  let commonnouns = map (\(hyoki, daihyo) -> lexicalitem hyoki "(CN)" (if hyoki==daihyo then 99 else 90) N (commonNounSR daihyo)) $ M.toList f2
  let propernames = map (\(hyoki, daihyo) -> lexicalitem hyoki "(PN)" (if hyoki==daihyo then 99 else 90) ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (properNameSR daihyo)) $ M.toList f3
  -- | 1+2+3
  let numeration = jumandicParsed ++ mylexiconFiltered ++ commonnouns ++ propernames ++ jumanCN
  return $ numeration `seq` numeration

-- | Read each line in "Juman.dic" and convert it to a CCG lexical item
-- | Meanwhile, common nouns and proper names that have a same phonetic form are to be bundled together into a single word.
parseJumanLine :: ([Node],(M.Map T.Text T.Text,M.Map T.Text T.Text)) -> [T.Text] -> ([Node],(M.Map T.Text T.Text,M.Map T.Text T.Text))
parseJumanLine (lexicalitems, (commonnouns, propernames)) jumanline = 
  case jumanline of
    (hyoki:(score':(cat':(daihyo':(yomi':(source':(caseframe:_))))))) 
      | T.isPrefixOf "名詞:普通名詞" cat' -> 
          let commonnouns' = M.insertWith' (\t1 t2 -> T.intercalate ";" [t1,t2]) hyoki (T.concat [daihyo',"/",yomi']) commonnouns in
          (lexicalitems, (commonnouns', propernames))
      | T.isPrefixOf "名詞:固有名詞" cat' || T.isPrefixOf "名詞:人名" cat' || T.isPrefixOf "名詞:地名" cat' || T.isPrefixOf "名詞:組織名" cat' ->
          let propernames' = M.insertWith' (\t1 t2 -> T.intercalate ";" [t1,t2]) hyoki (T.concat [daihyo',"/",yomi']) propernames in
          (lexicalitems, (commonnouns, propernames'))
      | otherwise ->
          let catsemlist = jumanPos2Cat (T.concat [daihyo',"/",yomi']) cat' caseframe in
          ([(lexicalitem hyoki (T.concat ["(J",T.take 3 source',")"]) (read (T.unpack score')::Integer) cat2 semsig) | (cat2,semsig) <- catsemlist]++lexicalitems, (commonnouns,propernames))
    _ -> (lexicalitems,(commonnouns,propernames))

-- | Main function 1 "jumanPos2Cat" that converts Juman entries to lexical items
jumanPos2Cat :: T.Text -> T.Text -> T.Text -> [(Cat,(Preterm,[Signature]))]
jumanPos2Cat daihyo ct caseframe 
  -- T.isPrefixOf "名詞:普通名詞"     ct  = constructCommonNoun daihyo
  -- T.isPrefixOf "名詞:人名"        ct  = constructProperName daihyo
  -- T.isPrefixOf "名詞:地名"        ct  = constructProperName daihyo
  -- T.isPrefixOf "名詞:組織名"      ct  = constructProperName daihyo
  -- T.isPrefixOf "名詞:固有名詞"     ct  = constructProperName daihyo
  | T.isPrefixOf "名詞:副詞的名詞"   ct  = [((modifiableS `SL` modifiableS) `BS` (defS anyPos [Attr]), (id,[]))]
  | T.isPrefixOf "名詞:時相名詞"     ct  = constructPredicate daihyo [Nda,Nna,Nno,Nni,Nemp] [Stem]
  | T.isPrefixOf "動詞:子音動詞カ行促音便形" ct  = constructVerb daihyo caseframe [V5IKU,V5YUK] [Stem]
  | T.isPrefixOf "動詞:子音動詞カ行"  ct  = constructVerb daihyo caseframe [V5k] [Stem]
  | T.isPrefixOf "動詞:子音動詞サ行"  ct  = constructVerb daihyo caseframe [V5s] [Stem]
  | T.isPrefixOf "動詞:子音動詞タ行"  ct  = constructVerb daihyo caseframe [V5t] [Stem]
  | T.isPrefixOf "動詞:子音動詞ナ行"  ct  = constructVerb daihyo caseframe [V5n] [Stem]
  | T.isPrefixOf "動詞:子音動詞マ行"  ct  = constructVerb daihyo caseframe [V5m] [Stem]
  | T.isPrefixOf "動詞:子音動詞ラ行イ形"     ct  = constructVerb daihyo caseframe [V5NAS] [Stem]
  | T.isPrefixOf "動詞:子音動詞ラ行"         ct  = constructVerb daihyo caseframe [V5r] [Stem]
  | T.isPrefixOf "動詞:子音動詞ワ行文語音便形" ct  = constructVerb daihyo caseframe [V5TOW] [Stem]
  | T.isPrefixOf "動詞:子音動詞ワ行"         ct  = constructVerb daihyo caseframe [V5w] [Stem]
  | T.isPrefixOf "動詞:子音動詞ガ行"  ct  = constructVerb daihyo caseframe [V5g] [Stem]
  | T.isPrefixOf "動詞:子音動詞バ行"  ct  = constructVerb daihyo caseframe [V5b] [Stem]
  | T.isPrefixOf "動詞:母音動詞"     ct  = constructVerb daihyo caseframe [V1] [Stem,Neg,Cont,NegL,EuphT]
  | T.isPrefixOf "動詞:カ変動詞"     ct  = constructVerb daihyo caseframe [VK] [Stem]
  | T.isPrefixOf "名詞:サ変名詞"     ct  = (constructCommonNoun daihyo) ++ (constructVerb daihyo caseframe [VSN,Nda] [Stem]) -- (262)
  | T.isPrefixOf "動詞:サ変動詞"     ct  = constructVerb daihyo caseframe [VS] [Stem]
  | T.isPrefixOf "動詞:ザ変動詞"     ct  = constructVerb daihyo caseframe [VZ] [Stem]
  | T.isPrefixOf "動詞:動詞性接尾辞ます型" ct = constructVerb daihyo caseframe [V5NAS] [Stem]
  | T.isPrefixOf "形容詞:イ形容詞アウオ段" ct = constructPredicate daihyo [Aauo] [Stem]
  | T.isPrefixOf "形容詞:イ形容詞イ段"    ct = constructPredicate daihyo [Ai] [Stem,Term]
  | T.isPrefixOf "形容詞:イ形容詞イ段特殊" ct = constructPredicate daihyo [Ai,Nna] [Stem] -- 大きい
  | T.isPrefixOf "形容詞:ナ形容詞"       ct = constructPredicate daihyo [Nda,Nna,Nni] [Stem]
  | T.isPrefixOf "形容詞:ナ形容詞特殊"    ct = constructPredicate daihyo [Nda,Nna] [Stem] -- 同じ
  | T.isPrefixOf "形容詞:ナノ形容詞"     ct = constructPredicate daihyo [Nda,Nna,Nno] [Stem]
  | T.isPrefixOf "形容詞:タル形容詞"     ct = constructPredicate daihyo [Ntar,Nto] [Stem]
  | T.isPrefixOf "副詞"   ct  = (constructPredicate daihyo [Nda,Nna,Nno,Nni,Nto,Nemp] [Stem]) ++ (constructCommonNoun daihyo)
  | T.isPrefixOf "連体詞" ct  = [(N `SL` N, modifierSR daihyo)]
  | T.isPrefixOf "接続詞" ct = [((T False 1 (S [F anyPos, F[Term,Pre,Imper], SF 2 [P,M], SF 3 [P,M], SF 4 [P,M], F[M], F[M]])) `SL` T False 1 (S [F anyPos, F[Term,Pre,Imper], SF 2 [P,M], SF 3 [P,M], SF 4 [P,M], F[M], F[M]]), (id, []))]
  | T.isPrefixOf "接頭辞:名詞接頭辞" ct   = [(N `SL` N, modifierSR daihyo)]
  | T.isPrefixOf "接頭辞:動詞接頭辞" ct   = [((T False 1 (defS verb [Stem]) `SL` (T False 1 (defS verb [Stem]))), ((Lam (Lam (App (Var 1) (Lam (Sigma (App (Con daihyo) (Var 0)) (App (Var 2) (Var 1))))))), [(daihyo, nPlacePredType 1)]))]
  | T.isPrefixOf "接頭辞:イ形容詞接頭辞"  ct   = [((defS [Aauo] [Stem] `BS` NP [F[Ga]]) `SL` (defS [Aauo] [Stem] `BS` NP [F[Ga]]), (id, []))]
  | T.isPrefixOf "接頭辞:ナ形容詞接頭辞"  ct   = [((defS [Nda] [Stem] `BS` NP [F[Ga]]) `SL` (defS [Nda] [Stem] `BS` NP [F[Ga]]), (id, []))]
  | T.isPrefixOf "接尾辞:名詞性名詞助数辞" ct  = constructCommonNoun daihyo -- 例：ビット、ヘクトパスカル
  | T.isPrefixOf "接尾辞:名詞性名詞接尾辞" ct  = [(N `BS` N, modifierSR daihyo)]
  | T.isPrefixOf "接尾辞:名詞性特殊接尾辞" ct  = constructCommonNoun daihyo
  | T.isPrefixOf "接尾辞:名詞性述語接尾辞" ct  = constructCommonNoun daihyo
  --  T.isPrefixOf "特殊:句点" ct =
  --  T.isPrefixOf "特殊:読点" ct =
  | T.isPrefixOf "特殊:括弧始" ct = [(LPAREN, (Unit, []))]
  | T.isPrefixOf "特殊:括弧終" ct = [(RPAREN, (Unit, []))]
  | T.isPrefixOf "数詞"       ct = constructCommonNoun daihyo
  | T.isPrefixOf "感動詞"     ct  = [(defS [Exp] [Term], (id, []))]
  | otherwise                    = [(defS [Exp] [Term], ((Con $ T.concat [T.pack "Juman Error: ", ct]), []))]

--constructProperName :: T.Text -> [(Cat, (Preterm, [Signature]))]
--constructProperName daihyo = [((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])), properNameSR daihyo)]

constructPredicate :: T.Text -> [FeatureValue] -> [FeatureValue] -> [(Cat, (Preterm, [Signature]))]
constructPredicate daihyo posF conjF = [(defS posF conjF `BS` NP [F[Ga]], predSR 1 daihyo)]

constructCommonNoun :: T.Text -> [(Cat, (Preterm, [Signature]))]
constructCommonNoun daihyo = [(N, commonNounSR daihyo)]

constructVerb :: T.Text -> T.Text -> [FeatureValue] -> [FeatureValue] -> [(Cat, (Preterm, [Signature]))]
constructVerb daihyo caseframe posF conjF =
  if caseframe == T.empty
    then [(defS posF conjF `BS` NP [F[Ga]], verbSR 1 daihyo)]
    else let caseframelist = map (T.split (==',')) $ T.split (=='#') caseframe in
         [(verbCat cf (defS posF conjF), (verbSR (length cf) daihyo)) | cf <- caseframelist]

verbCat :: [T.Text] -> Cat -> Cat
verbCat cf ct = case cf of
  [] -> ct
  (c:cs) | c == "ガ格" -> (verbCat cs ct) `BS` NP [F[Ga]]
         | c == "ヲ格" -> (verbCat cs ct) `BS` NP [F[O]]
         | c == "ニ格" -> (verbCat cs ct) `BS` NP [F[Ni]]
         --  c == "ト格" -> (verbCat cs ct) `BS` NP [F[To]]
         | c == "ト節" -> (verbCat cs ct) `BS` Sbar [F[ToCL]]
         | c == "によって" -> (verbCat cs ct) `BS` NP [F[Niyotte]]
         | otherwise -> (verbCat cs ct)
