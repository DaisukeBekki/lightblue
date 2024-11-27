{-# LANGUAGE DeriveGeneric, DefaultSignatures, RecordWildCards #-}

{-|
Module      : Parser.Language.Japanese.Lexicon
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

A Japanese CCG lexicon.
-}
module Parser.Language.Japanese.Lexicon (
  --Node(..),
  LexicalItems
  , LexicalResource(..)
  , lexicalResourceBuilder
  --isCONJ,
  , lookupLexicon
  , setupLexicon
  , LEX.emptyCategories
  , LEX.myLexicon
  ) where

import Prelude hiding (id)
import qualified System.Environment as E --base
import qualified Data.Text.Lazy as T     --text
import qualified Data.Text.Lazy.IO as T --text
import qualified Data.List as L          -- base
import qualified Data.Map as M           -- base
import Parser.CCG
import qualified Parser.Language.Japanese.MyLexicon as LEX
import Parser.Language.Japanese.Templates
import qualified Parser.Language.Japanese.Juman.CallJuman as JU
import DTS.UDTTdeBruijn as UDTT         --lightblue
import qualified DTS.DTTdeBruijn as DTT --lightblue

type UDTTpreterm = UDTT.Preterm
type Signature = DTT.Signature

terminator :: UDTT.Preterm
terminator = UDTT.Ann (UDTT.Lam UDTT.Top) (DTT.Pi DTT.Entity DTT.Type)

-- | Lexicon consists of a set of CCG Nodes
type LexicalItems = [Node]

data LexicalResource = 
  JapaneseSetA {
    baseLexicon :: [Node]
    , jumanDic :: [[T.Text]]
    , morphaName :: JU.MorphAnalyzerName
  } deriving (Eq, Show)

lexicalResourceBuilder :: JU.MorphAnalyzerName -> IO LexicalResource
lexicalResourceBuilder morphaName = do
  lightbluepath <- E.getEnv "LIGHTBLUE"
  jumandicData <- T.readFile $ lightbluepath ++ "src/Parser/Language/Japanese/Juman/Juman.dic"
  let jumanDic = map (T.split (=='\t')) $ T.lines jumandicData
  return $ JapaneseSetA LEX.myLexicon jumanDic morphaName

-- | This function takes a word and a lexicon and returns a set of CCG lexical entries whose PF is that word.
lookupLexicon :: T.Text -> LexicalItems -> [Node]
lookupLexicon word lexicon = filter (\l -> (pf l) == word) lexicon

-- | This function takes a sentence and returns a numeration needed to parse that sentence, i.e., a union of 
setupLexicon :: LexicalResource -> T.Text -> IO LexicalItems
setupLexicon JapaneseSetA{..} sentence = do
  --  1. Setting up lexical items provided by JUMAN++
  let jumandicFiltered = filter (\l -> (head l) `T.isInfixOf` sentence) jumanDic
  let (jumandicParsed,(cn,pn)) = L.foldl' parseJumanLine ([],(M.empty,M.empty)) $ jumandicFiltered
  --  2. Setting up private lexicon
  let mylexiconFiltered = filter (\l -> T.isInfixOf (pf l) sentence) baseLexicon
  --  3. Setting up compound nouns (returned from an execution of JUMAN)
  -- jumanCN <- JU.jumanCompoundNouns (T.replace "―" "、" sentence)
  jumanCN <- JU.compoundNouns morphaName sentence
  --  4. Accumulating common nons and proper names entries
  let commonnouns = map (\(hyoki, (daihyo,score')) -> lexicalitem hyoki "(CN)" score' N (commonNounSR daihyo)) $ M.toList cn
  let propernames = map (\(hyoki, (daihyo,score')) -> lexicalitem hyoki "(PN)" score' ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (properNameSR daihyo)) $ M.toList pn
  --  5. 1+2+3+4
  let numeration = jumandicParsed ++ mylexiconFiltered ++ commonnouns ++ propernames ++ jumanCN
  return $ numeration `seq` numeration

-- | Read each line in "Juman.dic" and convert it to a CCG lexical item
-- | Meanwhile, common nouns and proper names that have a same phonetic form are to be bundled together into a single word.
parseJumanLine :: ([Node],(M.Map T.Text (T.Text,Integer), M.Map T.Text (T.Text,Integer))) 
                  -> [T.Text]  -- ^ A line in Juman dictionary, filtered.
                  -> ([Node],(M.Map T.Text (T.Text,Integer),M.Map T.Text (T.Text,Integer)))
parseJumanLine (lexicalitems, (commonnouns, propernames)) jumanline = 
  case jumanline of
    (hyoki:(score':(cat':(daihyo':(yomi':(source':(caseframe:_))))))) 
      | T.isPrefixOf "名詞:普通名詞" cat' -> 
          let commonnouns' = M.insertWith (\(t1,s1) (t2,s2) -> (T.intercalate ";" [t1,t2], max s1 s2)) hyoki ((T.concat [daihyo',"/",yomi']),(read (T.unpack score')::Integer)) commonnouns in
          (lexicalitems, (commonnouns', propernames))
      | T.isPrefixOf "名詞:固有名詞" cat' || T.isPrefixOf "名詞:人名" cat' || T.isPrefixOf "名詞:地名" cat' || T.isPrefixOf "名詞:組織名" cat' ->
          let propernames' = M.insertWith (\(t1,s1) (t2,s2) -> (T.intercalate ";" [t1,t2], max s1 s2)) hyoki ((T.concat [daihyo',"/",yomi']),(read (T.unpack score')::Integer)) propernames in
          (lexicalitems, (commonnouns, propernames'))
      | otherwise ->
          let catsemlist = jumanPos2Cat (T.concat [daihyo',"/",yomi']) cat' caseframe in
          ([(lexicalitem hyoki (T.concat ["(J",T.take 3 source',")"]) (read (T.unpack score')::Integer) cat2 semsig) | (cat2,semsig) <- catsemlist]++lexicalitems, (commonnouns,propernames))
    _ -> (lexicalitems,(commonnouns,propernames))

-- | Main function 1 "jumanPos2Cat" that converts Juman entries to lexical items
jumanPos2Cat :: T.Text -> T.Text -> T.Text -> [(Cat,(UDTTpreterm,Signature))]
jumanPos2Cat daihyo ct caseframe 
  -- T.isPrefixOf "名詞:普通名詞"     ct  = constructCommonNoun daihyo
  -- T.isPrefixOf "名詞:人名"        ct  = constructProperName daihyo
  -- T.isPrefixOf "名詞:地名"        ct  = constructProperName daihyo
  -- T.isPrefixOf "名詞:組織名"      ct  = constructProperName daihyo
  -- T.isPrefixOf "名詞:固有名詞"     ct  = constructProperName daihyo
  | T.isPrefixOf "名詞:副詞的名詞"         ct  = constructSubordinateConjunction daihyo
    --constructPredicate daihyo [Nda,Nno,Nni,Nemp] [NStem]
  | T.isPrefixOf "名詞:時相名詞"           ct  = constructPredicate daihyo [Nda,Nna,Nno,Nni,Nemp] [NStem]
  | T.isPrefixOf "動詞:子音動詞カ行促音便形"  ct  = constructVerb daihyo caseframe [V5IKU,V5YUK] [Stem]
  | T.isPrefixOf "動詞:子音動詞カ行"        ct  = constructVerb daihyo caseframe [V5k] [Stem]
  | T.isPrefixOf "動詞:子音動詞サ行"        ct  = constructVerb daihyo caseframe [V5s] [Stem]
  | T.isPrefixOf "動詞:子音動詞タ行"        ct  = constructVerb daihyo caseframe [V5t] [Stem]
  | T.isPrefixOf "動詞:子音動詞ナ行"        ct  = constructVerb daihyo caseframe [V5n] [Stem]
  | T.isPrefixOf "動詞:子音動詞マ行"        ct  = constructVerb daihyo caseframe [V5m] [Stem]
  | T.isPrefixOf "動詞:子音動詞ラ行イ形"     ct  = constructVerb daihyo caseframe [V5NAS] [Stem]
  | T.isPrefixOf "動詞:子音動詞ラ行"         ct  = constructVerb daihyo caseframe [V5r] [Stem]
  | T.isPrefixOf "動詞:子音動詞ワ行文語音便形" ct  = constructVerb daihyo caseframe [V5TOW] [Stem]
  | T.isPrefixOf "動詞:子音動詞ワ行"         ct  = constructVerb daihyo caseframe [V5w] [Stem]
  | T.isPrefixOf "動詞:子音動詞ガ行"         ct  = constructVerb daihyo caseframe [V5g] [Stem]
  | T.isPrefixOf "動詞:子音動詞バ行"         ct  = constructVerb daihyo caseframe [V5b] [Stem]
  | T.isPrefixOf "動詞:母音動詞"            ct  = constructVerb daihyo caseframe [V1] [Stem,Neg,Cont,NegL,EuphT]
  | T.isPrefixOf "動詞:カ変動詞"            ct  = constructVerb daihyo caseframe [VK] [Stem]
  | T.isPrefixOf "名詞:サ変名詞"            ct  = ((constructCommonNoun daihyo) ++ (constructVerb daihyo caseframe [VS,VSN] [Stem]) 
                                             ++ (constructPredicate daihyo [Nda,Ntar] [NStem])) -- (262)
  | T.isPrefixOf "動詞:サ変動詞"            ct  = constructVerb daihyo caseframe [VS] [Stem]
  | T.isPrefixOf "動詞:ザ変動詞"            ct  = constructVerb daihyo caseframe [VZ] [Stem]
  | T.isPrefixOf "動詞:動詞性接尾辞ます型"    ct = constructVerb daihyo caseframe [V5NAS] [Stem]
  | T.isPrefixOf "形容詞:イ形容詞アウオ段"    ct = constructPredicate daihyo [Aauo] [Stem]
  | T.isPrefixOf "形容詞:イ形容詞イ段"       ct = constructPredicate daihyo [Ai] [Stem,Term]
  | T.isPrefixOf "形容詞:イ形容詞イ段特殊"    ct = constructPredicate daihyo [Ai,Nna] [Stem] -- 大きい
  | T.isPrefixOf "形容詞:ナ形容詞"          ct = constructPredicate daihyo [Nda,Nna,Nni] [NStem]
  | T.isPrefixOf "形容詞:ナ形容詞特殊"       ct = constructPredicate daihyo [Nda,Nna] [NStem] -- 同じ
  | T.isPrefixOf "形容詞:ナノ形容詞"        ct = constructPredicate daihyo [Nda,Nna,Nno,Nni] [NStem]
  | T.isPrefixOf "形容詞:タル形容詞"        ct = constructPredicate daihyo [Ntar,Nto] [Stem]
  | T.isPrefixOf "副詞"                  ct  = ((constructPredicate daihyo [Nda,Nna,Nno,Nni,Nto,Nemp] [NStem]) 
                                            ++ (constructCommonNoun daihyo))
  | T.isPrefixOf "連体詞"                 ct  = constructNominalPrefix daihyo
  | T.isPrefixOf "接続詞"                 ct = constructConjunction daihyo
  | T.isPrefixOf "接頭辞:名詞接頭辞"        ct   = constructNominalPrefix daihyo
  | T.isPrefixOf "接頭辞:動詞接頭辞"        ct   = [((defS verb [Stem] `SL` defS verb [Stem]), ((Lam (Lam (App (Var 1) (Lam (Sigma (App (Con daihyo) (Var 0)) (App (Var 2) (Var 1))))))), [(daihyo, nPlacePredType 1)]))]
  | T.isPrefixOf "接頭辞:イ形容詞接頭辞"     ct   = [((defS [Aauo] [Stem] `BS` NP [F[Ga]]) `SL` (defS [Aauo] [Stem] `BS` NP [F[Ga]]), (id, []))]
  | T.isPrefixOf "接頭辞:ナ形容詞接頭辞"     ct   = [((defS [Nda] [NStem] `BS` NP [F[Ga]]) `SL` (defS [Nda] [NStem] `BS` NP [F[Ga]]), (id, []))]
  | T.isPrefixOf "接尾辞:名詞性名詞助数辞"   ct  = constructNominalSuffix daihyo -- 例：ビット、ヘクトパスカル
  -- T.isPrefixOf "接尾辞:名詞性名詞接尾辞"  ct  = constructNominalSuffix daihyo
  | T.isPrefixOf "接尾辞:名詞性特殊接尾辞"   ct  = constructNominalSuffix daihyo
  | T.isPrefixOf "接尾辞:名詞性述語接尾辞"   ct  = constructNominalSuffix daihyo
  --  T.isPrefixOf "特殊:句点" ct =
  --  T.isPrefixOf "特殊:読点" ct =
  | T.isPrefixOf "特殊:括弧始"             ct = [(LPAREN, (Unit, []))]
  | T.isPrefixOf "特殊:括弧終"             ct = [(RPAREN, (Unit, []))]
  | T.isPrefixOf "数詞"                   ct = constructCommonNoun daihyo
  | T.isPrefixOf "感動詞"                 ct  = [(defS [Exp] [Term], (id, []))]
  | otherwise                                = [(defS [Exp] [Term], ((Con $ T.concat [T.pack "Juman Error: ", ct]), []))]

--constructProperName :: T.Text -> [(Cat, (UDTTpreterm, Signature))]
--constructProperName daihyo = [((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])), properNameSR daihyo)]

constructPredicate :: T.Text -> [FeatureValue] -> [FeatureValue] -> [(Cat, (UDTTpreterm, Signature))]
constructPredicate daihyo posF conjF = [(defS posF conjF `BS` NP [F[Ga]], predSR 1 daihyo)]

constructCommonNoun :: T.Text -> [(Cat, (UDTTpreterm, Signature))]
constructCommonNoun daihyo = [(N, commonNounSR daihyo)]

constructVerb :: T.Text -> T.Text -> [FeatureValue] -> [FeatureValue] -> [(Cat, (UDTTpreterm, Signature))]
constructVerb daihyo caseframe posF conjF =
  let caseframe' = if caseframe == T.empty
                     then "ガ"
                     else caseframe;
      caseframelist = T.split (=='#') caseframe' in
  [(verbCat cf posF conjF, verbSR daihyo event cf) | cf <- caseframelist]

constructNominalPrefix :: T.Text -> [(Cat, (UDTTpreterm, Signature))]
constructNominalPrefix daihyo = [(N `SL` N, nominalModifier daihyo)]

constructNominalSuffix :: T.Text -> [(Cat, (UDTTpreterm, Signature))]
constructNominalSuffix daihyo = [(N `BS` N, nominalModifier daihyo)]

constructConjunction :: T.Text -> [(Cat, (UDTTpreterm, Signature))]
constructConjunction daihyo = 
  [
  (((T False 1 (S [F anyPos, F[Term,NTerm,Pre,Imper], SF 2 [P,M], SF 3 [P,M], SF 4 [P,M], F[M], F[M]]))
    `SL` (T False 1 (S [F anyPos, F[Term,NTerm,Pre,Imper], SF 2 [P,M], SF 3 [P,M], SF 4 [P,M], F[M], F[M]]))), 
    ((Lam (Lam (Sigma (App (Var 1) terminator) ((App (App (Con daihyo) (Proj Snd $ Asp (Sigma Type (Var 0)))) (Var 0)))))), [(daihyo, DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type))]))
    ]

-- | S/S\S: λp.λq.λk.q（λe1.(p(λe2.c(e1,e2)))×k(e1))
constructSubordinateConjunction :: T.Text -> [(Cat, (UDTTpreterm, Signature))]
constructSubordinateConjunction daihyo = 
  [((modifiableS `SL` modifiableS) `BS` (S [F anyPos, F[Attr], SF 7 [P,M], SF 8 [P,M], SF 9 [P,M], F[M],F[M] ]), 
    (Lam (Lam (Lam (App (Var 1) (Lam (Sigma (App (Var 3) (Lam (App (App (Con daihyo) (Var 0)) (Var 1)))) (App (Var 2) (Var 1))))))),
    --((Lam (Lam (Lam (Sigma (App (Var 2) (Lam Top)) (Sigma (App (Var 2) (Var 1)) (App (App (Con daihyo) (Var 1)) (Var 0))))))),
     [(daihyo, DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type))]))
  ]
