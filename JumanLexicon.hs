{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures #-}

module JumanLexicon (
  --Node(..),
  Lexicon,
  --isCONJ,
  lookupLexicon,
  setupLexicon,
  LEX.emptyCategories
  ) where

import Prelude hiding (id)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Ratio as R
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

{- Some Marcos for CCG categories/features and DTS -}
id :: Preterm
id = Lam (Var 0)

verb :: [CatPos]
verb = [V5k, V5s, V5t, V5n, V5m, V5r, V5w, V5g, V5z, V5b, V5IKU, V5YUK, V5ARU, V5NAS, V5TOW, V1, VK, VS, VZ, VURU]

adjective :: [CatPos]
adjective = [Aauo, Ai, ANAS, ATII, ABES]

nomPred :: [CatPos]
nomPred = [Nda, Nna, Nno, Nni, Nemp, Ntar]

anyPos :: [CatPos]
anyPos = verb ++ adjective ++ nomPred ++ [Exp]

nonStem :: [CatConj]
nonStem = [Neg, NegL, Cont, Term, Attr, Hyp, Imper, Pre, TeForm]

{-
--anyConj :: [CatConj]
--anyConj = [Stem, UStem, Neg, Cont, Term, Attr, Hyp, Imper, Pre, EuphT, EuphD, ModU, ModD, ModS, VoR, VoS, VoE, TeForm, NiForm, Yooni]

--anyCase :: [CatCase] 
--anyCase = [Nc, Ga, O, Ni, To, Niyotte, No]

--anyS :: Cat
--anyS = S anyPos anyConj
-}

anySExStem :: Cat
anySExStem = S anyPos nonStem

-- | 語彙項目定義用マクロ
lexicalitem :: T.Text -> T.Text -> Integer -> Cat -> Preterm -> Node
lexicalitem word m r c s = Node {rs=LEX, pf=word, cat=c, sem=s, daughters=[], score=(r R.% 100), memo=m}


-- | Main function 1 "jumanPos2Cat" that converts Juman entries to lexical items
jumanPos2Cat :: T.Text -> T.Text -> T.Text -> [(Cat,Preterm)]
jumanPos2Cat daihyo ct caseframe 
  | T.isPrefixOf "名詞:普通名詞" ct    = [(N, Lam (App (Con daihyo) (Var 0)))]  
  | T.isPrefixOf "名詞:人名"    ct    = [((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Nc])), (Lam (App (Var 0) (Con daihyo))))]
  | T.isPrefixOf "名詞:地名"    ct    = [((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Nc])), (Lam (App (Var 0) (Con daihyo))))]
  | T.isPrefixOf "名詞:組織名"  ct    = [((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Nc])), (Lam (App (Var 0) (Con daihyo))))]
  | T.isPrefixOf "名詞:副詞的名詞" ct  = [(((T 1 anySExStem) `SL` (T 1 anySExStem)) `BS` (S anyPos [Attr]), (Lam (Var 0)))]
  | T.isPrefixOf "名詞:時相名詞" ct      = constructPredicate daihyo [Nda,Nna,Nno]
  | T.isPrefixOf "動詞:子音動詞カ行"  ct  = constructVerb daihyo caseframe [V5k]
  | T.isPrefixOf "動詞:子音動詞サ行"  ct  = constructVerb daihyo caseframe [V5s]
  | T.isPrefixOf "動詞:子音動詞タ行"  ct  = constructVerb daihyo caseframe [V5t]
  | T.isPrefixOf "動詞:子音動詞ナ行"  ct  = constructVerb daihyo caseframe [V5n]
  | T.isPrefixOf "動詞:子音動詞マ行"  ct  = constructVerb daihyo caseframe [V5m]
  | T.isPrefixOf "動詞:子音動詞ラ行"  ct  = constructVerb daihyo caseframe [V5r]
  | T.isPrefixOf "動詞:子音動詞ワ行"  ct  = constructVerb daihyo caseframe [V5w]
  | T.isPrefixOf "動詞:子音動詞ガ行"  ct  = constructVerb daihyo caseframe [V5g]
  | T.isPrefixOf "動詞:子音動詞バ行"  ct  = constructVerb daihyo caseframe [V5b]
  | T.isPrefixOf "動詞:母音動詞"     ct  = constructVerb daihyo caseframe [V1]
  | T.isPrefixOf "動詞:カ変動詞"     ct  = constructVerb daihyo caseframe [VK]
  | T.isPrefixOf "名詞:サ変名詞"     ct  = constructVerb daihyo caseframe [VS]
  | T.isPrefixOf "動詞:サ変動詞"     ct  = constructVerb daihyo caseframe [VS]
  | T.isPrefixOf "動詞:ザ変動詞"     ct  = constructVerb daihyo caseframe [VZ]
  | T.isPrefixOf "形容詞:イ形容詞アウオ段" ct = constructPredicate daihyo [Aauo]
  | T.isPrefixOf "形容詞:イ形容詞イ段"    ct = constructPredicate daihyo [Ai]
  | T.isPrefixOf "形容詞:ナ形容詞"       ct = constructPredicate daihyo [Nda,Nna,Nni]
  | T.isPrefixOf "形容詞:ナノ形容詞"     ct = constructPredicate daihyo [Nda,Nna,Nno]
  | T.isPrefixOf "副詞"  ct  = [((anySExStem `SL` anySExStem), (Lam (App (Con daihyo) (Var 0))))]
  | T.isPrefixOf "連体詞" ct  = [(N `SL` N, (Lam (Lam (Sigma (App (Var 1) (Var 0)) (App (Con daihyo) (Var 0))))))]
  | T.isPrefixOf "接続詞" ct = [((T 1 anySExStem `SL` T 1 anySExStem), id)]
  | T.isPrefixOf "接頭辞:イ形容詞" ct   = [((S [Aauo] [Stem] `BS` NP [Ga]) `SL` (S [Aauo] [Stem] `BS` NP [Ga]), id)]
  | T.isPrefixOf "接頭辞:ナ形容詞" ct   = [((S [Nda] [Stem] `BS` NP [Ga]) `SL` (S [Nda] [Stem] `BS` NP [Ga]), id)]
  -- T.isPrefixOf "接頭辞:名詞" ct   = [(N `SL` N, (Lam (Lam (Sigma (App (Var 1) (Var 0)) (App (Con daihyo) (Var 1))))))]
  -- T.isPrefixOf "接尾辞" t   = [(BS (T 1 anySExStem) (T 1 anySExStem), id)]
  -- T.isPrefixOf "感動詞" t   = [(S [Exp] [Term], id)]
  | otherwise      = [(S [Error] [Term], (Con $ T.concat [T.pack "Juman Error: ", ct]))]

constructPredicate :: T.Text -> [CatPos] -> [(Cat,Preterm)]
constructPredicate daihyo cpos =
  [(S cpos [Stem] `BS` NP [Ga], (Lam (App (Con daihyo) (Var 0))))]

constructVerb :: T.Text -> T.Text -> [CatPos] -> [(Cat, Preterm)]
constructVerb daihyo caseframe cpos =
  if caseframe == T.empty
    then [((S cpos [Stem] `BS` NP [Ga]) `BS` NP [Ni,O], (Lam (Lam (App (App (Con daihyo) (Var 0)) (Var 1)))))] 
    else let caseframelist = map (T.split (==',')) $ T.split (=='#') caseframe in
         [(f cf (S cpos [Stem]), g daihyo (length cf)) | cf <- caseframelist]

f :: [T.Text] -> Cat -> Cat
f cf ct = case cf of
  [] -> ct
  (c:cs) | c == "ガ格" -> (f cs ct) `BS` NP [Ga]
         | c == "ヲ格" -> (f cs ct) `BS` NP [O]
         | c == "ニ格" -> (f cs ct) `BS` NP [Ni]
         | c == "ト格" -> (f cs ct) `BS` NP [To]
         | c == "によって" -> (f cs ct) `BS` NP [Niyotte]
         | otherwise -> (f cs ct)

g :: T.Text -> Int -> Preterm
g daihyo i | i == 1 = (Lam (App (Con daihyo) (Var 0)))
           | i == 2 = (Lam (Lam (App (App (Con daihyo) (Var 0)) (Var 1))))
           | i == 3 = (Lam (Lam (Lam (App (App (App (Con daihyo) (Var 0)) (Var 1)) (Var 2)))))
           --  i == 4 = (Lam (Lam (Lam (Lam (App (App (App (App (Con daihyo) (Var 0)) (Var 1)) (Var 2)) (Var 3))))))
           | otherwise = Con $ T.concat ["Error: verb ",daihyo," of ", T.pack (show i), " arguments"]