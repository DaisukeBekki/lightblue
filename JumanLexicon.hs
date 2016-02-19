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
import qualified Data.Maybe as M
import qualified Data.Ratio as R
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
  return $ (M.mapMaybe parseJumanLine $ filter (\l -> T.isInfixOf (head l) sentence) $ map (T.split (=='|')) (T.lines jumandic))
           ++ (filter (\l -> T.isInfixOf (pf l) sentence) LEX.myLexicon)
           ++ jumanCN
  where 
    -- | parseJumanLine :: [T.Text] -> Maybe Node
    --   read each line in "Juman.dic" and convert it to a CCG lexical item
    parseJumanLine jumanline = 
      case jumanline of
        (hyoki:(score':(cat':(daihyo':_)))) -> 
          let (cat2,sem2) = jumanPos2Cat daihyo' cat' in
          Just (lexicalitem hyoki "(JU)" (tRatio score') cat2 sem2) --(T.concat [daihyo',":",cat']))
        _ -> Nothing
        where tRatio x = (110 - (read (T.unpack x)::Integer)) R.% 100

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
nonStem = [Neg, NegL, Cont, Term, Attr, Hyp, Imper, Pre, ModU, ModD, ModS, VoR, VoS, VoE, TeForm]

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
-- 
lexicalitem :: T.Text -> T.Text -> Rational -> Cat -> Preterm -> Node
lexicalitem word m r c s = Node {rs=LEX, pf=word, cat=c, sem=s, daughters=[], score=r, memo=m}


-- | Main function 1 "jumanPos2Cat" that converts Juman entries to lexical items
-- 
jumanPos2Cat :: T.Text -> T.Text -> (Cat,Preterm)
jumanPos2Cat daihyo t 
  | T.isPrefixOf "名詞:副詞的名詞" t  = (((T 1 anySExStem) `SL` (T 1 anySExStem)) `BS` (S anyPos [Attr]), (Lam (Var 0)))
  | T.isPrefixOf "名詞:普通名詞" t    = (N, Lam (App (Con daihyo) (Var 0)))  
  | T.isPrefixOf "名詞:サ変名詞" t    = (((S [VS] [Stem]) `BS` (NP [Ga])) `BS` (NP [O]), (Lam (Lam (App (Con daihyo) (Pair (Var 0) (Var 1))))))
  | T.isPrefixOf "名詞:人名"    t    = ((SL (T 1 anySExStem) (BS (T 1 anySExStem) (NP [Nc]))), (Lam (App (Var 0) (Con daihyo))))
  | T.isPrefixOf "名詞:地名"    t    = ((NP [Nc]), (Con daihyo))
  | T.isPrefixOf "名詞:組織名"  t    = ((NP [Nc]), (Con daihyo))
  | T.isPrefixOf "名詞:時相名詞" t    = (S [Nda,Nna,Nno] [Stem] `BS` NP [Ga], (Con daihyo))
  | T.isPrefixOf "動詞:母音動詞"  t   = (BS (BS (S [V1] [Stem]) (NP [Ga])) (NP [O]), 
                                      (Lam (Lam (App (Con daihyo) (Pair (Var 0) (Var 1))))))
  | T.isPrefixOf "動詞:子音動詞カ行"  t  = (BS (BS (S [V5k] [Stem]) (NP [Ga])) (NP [Ni,O]), 
                                        (Lam (Lam (App (Con daihyo) (Pair (Var 0) (Var 1))))))
  | T.isPrefixOf "動詞:子音動詞サ行"  t  = (BS (BS (S [V5s] [Stem]) (NP [Ga])) (NP [Ni,O]), 
                                        (Lam (Lam (App (Con daihyo) (Pair (Var 0) (Var 1))))))
  | T.isPrefixOf "動詞:子音動詞タ行"  t  = (BS (BS (S [V5t] [Stem]) (NP [Ga])) (NP [Ni,O]), 
                                        (Lam (Lam (App (Con daihyo) (Pair (Var 0) (Var 1))))))
  | T.isPrefixOf "動詞:子音動詞ナ行"  t  = (BS (BS (S [V5n] [Stem]) (NP [Ga])) (NP [Ni,O]), 
                                        (Lam (Lam (App (Con daihyo) (Pair (Var 0) (Var 1))))))
  | T.isPrefixOf "動詞:子音動詞マ行"  t  = (BS (BS (S [V5m] [Stem]) (NP [Ga])) (NP [Ni,O]), 
                                        (Lam (Lam (App (Con daihyo) (Pair (Var 0) (Var 1))))))
  | T.isPrefixOf "動詞:子音動詞ラ行"  t  = (BS (BS (S [V5r] [Stem]) (NP [Ga])) (NP [Ni,O]), 
                                        (Lam (Lam (App (Con daihyo) (Pair (Var 0) (Var 1))))))
  | T.isPrefixOf "動詞:子音動詞ワ行"  t  = (BS (BS (S [V5w] [Stem]) (NP [Ga])) (NP [Ni,O]), 
                                        (Lam (Lam (App (Con daihyo) (Pair (Var 0) (Var 1))))))
  | T.isPrefixOf "動詞:子音動詞ガ行"  t  = (BS (BS (S [V5g] [Stem]) (NP [Ga])) (NP [Ni,O]), 
                                        (Lam (Lam (App (Con daihyo) (Pair (Var 0) (Var 1))))))
  | T.isPrefixOf "動詞:子音動詞バ行"  t  = (BS (BS (S [V5b] [Stem]) (NP [Ga])) (NP [Ni,O]), 
                                        (Lam (Lam (App (Con daihyo) (Pair (Var 0) (Var 1))))))
  | T.isPrefixOf "動詞:カ変動詞"  t  = (BS (BS (S [VK] [Stem]) (NP [Ga])) (NP [Ni,O]), 
                                     (Lam (Lam (App (Con daihyo) (Pair (Var 0) (Var 1))))))
  | T.isPrefixOf "動詞:サ変動詞"  t  = (BS (BS (S [VS] [Stem]) (NP [Ga])) (NP [Ni,O]), 
                                     (Lam (Lam (App (Con daihyo) (Pair (Var 0) (Var 1))))))
  | T.isPrefixOf "動詞:ザ変動詞"  t  = (BS (BS (S [VZ] [Stem]) (NP [Ga])) (NP [Ni,O]), 
                                     (Lam (Lam (App (Con daihyo) (Pair (Var 0) (Var 1))))))
  | T.isPrefixOf "形容詞:イ形容詞アウオ段" t = (BS (S [Aauo] [Stem]) (NP [Ga]), 
                                     (Lam (App (Con daihyo) (Var 0))))
  | T.isPrefixOf "形容詞:イ形容詞イ段" t = (BS (S [Ai] [Stem]) (NP [Ga]), 
                                     (Lam (App (Con daihyo) (Var 0))))
  | T.isPrefixOf "形容詞:ナ形容詞" t = (BS (S [Nda,Nna,Nni] [Stem]) (NP [Ga]), 
                                     (Lam (App (Con daihyo) (Var 0))))
  | T.isPrefixOf "形容詞:ナノ形容詞" t = (BS (S [Nda,Nna,Nno] [Stem]) (NP [Ga]), 
                                      (Lam (App (Con daihyo) (Var 0))))
  | T.isPrefixOf "副詞"  t  = ((anySExStem `SL` anySExStem), 
                             (Lam (App (Con daihyo) (Var 0))))
  | T.isPrefixOf "連体詞" t  = (N `SL` N, 
                              (Lam (Lam (Sigma (App (Var 2) (Var 1)) (App (Con daihyo) (Var 1))))))
  | T.isPrefixOf "接続詞" t = (((T 1 anySExStem) `SL` (T 1 anySExStem)) `BS` (S anyPos [Term]), 
                             (Lam (Lam (Sigma (Var 1) (Var 1)))))
  | T.isPrefixOf "接頭辞:名詞" t   = (N `SL` N, 
                                   (Lam (Lam (Sigma (App (Var 1) (Var 0)) (App (Con daihyo) (Var 1))))))
  | T.isPrefixOf "接頭辞:イ形容詞" t   = (((S [Aauo] [Stem]) `BS` (NP [Ga])) `SL` ((S [Aauo] [Stem]) `BS` (NP [Ga])), 
                                       id)
  | T.isPrefixOf "接頭辞:ナ形容詞" t   = (((S [Nda] [Stem]) `BS` (NP [Ga])) `SL` ((S [Nda] [Stem]) `BS` (NP [Ga])), 
                                       id)
  -- T.isPrefixOf "接尾辞" t   = (BS (T 1 anySExStem) (T 1 anySExStem), id)
  -- T.isPrefixOf "感動詞" t   = (S [Exp] [Term], id)
  | otherwise      = (S [Error] [Term], (Con $ T.concat [T.pack "Juman Error: ", t]))

