{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures #-}

module MyLexicon (
  emptyCategories,
  myLexicon
  ) where 

import Prelude hiding (id)
import qualified Data.Text.Lazy as T
import Data.Ratio as R
import CombinatoryCategorialGrammar
import DependentTypes

-- | Some Marcos for CCG categories/features and DTS
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
nonStem = [Neg, Cont, Term, Attr, Hyp, Imper, Pre, ModU, ModS, VoR, VoS, VoE, NegL, TeForm]

anySExStem :: Cat
anySExStem = S anyPos nonStem

--anyConj :: [CatConj]
--anyConj = [Stem, UStem, Neg, Cont, Term, Attr, Hyp, Imper, Pre, EuphT, EuphD, ModU, ModS, VoR, VoS, VoE, TeForm, NiForm, Yooni]

--anyCase :: [CatCase] 
--anyCase = [Nc, Ga, O, Ni, To, Niyotte, No]

-- | 語彙項目定義用マクロ
lexicalitem :: T.Text -> T.Text -> Integer -> Cat -> Preterm -> Node
lexicalitem word source r c s = Node {rs=LEX, pf=word, cat=c, sem=s, daughters=[], score=(r R.% 100), memo=source}

-- | A list of lexical items whose PF is empty.
emptyCategories :: [Node]
emptyCategories = [
  -- 一段動詞活用語尾
  lexicalitem "\\emp" "(132)" 100
              ((S [V1] [Neg,Cont,NegL,EuphT]) `BS` (S [V1] [Stem]))
              id,
  -- カ行変格活用動詞語幹
  lexicalitem "\\emp" "(154)" 100
              (S [VK] [Stem] `BS` NP [Ga])
              (Lam (App (Con "来る") (Var 0))),
  -- サ行変格活用動詞語幹 -- とりあえずガヲ、ガヲトのパターンのみ。
  lexicalitem "\\emp" "(156)" 100
              ((S [VS] [Stem] `BS` NP [Ga]) `BS` NP [O])
              (Lam (Lam (App (Con "する") (Pair (Var 0) (Var 1))))),
  --lexicalitem "\\emp" "(156)" (100%100)
  --            (((S [VS] [Stem] `BS` NP [Ga]) `BS` NP [O]) `BS` S [To])
  --            (Lam (Lam (App (Con "する") (Pair (Var 0) (Var 1))))),
  -- 判定詞語幹
  lexicalitem "\\emp" "(235a)" 100
              ((S [Nda,Nno,Ntar] [Stem] `BS` NP [Ga]) `BS` N)
              id,
  lexicalitem "\\emp" "(235b)" 100
              ((S [Nda,Nno,Ntar] [Stem] `BS` NP [Ga]) `BS` NP [Nc])
              (Lam (Lam (Eq (Con "entity") (Var 0) (Var 1)))),
  -- サ変語幹→状詞語幹
  lexicalitem "\\emp" "(262)" 99
              (S [Nda,Nno] [Stem] `BS` S [VS] [Stem])
              id,
  -- サ変語幹→名詞
  lexicalitem "\\emp" "ss" 99
              ((((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Nc])) `BS` NP [No]) `BS` NP [No]) `BS` ((S [VS] [Stem] `BS` NP [Ga]) `BS` NP [O]))
              (Lam (Lam (Lam (Lam (Lamvec (Sigma (App (App (Var 4) (Var 3)) (Var 2)) (Appvec 1 (App (Var 2) (Var 0))))))))),
  -- 形式述語スル  (380)の２つを区別できるか
  lexicalitem "\\emp" "(380)" 100
              (S [VS] [Stem] `BS` S verb [Cont])
              id,
  -- 補助動詞「くる」
  lexicalitem "\\emp" "(416)" 100
              (S [VK] [Stem] `BS` S verb [TeForm])
              (Lam (App (Con "来る") (Var 0))),
  -- 空助詞
  lexicalitem "cm" "(515)" 80
              (((T 1 anySExStem) `SL` ((T 1 anySExStem) `BS` (NP [Ga,O,Ni]))) `BS` (NP [Nc]))
              (Lam (Lam (App (Var 0) (Var 1)))),
  -- 空冠詞（存在量化）
  lexicalitem "$\\exists$" "(544)" 99
              (((T 1 (S anyPos nonStem)) `SL` ((T 1 (S anyPos nonStem)) `BS` (NP [Nc]))) `SL` N)
              (Lam (Lam (Lamvec (Sigma (Con "entity") (Sigma (App (Var 3) (Var 0)) (Appvec 2 (App (Var 3) (Var 1)))))))),
  -- pro
  lexicalitem "$pro$" "(597)" 98
              (T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Ga,O,Ni,No]))
              (Lam (App (Var 0) (Asp 1 (Con "entity")))),
  -- 関係節化演算子(relativizer)
  lexicalitem "rel" "" 99
              ((N `SL` N) `BS` (S anyPos [Attr] `BS` NP [Ga,O,Ni,To]))
              (Lam (Lam (Lam (Sigma (App (Var 1) (Var 0)) (App (Var 3) (Var 1)))))), 
  -- イ形容詞終止形活用語彙
  lexicalitem "\\emp" "" 99
              (S [Ai] [Term] `BS` S [Ai] [Stem]) 
              id,
  -- 推量のウ
  lexicalitem "\\emp" "(349)" 99
              (S anyPos [Pre] `BS` S anyPos [ModU])
              (Lam (App (Con "ダロウ") (Var 0))),
  -- ダロウ接続形を派生する空範疇
  -- lexicalitem "\\emp" "(354)" (99%100)
  --             (S (verb++adjective) [ModD] `BS` S (verb++adjective) [Term])
  --             id,
  -- lexicalitem "\\emp" "(354)" (99%100)
  --             (S [Nda] [ModD] `BS` S [Nda] [Stem])
  --             id,
  -- 可能態
  lexicalitem "\\emp" "(652)" 99
              ((S [V1] [Stem] `BS` NP [Ga]) `BS` (S anyPos [VoE] `BS` NP [Ga]))
              (Lam (Lam (App (Con "可能") (Pair (Var 0) (App (Var 1) (Var 0)))))),
  lexicalitem "\\emp" "(652)" 99
              (((S [V1] [Stem] `BS` NP [Ni,Ga]) `BS` NP [Ga]) `BS` ((S anyPos [VoE] `BS` NP [Ga]) `BS` NP [O]))
              (Lam (Lam (Lam (App (Con "可能") (Pair (Var 0) (App (App (Var 2) (Var 1)) (Var 0)))))))
  ]

mylex :: [T.Text] -> T.Text -> Cat -> Preterm -> [Node]
mylex wds source cat' sem' = [(lexicalitem wd source 100 cat' sem') | wd <- wds ]

conjSuffix :: T.Text -> T.Text -> [CatPos] -> [CatConj] -> [Node]
conjSuffix wd source catpos catconj = [lexicalitem wd source 100 ((S catpos catconj) `BS` (S catpos [Stem])) id]

teidaiS :: Cat
teidaiS = S anyPos [Term,Imper,Pre,TeForm]

-- | A list of lexical items based on Bekki (2010).
myLexicon :: [Node]
myLexicon = concat [
  -- 格助詞
  mylex ["が"] "(524)" ((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Ga])) `BS` NP [Nc]) 
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["を"] "(524)" ((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [O])) `BS` NP [Nc]) 
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["に"] "(524)" ((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Ni])) `BS` NP [Nc])
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["と"] "(524)" ((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [To])) `BS` NP [Nc])
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["によって"] "(524)" ((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Niyotte])) `BS` NP [Nc])
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["の","が"] "(531)?" ((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [No])) `BS` NP [Nc])
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["へ"] "(516)" (((S anyPos nonStem `BS` NP [Ga]) `SL` (S anyPos nonStem `BS` NP [Ga])) `BS` (NP [Nc]))
        (Lam (Lam (Lam (Sigma (App (Var 2) (Var 1)) (App (Con "へ") (Pair (Var 0) (Var 3))))))),
  mylex ["で"] "(516)" (((S anyPos nonStem `BS` NP [Ga]) `SL` (S anyPos nonStem `BS` NP [Ga])) `BS` (NP [Nc]))
        (Lam (Lam (Lam (Sigma (App (Var 2) (Var 1)) (App (Con "で") (Pair (Var 0) (Var 3))))))),
  mylex ["から"] "(516)" (((S anyPos nonStem `BS` NP [Ga]) `SL` (S anyPos nonStem `BS` NP [Ga])) `BS` (NP [Nc]))
        (Lam (Lam (Lam (Sigma (App (Var 2) (Var 1)) (App (Con "から") (Pair (Var 0) (Var 3))))))),
  mylex ["まで"] "(516)" (((S anyPos nonStem `BS` NP [Ga]) `SL` (S anyPos nonStem `BS` NP [Ga])) `BS` (NP [Nc]))
        (Lam (Lam (Lam (Sigma (App (Var 2) (Var 1)) (App (Con "まで") (Pair (Var 0) (Var 3))))))),
  mylex ["にて"] "(516)" (((S anyPos nonStem `BS` NP [Ga]) `SL` (S anyPos nonStem `BS` NP [Ga])) `BS` (NP [Nc]))
        (Lam (Lam (Lam (Sigma (App (Var 2) (Var 1)) (App (Con "にて") (Pair (Var 0) (Var 3))))))),
  -- 格助詞（の）
  mylex ["の"] "(531)" (((NP [Nc]) `SL` N) `BS` (NP [Nc]))
        (Lam (Lam (Proj Fst (Asp 1 (Sigma (Con "entity") (Sigma (App (Var 2) (Var 1)) (App (Con "of") (Pair (Var 3) (Var 1))))))))),
  -- 等位接続
  mylex ["と"] "" CONJ (Lam (Lam (Sigma (Var 1) (Var 1)))),
  mylex ["や"] "" CONJ (Lam (Lam (Sigma (Var 1) (Var 1)))),
  mylex ["か"] "" CONJ (Lam (Lam (Pi (Not (Var 1)) (Var 1)))),
  -- 動詞活用語尾
  conjSuffix "か" "(112)" [V5k,V5IKU,V5YUK] [Neg,VoR,VoS,NegL],
  conjSuffix "き" "(112)" [V5k,V5IKU,V5YUK] [Cont],
  conjSuffix "く" "(112)" [V5k,V5IKU,V5YUK] [Term,Attr],
  conjSuffix "け" "(112)" [V5k,V5IKU,V5YUK] [Hyp,Imper,VoE],
  conjSuffix "こ" "(112)" [V5k,V5IKU,V5YUK] [ModU],
  --
  conjSuffix "さ" "(77)" [V5s] [Neg,VoR,VoS,NegL],
  conjSuffix "し" "(77)" [V5s] [Cont,EuphT],
  conjSuffix "す" "(77)" [V5s] [Term,Attr],
  conjSuffix "せ" "(77)" [V5s] [Hyp,Imper,VoE],
  conjSuffix "そ" "(77)" [V5s] [ModU],
  --
  conjSuffix "た" "(78)" [V5t] [Neg,VoR,VoS,NegL],
  conjSuffix "ち" "(78)" [V5t] [Cont],
  conjSuffix "つ" "(78)" [V5t] [Term,Attr],
  conjSuffix "て" "(78)" [V5t] [Hyp,Imper,VoE],
  conjSuffix "と" "(78)" [V5t] [ModU],
  --
  conjSuffix "な" "(79)" [V5n] [Neg,VoR,VoS,NegL],
  conjSuffix "に" "(79)" [V5n] [Cont],
  conjSuffix "ぬ" "(79)" [V5n] [Term,Attr],
  conjSuffix "ね" "(79)" [V5n] [Hyp,Imper,VoE],
  conjSuffix "の" "(79)" [V5n] [ModU],
  --
  conjSuffix "ま" "(80)" [V5m] [Neg,VoR,VoS,NegL],
  conjSuffix "み" "(80)" [V5m] [Cont],
  conjSuffix "む" "(80)" [V5m] [Term,Attr],
  conjSuffix "め" "(80)" [V5m] [Hyp,Imper,VoE],
  conjSuffix "も" "(80)" [V5m] [ModU],
  --
  conjSuffix "ら" "(125)" [V5r,V5NAS] [Neg],
  conjSuffix "ら" "(125)" [V5r,V5ARU,V5NAS] [VoR,VoS,NegL],
  conjSuffix "り" "(125)" [V5r,V5ARU,V5NAS] [Cont],
  conjSuffix "る" "(125)" [V5r,V1,V5ARU,V5NAS] [Term,Attr],
  conjSuffix "れ" "(125)" [V5r,V5ARU,V5NAS] [Hyp,Imper,VoE],
  conjSuffix "ろ" "(125)" [V5r,V5ARU,V5NAS] [ModU],
  conjSuffix "ん" "(125)" [V5b] [Neg,Term,Attr],--Term,Attr??
  --
  conjSuffix "わ" "(130)" [V5w,V5TOW] [Neg,VoR,VoS,NegL],
  conjSuffix "い" "(130)" [V5w,V5TOW] [Cont],
  conjSuffix "う" "(130)" [V5w,V5TOW] [Term,Attr],
  conjSuffix "え" "(130)" [V5w,V5TOW] [Hyp,Imper,VoE],
  conjSuffix "お" "(130)" [V5w,V5TOW] [ModU],
  --
  conjSuffix "が" "(83)" [V5g] [Neg,VoR,VoS,NegL],
  conjSuffix "ぎ" "(83)" [V5g] [Cont],
  conjSuffix "ぐ" "(83)" [V5g] [Term,Attr],
  conjSuffix "げ" "(83)" [V5g] [Hyp,Imper,VoE],
  conjSuffix "ご" "(83)" [V5g] [ModU],
  --
  conjSuffix "ば" "(84)" [V5b] [Neg,VoR,VoS,NegL],
  conjSuffix "び" "(84)" [V5b] [Cont],
  conjSuffix "ぶ" "(84)" [V5b] [Term,Attr],
  conjSuffix "べ" "(84)" [V5b] [Hyp,Imper,VoE],
  conjSuffix "ぼ" "(84)" [V5b] [ModU],
  --
  conjSuffix "ら" "(132)" [V1] [VoR],
  conjSuffix "さ" "(132)" [V1] [VoS],
  --conjSuffix "る" [V1] [Term,Attr],
  conjSuffix "れ" "(132)" [V1] [Hyp],
  conjSuffix "ろ" "(132)" [V1] [Imper],
  conjSuffix "よ" "(132)" [V1] [Imper,ModU],
  conjSuffix "い" "(132)" [V1] [Imper],
  conjSuffix "ん" "(132)" [V1] [Neg,Term,Attr],
  -- 音便形
  conjSuffix "い" "(76)-" [V5k,V5g] [EuphT],
  conjSuffix "っ" "(78)-" [V5t,V5r,V5w,V5IKU,V5ARU,V5NAS] [EuphT],
  conjSuffix "ん" "(79)-" [V5n,V5m,V5b] [EuphD],  
  conjSuffix "う" "(128)" [V5TOW] [EuphT],
  conjSuffix "い" "(123)" [V5NAS] [Cont,Imper],
  --- カ変動詞
  conjSuffix "来" "(155)" [VK] [Neg,Cont,EuphT,NegL],
  conjSuffix "来ら" "(155)" [VK] [VoR],
  conjSuffix "来さ" "(155)" [VK] [VoS],
  conjSuffix "来る" "(155)" [VK] [Term,Attr],
  conjSuffix "来れ" "(155)" [VK] [Hyp],
  conjSuffix "来い" "(155)" [VK] [Imper],
  conjSuffix "来よ" "(155)" [VK] [ModU],
  conjSuffix "こ" "(155)" [VK] [Neg,NegL],
  conjSuffix "き" "(155)" [VK] [Cont,EuphT],
  conjSuffix "こら" "(155)" [VK] [VoR],
  conjSuffix "こさ" "(155)" [VK] [VoS],
  conjSuffix "くる" "(155)" [VK] [Term,Attr],
  conjSuffix "くれ" "(155)" [VK] [Hyp],
  conjSuffix "こい" "(155)" [VK] [Imper],
  conjSuffix "こよ" "(155)" [VK] [ModU],
  --conjSuffix "来ん" "(155)" [VK] [TermN],
  --- サ変動詞
  conjSuffix "さ" "(157)" [VS] [VoR,VoS],
  conjSuffix "し" "(157)" [VS] [Neg,Cont,EuphT],
  conjSuffix "する" "(157)" [VS] [Term,Attr],
  conjSuffix "すれ" "(157)" [VS] [Hyp],
  conjSuffix "しろ" "(157)" [VS] [Imper],
  conjSuffix "しよ" "(157)" [VS] [ModU],
  conjSuffix "せ" "(157)" [VS] [NegL],
  conjSuffix "す" "(157)" [VS] [Term],
  conjSuffix "せよ" "(157)" [VS] [Imper],
  conjSuffix "せい" "(157)" [VS] [Imper],
  conjSuffix "すん" "(157)" [VS] [Term,Attr],
  --- ザ変動詞
  conjSuffix "じ" "(164)" [VZ] [Neg,Cont,EuphT],
  conjSuffix "ずる" "(164)" [VZ] [Term,Attr],
  conjSuffix "ずれ" "(164)" [VZ] [Hyp],
  conjSuffix "じろ" "(164)" [VZ] [Imper],
  conjSuffix "じよ" "(164)" [VZ] [ModU],
  conjSuffix "ぜ" "(164)" [VZ] [NegL],
  conjSuffix "ぜら" "(164)" [VZ] [VoR],
  conjSuffix "ず" "(164)" [VZ] [Term],
  conjSuffix "ぜよ" "(164)" [VZ] [Imper],
  --- ウル型活用動詞
  mylex ["得る","うる"] "(169)" ((S [VURU] [Term,Attr] `BS` NP [Ga]) `BS` NP [O]) (Lam (Lam (App (Con "得る") (Pair (Var 0) (Var 1))))),
  -- 形容詞活用語尾
  --adverb = [Aauo, Ai, ANAS, ABES]
  conjSuffix "く"  "(174)" [Aauo,Ai,ANAS,ATII,ABES] [Cont],
  conjSuffix "い"  "(174)" [Aauo,Ai,ANAS,ATII] [Term,Attr],
  conjSuffix "けれ" "(174)" [Aauo,Ai,ANAS,ATII] [Hyp],
  conjSuffix "かろ" "(174)" [Aauo,Ai,ANAS,ATII] [ModU],
  conjSuffix "かっ" "(174)" [Aauo,Ai,ANAS,ATII] [EuphT],
  conjSuffix "から" "(175)" [Aauo,Ai,ANAS,ATII,ABES] [NegL],
  mylex ["う"] "(174)" ((S [Aauo,Ai,ANAS,ATII] [Cont]) `BS` (S [Aauo,Ai,ANAS,ATII] [UStem])) id,
  conjSuffix "し"   "(175)" [Aauo,ANAS,ABES] [Term],
  conjSuffix "き"   "(175)" [Aauo,Ai,ANAS,ATII,ABES] [Attr], 
  conjSuffix "かれ" "(175)" [Aauo,Ai,ANAS,ATII,ABES] [Imper],
  -- ナシ型活用形容詞
  mylex ["良","よ"] "(173)" (S [ANAS] [Stem,UStem] `BS` NP [Ga]) 
        (Lam (App (Con "良い") (Var 0))),
  mylex ["無","な"] "(173)" (S [ANAS] [Stem] `BS` NP [Ga]) 
        (Lam (App (Con "無い") (Var 0))),
  mylex ["無","の"] "(173)" (S [ANAS] [UStem] `BS` NP [Ga]) 
        (Lam (App (Con "無い") (Var 0))),
  -- チイ形活用形容詞
  mylex ["弱っち","よわっち"] "(196)"
        (S [ATII] [Stem] `BS` NP [Ga]) (Lam (App (Con "弱っちい") (Var 0))),
  mylex ["ちゃち"] "(196)" (S [ATII] [Stem] `BS` NP [Ga]) (Lam (App (Con "ちゃちい") (Var 0))),
  mylex ["ばばっち","ばばち"] "(196)" (S [ATII] [Stem] `BS` NP [Ga]) (Lam (App (Con "ばばちい") (Var 0))),
  mylex ["ぼろっち","ぼろち"] "(196)" (S [ATII] [Stem] `BS` NP [Ga]) (Lam (App (Con "ぼろちい") (Var 0))),
  mylex ["みみっち","みみち"] "(196)" (S [ATII] [Stem] `BS` NP [Ga]) (Lam (App (Con "みみっちい") (Var 0))),
  -- ベシ形活用形容詞
  mylex ["如","ごと"] "(199)" ((S [ABES] [Stem] `BS` NP [Ga]) `BS` NP [Ga,No]) (Lam (Lam (App (Con "如") (Pair (Var 1) (Var 0))))),
  mylex ["如","ごと"] "(199)" (S [ABES] [Stem] `BS` S anyPos [Attr]) (Lam (App (Con "如") (Var 0))),
  mylex ["べ"] "(200)" (S [ABES] [Stem] `BS` S verb [Term]) (Lam (App (Con "べし") (Var 0))),
  -- 状詞語幹
  conjSuffix "だ" "(218)" [Nda] [Term],
  conjSuffix "だっ" "(218)" [Nda] [EuphT],
  conjSuffix "です" "(219)" [Nda] [Term],
  conjSuffix "でし" "(219)" [Nda] [EuphT],
  conjSuffix "な" "(220)" [Nna] [Attr],
  conjSuffix "の" "(220)" [Nno] [Attr],
  conjSuffix "なら" "(221)" [Nda] [NegL],
  conjSuffix "なり" "(221)" [Nda] [Term],
  conjSuffix "なる" "(221)" [Nda] [Attr],
  conjSuffix "なれ" "(221)" [Nda] [Hyp,Imper],
  conjSuffix "たら" "(222)" [Ntar] [NegL],
  conjSuffix "たり" "(222)" [Ntar] [Term],
  conjSuffix "たる" "(222)" [Ntar] [Attr],
  conjSuffix "たれ" "(222)" [Ntar] [Hyp,Imper],
  -- 助動詞（過去）
  mylex ["たり"] "(308)" (S anyPos [Cont] `BS` S anyPos [EuphT]) (Lam (App (Con "過去") (Var 0))),
  mylex ["た"] "(308)" (S anyPos [Term,Attr] `BS` S anyPos [EuphT]) (Lam (App (Con "過去") (Var 0))),
  mylex ["たら"] "(308)" (S anyPos [Hyp] `BS` S anyPos [EuphT]) (Lam (App (Con "過去") (Var 0))),
  mylex ["たろ"] "(308)" (S anyPos [ModU] `BS` S anyPos [EuphT]) (Lam (App (Con "過去") (Var 0))),
  mylex ["だり"] "(309)" (S anyPos [Cont] `BS` S anyPos [EuphD]) (Lam (App (Con "過去") (Var 0))),
  mylex ["だ"] "(309)" (S anyPos [Term,Attr] `BS` S anyPos [EuphD]) (Lam (App (Con "過去") (Var 0))),
  mylex ["だら"] "(309)" (S anyPos [Hyp] `BS` S anyPos [EuphD]) (Lam (App (Con "過去") (Var 0))),
  mylex ["だろ"] "(309)" (S anyPos [ModU] `BS` S anyPos [EuphD]) (Lam (App (Con "過去") (Var 0))),
  -- 助動詞（丁寧）
  mylex ["ます"] "(310)" (S verb [Term,Attr] `BS` S verb [Cont]) (Lam (Var 0)),
  mylex ["ませ"] "(310)" (S verb [Imper] `BS` S verb [Cont]) (Lam (Var 0)),
  mylex ["まし"] "(310)" (S verb [Imper,EuphT] `BS` S verb [Cont]) (Lam (Var 0)),
  mylex ["ましょ"] "(310)" (S verb [ModU] `BS` S verb [Cont]) (Lam (Var 0)),
  mylex ["ませ"] "(311)" (S verb [Neg] `BS` S verb [Cont]) (Lam (Var 0)),
  mylex ["まする"] "(311)" (S verb [Term,Attr] `BS` S verb [Cont]) (Lam (Var 0)),
  mylex ["ますれ"] "(311)" (S verb [Hyp] `BS` S verb [Cont]) (Lam (Var 0)),
  mylex ["ませい"] "(311)" (S verb [Imper] `BS` S verb [Cont]) (Lam (Var 0)),
  mylex ["です"] "(318)" (S [Aauo] [Term] `BS` S [Aauo] [Term]) (Lam (Var 0)),
  -- ます＋です
  mylex ["ませんで"] "(321)" (S verb[TeForm] `BS` S verb [Cont]) (Not (Lam (Var 0))),
  mylex ["ませんです"] "(321)" (S verb [Term,Attr] `BS` S verb [Cont]) (Not (Lam (Var 0))),
  mylex ["ませんでし"] "(321)" (S verb [EuphT] `BS` S verb [Cont]) (Lam (Not (Var 0))),
 -- 助動詞（否定）
  mylex ["ぬ","ん"] "(325)" (S anyPos [Term,Attr] `BS` S anyPos [NegL]) (Lam (Not (Var 0))),
  mylex ["ね"] "(325)" (S anyPos [Hyp] `BS` S anyPos [NegL]) (Lam (Not (Var 0))),
  mylex ["ざら"] "(330)" (S anyPos [NegL] `BS` S anyPos [NegL]) (Lam (Not (Var 0))),
  mylex ["ずに"] "(330)" (S anyPos [NiForm] `BS` S anyPos [NegL]) (Lam (Not (Var 0))),
  mylex ["ず"] "(330)" (S anyPos [Term] `BS` S anyPos [NegL]) (Lam (Not (Var 0))),
  mylex ["ざる"] "(330)" (S anyPos [Attr] `BS` S anyPos [NegL]) (Lam (Not (Var 0))),
  mylex ["ざれ"] "(330)" (S anyPos [Hyp,Imper] `BS` S anyPos [NegL]) (Lam (Not (Var 0))),
  mylex ["ないで"] "(333)" (S verb [TeForm] `BS` S verb [Neg]) (Lam (Not (Var 0))),
  mylex ["んで"] "(333)" (S verb [TeForm] `BS` S verb [NegL]) (Lam (Not (Var 0))),
  mylex ["な"] "(343)" (S verb [Imper] `BS` S verb [Term]) (Lam (Not (Var 0))),
  mylex ["な"] "(345)" (S verb [Imper] `BS` S verb [Cont]) id,
  mylex ["なかれ"] "(346)" (S verb [Imper] `BS` S verb [Term]) (Lam (Not (Var 0))),
   -- 助動詞（推量）
  mylex ["う"] "(349)" (S anyPos [Pre] `BS` S anyPos [ModU]) (Lam (App (Con "推量") (Var 0))),
  mylex ["う"] "(349)" (S anyPos [Pre] `BS` S anyPos [ModU]) (Lam (App (Con "意向") (Var 0))),
  mylex ["ん"] "(353)" (S anyPos [Pre] `BS` S anyPos [NegL]) (Lam (App (Con "意向") (Var 0))),
  mylex ["だろう","であろう","だろ"] "(357)" (S verb [Pre] `BS` S verb [Term]) (Lam (App (Con "ダロウ") (Var 0))),
  mylex ["だろう","であろう","だろ"] "(357)" (S adjective [Pre] `BS` S adjective [Term]) (Lam (App (Con "ダロウ") (Var 0))),
  mylex ["だろう","であろう","だろ"] "(357)" (S [Nda] [Pre] `BS` S [Nda] [Stem]) (Lam (App (Con "ダロウ") (Var 0))),
  mylex ["でしょう","でしょ"] "(357)" (S verb [Pre] `BS` S verb [Term]) (Lam (App (Con "ダロウ") (Var 0))),
  mylex ["でしょう","でしょ"] "(357)" (S adjective [Pre] `BS` S adjective [Term]) (Lam (App (Con "ダロウ") (Var 0))),
  mylex ["でしょう","でしょ"] "(357)" (S [Nda] [Pre] `BS` S [Nda] [Stem]) (Lam (App (Con "ダロウ") (Var 0))),
  mylex ["まい"] "(359a)" (S verb [Pre] `BS` S verb [Term]) (Lam (App (Con "ダロウ") (Not (Var 0)))),
  mylex ["まい"] "(359b)" (S verb [Pre] `BS` S [V1,VK,VS,VZ] [Neg]) (Lam (App (Con "ダロウ") (Not (Var 0)))),
  --mylex "まい" "(359c)" (S verb [Pre] `BS` S [VS,VZ] [Term]) (Lam (App (Con "ダロウ") (Not (Var 0)))),
  -- 動詞テ形
  mylex ["て"] "(369)" (S verb [TeForm] `BS` S verb [EuphT]) id,
  mylex ["で"] "(370)" (S verb [TeForm] `BS` S verb [EuphD]) id,
  mylex ["たって"] "(369)" (S anyPos [TeForm] `BS` S verb [EuphT]) id,
  mylex ["だって"] "(370)" (S anyPos [TeForm] `BS` S verb [EuphD]) id,
  -- 形容詞テ形
  mylex ["て"] "(371)" (S adjective [TeForm] `BS` S adjective [Cont]) id,
  mylex ["って"] "(371)" (S adjective [TeForm] `BS` S adjective [Cont]) id,
  mylex ["たって"] "(371)" (S adjective [TeForm] `BS` S adjective [Cont]) id,
  -- 状詞テ形
  mylex ["で"] "(372)" (S [Nda] [TeForm] `BS` S [Nda] [Stem]) id,
  mylex ["だって"] "(372)" (S [Nda] [TeForm] `BS` S [Nda] [Stem]) id,
  -- 動詞ニ形
  mylex ["に"] "(376)" (S verb [NiForm] `BS` S verb [Cont]) id,
  -- 状詞ニ形
  mylex ["に"] "(377)" (S [Nda] [NiForm] `BS` S [Nda] [Stem]) id,
  -- 動詞性接尾語
  -- 6.1.1 形式述語
  mylex ["居","い"] "(381)" (S [V1] [Stem] `BS` S verb [TeForm,NiForm]) (Lam (App (Con "テイル") (Var 0))),
  mylex ["有","あ"] "(381)" (S [V5ARU] [Stem] `BS` S adjective [Cont]) id,
  mylex ["有","あ"] "(381)" (S [V5ARU] [Stem] `BS` S [Nda] [TeForm]) id,
  mylex ["な"] "(383)" ((S [ANAS] [Stem] `BS` NP [Ga]) `BS` (S adjective [Cont]) `BS` NP [Ga]) (Lam (Lam (Not (App (Var 0) (Var 1))))),
  mylex ["な"] "(383)" ((S [ANAS] [Stem] `BS` NP [Ga]) `BS` (S [Nda] [TeForm] `BS` NP [Ga])) (Lam (Lam (Not (App (Var 0) (Var 1))))),
  mylex ["な"] "(384)" ((S [V5r] [Stem] `BS` NP [Ga]) `BS` (S adjective [Cont] `BS` NP [Ga])) (Lam (Lam (App (Con "成る") (Pair (Var 0) (Var 1))))),
  mylex ["な"] "(384)" ((S [V5r] [Stem] `BS` NP [Ga]) `BS` (S [Nda] [NiForm] `BS` NP [Ga])) (Lam (Lam (App (Con "成る") (Pair (Var 0) (Var 1))))),
  -- い省略
  mylex ["て"] "(403)" (S [V1] [Stem] `BS` S verb [EuphT]) (Lam (App (Con "テイル") (Var 0))),
  -- 副助詞
  mylex ["は"] "(550)" (((T 1 teidaiS) `SL` ((T 1 teidaiS) `BS` (NP [Ga,O]))) `BS` (NP [Nc]))
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["には"] "(550)" (((T 1 teidaiS) `SL` ((T 1 teidaiS) `BS` (NP [Ni]))) `BS` (NP [Nc]))
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["も"] "(385)" (((T 1 teidaiS) `SL` ((T 1 teidaiS) `BS` (NP [Ga,O]))) `BS` (NP [Nc]))
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["にも"] "(385)" (((T 1 teidaiS) `SL` ((T 1 teidaiS) `BS` (NP [Ni]))) `BS` (NP [Nc]))
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["こそ"] "new" (((T 1 anySExStem) `SL` ((T 1 anySExStem) `BS` (NP [Ga,O]))) `BS` (NP [Nc]))
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["にこそ"] "new" (((T 1 anySExStem) `SL` ((T 1 anySExStem) `BS` (NP [Ni]))) `BS` (NP [Nc]))
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["さえ"] "(387)" (((T 1 anySExStem) `SL` ((T 1 anySExStem) `BS` (NP [Ga,O]))) `BS` (NP [Nc]))
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["にさえ","さえに"] "(387)" (((T 1 anySExStem) `SL` ((T 1 anySExStem) `BS` (NP [Ni]))) `BS` (NP [Nc]))
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["だけ"] "new" (((T 1 anySExStem) `SL` ((T 1 anySExStem) `BS` (NP [Ga,O]))) `BS` (NP [Nc]))
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["にだけ","だけに"] "new" (((T 1 anySExStem) `SL` ((T 1 anySExStem) `BS` (NP [Ni]))) `BS` (NP [Nc]))
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["ばかり"] "new" (((T 1 anySExStem) `SL` ((T 1 anySExStem) `BS` (NP [Ga,O]))) `BS` (NP [Nc]))
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["にばかり","ばかりに"] "new" (((T 1 anySExStem) `SL` ((T 1 anySExStem) `BS` (NP [Ni]))) `BS` (NP [Nc])) 
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["のみ"] "new" (((T 1 anySExStem) `SL` ((T 1 anySExStem) `BS` (NP [Ga,O]))) `BS` (NP [Nc]))
        (Lam (Lam (App (Var 0) (Var 1)))),
  mylex ["にのみ","のみに"] "new" (((T 1 anySExStem) `SL` ((T 1 anySExStem) `BS` (NP [Ni]))) `BS` (NP [Nc]))
        (Lam (Lam (App (Var 0) (Var 1)))),
  -- 6.1.3 補助動詞（連用形接続）
  mylex ["始め","はじめ"] "(412)" (S [V1] [Stem] `BS` S verb [Cont]) (Lam (App (Con "始める") (Var 0))),
  mylex ["込","こ"] "(412)" (S [V5m] [Stem] `BS` S verb [Cont]) (Lam (App (Con "込む") (Var 0))),
  mylex ["出","だ"] "(412)" (S [V5s] [Stem] `BS` S verb [Cont]) (Lam (App (Con "出す") (Var 0))),
  mylex ["合","あ"] "(412)" (S [V5w] [Stem] `BS` S verb [Cont]) (Lam (App (Con "合う") (Var 0))),
  mylex ["続け","つづけ"] "(412)" (S [V1] [Stem] `BS` S verb [Cont]) (Lam (App (Con "続ける") (Var 0))),
  mylex ["かけ"] "(412)" (S [V1] [Stem] `BS` S verb [Cont]) (Lam (App (Con "かける") (Var 0))),
  mylex ["上げ","あげ"] "(412)" (S [V1] [Stem] `BS` S verb [Cont]) (Lam (App (Con "上げる") (Var 0))),
  mylex ["切","き"] "(412)" (S [V5r] [Stem] `BS` S verb [Cont]) (Lam (App (Con "切る") (Var 0))),
  mylex ["付け","つけ"] "(412)" (S [V1] [Stem] `BS` S verb [Cont]) (Lam (App (Con "付ける") (Var 0))),
  mylex ["過ぎ","すぎ"] "(412)" (S [V1] [Stem] `BS` S verb [Cont]) (Lam (App (Con "過ぎる") (Var 0))),
  mylex ["あぐ"] "(412)" (S [V5m] [Stem] `BS` S verb [Cont]) (Lam (App (Con "あぐむ") (Var 0))),
  mylex ["かね"] "(412)" (S [V1] [Stem] `BS` S verb [Cont]) (Lam (App (Con "かねる") (Var 0))),
  mylex ["やが"] "(412)" (S [V5r] [Stem] `BS` S verb [Cont]) (Lam (App (Con "やがる") (Var 0))),
  -- 6.1.4 補助動詞（テ形接続）
  mylex ["お"] "(416)" (S [V5k] [Stem] `BS` S verb [TeForm]) (Lam (App (Con "テオク") (Var 0))),
  mylex ["仕舞","しま"] "(416)" (S [V5w] [Stem] `BS` S verb [TeForm]) (Lam (App (Con "テシマウ") (Var 0))),
  mylex ["行","い"] "(416)" (S [V5k] [Stem] `BS` S verb [TeForm]) (Lam (App (Con "行く") (Var 0))),
  mylex ["見","み"] "(416)" (S [V1] [Stem] `BS` S verb [TeForm]) (Lam (App (Con "見る") (Var 0))),
  mylex ["見せ","みせ"] "(416)" (S [V1] [Stem] `BS` S verb [TeForm]) (Lam (App (Con "見せる") (Var 0))),
  -- 6.1.5 授受表現
  mylex ["上げ","あげ"] "(436)" ((S [V1] [Stem] `BS` NP [Ga]) `BS` (S verb [TeForm] `BS` NP [Ga]))
        (Lam (Lam (App (Con "あげる") (Pair (Var 0) (App (Var 1) (Var 0)))))),
  mylex ["貰","もら"] "(436)" (((S [V5w] [Stem] `BS` NP [Ga]) `BS` NP [Ni]) `BS` (S verb [TeForm] `BS` NP [Ga]))
        (Lam (Lam (Lam (App (Con "もらう") (Pair (Var 0) (Pair (Var 1) (App (Var 2) (Var 1)))))))),
  -- 6.1.6 −がる
  mylex ["が"] "(443)" (((S [V5r] [Stem] `BS` NP [Ga]) `BS` NP [O]) `BS` (S [Aauo,Ai,ANAS] [Stem] `BS` NP [Ga]))
        (Lam (Lam (Lam (App (Con "がる") (Pair (Var 0) (App (Var 2) (Var 1))))))),
  -- 6.1.7 −めく
  mylex ["め"] "(453)" ((S [V5k] [Stem] `BS` NP [Ga]) `BS` N) (Lam (Lam (App (Con "めく") (Pair (Var 0) (App (Var 1) (Var 0)))))),
  -- 形容詞性接尾語
  -- 6.2.1 ない
  mylex ["な"] "(455)" (S [ANAS] [Stem] `BS` S anyPos [Neg]) (Lam (Not (Var 0))),
  mylex ["無","な"] "(458)" (S [Aauo,ANAS] [Stem] `BS` NP [Ga]) (Lam (App (Con "無い") (Var 0))),
  mylex ["ねえ","ねぇ","ねー"] "(455)" (S [ANAS] [Term,Attr] `BS` S anyPos [Neg]) (Lam (Not (Var 0))),
  mylex ["ねえ","ねぇ","ねー"] "(458)" (S [Aauo,ANAS] [Term,Attr] `BS` NP [Ga]) (Lam (App (Con "無い") (Var 0))),
  mylex ["無し","なし","ナシ"] "(467)" (S [Nda,Nno,Nni] [Stem] `BS` NP [Ga]) (Lam (App (Con "無い") (Var 0))),
  -- 6.2.2 たい
  mylex ["た"] "(474a)" ((S [Aauo] [Stem] `BS` NP [Ga]) `BS` (S verb [Cont] `BS` NP [Ga])) (Lam (Lam (App (Con "たい") (App (Var 1) (Var 0))))),
  mylex ["た"] "(474b)" (((S [Aauo] [Stem] `BS` NP [Ni,Ga]) `BS` NP [Ga]) `BS` ((S verb [Cont] `BS` NP [Ga])) `BS` NP [O]) (Lam (Lam (Lam (App (Con "たい") (App (App (Var 2) (Var 1)) (Var 0)))))),
  mylex ["難","にく"] "(475a)" ((S [Aauo] [Stem] `BS` NP [Ga]) `BS` (S verb [Cont] `BS` NP [Ga])) (Lam (Lam (App (Con "難") (App (Var 1) (Var 0))))),
  mylex ["難","にく"] "(475b)" (((S [Aauo] [Stem] `BS` NP [Ni,Ga]) `BS` NP [Ga]) `BS` ((S verb [Cont] `BS` NP [Ga])) `BS` NP [O]) (Lam (Lam (Lam (App (Con "難") (App (App (Var 2) (Var 1)) (Var 0)))))),
  mylex ["易","やす"] "(476a)" ((S [Aauo] [Stem] `BS` NP [Ga]) `BS` (S verb [Cont] `BS` NP [Ga])) (Lam (Lam (App (Con "易") (App (Var 1) (Var 0))))),
  mylex ["易","やす"] "(476b)" (((S [Aauo] [Stem] `BS` NP [Ni,Ga]) `BS` NP [Ga]) `BS` ((S verb [Cont] `BS` NP [Ga])) `BS` NP [O]) (Lam (Lam (Lam (App (Con "易") (App (App (Var 2) (Var 1)) (Var 0)))))),
  -- 6.2.3-6.2.6
  mylex ["らし"] "(478)" (S [Ai] [Stem] `BS` S verb [Term]) (Lam (App (Con "ラシイ") (Var 0))),
  mylex ["らし"] "(478)" (S [Ai] [Stem] `BS` S adjective [Term]) (Lam (App (Con "ラシイ") (Var 0))),
  mylex ["らし"] "(478)" (S [Ai] [Stem] `BS` S [Nda] [Stem]) (Lam (App (Con "ラシイ") (Var 0))),
  mylex ["っぽ"] "(480)" (S [Aauo] [Stem] `BS` S [Aauo,Nda] [Stem]) (Lam (App (Con "ぽい") (Var 0))),
  mylex ["くさ"] "(478)" (S [Ai] [Stem] `BS` S verb [Term]) (Lam (App (Con "くさい") (Var 0))),
  mylex ["くさ"] "(478)" (S [Ai] [Stem] `BS` S adjective [Term]) (Lam (App (Con "くさい") (Var 0))),
  mylex ["くさ"] "(478)" (S [Ai] [Stem] `BS` S [Nda] [Stem]) (Lam (App (Con "くさい") (Var 0))),
  mylex ["べき"] "(488)" (S [Nda] [Attr] `BS` S verb [Term]) (Lam (App (Con "べき") (Var 0))),
  -- 状詞性接尾語
  mylex ["よう"] "(493)" (S [Nda,Nna,Nni] [Stem] `BS` S anyPos [Attr]) (Lam (App (Con "ヨウダ") (Var 0))),
  mylex ["そう"] "(497)" (S [Nda] [Stem] `BS` S anyPos [Term]) (Lam (App (Con "ソウダ伝聞") (Var 0))),
  mylex ["そう"] "(498)" (S [Nda,Nna,Nni] [Stem] `BS` S anyPos [ModS]) (Lam (App (Con "ソウダ推量") (Var 0))),
  mylex ["がち"] "(506)" (S [Nda,Nna,Nni] [Stem] `BS` S anyPos [Cont]) (Lam (App (Con "がち") (Var 0))),
  mylex ["みたい"] "(507)" (S [Nda,Nna,Nni] [Stem] `BS` S verb [Term]) (Lam (App (Con "ミタイ") (Var 0))),
  mylex ["みたい"] "(507)" (S [Nda,Nna,Nni] [Stem] `BS` S adjective [Term]) (Lam (App (Con "ミタイ") (Var 0))),
  mylex ["みたい"] "(507)" (S [Nda,Nna,Nni] [Stem] `BS` S [Nda] [Stem]) (Lam (App (Con "ミタイ") (Var 0))),
  mylex ["的","てき"] "(508)" ((S [Nda,Nna,Nni] [Stem] `BS` NP [Ga]) `BS` NP [Nc]) (Lam (Lam (Eq (Con "Entity") (Var 0) (Var 1)))),
  mylex ["的","てき"] "(508)" ((S [Nda,Nna,Nni] [Stem] `BS` NP [Ga]) `BS` N) (Lam (Lam (App (Var 1) (Var 0)))),
  mylex ["気味","ぎみ"] "(509)" ((S [Nda,Nna,Nno,Nni] [Stem] `BS` NP [Ga]) `BS` N) (Lam (Lam (App (Var 1) (Var 0)))),
  mylex ["なの"] "(510)" (S [Nda] [Stem] `BS` S [Nda] [Stem]) (Lam (App (Con "ナノ") (Var 0))),
  mylex ["の"] "(511)" (S [Nda] [Stem] `BS` S anyPos [Attr]) (Lam (App (Con "ノダ") (Var 0))),
  mylex ["筈","はず","ハズ"] "(511)" (S [Nda] [Stem] `BS` S anyPos [Attr]) (Lam (App (Con "ハズ") (Var 0))),
  mylex ["訳","わけ","ワケ"] "(511)" (S [Nda] [Stem] `BS` S anyPos [Attr]) (Lam (App (Con "ハズ") (Var 0))),
  mylex ["もの"] "(511)" (S [Nda] [Stem] `BS` S anyPos [Attr]) (Lam (App (Con "モノ") (Var 0))),
  -- 照応
  mylex ["ここ","そこ","あそこ"] "(586)" (T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Nc])) 
        (Lam (Lamvec (Appvec 0 (App (Var 1) (Asp 1 (Proj Fst (Sigma (Con "entity") (App (Con "場所") (Var 0))))))))),
  mylex ["この","その","あの"] "(589)" ((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Nc])) `SL` N)
        (Lam (Lam (Lamvec (Appvec 0 (App (Var 1) (Asp 1 (Proj Fst (Sigma (Con "entity") (App (Var 3) (Var 0)))))))))),
  -- 代名詞
  mylex ["彼","かれ","カレ"] "" (NP [Nc]) 
        (Proj Fst (Asp 1 (Sigma (Con "entity") (App (Con "男") (Var 0))))),
  mylex ["彼女","かのじょ","カノジョ"] "" (NP [Nc]) 
        (Proj Fst (Asp 1 (Sigma (Con "entity") (App (Con "女") (Var 0))))),
  -- 連体詞
  mylex ["こう"] "" (anySExStem `SL` anySExStem) id,
  mylex ["そう"] "" (anySExStem `SL` anySExStem) id,
  mylex ["ああ"] "" (anySExStem `SL` anySExStem) id,
  mylex ["どう"] "" (anySExStem `SL` anySExStem) id,
  mylex ["このよう"] "" (S [Nna,Nni] [Stem] `BS` NP [Ga]) (Lam (Var 0)),
  mylex ["あのよう"] "" (S [Nna,Nni] [Stem] `BS` NP [Ga]) (Lam (Var 0)),
  mylex ["そのよう"] "" (S [Nna,Nni] [Stem] `BS` NP [Ga]) (Lam (Var 0)),
  mylex ["どのよう"] "" (S [Nna,Nni] [Stem] `BS` NP [Ga]) (Lam (Var 0)),
  -- カ節
  mylex ["か","かどうか"] "(603)" (T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Nc]) `BS` S (verb++adjective) [Term]) 
        (Lam (Lam (App (Var 0) (App (Con "カドウカ") (Var 1))))), 
  mylex ["か","かどうか"] "(603)" (T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Nc]) `BS` S [Nda] [Stem]) 
        (Lam (Lam (App (Var 0) (App (Con "カドウカ") (Var 1))))), 
  -- 態
  mylex ["れ"] "(607)" (((S [V1] [Stem] `BS` NP [Ga]) `BS` NP [Ni]) `BS` (S anyPos [VoR] `BS` NP [Ga])) 
        (Lam (Lam (Lam (Sigma (App (Var 2) (Var 1)) (App (Con "迷惑") (Pair (Var 1) (Var 0))))))),
  mylex ["れ"] "(608)" (((S [V1] [Stem] `BS` NP [Ga]) `BS` NP [Ni,Niyotte]) `BS` ((S anyPos [VoR] `BS` NP [Ga]) `BS` NP [Ni,O]))
        (Lam (Lam (Lam (App (App (Var 2) (Var 0)) (Var 1))))),
  mylex ["せ"] "(629)" (((S [V1] [Stem] `BS` NP [Ga]) `BS` NP [Ni]) `BS` (S anyPos [VoS] `BS` NP [Ga]))
        (Lam (Lam (Lam (Sigma (App (Var 2) (Var 1)) (App (Con "使役") (Pair (Var 1) (Var 0))))))),
  mylex ["せ"] "(635)" (((S [V1] [Stem] `BS` NP [Ga]) `BS` NP [O]) `BS` (S anyPos [VoS] `BS` NP [Ga]))
        (Lam (Lam (Lam (Sigma (App (Var 2) (Var 1)) (App (Con "使役") (Pair (Var 1) (Var 0))))))),
  mylex ["れ"] "(660a)" ((S [V1] [Stem] `BS` NP [Ga]) `BS` (S [V1,VK] [VoR] `BS` NP [Ga]))
        (Lam (Lam (App (Con "可能") (Pair (Var 0) (App (Var 1) (Var 0)))))),
  mylex ["れ"] "(660b)" (((S [V1] [Stem] `BS` NP [Ni,Ga]) `BS` NP [Ga]) `BS` ((S [V1,VK] [VoR] `BS` NP [Ga]) `BS` NP [O]))
        (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (App (App (Var 2) (Var 1)) (Var 0))))))),
  mylex ["得","え"] "(661a)" ((S [V1] [Stem] `BS` NP [Ga]) `BS` (S verb [Cont] `BS` NP [Ga]))
        (Lam (Lam (App (Con "可能") (Pair (Var 0) (App (Var 1) (Var 0)))))),
  mylex ["得","え"] "(661b)" (((S [V1] [Stem] `BS` NP [Ga,Ni]) `BS` NP [Ga]) `BS` ((S verb [Cont] `BS` NP [Ga]) `BS` NP [O]))
        (Lam (Lam (Lam (App (Con "可能") (Pair (Var 0) (App (App (Var 2) (Var 1)) (Var 0))))))),
  mylex ["得る","うる"] "(662a)" ((S [VURU] [Term,Attr] `BS` NP [Ga]) `BS` (S verb [Cont] `BS` NP [Ga]))
        (Lam (Lam (App (Con "可能") (Pair (Var 0) (App (Var 1) (Var 0)))))),
  mylex ["得る","うる"] "(662b)" (((S [VURU] [Term,Attr] `BS` NP [Ga,Ni]) `BS` NP [Ga]) `BS` ((S verb [Cont] `BS` NP [Ga]) `BS` NP [O]))
        (Lam (Lam (Lam (App (Con "可能") (Pair (Var 0) (App (App (Var 2) (Var 1)) (Var 0))))))),
  mylex ["れ"] "(666)" (S [V1] [VoE] `BS` S [V1] [Stem])
        id,
  mylex ["来れ","これ"] "(667)" (S [VK] [VoE] `BS` S [VK] [Stem]) id,
  -- 複文
  mylex ["が"] "(711)" ((T 1 anySExStem `SL` T 1 anySExStem) `BS` S anyPos [Term]) (Lam (Lam (Sigma (Var 1) (Var 0)))),
  mylex ["し"] "(713)" ((T 1 anySExStem `SL` T 1 anySExStem) `BS` S anyPos [Term]) (Lam (Lam (Sigma (Var 1) (Var 0)))),
  --mylex ["が"] "(711)" ((T 1 anySExStem `SL` T 1 anySExStem) `BS` S anyPos [Term]) 
  --      (Lam (Lam (Lamvec (Lamvec (Sigma (Appvec 0 (Var 3)) (Appvec 1 (Appvec 2 (Var 3)))))))),
  -- Wh句
  mylex ["誰","だれ"] "" (NP [Nc]) (Con "誰"),
  mylex ["何","なに"] "" (NP [Nc]) (Con "何"),
  mylex ["どこ"] "" (NP [Nc]) (Con "どこ"),
  -- 従属節導入表現
  -- 連用節
  mylex ["に"] "" ((T 1 anySExStem `SL` T 1 anySExStem) `BS` (S [Nni] [Stem])) (Lam (Lam (Sigma (Var 1) (Var 1)))),
  -- 条件節
  mylex ["ば"] "" ((anySExStem `SL` anySExStem) `BS` (S anyPos [Neg,Hyp])) (Lam (Lam (Pi (Var 1) (Var 1)))),
  -- 終助詞
  mylex ["か"] "" (S anyPos [Term] `BS` S anyPos [Term]) id,
  mylex ["ね"] "" (S anyPos [Term] `BS` S anyPos [Term]) id,
  mylex ["よ"] "" (S anyPos [Term] `BS` S anyPos [Term]) id,
  mylex ["さ"] "" (S anyPos [Term] `BS` S anyPos [Term]) id,
  -- 句読点
  -- mylex "。"  "" (anyS `BS` anyS) id,
  -- mylex "、"  "" (anyS `BS` anyS) id,
  -- 括弧
  mylex ["「","（","(","『","《","〈","【","［","[","−","-"] "" LPAREN Unit,
  mylex ["」","）",")","』","》","〉","】","］","]","−","-"] "" RPAREN Unit,
  -- 量化表現：Q-no N
  mylex ["すべての","あらゆる","一人一人の","各","各々の","それぞれの"] "(534)" ((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Nc])) `SL` N) 
        (Lam (Lam (Lamvec (Pi (Sigma (Con "entity") (App (Var 3) (Var 0))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),
  mylex ["一人の","或る","ある","何人かの","数人の"] "(534)" ((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Nc])) `SL` N) 
        (Lam (Lam (Lamvec (Sigma (Sigma (Con "entity") (App (Var 3) (Var 0))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),
  -- 量化表現：N-no Q
  mylex ["の一人一人","のそれぞれ","のすべて","の全員"] "(535)" ((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Nc])) `BS` N) 
        (Lam (Lam (Lamvec (Pi (Sigma (Con "entity") (App (Var 3) (Var 0))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),
  mylex ["の一人","の何人か","の数人","の誰か"] "(535)" ((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Nc])) `BS` N) 
        (Lam (Lam (Lamvec (Sigma (Sigma (Con "entity") (App (Var 3) (Var 0))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),
  -- 量化表現：Q N
  mylex ["全員"] "(536)" ((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Nc])) `BS` N) 
        (Lam (Lam (Lamvec (Pi (Sigma (Con "entity") (App (Var 3) (Var 0))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),
  mylex ["一人"] "(536)" ((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Nc])) `BS` N) 
        (Lam (Lam (Lamvec (Sigma (Sigma (Con "entity") (App (Var 3) (Var 0))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),
  -- 遊離数量詞
  -- mylex ["全員","みな","誰も","すべて","それぞれ"]
  -- mylex ["一人","誰か"]
  -- 存在動詞
  mylex ["い"] "" (S [V1] [Stem] `BS` NP [Ga]) (Lam (Sigma (Con "entity") (Eq (Con "entity") (Var 0) (Var 1)))),
  mylex ["あ"] "" (S [V5ARU] [Stem] `BS` NP [Ga]) (Lam (Sigma (Con "entity") (Eq (Con "entity") (Var 0) (Var 1)))),
  -- JSeM語彙
  mylex ["世界最高"] "" (S [Nda,Nno,Nni,Ntar] [Stem] `BS` NP [Ga]) 
        (Lam (App (Con "世界最高") (Var 0)))
  ]

