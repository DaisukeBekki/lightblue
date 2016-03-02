{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures #-}

{-|
Module      : MyLexicon
Description : A user-defined lexicon of Japanese
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : alpha
-}
module MyLexicon (
  emptyCategories,
  myLexicon
  ) where 

import Prelude hiding (id)
import qualified Data.Text.Lazy as T
import CombinatoryCategorialGrammar
import DependentTypes

-- | A list of empty categories (i.e. lexical items whose PF is empty).
emptyCategories :: [Node]
emptyCategories = [
  -- 一段動詞活用語尾
  lexicalitem "\\emp" "(132)" 100
              ((S [V1] [Neg,Cont,NegL,EuphT]) `BS` (S [V1] [Stem]))
              id,
  -- カ行変格活用動詞語幹
  lexicalitem "\\emp" "(154)" 100
              (S [VK] [Stem] `BS` NP [Ga])
              (verbSR 1 "来る"),
  -- サ行変格活用動詞語幹 -- とりあえずガヲ、ガヲトのパターンのみ。
  lexicalitem "\\emp" "(156)" 100
              ((S [VS] [Stem] `BS` NP [Ga]) `BS` NP [O])
              (verbSR 2 "する"),
  --lexicalitem "\\emp" "(156)" (100%100)
  --            (((S [VS] [Stem] `BS` NP [Ga]) `BS` NP [O]) `BS` S [To])
  --            (Lam (Lam (App (Con "する") (Pair (Var 0) (Var 1))))),
  -- 判定詞語幹
  lexicalitem "\\emp" "(235a)" 100
              ((S [Nda,Nno,Ntar] [Stem] `BS` NP [Ga]) `BS` N)
              id,
  lexicalitem "\\emp" "(235b)" 100
              ((S [Nda,Nno,Ntar] [Stem] `BS` NP [Ga]) `BS` NP [Nc])
              (Lam (Lam (Lam (Sigma (Eq (Con "entity") (Var 1) (Var 2)) (App (Var 1) (Var 0)))))),
  -- サ変語幹→状詞語幹
  lexicalitem "\\emp" "(262)" 99
              (S [Nda,Nno] [Stem] `BS` S [VS] [Stem])
              id,
  -- サ変語幹→名詞
  lexicalitem "\\emp" "ss" 99
              ((((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) `BS` NP [No]) `BS` NP [No]) `BS` ((S [VS] [Stem] `BS` NP [Ga]) `BS` NP [O]))
              (Lam (Lam (Lam (Lam (Lamvec (App (App (App (Var 4) (Var 3)) (Var 2)) (Lam (Appvec 1 (App (Var 2) (Var 0)))))))))),
  -- 形式述語スル  (380)の２つを区別できるか
  lexicalitem "\\emp" "(380)" 100
              (S [VS] [Stem] `BS` S verb [Cont])
              id,
  -- 補助動詞「くる」
  lexicalitem "\\emp" "(416)" 100
              (S [VK] [Stem] `BS` S verb [TeForm])
              (eventModifier "来る"),
              --(Lam (App (Con "来る") (Var 0))),
  -- 空助詞
  lexicalitem "cm" "(515)" 50
              (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ga,O,Ni]))) `BS` (NP [Nc]))
              argumentCM,
  -- 空冠詞（存在量化）
  lexicalitem "$\\exists$" "(544)" 99
              (((T True 1 (S anyPos nonStem)) `SL` ((T True 1 (S anyPos nonStem)) `BS` (NP [Nc]))) `SL` N)
              (Lam (Lam (Lamvec (Sigma (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),
  -- pro
  lexicalitem "$pro$" "(597)" 98
              (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Ga,O,Ni,To,No]))
              (Lam (App (Var 0) (Asp 1 (Con "entity")))),
  -- 関係節化演算子(relativizer)
  lexicalitem "rel" "(670)+" 99 -- to be revised
              ((N `SL` N) `BS` (S anyPos [Attr] `BS` NP [Ga,O,Ni,To]))
              (Lam (Lam (Lam (Lam (Sigma (App (App (Var 3) (Var 1)) (Lam Top)) (App (App (Var 3) (Var 2)) (Var 1))))))),
  --lexicalitem "rel-" "(670)+" 90
  --            ((N `SL` N) `BS` S anyPos [Attr]) -- to be revised
  --            (Lam (Lam (Lam (Sigma (App (Var 1) (Var 0)) (App (Var 3) (Lam (Sigma (Pi (Con "event") (Pi (Con "entity") Type)) (App (App (Var 0) (Var 1)) (Var 3))))))))),
  -- イ形容詞終止形活用語彙
  lexicalitem "\\emp" "" 99
              (S [Ai] [Term] `BS` S [Ai] [Stem]) 
              id,
  -- 推量のウ
  lexicalitem "\\emp" "(349)" 99
              (S anyPos [Pre] `BS` S anyPos [ModU])
              (modal "ダロウ[MCN]"),
  -- ダロウ接続形を派生する空範疇
  -- lexicalitem "\\emp" "(354)" (99%100)
  --             (S (verb++adjective) [ModD] `BS` S (verb++adjective) [Term])
  --             id,
  -- lexicalitem "\\emp" "(354)" (99%100)
  --             (S [Nda] [ModD] `BS` S [Nda] [Stem])
  --             id,
  -- 可能態
  lexicalitem "\\emp" "(652a)" 99
              ((S [V1] [Stem] `BS` NP [Ga]) `BS` (S anyPos [VoE] `BS` NP [Ga]))
              (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (Lam (App (App (Var 3) (Var 0)) (Var 1)))))))),
  lexicalitem "\\emp" "(652b)" 99
              (((S [V1] [Stem] `BS` NP [Ni,Ga]) `BS` NP [Ga]) `BS` ((S anyPos [VoE] `BS` NP [Ga]) `BS` NP [O]))
              (Lam (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (Lam (App (App (App (Var 4) (Var 3)) (Var 0)) (Var 1))))))))),
  -- 状詞の副詞用法: \p.\q.\v.\c.qv(\e.pe ∧ ce)
  lexicalitem "\\emp" "(730)" 99
              ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` (S [Nemp] [Stem] `BS` NP [Ga]))
              (Lam (Lam (Lam (App (Var 1) (Lam (App (App (Var 3) (Var 0)) (Lam (App (Var 2) (Var 1)))))))))
  ]

{- Some Macros for adding lexical items to lexicon -}

mylex :: [T.Text] -> T.Text -> Cat -> Preterm -> [Node]
mylex wds source cat' sem' = [(lexicalitem wd source 100 cat' sem') | wd <- wds ]

conjSuffix :: T.Text -> T.Text -> [CatPos] -> [CatConj] -> [Node]
conjSuffix wd source catpos catconj = [lexicalitem wd source 100 ((S catpos catconj) `BS` (S catpos [Stem])) id]

teidaiS :: Cat
teidaiS = S anyPos [Term,Imper,Pre,TeForm]

-- | A list of (mostly functional) lexical items extracted from Bekki (2010).
myLexicon :: [Node]
myLexicon = concat $ [
  -- 格助詞
  -- argument:
  mylex ["が"] "(524)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Ga])) `BS` NP [Nc]) argumentCM,
  mylex ["を"] "(524)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [O])) `BS` NP [Nc]) argumentCM,
  mylex ["に"] "(524)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Ni])) `BS` NP [Nc]) argumentCM,
  mylex ["と"] "(524)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [To])) `BS` NP [Nc]) argumentCM,
  mylex ["によって"] "(524)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Niyotte])) `BS` NP [Nc]) argumentCM,
  mylex ["の","が"] "(531)?" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [No])) `BS` NP [Nc]) argumentCM,
  -- adjunct:
  mylex ["へ"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [Nc]) (adjunctCM "終点"), 
  mylex ["で"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [Nc]) (adjunctCM "場所"), 
  mylex ["から"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [Nc]) (adjunctCM "始点"), 
  mylex ["まで"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [Nc]) (adjunctCM "終点"), 
  mylex ["にて"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [Nc]) (adjunctCM "場所"), 
  -- 格助詞（の）
  mylex ["の"] "(531)" (((NP [Nc]) `SL` N) `BS` (NP [Nc])) -- to be revised
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
  mylex ["得る","うる"] "(169)" ((S [VURU] [Term,Attr] `BS` NP [Ga]) `BS` NP [O]) (verbSR 2 "得る"),
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
  mylex ["良","よ"] "(173)" (S [ANAS] [Stem,UStem] `BS` NP [Ga]) (predSR "良い"),
  mylex ["無","な"] "(173)" (S [ANAS] [Stem] `BS` NP [Ga]) (predSR "無い"),
  mylex ["無","の"] "(173)" (S [ANAS] [UStem] `BS` NP [Ga]) (predSR "無い"),
  -- チイ形活用形容詞
  mylex ["弱っち","よわっち"] "(196)" (S [ATII] [Stem] `BS` NP [Ga]) (predSR "弱っちい"),
  mylex ["ちゃち"]           "(196)" (S [ATII] [Stem] `BS` NP [Ga]) (predSR "ちゃちい"),
  mylex ["ばばっち","ばばち"] "(196)" (S [ATII] [Stem] `BS` NP [Ga]) (predSR "ばばちい"),
  mylex ["ぼろっち","ぼろち"] "(196)" (S [ATII] [Stem] `BS` NP [Ga]) (predSR "ぼろちい"),
  mylex ["みみっち","みみち"] "(196)" (S [ATII] [Stem] `BS` NP [Ga]) (predSR "みみっちい"),
  -- ベシ形活用形容詞
  mylex ["如","ごと"] "(199)" ((S [ABES] [Stem] `BS` NP [Ga]) `BS` NP [Ga,No]) (verbSR 2 "如し"),
  mylex ["如","ごと"] "(199)" (S [ABES] [Stem] `BS` S anyPos [Attr]) (verbSR 2 "如し"),
  mylex ["べ"] "(200)" (S [ABES] [Stem] `BS` S verb [Term]) (modal "べし"),
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
  mylex ["たり"] "(308)" (S anyPos [Cont] `BS` S anyPos [EuphT])     (eventModifier "タ[MCN]"),
  mylex ["た"] "(308)" (S anyPos [Term,Attr] `BS` S anyPos [EuphT]) (eventModifier "タ[MCN]"),
  mylex ["たら"] "(308)" (S anyPos [Hyp] `BS` S anyPos [EuphT])     (eventModifier "タ[MCN]"),
  mylex ["たろ"] "(308)" (S anyPos [ModU] `BS` S anyPos [EuphT])    (eventModifier "タ[MCN]"),
  mylex ["だり"] "(309)" (S anyPos [Cont] `BS` S anyPos [EuphD])    (eventModifier "タ[MCN]"),
  mylex ["だ"] "(309)" (S anyPos [Term,Attr] `BS` S anyPos [EuphD]) (eventModifier "タ[MCN]"),
  mylex ["だら"] "(309)" (S anyPos [Hyp] `BS` S anyPos [EuphD])     (eventModifier "タ[MCN]"),
  mylex ["だろ"] "(309)" (S anyPos [ModU] `BS` S anyPos [EuphD])    (eventModifier "タ[MCN]"),
  -- 助動詞（丁寧）
  mylex ["ます"]   "(310)" (S verb [Term,Attr] `BS` S verb [Cont])   id,
  mylex ["ませ"]   "(310)" (S verb [Imper] `BS` S verb [Cont])       id,
  mylex ["まし"]   "(310)" (S verb [Imper,EuphT] `BS` S verb [Cont]) id,
  mylex ["ましょ"] "(310)" (S verb [ModU] `BS` S verb [Cont])        id,
  mylex ["ませ"]   "(311)" (S verb [Neg] `BS` S verb [Cont])        id,
  mylex ["まする"] "(311)" (S verb [Term,Attr] `BS` S verb [Cont])  id,
  mylex ["ますれ"] "(311)" (S verb [Hyp] `BS` S verb [Cont])        id,
  mylex ["ませい"] "(311)" (S verb [Imper] `BS` S verb [Cont])      id,
  mylex ["です"]   "(318)" (S [Aauo] [Term] `BS` S [Aauo] [Term])   id,
  -- ません＋です
  mylex ["ませんで"]   "(321)" (S verb[TeForm] `BS` S verb [Cont])     negOperator,
  mylex ["ませんです"] "(321)" (S verb [Term,Attr] `BS` S verb [Cont]) negOperator,
  mylex ["ませんでし"] "(321)" (S verb [EuphT] `BS` S verb [Cont])     negOperator,
 -- 助動詞（否定）
  mylex ["ぬ","ん"] "(325)" (S anyPos [Term,Attr] `BS` S anyPos [NegL]) negOperator,
  mylex ["ね"]      "(325)" (S anyPos [Hyp] `BS` S anyPos [NegL])       negOperator,
  mylex ["ざら"]     "(330)" (S anyPos [NegL] `BS` S anyPos [NegL])     negOperator,
  mylex ["ずに"]     "(330)" (S anyPos [NiForm] `BS` S anyPos [NegL])   negOperator,
  mylex ["ず"]      "(330)" (S anyPos [Term] `BS` S anyPos [NegL])     negOperator,
  mylex ["ざる"]    "(330)" (S anyPos [Attr] `BS` S anyPos [NegL])      negOperator,
  mylex ["ざれ"]    "(330)" (S anyPos [Hyp,Imper] `BS` S anyPos [NegL]) negOperator,
  mylex ["ないで"]  "(333)" (S verb [TeForm] `BS` S verb [Neg])         negOperator,
  mylex ["んで"]    "(333)" (S verb [TeForm] `BS` S verb [NegL])        negOperator,
  mylex ["な"]     "(343)" (S verb [Imper] `BS` S verb [Term])         negOperator,
  mylex ["な"]     "(345)" (S verb [Imper] `BS` S verb [Cont])         id,
  mylex ["なかれ"] "(346)" (S verb [Imper] `BS` S verb [Term])          negOperator,
   -- 助動詞（推量）
  mylex ["う"] "(349)" (S anyPos [Pre] `BS` S anyPos [ModU]) (modal "推量"),
  mylex ["う"] "(349)" (S anyPos [Pre] `BS` S anyPos [ModU]) (modal "意向"),
  mylex ["ん"] "(353)" (S anyPos [Pre] `BS` S anyPos [NegL]) (modal "意向"),
  mylex ["だろう","であろう","だろ"] "(357)" (S verb [Pre] `BS` S verb [Term])           (modal "MCN:ダロウ"),
  mylex ["だろう","であろう","だろ"] "(357)" (S adjective [Pre] `BS` S adjective [Term]) (modal "MCN:ダロウ"),
  mylex ["だろう","であろう","だろ"] "(357)" (S [Nda] [Pre] `BS` S [Nda] [Stem])         (modal "MCN:ダロウ"),
  mylex ["でしょう","でしょ"] "(357)" (S verb [Pre] `BS` S verb [Term])                 (modal "MCN:ダロウ"),
  mylex ["でしょう","でしょ"] "(357)" (S adjective [Pre] `BS` S adjective [Term])       (modal "MCN:ダロウ"),
  mylex ["でしょう","でしょ"] "(357)" (S [Nda] [Pre] `BS` S [Nda] [Stem])               (modal "MCN:ダロウ"),
  mylex ["まい"] "(359a)" (S verb [Pre] `BS` S verb [Term])         (Lam (Lam (App (Con "MCN:ダロウ") (Not (App (Var 1) (Var 0)))))),
  mylex ["まい"] "(359b)" (S verb [Pre] `BS` S [V1,VK,VS,VZ] [Neg]) (Lam (Lam (App (Con "MCN:ダロウ") (Not (App (Var 1) (Var 0)))))),
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
  mylex ["居","い"] "(381)" (S [V1] [Stem] `BS` S verb [TeForm,NiForm]) (eventModifier "MCN:テイル"),
  mylex ["有","あ"] "(381)" (S [V5ARU] [Stem] `BS` S adjective [Cont]) (eventModifier "MCN:テアル"),
  mylex ["有","あ"] "(381)" (S [V5ARU] [Stem] `BS` S [Nda] [TeForm]) (eventModifier "MCN:テアル"),
  mylex ["な"] "(383)" ((S [ANAS] [Stem] `BS` NP [Ga]) `BS` (S adjective [Cont]) `BS` NP [Ga]) negOperator,
  mylex ["な"] "(383)" ((S [ANAS] [Stem] `BS` NP [Ga]) `BS` (S [Nda] [TeForm] `BS` NP [Ga])) negOperator,
  mylex ["な"] "(384)" ((S [V5r] [Stem] `BS` NP [Ga]) `BS` (S adjective [Cont] `BS` NP [Ga])) (intensionalVerb 1 "成る"),
  mylex ["な"] "(384)" ((S [V5r] [Stem] `BS` NP [Ga]) `BS` (S [Nda] [NiForm] `BS` NP [Ga])) (intensionalVerb 1 "成る"),
  -- い省略
  mylex ["て"] "(403)" (S [V1] [Stem] `BS` S verb [EuphT]) (eventModifier "MCN:テイル"),
  -- 副助詞
  mylex ["は"] "(550)" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [Ga,O]))) `BS` (NP [Nc])) argumentCM,
  mylex ["には"] "(550)" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [Ni]))) `BS` (NP [Nc])) argumentCM,
  mylex ["も"] "(385)" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [Ga,O]))) `BS` (NP [Nc])) argumentCM,
  mylex ["にも"] "(385)" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [Ni]))) `BS` (NP [Nc])) argumentCM,
  mylex ["こそ"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ga,O]))) `BS` (NP [Nc])) argumentCM,
  mylex ["にこそ"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ni]))) `BS` (NP [Nc])) argumentCM,
  mylex ["さえ"] "(387)" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ga,O]))) `BS` (NP [Nc])) argumentCM,
  mylex ["にさえ","さえに"] "(387)" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ni]))) `BS` (NP [Nc])) argumentCM,
  mylex ["だけ"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ga,O]))) `BS` (NP [Nc])) argumentCM,
  mylex ["にだけ","だけに"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ni]))) `BS` (NP [Nc])) argumentCM,
  mylex ["ばかり"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ga,O]))) `BS` (NP [Nc])) argumentCM,
  mylex ["にばかり","ばかりに"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ni]))) `BS` (NP [Nc])) argumentCM,
  mylex ["のみ"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ga,O]))) `BS` (NP [Nc])) argumentCM,
  mylex ["にのみ","のみに"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ni]))) `BS` (NP [Nc])) argumentCM,
  -- 6.1.3 補助動詞（連用形接続）
  mylex ["始め","はじめ"] "(412)" (S [V1] [Stem] `BS` S verb [Cont]) (eventModifier "始める"),
  mylex ["込","こ"] "(412)" (S [V5m] [Stem] `BS` S verb [Cont])     (eventModifier "込む"),
  mylex ["出","だ"] "(412)" (S [V5s] [Stem] `BS` S verb [Cont])     (eventModifier "出す"),
  mylex ["合","あ"] "(412)" (S [V5w] [Stem] `BS` S verb [Cont])     (eventModifier "合う"),
  mylex ["続け","つづけ"] "(412)" (S [V1] [Stem] `BS` S verb [Cont]) (eventModifier "続ける"),
  mylex ["かけ"] "(412)" (S [V1] [Stem] `BS` S verb [Cont])         (eventModifier "かける"),
  mylex ["上げ","あげ"] "(412)" (S [V1] [Stem] `BS` S verb [Cont])   (eventModifier "上げる"),
  mylex ["切","き"] "(412)" (S [V5r] [Stem] `BS` S verb [Cont])     (eventModifier "切る"),
  mylex ["付け","つけ"] "(412)" (S [V1] [Stem] `BS` S verb [Cont])   (eventModifier "付ける"),
  mylex ["過ぎ","すぎ"] "(412)" (S [V1] [Stem] `BS` S verb [Cont])   (eventModifier "過ぎる"),
  mylex ["あぐ"] "(412)" (S [V5m] [Stem] `BS` S verb [Cont])        (eventModifier "あぐむ"),
  mylex ["かね"] "(412)" (S [V1] [Stem] `BS` S verb [Cont])         (eventModifier "かねる"),
  mylex ["やが"] "(412)" (S [V5r] [Stem] `BS` S verb [Cont])        (eventModifier "やがる"),
  -- 6.1.4 補助動詞（テ形接続）
  mylex ["お"] "(416)" (S [V5k] [Stem] `BS` S verb [TeForm])        (eventModifier "ASP:テオク"),
  mylex ["仕舞","しま"] "(416)" (S [V5w] [Stem] `BS` S verb [TeForm]) (eventModifier "ASP:テシマウ"),
  mylex ["行","い"] "(416)" (S [V5k] [Stem] `BS` S verb [TeForm])    (eventModifier "ASP:イク"),
  mylex ["見","み"] "(416)" (S [V1] [Stem] `BS` S verb [TeForm])     (eventModifier "ASP:ミル"),
  mylex ["見せ","みせ"] "(416)" (S [V1] [Stem] `BS` S verb [TeForm]) (eventModifier "ASP:ミセル"),
  -- 6.1.5 授受表現
  mylex ["上げ","あげ"] "(436)" ((S [V1] [Stem] `BS` NP [Ga]) `BS` (S verb [TeForm] `BS` NP [Ga]))
        (Lam (Lam (Lam (App (Con "アゲル") (Pair (Var 1) (App (App (Var 2) (Var 1)) (Var 0))))))),
  mylex ["貰","もら"] "(436)" (((S [V5w] [Stem] `BS` NP [Ga]) `BS` NP [Ni]) `BS` (S verb [TeForm] `BS` NP [Ga]))
        (Lam (Lam (Lam (Lam (App (Con "モラウ") (Pair (Var 1) (Pair (Var 2) (App (App (Var 3) (Var 2)) (Var 0))))))))),
  -- 6.1.6 −がる
  mylex ["が"] "(443)" (((S [V5r] [Stem] `BS` NP [Ga]) `BS` NP [O]) `BS` (S [Aauo,Ai,ANAS] [Stem] `BS` NP [Ga])) (intensionalVerb 2 "ガル"),
  -- 6.1.7 −めく
  mylex ["め"] "(453)" ((S [V5k] [Stem] `BS` NP [Ga]) `BS` N) (Lam (Lam (Lam (App (Con "メク") (Pair (Var 0) (App (App (Var 2) (Var 1)) (Var 0))))))), -- to be revised
  -- 形容詞性接尾語
  -- 6.2.1 ない
  mylex ["な"] "(455)" (S [ANAS] [Stem] `BS` S anyPos [Neg]) negOperator,
  mylex ["無","な"] "(458)" (S [Aauo,ANAS] [Stem] `BS` NP [Ga]) (predSR "無い"),
  mylex ["ねえ","ねぇ","ねー"] "(455)" (S [ANAS] [Term,Attr] `BS` S anyPos [Neg]) negOperator,
  mylex ["ねえ","ねぇ","ねー"] "(458)" (S [Aauo,ANAS] [Term,Attr] `BS` NP [Ga]) (predSR "無い"),
  mylex ["無し","なし","ナシ"] "(467)" (S [Nda,Nno,Nni] [Stem] `BS` NP [Ga])    (predSR "無い"),
  -- 6.2.2 たい
  mylex ["た"] "(474a)" ((S [Aauo] [Stem] `BS` NP [Ga]) `BS` (S verb [Cont] `BS` NP [Ga])) (intensionalVerb 1 "たい"),
  mylex ["た"] "(474b)" (((S [Aauo] [Stem] `BS` NP [Ni,Ga]) `BS` NP [Ga]) `BS` ((S verb [Cont] `BS` NP [Ga])) `BS` NP [O]) (intensionalVerb 2 "たい"),
  mylex ["難","にく"] "(475a)" ((S [Aauo] [Stem] `BS` NP [Ga]) `BS` (S verb [Cont] `BS` NP [Ga])) (intensionalVerb 1 "難"),
  mylex ["難","にく"] "(475b)" (((S [Aauo] [Stem] `BS` NP [Ni,Ga]) `BS` NP [Ga]) `BS` ((S verb [Cont] `BS` NP [Ga])) `BS` NP [O]) (intensionalVerb 2 "難"),
  mylex ["易","やす"] "(476a)" ((S [Aauo] [Stem] `BS` NP [Ga]) `BS` (S verb [Cont] `BS` NP [Ga])) (intensionalVerb 1 "易"),
  mylex ["易","やす"] "(476b)" (((S [Aauo] [Stem] `BS` NP [Ni,Ga]) `BS` NP [Ga]) `BS` ((S verb [Cont] `BS` NP [Ga])) `BS` NP [O]) (intensionalVerb 2 "易"),
  -- 6.2.3-6.2.6
  mylex ["らし"] "(478)" (S [Ai] [Stem] `BS` S verb [Term])      (modal "MCN:ラシイ"),
  mylex ["らし"] "(478)" (S [Ai] [Stem] `BS` S adjective [Term]) (modal "MCN:ラシイ"),
  mylex ["らし"] "(478)" (S [Ai] [Stem] `BS` S [Nda] [Stem])     (modal "MCN:ラシイ"),
  mylex ["っぽ"] "(480)" (S [Aauo] [Stem] `BS` S [Aauo,Nda] [Stem]) (modal "MCN:ポイ"),
  mylex ["くさ"] "(478)" (S [Ai] [Stem] `BS` S verb [Term])         (modal "MCN:クサイ"),
  mylex ["くさ"] "(478)" (S [Ai] [Stem] `BS` S adjective [Term])    (modal "MCN:クサイ"),
  mylex ["くさ"] "(478)" (S [Ai] [Stem] `BS` S [Nda] [Stem])       (modal "MCN:クサイ"),
  mylex ["べき"] "(488)" (S [Nda] [Attr] `BS` S verb [Term])       (modal "MCN:ベキ"),
  -- 状詞性接尾語
  mylex ["よう"] "(493)" (S [Nda,Nna,Nni] [Stem] `BS` S anyPos [Attr]) (modal "MCN:ヨウダ"),
  mylex ["そう"] "(497)" (S [Nda] [Stem] `BS` S anyPos [Term])         (modal "MCN:ソウダ伝聞"),
  mylex ["そう"] "(498)" (S [Nda,Nna,Nni] [Stem] `BS` S anyPos [ModS]) (modal "MCN:ソウダ推量"),
  mylex ["がち"] "(506)" (S [Nda,Nna,Nni] [Stem] `BS` S anyPos [Cont]) (modal "MCN:ガチダ"),
  mylex ["みたい"] "(507)" (S [Nda,Nna,Nni] [Stem] `BS` S verb [Term]) (modal "MCN:ミタイダ"),
  mylex ["みたい"] "(507)" (S [Nda,Nna,Nni] [Stem] `BS` S adjective [Term]) (modal "MCN:ミタイダ"),
  mylex ["みたい"] "(507)" (S [Nda,Nna,Nni] [Stem] `BS` S [Nda] [Stem]) (modal "MCN:ミタイダ"),
  mylex ["的","てき"] "(508)" ((S [Nda,Nna,Nni] [Stem] `BS` NP [Ga]) `BS` NP [Nc]) (Lam (Lam (Eq (Con "Entity") (Var 0) (Var 1)))),-- ??
  mylex ["的","てき"] "(508)" ((S [Nda,Nna,Nni] [Stem] `BS` NP [Ga]) `BS` N) (Lam (Lam (App (Var 1) (Var 0)))),-- ??
  mylex ["気味","ぎみ"] "(509)" ((S [Nda,Nna,Nno,Nni] [Stem] `BS` NP [Ga]) `BS` N) (Lam (Lam (App (Var 1) (Var 0)))),-- ??
  mylex ["なの"] "(510)" (S [Nda] [Stem] `BS` S [Nda] [Stem])          (modal "MCN:ナノダ"),
  mylex ["の"] "(511)" (S [Nda] [Stem] `BS` S anyPos [Attr])           (modal "MCN:ノダ"),
  mylex ["筈","はず","ハズ"] "(511)" (S [Nda] [Stem] `BS` S anyPos [Attr]) (modal "MCN:ハズダ"),
  mylex ["訳","わけ","ワケ"] "(511)" (S [Nda] [Stem] `BS` S anyPos [Attr]) (modal "MCN:ワケダ"),
  mylex ["もの"] "(511)" (S [Nda] [Stem] `BS` S anyPos [Attr])            (modal "MCN:モノダ"),
  -- 照応
  mylex ["ここ","そこ","あそこ"] "(586)" (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) 
        (Lam (App (Var 1) (Asp 1 (Proj Fst (Sigma (Con "entity") (App (Con "場所") (Var 0))))))),
  mylex ["この","その","あの"] "(589)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) `SL` N) -- to be revised
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
  mylex ["このよう"] "" (S [Nna,Nni] [Stem] `BS` NP [Ga]) id,
  mylex ["あのよう"] "" (S [Nna,Nni] [Stem] `BS` NP [Ga]) id,
  mylex ["そのよう"] "" (S [Nna,Nni] [Stem] `BS` NP [Ga]) id,
  mylex ["どのよう"] "" (S [Nna,Nni] [Stem] `BS` NP [Ga]) id,
  -- カ節
  mylex ["か","かどうか"] "(603)" (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc]) `BS` S (verb++adjective) [Term]) 
        (Lam (Lam (App (Var 0) (App (Con "ドウカ") (Var 1))))), 
  mylex ["か","かどうか"] "(603)" (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc]) `BS` S [Nda] [Stem]) 
        (Lam (Lam (App (Var 0) (App (Con "ドウカ") (Var 1))))), 
  -- 態
  mylex ["れ"] "(607)" (((S [V1] [Stem] `BS` NP [Ga]) `BS` NP [Ni]) `BS` (S anyPos [VoR] `BS` NP [Ga])) 
        (Lam (Lam (Lam (Lam (App (App (Var 3) (Var 2)) (Lam (Sigma (Con "event") (Sigma (App (App (App (Con "迷惑") (Var 1)) (Var 3)) (Var 0)) (App (Var 3) (Var 1)))))))))),
  mylex ["れ"] "(608)" (((S [V1] [Stem] `BS` NP [Ga]) `BS` NP [Ni,Niyotte]) `BS` ((S anyPos [VoR] `BS` NP [Ga]) `BS` NP [Ni,O]))
        (Lam (Lam (Lam (Lam (App (App (App (Var 3) (Var 1)) (Var 2)) (Var 0)))))),
  mylex ["せ"] "(629)" (((S [V1] [Stem] `BS` NP [Ga]) `BS` NP [Ni]) `BS` (S anyPos [VoS] `BS` NP [Ga]))
        (Lam (Lam (Lam (Lam (App (App (Var 3) (Var 2)) (Lam (Sigma (Con "event") (Sigma (App (App (App (Con "使役") (Var 1)) (Var 3)) (Var 0)) (App (Var 3) (Var 1)))))))))),
  mylex ["せ"] "(635)" (((S [V1] [Stem] `BS` NP [Ga]) `BS` NP [O]) `BS` (S anyPos [VoS] `BS` NP [Ga]))
        (Lam (Lam (Lam (Lam (App (App (Var 3) (Var 2)) (Lam (Sigma (Con "event") (Sigma (App (App (App (Con "使役") (Var 1)) (Var 3)) (Var 0)) (App (Var 3) (Var 1)))))))))), 
  -- 以下、「可能」はstateを導入すべきか。
  mylex ["れ"] "(660a)" ((S [V1] [Stem] `BS` NP [Ga]) `BS` (S [V1,VK] [VoR] `BS` NP [Ga]))
        (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (Lam (App (App (Var 3) (Var 0)) (Var 1)))))))),
  mylex ["れ"] "(660b)" (((S [V1] [Stem] `BS` NP [Ni,Ga]) `BS` NP [Ga]) `BS` ((S [V1,VK] [VoR] `BS` NP [Ga]) `BS` NP [O]))
        (Lam (Lam (Lam (Lam (App (Con "可能") (Pair (Var 2) (Lam (App (App (App (Var 4) (Var 0)) (Var 2)) (Var 1))))))))),
  mylex ["得","え"] "(661a)" ((S [V1] [Stem] `BS` NP [Ga]) `BS` (S verb [Cont] `BS` NP [Ga]))
        (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (App (App (Var 2) (Var 1)) (Var 0))))))),
  mylex ["得","え"] "(661b)" (((S [V1] [Stem] `BS` NP [Ga,Ni]) `BS` NP [Ga]) `BS` ((S verb [Cont] `BS` NP [Ga]) `BS` NP [O]))
        (Lam (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (App (App (App (Var 3) (Var 2)) (Var 1)) (Var 0)))))))),
  mylex ["得る","うる"] "(662a)" ((S [VURU] [Term,Attr] `BS` NP [Ga]) `BS` (S verb [Cont] `BS` NP [Ga]))
        (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (App (App (Var 2) (Var 1)) (Var 0))))))),
  mylex ["得る","うる"] "(662b)" (((S [VURU] [Term,Attr] `BS` NP [Ga,Ni]) `BS` NP [Ga]) `BS` ((S verb [Cont] `BS` NP [Ga]) `BS` NP [Ni,O]))
        (Lam (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (App (App (App (Var 3) (Var 2)) (Var 1)) (Var 0)))))))),
  mylex ["れ"] "(666)" (S [V1] [VoE] `BS` S [V1] [Stem])
        id,
  mylex ["来れ","これ"] "(667)" (S [VK] [VoE] `BS` S [VK] [Stem]) id,
  mylex ["出来","でき"] "new" ((S [V1] [Term,Attr] `BS` NP [Ga]) `BS` (S [V1] [Stem] `BS` NP [Ga]))
        (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (App (App (Var 2) (Var 1)) (Var 0))))))),
  mylex ["出来","でき"] "new" (((S [V1] [Term,Attr] `BS` NP [Ga,Ni]) `BS` NP [Ga]) `BS` ((S [VS] [Stem] `BS` NP [Ga]) `BS` NP [Ni,O]))
        (Lam (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (App (App (App (Var 3) (Var 2)) (Var 1)) (Var 0)))))))),
  -- 複文
  mylex ["が"] "(711)" ((T True 1 anySExStem `SL` T True 1 anySExStem) `BS` S anyPos [Term]) (Lam (Lam (Lam (Sigma (App (Var 2) (Lam Top)) (App (Var 1) (Var 0)))))),
  mylex ["し"] "(713)" ((T True 1 anySExStem `SL` T True 1 anySExStem) `BS` S anyPos [Term]) (Lam (Lam (Lam (Sigma (App (Var 2) (Lam Top)) (App (Var 1) (Var 0)))))),
  -- 状詞の副詞用法
  mylex ["に"] "(728)" ((T True 1 anySExStem `SL` T True 1 anySExStem) `BS` (S [Nni] [Stem] `BS` NP [Ga])) (Lam (Lam (Lamvec (Lam (App (Appvec 1 (Var 2)) (Lam (Sigma (App (Var 4) (Var 0)) (App (Var 1) (Var 0))))))))),
  mylex ["と"] "(731)" ((T True 1 anySExStem `SL` T True 1 anySExStem) `BS` (S [Nto] [Stem] `BS` NP [Ga])) (Lam (Lam (Lamvec (Lam (App (Appvec 1 (Var 2)) (Lam (Sigma (App (Var 4) (Var 0)) (App (Var 1) (Var 0))))))))),
  --mylex ["が"] "(711)" ((T True 1 anySExStem `SL` T True 1 anySExStem) `BS` S anyPos [Term]) 
  --      (Lam (Lam (Lamvec (Lamvec (Sigma (Appvec 0 (Var 3)) (Appvec 1 (Appvec 2 (Var 3)))))))),
  -- Wh句
  mylex ["誰","だれ"] "" (NP [Nc]) (Con "誰"),
  mylex ["何","なに"] "" (NP [Nc]) (Con "何"),
  mylex ["どこ"] "" (NP [Nc]) (Con "どこ"),
  -- 従属節導入表現
  -- 連用節
  --mylex ["に"] "" ((T True 1 anySExStem `SL` T True 1 anySExStem) `BS` (S [Nni] [Stem])) (Lam (Lam (Sigma (Var 1) (Var 1)))),
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
  mylex ["すべての","あらゆる","一人一人の","各","各々の","それぞれの"] "(534)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) `SL` N) 
        (Lam (Lam (Lamvec (Pi (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),
  mylex ["一人の","或る","ある","何人かの","数人の"] "(534)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) `SL` N) 
        (Lam (Lam (Lamvec (Sigma (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),
  -- 量化表現：N-no Q
  mylex ["の一人一人","のそれぞれ","のすべて","の全員"] "(535)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) `BS` N) 
        (Lam (Lam (Lamvec (Pi (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),
  mylex ["の一人","の何人か","の数人","の誰か"] "(535)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) `BS` N) 
        (Lam (Lam (Lamvec (Sigma (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),
  -- 量化表現：Q N
  mylex ["全員"] "(536)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) `BS` N) 
        (Lam (Lam (Lamvec (Pi (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),
  mylex ["一人"] "(536)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) `BS` N) 
        (Lam (Lam (Lamvec (Sigma (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),
  -- 遊離数量詞
  -- mylex ["全員","みな","誰も","すべて","それぞれ"]
  -- mylex ["一人","誰か"]
  -- 存在動詞
  mylex ["い"] "" (S [V1] [Stem] `BS` NP [Ga]) (Lam (Lam (Sigma (Con "entity") (Sigma (Eq (Con "entity") (Var 0) (Var 2)) (App (Var 2) (Var 0)))))),
  mylex ["あ"] "" (S [V5ARU] [Stem] `BS` NP [Ga]) (Lam (Lam (Sigma (Con "entity") (Sigma (Eq (Con "entity") (Var 0) (Var 2)) (App (Var 2) (Var 0)))))),
  -- 状詞
  mylex ["多分"] "(Adv)" (S [Nna,Nni] [Stem] `BS` NP [Ga]) (predSR "多分"),
  mylex ["多分"] "(Adv)" (S [Nemp] [Stem] `BS` NP [Ga]) (modal "タブン"),
  -- JSeM語彙
  mylex ["世界最高"] "" (S [Nda,Nno,Nni,Ntar] [Stem] `BS` NP [Ga]) (predSR "世界最高")
  ]

