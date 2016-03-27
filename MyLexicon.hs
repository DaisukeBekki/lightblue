{-# Options -Wall #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures #-}

{-|
Module      : MyLexicon
Description : A user-defined lexicon of Japanese
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}
module MyLexicon (
  emptyCategories,
  myLexicon
  ) where 

import Prelude hiding (id)
import qualified Data.Text.Lazy as T
import CombinatoryCategorialGrammar
import DependentTypes
import Data.Ratio

ec :: T.Text -> T.Text -> Integer -> Cat -> Preterm -> Node
ec word num r c s = Node {rs=EC, pf=word, cat=c, sem=s, daughters=[], score=(r % 100), source=num}

-- | A list of empty categories (i.e. lexical items whose PF is empty).
emptyCategories :: [Node]
emptyCategories = [
  -- 一段動詞活用語尾
  --lexicalitem "\\emp" "(132)" 100
  --            ((defS [V1] [Neg,Cont,NegL,EuphT]) `BS` (defS [V1] [Stem]))
  --            id,
  -- カ行変格活用動詞語幹
  ec "come" "(154)" 100
              ((defS [VK] [Stem] `BS` NP [Ga]) `BS` NP [Ni])
              (verbSR 2 "来る"),
  -- サ行変格活用動詞語幹 -- とりあえずガヲ、ガヲトのパターンのみ。
  ec "\\emp" "(156)" 100
              ((defS [VS] [Stem] `BS` NP [Ga]) `BS` NP [O])
              (verbSR 2 "する"),
  --lexicalitem "\\emp" "(156)" (100%100)
  --            (((defS [VS] [Stem] `BS` NP [Ga]) `BS` NP [O]) `BS` defS [To])
  --            (Lam (Lam (App (Con "する") (Pair (Var 0) (Var 1))))),
  -- イ形容詞終止形活用語彙
  ec "\\emp" "(175)" 100
              (S [Ai] [Term] [M,M,F 1 PM,M,M] `BS` S [Ai] [Stem] [M,M,F 1 PM,M,M])
              id,
  -- 判定詞語幹
  ec "be" "(235a)" 100
              ((defS [Nda,Nno,Ntar] [Stem] `BS` NP [Ga]) `BS` N)
              id,
  ec "be" "(235b)" 100
              ((defS [Nda] [Stem] `BS` NP [Ga]) `BS` NP [Nc])
              (Lam (Lam (Lam (Sigma (Eq (Con "entity") (Var 1) (Var 2)) (App (Var 1) (Var 0)))))),
  -- サ変語幹→状詞語幹
  ec "\\emp" "(262)" 99
              (defS [Nda,Nno] [Stem] `BS` defS [VSN] [Stem])
              id,
  -- サ変語幹→名詞
  ec "\\emp" "ss" 99
              ((((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) `BS` NP [No]) `BS` NP [No]) `BS` ((defS [VS] [Stem] `BS` NP [Ga]) `BS` NP [O]))
              (Lam (Lam (Lam (Lam (Lamvec (App (App (App (Var 4) (Var 3)) (Var 2)) (Lam (Appvec 1 (App (Var 2) (Var 0)))))))))),
  -- 形式述語スル
  ec "do" "(380a)" 100
              (defS [VS,VSN] [Stem] `BS` S verb [Cont] [M,M,M,M,P])
              id,
  ec "do" "(380b)" 100
              (defS [VS,VSN] [Stem] `BS` S verb [Cont] [P,PM,PM,M,PM])
              id,
  -- 補助動詞「くる」
  ec "come" "(416)" 100
              (defS [VK] [Stem] `BS` defS verb [TeForm])
              (eventModifier "来る"),
              --(Lam (App (Con "来る") (Var 0))),
  -- 推量のウ
  ec "\\emp" "(349)" 99
              (S anyPos [Pre] [(F 1 PM),(F 2 PM),(F 3 PM),M,M] `BS` S anyPos [ModU] [(F 1 PM),(F 2 PM),(F 3 PM),M,M])
              (modal "ウ[MCN]"),
  -- 可能態
  ec "\\emp" "(652a)" 99
              ((defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` NP [Ga]) `BS` (defS anyPos [VoE] `BS` NP [Ga]))
              (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (Lam (App (App (Var 3) (Var 0)) (Var 1)))))))),
  ec "\\emp" "(652b)" 99
              (((defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` NP [Ni,Ga]) `BS` NP [Ga]) `BS` ((defS anyPos [VoE] `BS` NP [Ga]) `BS` NP [O]))
              (Lam (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (Lam (App (App (App (Var 4) (Var 3)) (Var 0)) (Var 1))))))))),
  -- 状詞の副詞用法: \p.\q.\v.\c.qv(\e.pe ∧ ce)
  ec "\\emp" "(730)" 99
              ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` (defS [Nemp] [Stem] `BS` NP [Ga]))
              (Lam (Lam (Lam (App (Var 1) (Lam (App (App (Var 3) (Var 0)) (Lam (App (Var 2) (Var 1))))))))),
  -- 空冠詞（存在量化）
  ec "∃ " "(544)" 99
              ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) `SL` N)
              (Lam (Lam (Lamvec (Sigma (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),
  -- 空助詞
  ec "cm" "(515)" 50
              (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ga,O,Ni]))) `BS` (NP [Nc]))
              argumentCM,
  -- pro1
  ec "pro" "(597)" 95
              (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Ga,O,Ni,To,No]))
              (Lam (App (Var 0) (Asp 1 (Con "entity")))),
  -- pro2
  ec "pro" "(597)" 90
              (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Ga,O,Ni,To,No]))
              (Lam (App (Var 0) (Asp 2 (Con "entity")))),
  -- pro3
  --lexicalitem "$pro_3$" "(597)" 80
  --            (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Ga,O,Ni,To,No]))
  --            (Lam (App (Var 0) (Asp 3 (Con "entity")))),
  -- 関係節化演算子(relativizer)
  ec "rel" "(670)" 99
              ((N `SL` N) `BS` (S anyPos [Attr] [PM,PM,PM,M,M] `BS` NP [Ga,O,Ni,To]))
              (Lam (Lam (Lam (Lam (Sigma (App (App (Var 3) (Var 1)) (Lam Top)) (App (App (Var 3) (Var 2)) (Var 1))))))),
  ec "rel-ext" "(670)+" 96
              ((N `SL` N) `BS` (S anyPos [Attr] [PM,PM,PM,M,M]))
              (Lam (Lam (Lam (Lam (Sigma (App (App (Var 2) (Var 1)) (Var 0)) (Sigma (App (Var 4) (Lam Top)) (Sigma (Pi (Con "event") (Pi (Con "entity") Type)) (App (App (Var 0) (Var 1)) (Var 4))))))))),
  -- 連用節、テ節
  ec "\\emp" "new" 99
              ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` (S anyPos [Cont] [M,PM,PM,M,M]))
              (Lam (Lam (Lam (Sigma (App (Var 2) (Lam Top)) (App (Var 2) (Var 1)))))),
  ec "\\emp" "new" 99
              ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` (S anyPos [TeForm] [M,PM,PM,M,M]))
              (Lam (Lam (Lam (Sigma (App (Var 2) (Lam Top)) (App (Var 2) (Var 1))))))
  -- ダロウ接続形を派生する空範疇
  -- lexicalitem "\\emp" "(354)" (99%100)
  --             (defS (verb++adjective) [ModD] `BS` defS (verb++adjective) [Term])
  --             id,
  -- lexicalitem "\\emp" "(354)" (99%100)
  --             (defS [Nda] [ModD] `BS` defS [Nda] [Stem])
  --             id,
  ]

{- Some Macros for adding lexical items to lexicon -}

mylex :: [T.Text] -> T.Text -> Cat -> Preterm -> [Node]
mylex wds num cat' sem' = [(lexicalitem wd num 100 cat' sem') | wd <- wds ]

conjSuffix :: T.Text -> T.Text -> [PosFeature] -> [ConjFeature] -> [Node]
conjSuffix wd num catpos catconj = [lexicalitem wd num 100 ((defS catpos catconj) `BS` (defS catpos [Stem])) id]

teidaiS :: Cat
teidaiS = S anyPos [Term,Imper,Pre,TeForm] [PM,PM,PM,M,M]

-- | A list of (mostly functional) lexical items extracted from Bekki (2010).
myLexicon :: [Node]
myLexicon = concat $ [
  -- 格助詞
  -- argument:
  --mylex ["が"] "(524)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Ga])) `BS` NP [Nc]) argumentCM,
  mylex ["を"] "(524)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [O])) `BS` NP [Nc]) argumentCM,
  mylex ["に"] "(524)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Ni])) `BS` NP [Nc]) argumentCM,
  mylex ["と"] "(524)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [To])) `BS` NP [Nc]) argumentCM,
  mylex ["によって"] "(524)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Niyotte])) `BS` NP [Nc]) argumentCM,
  mylex ["が","の"] "(531)?" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Ga,No])) `BS` NP [Nc]) argumentCM,
  -- adjunct:
  mylex ["へ","へは"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [Nc]) (adjunctCM "終点"), 
  mylex ["で","では"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [Nc]) (adjunctCM "場所"), 
  mylex ["から","からは"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [Nc]) (adjunctCM "始点"), 
  mylex ["まで","までは"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [Nc]) (adjunctCM "終点"), 
  mylex ["より","よりは"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [Nc]) (adjunctCM "起点"), 
  mylex ["にて"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [Nc]) (adjunctCM "場所"), 
  -- 格助詞（の）
  mylex ["の"] "(531)+" ((N `SL` N) `BS` NP [Nc]) -- これだとダメ
               (Lam (Lam (Lam (Lam (Sigma (App (App (Con "の[MCN]") (Var 1)) (Var 3)) (App (App (Var 2) (Var 1)) (Var 0))))))),
  -- 等位接続（連言）
  mylex ["と"] "new" CONJ (Lam (Lam (Sigma (Var 1) (Var 1)))),
  mylex ["や"] "new" CONJ (Lam (Lam (Sigma (Var 1) (Var 1)))),
  mylex ["そして"] "new" CONJ (Lam (Lam (Sigma (Var 1) (Var 1)))),
  mylex ["および"] "new" CONJ (Lam (Lam (Sigma (Var 1) (Var 1)))),
  mylex ["ならびに","並びに"] "new" CONJ (Lam (Lam (Sigma (Var 1) (Var 1)))),
  mylex ["やら"] "new" CONJ (Lam (Lam (Sigma (Var 1) (Var 1)))),
  mylex ["だの"] "new" CONJ (Lam (Lam (Sigma (Var 1) (Var 1)))),
  mylex ["に"] "new" CONJ (Lam (Lam (Sigma (Var 1) (Var 1)))),
  mylex ["なり"] "new" CONJ (Lam (Lam (Sigma (Var 1) (Var 1)))),
  -- 等位接続（選言）
  mylex ["か"] "new" CONJ (Lam (Lam (Pi (Not (Var 1)) (Var 1)))),
  mylex ["または","又は"] "new" CONJ (Lam (Lam (Pi (Not (Var 1)) (Var 1)))),
  mylex ["もしくは"] "new" CONJ (Lam (Lam (Pi (Not (Var 1)) (Var 1)))),
  mylex ["あるいは"] "new" CONJ (Lam (Lam (Pi (Not (Var 1)) (Var 1)))),
  mylex ["ないしは","ないし"] "new" CONJ (Lam (Lam (Pi (Not (Var 1)) (Var 1)))),
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
  mylex ["ん"] "(81)" (S [V5r] [Neg,Term,Attr] [M,M,M,P,M] `BS` defS [V5r] [Stem]) id,
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
  -- conjSuffix "る" "(132)" [V1] [Term,Attr], -- ５段とまとめた。
  conjSuffix "れ" "(132)" [V1] [Hyp],
  conjSuffix "ろ" "(132)" [V1] [Imper],
  conjSuffix "よ" "(132)" [V1] [Imper,ModU],
  conjSuffix "い" "(132)" [V1] [Imper],
  mylex ["ん"] "(132)" (S [V1] [Neg,Term,Attr] [M,M,M,P,M] `BS` defS [V1] [Stem]) id,
  -- 音便形
  conjSuffix "い" "(76)-" [V5k,V5g] [EuphT],
  conjSuffix "っ" "(78)-" [V5t,V5r,V5w,V5IKU,V5ARU,V5NAS] [EuphT],
  conjSuffix "ん" "(79)-" [V5n,V5m,V5b] [EuphD],  
  conjSuffix "う" "(128)" [V5TOW] [EuphT],
  conjSuffix "い" "(123)" [V5NAS] [Cont,Imper],
  -- ナサル型活用動詞
  mylex ["なさ"] "(122)" (defS [V5NAS] [Stem] `BS` defS verb [Cont]) id,
  mylex ["なさ"] "(122)" (defS [V5NAS] [Stem] `BS` defS [VSN] [Stem]) id,
  mylex ["いらっしゃ"] "(122)" (defS [V5NAS] [Stem] `BS` NP [Ga]) (verbSR 1 "来る"),
  mylex ["いらっしゃ"] "(122)" (defS [V5NAS] [Stem] `BS` defS anyPos [TeForm]) (eventModifier "来る"),
  mylex ["仰","おっしゃ"] "(122)" ((defS [V5NAS] [Stem] `BS` NP [Ga]) `BS` Sbar [ToCL]) (verbSR 2 "言う"),
  mylex ["下さ","くださ"] "(122)" (((defS [V5NAS] [Stem] `BS` NP [Ga]) `BS` NP [Ni]) `BS` NP [O]) (verbSR 3 "クレル"),
  mylex ["下さ","くださ"] "(122)" (defS [V5NAS] [Stem] `BS` defS anyPos [TeForm]) (eventModifier "クレル"),
  mylex ["ござ","御座"] "(122)" (defS [V5NAS] [Stem] `BS` defS anyPos [Cont]) id,
  -- 例外的な命令形
  mylex ["くれ"] "(149)" (((defS [V1] [Imper] `BS` NP [Ga]) `BS` NP [Ni]) `BS` NP [O]) (verbSR 3 "くれる"),
  mylex ["くれ"] "(150)" (defS [V1] [Imper] `BS` S verb [TeForm] [M,M,PM,M,M]) id,
  mylex ["射れ","いれ"] "(151)" ((defS [V5r] [Imper] `BS` NP [Ga]) `BS` NP [O]) (verbSR 2 "射る"),
  mylex ["蹴ろ","けろ"] "(152)" ((defS [V1] [Imper] `BS` NP [Ga]) `BS` NP [O]) (verbSR 2 "蹴る"),
  mylex ["捻ろ","ひねろ"] "(153)" ((defS [V1] [Imper] `BS` NP [Ga]) `BS` NP [O]) (verbSR 2 "捻る"),
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
  mylex ["来ん","くん"] "(155)" (S [VK] [Term,Attr] [M,M,M,P,M] `BS` defS [VK] [Stem]) id,
  --- サ変動詞
  conjSuffix "さ" "(157)" [VS,VSN] [VoR,VoS],
  conjSuffix "し" "(157)" [VS,VSN] [Neg,Cont,EuphT],
  conjSuffix "する" "(157)" [VS,VSN] [Term,Attr],
  conjSuffix "すれ" "(157)" [VS,VSN] [Hyp],
  conjSuffix "しろ" "(157)" [VS,VSN] [Imper],
  conjSuffix "しよ" "(157)" [VS,VSN] [ModU],
  conjSuffix "せ" "(157)" [VS,VSN] [NegL],
  conjSuffix "す" "(157)" [VS,VSN] [Term],
  conjSuffix "せよ" "(157)" [VS,VSN] [Imper],
  conjSuffix "せい" "(157)" [VS,VSN] [Imper],
  mylex ["すん"] "(157)" (S [VS,VSN] [Term,Attr] [M,M,M,P,M] `BS` defS [VS,VSN] [Stem]) id,
  --- ザ変動詞
  conjSuffix "じ" "(164)" [VZ] [Neg,NegL,Cont,EuphT],
  conjSuffix "ずる" "(164)" [VZ] [Term,Attr],
  conjSuffix "ずれ" "(164)" [VZ] [Hyp],
  conjSuffix "じろ" "(164)" [VZ] [Imper],
  conjSuffix "じよ" "(164)" [VZ] [ModU],
  conjSuffix "ぜ" "(164)" [VZ] [NegL],
  conjSuffix "ぜら" "(164)" [VZ] [VoR],
  conjSuffix "ず" "(164)" [VZ] [Term],
  conjSuffix "ぜよ" "(164)" [VZ] [Imper],
  --- ウル型活用動詞
  mylex ["得る","うる"] "(169)" ((defS [VURU] [Term,Attr] `BS` NP [Ga]) `BS` NP [O]) (verbSR 2 "得る"),
  -- 形容詞活用語尾
  --adverb = [Aauo, Ai, ANAS, ABES]
  mylex ["く"]  "(174)" (S [Aauo,Ai,ANAS,ATII,ABES] [Cont] [M,M,F 1 PM,M,M] `BS` S [Aauo,Ai,ANAS,ATII,ABES] [Stem] [M,M,F 1 PM,M,M]) id,
  mylex ["い","し"]  "(174)" (S [Aauo,Ai,ANAS,ATII] [Term,Attr] [M,M,F 1 PM,M,M] `BS` S [Aauo,Ai,ANAS,ATII,ABES] [Stem] [M,M,F 1 PM,M,M]) id, --- 「し」
  mylex ["けれ"] "(174)" (S [Aauo,Ai,ANAS,ATII] [Hyp] [M,M,F 1 PM,M,M] `BS` S [Aauo,Ai,ANAS,ATII,ABES] [Stem] [M,M,F 1 PM,M,M]) id,
  mylex ["かろ"] "(174)" (S [Aauo,Ai,ANAS,ATII] [ModU] [M,M,F 1 PM,M,M] `BS` S [Aauo,Ai,ANAS,ATII,ABES] [Stem] [M,M,F 1 PM,M,M]) id,
  mylex ["かっ"] "(174)" (S [Aauo,Ai,ANAS,ATII] [EuphT] [M,M,F 1 PM,M,M] `BS` S [Aauo,Ai,ANAS,ATII,ABES] [Stem] [M,M,F 1 PM,M,M]) id,
  mylex ["から"] "(175)" (S [Aauo,Ai,ANAS,ATII,ABES] [NegL] [M,M,F 1 PM,M,M] `BS` S [Aauo,Ai,ANAS,ATII,ABES] [Stem] [M,M,F 1 PM,M,M]) id,
  mylex ["う"]   "(174)" (S [Aauo,Ai,ANAS,ATII] [Cont] [M,M,F 1 PM,M,M] `BS` S [Aauo,Ai,ANAS,ATII] [UStem] [M,M,F 1 PM,M,M]) id,
  mylex ["し"]   "(175)" (S [Aauo,ANAS,ABES] [Term] [M,M,F 1 PM,M,M] `BS` S [Aauo,Ai,ANAS,ATII,ABES] [Stem] [M,M,F 1 PM,M,M]) id,
  mylex ["き"]   "(175)" (S [Aauo,Ai,ANAS,ATII,ABES] [Attr] [M,M,F 1 PM,M,M] `BS` S [Aauo,Ai,ANAS,ATII,ABES] [Stem] [M,M,F 1 PM,M,M]) id,
  mylex ["かれ"] "(175)" (S [Aauo,Ai,ANAS,ATII,ABES] [Imper] [M,M,F 1 PM,M,M] `BS` S [Aauo,Ai,ANAS,ATII,ABES] [Stem] [M,M,F 1 PM,M,M]) id,
  -- ナシ型活用形容詞
  mylex ["良","よ"] "(173)" (defS [ANAS] [Stem,UStem] `BS` NP [Ga]) (predSR 1 "良い"),
  mylex ["無","な"] "(173)" (S [ANAS] [Stem] [M,M,P,M,M] `BS` NP [Ga]) (predSR 1 "無い"), --  +nとした
  mylex ["無","な"] "(173)+" ((S [ANAS] [Stem] [M,M,P,M,M] `BS` NP [Ga]) `BS` NP [Ni]) (predSR 2 "無い"),
  mylex ["無","の"] "(173)" (S [ANAS] [UStem] [M,M,P,M,M] `BS` NP [Ga]) (predSR 1 "無い"), -- +nとした
  -- チイ形活用形容詞
  mylex ["弱っち","よわっち"] "(196)" (defS [ATII] [Stem] `BS` NP [Ga]) (predSR 1 "弱っちい"),
  mylex ["ちゃち"]           "(196)" (defS [ATII] [Stem] `BS` NP [Ga]) (predSR 1 "ちゃちい"),
  mylex ["ばばっち","ばばち"] "(196)" (defS [ATII] [Stem] `BS` NP [Ga]) (predSR 1 "ばばちい"),
  mylex ["ぼろっち","ぼろち"] "(196)" (defS [ATII] [Stem] `BS` NP [Ga]) (predSR 1 "ぼろちい"),
  mylex ["みみっち","みみち"] "(196)" (defS [ATII] [Stem] `BS` NP [Ga]) (predSR 1 "みみっちい"),
  -- ベシ形活用形容詞
  mylex ["如","ごと"] "(199)" ((defS [ABES] [Stem] `BS` NP [Ga]) `BS` NP [Ga,No]) (verbSR 2 "如し"),
  mylex ["如","ごと"] "(199)" (defS [ABES] [Stem] `BS` defS anyPos [Attr]) (verbSR 2 "如し"),
  mylex ["べ"] "(200)" (defS [ABES] [Stem] `BS` defS verb [Term]) (modal "べし"),
  -- 形容詞派生形
  mylex ["さ","み"] "new" (N `BS` (defS adjective [Stem] `BS` NP [Ga])) id,
  -- 状詞語幹
  conjSuffix "だ" "(218)" [Nda] [Term],
  conjSuffix "だっ" "(218)" [Nda] [EuphT],
  mylex ["です"] "(219)" (S [Nda] [Term] [M,P,M,M,M] `BS` S [Nda] [Stem] [M,M,M,M,M]) id,
  mylex ["でし"] "(219)" (S [Nda] [EuphT] [M,P,M,M,M] `BS` S [Nda] [Stem] [M,M,M,M,M]) id,
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
  mylex ["たり"] "(308)" (S anyPos [Cont] [P,(F 1 PM),(F 2 PM),M,M] `BS` S anyPos [EuphT] [M,(F 1 PM),(F 2 PM),M,M])     (eventModifier "タ[MCN]"),
  mylex ["た"] "(308)"  (S anyPos [Term,Attr] [P,(F 1 PM),(F 2 PM),M,M] `BS` S anyPos [EuphT] [M,(F 1 PM),(F 2 PM),M,M]) (eventModifier "タ[MCN]"),
  mylex ["たら"] "(308)" (S anyPos [Hyp] [P,(F 1 PM),(F 2 PM),M,M] `BS` S anyPos [EuphT] [M,(F 1 PM),(F 2 PM),M,M])      (eventModifier "タ[MCN]"),
  mylex ["たろ"] "(308)" (S anyPos [ModU] [P,(F 1 PM),(F 2 PM),M,M] `BS` S anyPos [EuphT] [M,(F 1 PM),(F 2 PM),M,M])     (eventModifier "タ[MCN]"),
  mylex ["だり"] "(309)" (S verb [Cont] [P,M,(F 1 PM),M,M] `BS` S verb [EuphD] [M,M,(F 1 PM),M,M])     (eventModifier "タ[MCN]"),
  mylex ["だ"] "(309)" (S verb [Term,Attr] [P,M,(F 1 PM),M,M] `BS` S verb [EuphD] [M,M,(F 1 PM),M,M])  (eventModifier "タ[MCN]"),
  mylex ["だら"] "(309)" (S verb [Hyp] [P,M,(F 1 PM),M,M] `BS` S verb [EuphD] [M,M,(F 1 PM),M,M])      (eventModifier "タ[MCN]"),
  mylex ["だろ"] "(309)" (S verb [ModU] [P,M,(F 1 PM),M,M] `BS` S verb [EuphD] [M,M,(F 1 PM),M,M])     (eventModifier "タ[MCN]"),
  -- 助動詞（丁寧）
  mylex ["ます"]   "(310)" (S verb [Term,Attr] [M,P,M,M,M] `BS` defS verb [Cont])   id,
  mylex ["ませ"]   "(310)" (S verb [Imper] [M,P,M,M,M] `BS` defS verb [Cont])       id,
  mylex ["まし"]   "(310)" (S verb [Imper,EuphT] [M,P,M,M,M] `BS` defS verb [Cont]) id,
  mylex ["ましょ"] "(310)" (S verb [ModU] [M,P,M,M,M] `BS` defS verb [Cont])        id,
  mylex ["ませ"]   "(311)" (S verb [Neg] [M,P,M,M,M] `BS` defS verb [Cont])        id,
  mylex ["まする"] "(311)" (S verb [Term,Attr] [M,P,M,M,M] `BS` defS verb [Cont])  id,
  mylex ["ますれ"] "(311)" (S verb [Hyp] [M,P,M,M,M] `BS` defS verb [Cont])        id,
  mylex ["ませい"] "(311)" (S verb [Imper] [M,P,M,M,M] `BS` defS verb [Cont])      id,
  mylex ["です"]   "(318)" (S adjective [Term] [(F 1 PM),P,M,M,M] `BS` S adjective [Term] [(F 1 PM),M,M,M,M])   id,
  -- ません＋です
  mylex ["ませんで"]   "(321)" (S verb [TeForm] [M,P,P,M,M] `BS` defS verb [Cont])     negOperator,
  mylex ["ませんです"] "(321)" (S verb [Term,Attr] [M,P,P,M,M] `BS` defS verb [Cont]) negOperator,
  mylex ["ませんでし"] "(321)" (S verb [EuphT] [M,P,P,M,M] `BS` defS verb [Cont])     negOperator,
 -- 助動詞（否定）
  mylex ["ぬ","ん"] "(325)" (S anyPos [Term,Attr] [M,(F 1 PM),P,M,M] `BS` S anyPos [NegL] [M,(F 1 PM),PM,M,M]) negOperator,
  mylex ["ね"]      "(325)" (S anyPos [Hyp] [M,M,P,M,M] `BS` defS anyPos [NegL])       negOperator,
  mylex ["ざら"]     "(330)" (S anyPos [NegL] [M,M,P,M,M] `BS` defS anyPos [NegL])     negOperator,
  mylex ["ずに"]     "(330)" (S anyPos [NiForm] [M,M,P,M,M] `BS` defS anyPos [NegL])   negOperator,
  mylex ["ず"]      "(330)" (S anyPos [Term,NiForm] [M,M,P,M,M] `BS` defS anyPos [NegL])     negOperator, -- NiFormを加えた
  mylex ["ざる"]    "(330)" (S anyPos [Attr] [M,M,P,M,M] `BS` defS anyPos [NegL])      negOperator,
  mylex ["ざれ"]    "(330)" (S anyPos [Hyp,Imper] [M,M,P,M,M] `BS` defS anyPos [NegL])                  negOperator,
  mylex ["ないで"]  "(335)" (S verb [TeForm] [M,M,P,M,M] `BS` S verb [Neg] [M,M,PM,PM,M])               negOperator,
  mylex ["んで"]    "(336)" (S verb [TeForm] [M,(F 1 PM),P,M,M] `BS` S verb [NegL] [M,(F 1 PM),PM,M,M]) negOperator,
  mylex ["な"]     "(343)" (S verb [Imper] [M,F 1 PM,P,M,M] `BS` S verb [Term] [M,F 1 PM,M,PM,M])       negOperator,
  mylex ["な"]     "(345)" (defS verb [Imper] `BS` S verb [Cont] [M,M,M,PM,M])                          id,
  mylex ["なかれ"] "(346)" (S verb [Imper] [M,M,P,M,M] `BS` defS verb [Term])                 negOperator,
   -- 助動詞（推量）
  mylex ["う"] "(349)" (S anyPos [Pre] [(F 1 PM),(F 2 PM),(F 3 PM),M,M] `BS` S anyPos [ModU] [(F 1 PM),(F 2 PM),(F 3 PM),M,M])
                       (modal "ウ[MCN]"),
  mylex ["ん"] "(353)" (T False 1 (defS anyPos [Pre]) `BS` T False 1 (defS anyPos [NegL]))
                       (modal "ン[MCN]"),
  mylex ["だろう","であろう","だろ"] "(357)" (S (verb++adjective) [Pre] [(F 1 PM),M,M,M,M] `BS` S (verb++adjective) [Term] [(F 1 PM),M,PM,M,M])
                       (modal "ダロウ[MCN]"), -- ここから作業再開 (3/8)
  mylex ["だろう","であろう","だろ"] "(357)" (S [Nda] [Pre] [(F 1 PM),M,M,M,M] `BS` S [Nda] [Stem] [(F 1 PM),M,M,M,M])
                       (modal "ダロウ[MCN]"),
  mylex ["でしょう","でしょ"] "(357)" (S (verb++adjective) [Pre] [(F 1 PM),P,M,M,M] `BS` S (verb++adjective) [Term] [(F 1 PM),PM,PM,M,M])
                       (modal "ダロウ[MCN]"),
  mylex ["でしょう","でしょ"] "(357)" (S [Nda] [Pre] [(F 1 PM),P,M,M,M] `BS` S [Nda] [Stem] [(F 1 PM),PM,PM,M,M])
                       (modal "ダロウ[MCN]"),
  mylex ["まい"] "(359a)" (S verb [Pre] [M,(F 1 PM),P,M,M] `BS` S verb [Term] [M,(F 1 PM),M,M,M])
                         (Lam (Lam (App (Con "ダロウ[MCN]") (Not (App (Var 1) (Var 0)))))),
  mylex ["まい"] "(359b)" (S verb [Pre] [M,(F 1 PM),P,M,M] `BS` S [V1,VK,VS,VSN,VZ] [Neg] [M,(F 1 PM),M,M,M])
                         (Lam (Lam (App (Con "ダロウ[MCN]") (Not (App (Var 1) (Var 0)))))),
  mylex ["まい"] "(359c)" (S verb [Pre] [M,(F 1 PM),P,M,M] `BS` S [VS,VSN,VZ] [Term] [M,(F 1 PM),M,M,M])
                         (Lam (Lam (App (Con "ダロウ[MCN]") (Not (App (Var 1) (Var 0)))))),
  -- 動詞テ形
  mylex ["て"] "(369)" (S verb [TeForm] [M,(F 1 PM),M,M,M] `BS` S verb [EuphT] [M,(F 1 PM),M,M,M]) id,
  mylex ["で"] "(370)" (defS verb [TeForm] `BS` defS verb [EuphD]) id,
  mylex ["たって"] "(369)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` S verb [EuphT] [M,PM,M,M,M]) id, --- CCG本から訂正 -- 意味表示要訂正
  mylex ["だって"] "(370)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` defS verb [EuphD] ) id,          --- CCG本から訂正 -- 意味表示要訂正
  -- 形容詞テ形
  mylex ["て","って"] "(371)" (defS adjective [TeForm] `BS` defS adjective [Cont]) id,
  mylex ["たって"] "(371)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` defS adjective [Cont]) id,       --- CCG本から訂正 -- 意味表示要訂正
  -- 状詞テ形
  mylex ["で"] "(372)" (defS [Nda] [TeForm] `BS` defS [Nda] [Stem]) id,
  mylex ["だって"] "(372)" (S [Nda] [TeForm] [P,M,M,M,M] `BS` defS [Nda] [Stem]) id,
  -- 動詞ニ形
  mylex ["に"] "(376)" (defS verb [NiForm] `BS` defS verb [Cont]) id,
  -- 状詞ニ形
  mylex ["に"] "(377)" (defS [Nda] [NiForm] `BS` defS [Nda] [Stem]) id,
  -- 動詞性接尾語
  -- 6.1.1 形式述語 
  mylex ["居","い"] "(381)" (defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` defS verb [TeForm,NiForm]) (eventModifier "テイル[ASP]"),
  mylex ["有","あ"] "(381)" (defS [V5ARU] [Stem] `BS` defS adjective [Cont]) (eventModifier "テアル[ASP]"),
  mylex ["有","あ"] "(381)" (defS [V5ARU] [Stem] `BS` defS [Nda] [TeForm]) id,
  mylex ["な"] "(383)" ((defS [ANAS] [Stem] `BS` NP [Ga]) `BS` (defS adjective [Cont]) `BS` NP [Ga]) negOperator,
  mylex ["な"] "(383)" ((defS [ANAS] [Stem] `BS` NP [Ga]) `BS` (defS [Nda] [TeForm] `BS` NP [Ga])) negOperator,
  mylex ["な"] "(384)" ((defS [V5r] [Stem] `BS` NP [Ga]) `BS` (defS adjective [Cont] `BS` NP [Ga])) (intensionalEvent 1 "成る"),
  mylex ["な"] "(384)" ((defS [V5r] [Stem] `BS` NP [Ga]) `BS` (defS [Nda] [NiForm] `BS` NP [Ga])) (intensionalEvent 1 "成る"),
  -- い省略
  mylex ["て"] "(403)" (defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` defS verb [EuphT]) (eventModifier "テイル[ASP]"),
  -- 取り立て（副助詞）
  --mylex ["は"] "(385)" (S anyPos nonStem [M,M,M,M,P] `BS` defS anyPos nonStem) (Lam (Lam 
  mylex ["は"] "(550)" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [Ga,O]))) `BS` (NP [Nc])) argumentCM,
  mylex ["には"] "(550)" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [Ni]))) `BS` (NP [Nc])) argumentCM,
  mylex ["とは"] "new" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [To]))) `BS` (NP [Nc])) argumentCM,
  --
  mylex ["も"] "(385)" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [Ga,O]))) `BS` (NP [Nc])) argumentCM,
  mylex ["にも"] "(385)" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [Ni]))) `BS` (NP [Nc])) argumentCM,
  mylex ["とも"] "(385)" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [To]))) `BS` (NP [Nc])) argumentCM,
  --
  mylex ["こそ"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ga,O]))) `BS` (NP [Nc])) argumentCM,
  mylex ["にこそ"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ni]))) `BS` (NP [Nc])) argumentCM,
  --
  mylex ["さえ"] "(387)" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ga,O]))) `BS` (NP [Nc])) argumentCM,
  mylex ["にさえ","さえに"] "(387)" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ni]))) `BS` (NP [Nc])) argumentCM,
  mylex ["とさえ","さえと"] "(387)" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [To]))) `BS` (NP [Nc])) argumentCM,
  --
  mylex ["だけ"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ga,O]))) `BS` (NP [Nc])) argumentCM,
  mylex ["にだけ","だけに"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ni]))) `BS` (NP [Nc])) argumentCM,
  mylex ["とだけ","だけと"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [To]))) `BS` (NP [Nc])) argumentCM,
  --
  mylex ["ばかり","ばっかり","ばっか"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ga,O]))) `BS` (NP [Nc])) argumentCM,
  mylex ["にばかり","にばっかり","にばっか"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ni]))) `BS` (NP [Nc])) argumentCM,
  mylex ["ばかりに","ばっかりに","ばっかに"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ni]))) `BS` (NP [Nc])) argumentCM,
  mylex ["とばかり","とばっかり","とばっか"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [To]))) `BS` (NP [Nc])) argumentCM,
  mylex ["ばかりと","ばっかりと","ばっかと"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [To]))) `BS` (NP [Nc])) argumentCM,
  --
  mylex ["のみ"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ga,O]))) `BS` (NP [Nc])) argumentCM,
  mylex ["にのみ","のみに"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [Ni]))) `BS` (NP [Nc])) argumentCM,
  --
  --mylex ["しか"]
  --mylex ["にしか"]
  --
  --mylex ["こそ"]
  --mylex ["こそが"]
  --mylex ["こそを"]
  --mylex ["にこそ"]
  --mylex ["なら"]
  mylex ["って"] "new" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [Ga]))) `BS` (NP [Nc])) argumentCM,
  --mylex ["ったら"]
  --mylex ["では","じゃ","じゃあ"]
  --mylex ["でも"]
  --mylex ["でさえ"]
  --mylex ["すら"]
  --mylex ["まで"] -- 〜してまで
  --mylex ["など"] -- 太郎をなど、太郎になど
  --mylex ["なんか"] -- 太郎になんか
  --mylex ["なんて"]
  --mylex ["なんぞ"]
  --mylex ["くらい"]
  --mylex ["どころ"]
  --mylex ["とて"]
  -- 6.1.3 補助動詞（連用形接続）
  mylex ["始め","はじめ"] "(412)" (defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` defS verb [Cont]) (eventModifier "始める"),
  mylex ["込","こ"] "(412)" (defS [V5m] [Stem] `BS` defS verb [Cont])     (eventModifier "込む"),
  mylex ["出","だ"] "(412)" (defS [V5s] [Stem] `BS` defS verb [Cont])     (eventModifier "出す"),
  mylex ["合","あ"] "(412)" (defS [V5w] [Stem] `BS` defS verb [Cont])     (eventModifier "合う"),
  mylex ["続け","つづけ"] "(412)" (defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` defS verb [Cont]) (eventModifier "続ける"),
  mylex ["かけ"] "(412)" (defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` defS verb [Cont])         (eventModifier "かける"),
  mylex ["上げ","あげ"] "(412)" (defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` defS verb [Cont])   (eventModifier "上げる"),
  mylex ["切","き"] "(412)" (defS [V5r] [Stem] `BS` defS verb [Cont])     (eventModifier "切る"),
  mylex ["付け","つけ"] "(412)" (defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` defS verb [Cont])   (eventModifier "付ける"),
  mylex ["過ぎ","すぎ"] "(412)" (defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` defS verb [Cont])   (eventModifier "過ぎる"),
  mylex ["あぐ"] "(412)" (defS [V5m] [Stem] `BS` defS verb [Cont])        (eventModifier "あぐむ"),
  mylex ["かね"] "(412)" (defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` defS verb [Cont])         (eventModifier "かねる"),
  mylex ["やが"] "(412)" (defS [V5r] [Stem] `BS` defS verb [Cont])        (eventModifier "やがる"),
  -- 6.1.4 補助動詞（テ形接続）
  mylex ["お"] "(416)" (defS [V5k] [Stem] `BS` defS verb [TeForm])        (eventModifier "テオク[ASP]"),
  mylex ["仕舞","しま"] "(416)" (defS [V5w] [Stem] `BS` defS verb [TeForm]) (eventModifier "テシマウ[ASP]"),
  mylex ["行","い"] "(416)" (defS [V5k] [Stem] `BS` defS verb [TeForm])    (eventModifier "テイク[ASP]"),
  mylex ["見","み"] "(416)" (defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` defS verb [TeForm])     (eventModifier "テミル[ASP]"),
  mylex ["見せ","みせ"] "(416)" (defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` defS verb [TeForm]) (eventModifier "テミセル[ASP]"),
  mylex ["ちゃ","ちま"] "(426)" (defS [V5w] [Stem] `BS` defS verb [EuphT]) (eventModifier "テシマウ[ASP]"),
  mylex ["じゃ","じま"] "(427)" (defS [V5w] [Stem] `BS` defS verb [EuphD]) (eventModifier "テシマウ[ASP]"),
  --mylex ["困","こま"] "(434a)" ((defS [V5r] [Stem] `BS` NP [Ga]) `BS` S anyPos [TeForm]) (Lam p (Lam x (Lam c (Pi (App p (Lam Top)) (App (Con "困る") ()))))) -- ここまで
  --mylex ["拙","まず"] "(434b)" ((defS [Aauo] [Stem] `BS` NP [Ga]) `BS` S anyPos [TeForm]) (Lam (Lam (Lam (Pi ))))
  -- 6.1.5 授受表現
  mylex ["上げ","あげ"] "(436)" ((defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` NP [Ga]) `BS` (defS verb [TeForm] `BS` NP [Ga]))
        (Lam (Lam (Lam (App (Con "アゲル") (Pair (Var 1) (App (App (Var 2) (Var 1)) (Var 0))))))),
  mylex ["貰","もら"] "(436)" (((defS [V5w] [Stem] `BS` NP [Ga]) `BS` NP [Ni]) `BS` (defS verb [TeForm] `BS` NP [Ga]))
        (Lam (Lam (Lam (Lam (App (Con "モラウ") (Pair (Var 1) (Pair (Var 2) (App (App (Var 3) (Var 2)) (Var 0))))))))),
  -- 6.1.6 −がる
  mylex ["が"] "(443)" ((defS [V5r] [Stem] `BS` NP [Ga]) `BS` (defS [Aauo,Ai,ANAS] [Stem] `BS` NP [Ga])) (intensionalEvent 1 "ガル"),
  -- 6.1.7 −めく
  mylex ["め"] "(453)" ((defS [V5k] [Stem] `BS` NP [Ga]) `BS` N) (Lam (Lam (Lam (App (Con "メク") (Pair (Var 0) (App (App (Var 2) (Var 1)) (Var 0))))))), -- to be revised
  -- 形容詞性接尾語
  -- 6.2.1 ない
  mylex ["な"] "(455)" (S [ANAS] [Stem] [M,M,P,M,M] `BS` S anyPos [Neg] [M,M,PM,PM,M]) negOperator,
  mylex ["無","な"] "(458)" (S [Aauo,ANAS] [Stem] [M,M,P,M,M] `BS` NP [Ga]) (predSR 1 "無い"),
  mylex ["ねえ","ねぇ","ねー"] "(455)" (S [ANAS] [Term,Attr] [M,M,P,M,M] `BS` defS anyPos [Neg]) negOperator,
  mylex ["ねえ","ねぇ","ねー"] "(458)" (S [Aauo,ANAS] [Term,Attr] [M,M,P,M,M] `BS` NP [Ga]) (predSR 1 "無い"),
  mylex ["無し","なし","ナシ"] "(467)" (S [Nda,Nno,Nni] [Stem] [M,M,P,M,M] `BS` NP [Ga])    (predSR 1 "無い"),
  -- 6.2.2 たい
  mylex ["た"] "(474a)" ((defS [Aauo] [Stem] `BS` NP [Ga]) `BS` (defS verb [Cont] `BS` NP [Ga])) (intensionalState 1 "タイ"),
  mylex ["た"] "(474b)" (((defS [Aauo] [Stem] `BS` NP [Ni,Ga]) `BS` NP [Ga]) `BS` ((defS verb [Cont] `BS` NP [Ga])) `BS` NP [O]) (intensionalState 2 "たい"),
  mylex ["難","にく"] "(475a)" ((defS [Aauo] [Stem] `BS` NP [Ga]) `BS` (defS verb [Cont] `BS` NP [Ga])) (intensionalState 1 "ニクイ"),
  mylex ["難","にく"] "(475b)" (((defS [Aauo] [Stem] `BS` NP [Ni,Ga]) `BS` NP [Ga]) `BS` ((defS verb [Cont] `BS` NP [Ga])) `BS` NP [O]) (intensionalState 2 "難"),
  mylex ["易","やす"] "(476a)" ((defS [Aauo] [Stem] `BS` NP [Ga]) `BS` (defS verb [Cont] `BS` NP [Ga])) (intensionalState 1 "ヤスイ"),
  mylex ["易","やす"] "(476b)" (((defS [Aauo] [Stem] `BS` NP [Ni,Ga]) `BS` NP [Ga]) `BS` ((defS verb [Cont] `BS` NP [Ga])) `BS` NP [O]) (intensionalState 2 "易"),
  -- 6.2.3-6.2.6
  mylex ["らし"] "(478)" (defS [Ai] [Stem] `BS` defS verb [Term])      (modal "ラシイ[MCN]"),
  mylex ["らし"] "(478)" (defS [Ai] [Stem] `BS` defS adjective [Term]) (modal "ラシイ[MCN]"),
  mylex ["らし"] "(478)" (defS [Ai] [Stem] `BS` defS [Nda] [Stem])     (modal "ラシイ[MCN]"),
  mylex ["っぽ"] "(480)" (defS [Aauo] [Stem] `BS` defS [Aauo,Nda] [Stem]) (modal "ポイ[MCN]"),
  mylex ["くさ"] "(478)" (defS [Ai] [Stem] `BS` defS verb [Term])         (modal "クサイ[MCN]"),
  mylex ["くさ"] "(478)" (defS [Ai] [Stem] `BS` defS adjective [Term])    (modal "クサイ[MCN]"),
  mylex ["くさ"] "(478)" (defS [Ai] [Stem] `BS` defS [Nda] [Stem])       (modal "クサイ[MCN]"),
  mylex ["べき"] "(488)" (defS [Nda] [Attr] `BS` defS verb [Term])       (modal "ベキ[MCN]"),
  -- 状詞性接尾語
  mylex ["よう"] "(493)" (defS [Nda,Nna,Nni] [Stem] `BS` defS anyPos [Attr]) (modal "ヨウダ[MCN]"),
  mylex ["そう"] "(497)" (defS [Nda] [Stem] `BS` defS anyPos [Term])         (modal "ソウダ伝聞[MCN]"),
  mylex ["そう"] "(498)" (defS [Nda,Nna,Nni] [Stem] `BS` defS anyPos [ModS]) (modal "ソウダ推量[MCN]"),
  mylex ["がち"] "(506)" (defS [Nda,Nna,Nni] [Stem] `BS` defS anyPos [Cont]) (modal "ガチダ[MCN]"),
  mylex ["みたい"] "(507)" (defS [Nda,Nna,Nni] [Stem] `BS` defS verb [Term]) (modal "ミタイダ[MCN]"),
  mylex ["みたい"] "(507)" (defS [Nda,Nna,Nni] [Stem] `BS` defS adjective [Term]) (modal "ミタイダ[MCN]"),
  mylex ["みたい"] "(507)" (defS [Nda,Nna,Nni] [Stem] `BS` defS [Nda] [Stem]) (modal "ミタイダ[MCN]"),
  mylex ["的","てき"] "(508)" ((defS [Nda,Nna,Nni] [Stem] `BS` NP [Ga]) `BS` NP [Nc]) (Lam (Lam (Eq (Con "Entity") (Var 0) (Var 1)))),-- ??
  mylex ["的","てき"] "(508)" ((defS [Nda,Nna,Nni] [Stem] `BS` NP [Ga]) `BS` N) (Lam (Lam (App (Var 1) (Var 0)))),-- ??
  mylex ["気味","ぎみ"] "(509)" ((defS [Nda,Nna,Nno,Nni] [Stem] `BS` NP [Ga]) `BS` N) (Lam (Lam (App (Var 1) (Var 0)))),-- ??
  mylex ["なの"] "(510)" (defS [Nda] [Stem] `BS` defS [Nda] [Stem])          (modal "ナノダ[MCN]"),
  mylex ["の","ん"] "(511)" (defS [Nda] [Stem] `BS` S anyPos [Attr] [M,M,M,PM,M])           (modal "ノダ[MCN]"),
  mylex ["筈","はず","ハズ"] "(511)" (defS [Nda] [Stem] `BS` defS anyPos [Attr]) (modal "ハズダ[MCN]"),
  mylex ["訳","わけ","ワケ"] "(511)" (defS [Nda] [Stem] `BS` defS anyPos [Attr]) (modal "ワケダ[MCN]"),
  mylex ["つもり"] "new" (defS [Nda] [Stem] `BS` defS anyPos [Attr]) (modal "ツモリ[MCN]"),
  mylex ["もの","もん"] "(511)" (defS [Nda] [Stem] `BS` defS anyPos [Attr])            (modal "モノダ[MCN]"),
  -- べきだ
  -- ふうだ
  -- ことだ
  -- ばかりだ、ばっかりだ、ばっかだ、
  -- 照応
  mylex ["これ","それ","あれ"] "new" (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) 
        (Lam (App (Var 0) (Asp 1 (Proj Fst (Sigma (Con "entity") (Not (App (Con "有生") (Var 0)))))))),
  mylex ["ここ","そこ","あそこ"] "(586)" (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) 
        (Lam (App (Var 0) (Asp 1 (Proj Fst (Sigma (Con "entity") (App (Con "場所") (Var 0))))))),
  mylex ["この","その","あの"] "(589)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) `SL` N)
        (Lam (Lam (Lamvec (Appvec 0 (App (Var 1) (Proj Fst (Asp 1 (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top)))))))))),
  -- 代名詞
  mylex ["彼","かれ","カレ"] "new" (NP [Nc]) 
        (Proj Fst (Asp 1 (Sigma (Con "entity") (App (Con "男") (Var 0))))),
  mylex ["彼女","かのじょ","カノジョ"] "new" (NP [Nc]) 
        (Proj Fst (Asp 1 (Sigma (Con "entity") (App (Con "女") (Var 0))))),
  -- 連体詞
  mylex ["こう"] "new" (anySExStem `SL` anySExStem) (eventModifier "こう"),
  mylex ["そう"] "new" (anySExStem `SL` anySExStem) (eventModifier "そう"),
  mylex ["ああ"] "new" (anySExStem `SL` anySExStem) (eventModifier "ああ"),
  mylex ["どう"] "new" (anySExStem `SL` anySExStem) (eventModifier "どう"),
  mylex ["このよう"] "new" (defS [Nna,Nni] [Stem] `BS` NP [Ga]) id,
  mylex ["あのよう"] "new" (defS [Nna,Nni] [Stem] `BS` NP [Ga]) id,
  mylex ["そのよう"] "new" (defS [Nna,Nni] [Stem] `BS` NP [Ga]) id,
  mylex ["どのよう"] "new" (defS [Nna,Nni] [Stem] `BS` NP [Ga]) id,
  mylex ["こういう"] "new" (N `SL` N) id,
  mylex ["ああいう"] "new" (N `SL` N) id,
  mylex ["そういう"] "new" (N `SL` N) id,
  mylex ["どういう"] "new" (N `SL` N) id,
  -- カ節
  mylex ["か","かどうか"] "(603)" (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc]) `BS` defS (verb++adjective) [Term]) 
        (Lam (Lam (App (Var 0) (App (Con "ドウカ") (Var 1))))), 
  mylex ["か","かどうか"] "(603)" (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc]) `BS` defS [Nda] [Stem]) 
        (Lam (Lam (App (Var 0) (App (Con "ドウカ") (Var 1))))), 
  -- 態
  mylex ["れ"] "(607)" (((defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` NP [Ga]) `BS` NP [Ni]) `BS` (defS anyPos [VoR] `BS` NP [Ga])) 
        (Lam (Lam (Lam (Lam (App (App (Var 3) (Var 2)) (Lam (Sigma (Con "event") (Sigma (App (App (App (Con "迷惑") (Var 1)) (Var 3)) (Var 0)) (App (Var 3) (Var 1)))))))))),
  mylex ["れ"] "(608)" (((defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` NP [Ga]) `BS` NP [Ni,Niyotte]) `BS` ((defS anyPos [VoR] `BS` NP [Ga]) `BS` NP [Ni,O]))
        (Lam (Lam (Lam (Lam (App (App (App (Var 3) (Var 1)) (Var 2)) (Var 0)))))),
  mylex ["せ"] "(629)" (((defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` NP [Ga]) `BS` NP [Ni]) `BS` (defS anyPos [VoS] `BS` NP [Ga]))
        (Lam (Lam (Lam (Lam (App (App (Var 3) (Var 2)) (Lam (Sigma (Con "event") (Sigma (App (App (App (Con "使役") (Var 1)) (Var 3)) (Var 0)) (App (Var 3) (Var 1)))))))))),
  mylex ["せ"] "(635)" (((defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` NP [Ga]) `BS` NP [O]) `BS` (defS anyPos [VoS] `BS` NP [Ga]))
        (Lam (Lam (Lam (Lam (App (App (Var 3) (Var 2)) (Lam (Sigma (Con "event") (Sigma (App (App (App (Con "使役") (Var 1)) (Var 3)) (Var 0)) (App (Var 3) (Var 1)))))))))), 
  -- 以下、「可能」はstateを導入すべきか。
  mylex ["れ"] "(660a)" ((defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` NP [Ga]) `BS` (defS [V1,VK] [VoR] `BS` NP [Ga]))
        (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (Lam (App (App (Var 3) (Var 0)) (Var 1)))))))),
  mylex ["れ"] "(660b)" (((defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` NP [Ni,Ga]) `BS` NP [Ga]) `BS` ((defS [V1,VK] [VoR] `BS` NP [Ga]) `BS` NP [O]))
        (Lam (Lam (Lam (Lam (App (Con "可能") (Pair (Var 2) (Lam (App (App (App (Var 4) (Var 0)) (Var 2)) (Var 1))))))))),
  mylex ["得","え"] "(661a)" ((defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` NP [Ga]) `BS` (defS verb [Cont] `BS` NP [Ga]))
        (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (App (App (Var 2) (Var 1)) (Var 0))))))),
  mylex ["得","え"] "(661b)" (((defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` NP [Ga,Ni]) `BS` NP [Ga]) `BS` ((defS verb [Cont] `BS` NP [Ga]) `BS` NP [O]))
        (Lam (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (App (App (App (Var 3) (Var 2)) (Var 1)) (Var 0)))))))),
  mylex ["得る","うる"] "(662a)" ((defS [VURU] [Term,Attr] `BS` NP [Ga]) `BS` (defS verb [Cont] `BS` NP [Ga]))
        (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (App (App (Var 2) (Var 1)) (Var 0))))))),
  mylex ["得る","うる"] "(662b)" (((defS [VURU] [Term,Attr] `BS` NP [Ga,Ni]) `BS` NP [Ga]) `BS` ((defS verb [Cont] `BS` NP [Ga]) `BS` NP [Ni,O]))
        (Lam (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (App (App (App (Var 3) (Var 2)) (Var 1)) (Var 0)))))))),
  mylex ["れ"] "(666)" (defS [V1] [VoE] `BS` defS [V1] [Stem])
        id,
  mylex ["来れ","これ"] "(667)" (defS [VK] [VoE] `BS` defS [VK] [Stem]) id,
  mylex ["でき","出来"] "new" ((defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` NP [Ga,Ni]) `BS` (defS [VSN] [Stem] `BS` NP [Ga]))
        (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (Lam (App (App (Var 3) (Var 0)) (Var 1)))))))),
  mylex ["でき","出来"] "new" ((defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` NP [Ga,Ni]) `BS` NP [Ga])
        (Lam (Lam (Lam (App (App (Con "可能") (Var 2)) (Var 1))))),
  -- 接続詞
  mylex ["が"] "(711)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` S anyPos [Term] [PM,PM,PM,M,M]) 
        (Lam (Lam (Lam (Sigma (App (Var 2) (Lam Top)) (App (Var 2) (Var 1)))))),
  mylex ["し"] "(713)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` S anyPos [Term] [PM,PM,PM,M,M]) 
        (Lam (Lam (Lam (Sigma (App (Var 2) (Lam Top)) (App (Var 2) (Var 1)))))),
  -- そして
  -- それとも
  -- それに
  -- けど,けども,けれど,けれども
  -- から、ので
  -- まで
  -- より
  -- まま
  -- きり,っきり
  -- つつ
  -- ながら
  -- がてら
  -- やいなや
  -- ど,ども
  -- きや
  -- 状詞の副詞用法
  mylex ["に"] "(728)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` (defS [Nni] [Stem] `BS` NP [Ga])) 
        (Lam (Lam (Lam (App (Var 1) (Lam (Sigma (App (App (Var 3) (Var 0)) (Lam Top)) (App (Var 2) (Var 1)))))))),
  mylex ["と"] "(731)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` (defS [Nto] [Stem] `BS` NP [Ga])) 
        (Lam (Lam (Lam (App (Var 1) (Lam (Sigma (App (App (Var 3) (Var 0)) (Lam Top)) (App (Var 2) (Var 1)))))))),
  --mylex ["が"] "(711)" ((T True 1 anySExStem `SL` T True 1 anySExStem) `BS` defS anyPos [Term]) 
  --      (Lam (Lam (Lamvec (Lamvec (Sigma (Appvec 0 (Var 3)) (Appvec 1 (Appvec 2 (Var 3)))))))),
  -- Wh句
  mylex ["誰","だれ"] "new" (NP [Nc]) (Con "誰"),
  mylex ["何","なに"] "new" (NP [Nc]) (Con "何"),
  mylex ["何処","どこ"] "new" (NP [Nc]) (Con "どこ"),
  -- 従属節導入表現
  mylex ["と","とは","とも"] "new" (Sbar [ToCL] `BS` S anyPos [Term,Imper] [PM,PM,PM,M,M]) id,
  mylex ["ように","ようには","ようにも"] "new" (Sbar [YooniCL] `BS` S anyPos [Attr] [PM,PM,PM,M,M]) id,
  mylex ["という"] "new" ((N `SL` N) `BS` S anyPos [Term] [PM,PM,PM,M,M]) 
  (Lam (Lam (Lam (Lam (Sigma (App (Var 3) (Lam Top)) (App (App (Var 3) (Var 2)) (Lam (Sigma (App (App (Con "content") (Var 1)) (Var 3)) (App (Var 3) (Var 1)))))))))),
  --- とする
  mylex ["と"] "new" ((defS [VS] [Stem]) `BS` (S anyPos [Pre] [PM,PM,PM,M,M])) (modal "トスル[MCN]"),
  -- 連用節
  --mylex ["に"] "new" ((T True 1 anySExStem `SL` T True 1 anySExStem) `BS` (defS [Nni] [Stem])) (Lam (Lam (Sigma (Var 1) (Var 1)))),
  -- 条件節
  mylex ["ば"] "new" ((anySExStem `SL` anySExStem) `BS` (defS anyPos [Neg,Hyp])) (Lam (Lam (Pi (Var 1) (Var 1)))),
  -- 終助詞
  mylex ["か"] "new" (S anyPos [Term] [PM,PM,PM,M,M] `BS` S anyPos [Term] [PM,PM,PM,M,M]) id,
  mylex ["ね","ねえ","ねー","ネ"] "new" (defS anyPos [Term] `BS` defS anyPos [Term]) id,
  mylex ["よ","ヨ"] "new" (defS anyPos [Term] `BS` defS anyPos [Term]) id,
  mylex ["さ","さあ","さー","サ"] "new" (defS anyPos [Term] `BS` defS anyPos [Term]) id,
  -- かしら
  -- やろ
  -- ぞ
  -- ぜ
  -- で
  -- なあ
  -- わ
  -- ねん
  -- っけ
  -- や
  -- かい
  -- 句読点
  -- mylex "。"  "" (anyS `BS` anyS) id,
  -- mylex "、"  "" (anyS `BS` anyS) id,
  -- 括弧
  --mylex ["「","（","(","『","《","〈","【","［","[","−","-"] "new" LPAREN Unit,
  --mylex ["」","）",")","』","》","〉","】","］","]","−","-"] "new" RPAREN Unit,
  mylex ["(","[","−","-"] "new" LPAREN Unit,
  mylex [")","]","−","-"] "new" RPAREN Unit,
  -- 形式名詞
  mylex ["こと","事"] "new" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) `BS` (S anyPos [Attr] [PM,PM,PM,M,M])) (Lam (Lam (Lamvec (Sigma (Con "entity") (Sigma (Sigma (Con "state") (App (App (Con "こと") (Var 1 )) (Var 0))) (Sigma (App (App (Con "content") (App (Var 4) (Lam Top))) (Var 1)) (Appvec 3 (App (Var 4) (Var 2))))))))),
  mylex ["の"] "new" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) `BS` (S anyPos [Attr] [PM,PM,PM,M,M])) (Lam (Lam (Lamvec (Sigma (Con "entity") (Sigma (Sigma (Con "state") (App (App (Con "の") (Var 1 )) (Var 0))) (Sigma (App (App (Con "content") (App (Var 4) (Lam Top))) (Var 1)) (Appvec 3 (App (Var 4) (Var 2))))))))),
  -- 量化表現：Q-no N
  mylex ["すべての","あらゆる","一人一人の","各","各々の","それぞれの"] "(534)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) `SL` N) 
        (Lam (Lam (Lamvec (Pi (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),
  mylex ["一人の","或る","ある","何人かの","数人の"] "(534)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) `SL` N) 
        (Lam (Lam (Lamvec (Sigma (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),
  -- 量化表現：N-no Q
  mylex ["の一人一人","のそれぞれ","のすべて","の全員","の誰も"] "(535)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Nc])) `BS` N) 
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
  mylex ["い"] "new" (defS [V1] [Stem,Neg,Cont,NegL,EuphT] `BS` NP [Ga]) (Lam (Lam (Sigma (Con "entity") (Sigma (Eq (Con "entity") (Var 0) (Var 2)) (App (Var 2) (Var 0)))))),
  mylex ["あ"] "new" (defS [V5ARU] [Stem] `BS` NP [Ga]) (Lam (Lam (Sigma (Con "entity") (Sigma (Eq (Con "entity") (Var 0) (Var 2)) (App (Var 2) (Var 0)))))),
  -- 状詞
  mylex ["多分"] "(Adv)" (defS [Nna,Nni] [Stem] `BS` NP [Ga]) (predSR 1 "多分"),
  mylex ["多分"] "(Adv)" (defS [Nemp] [Stem] `BS` NP [Ga]) (modal "タブン"),
  -- JSeM語彙
  mylex ["世界最高"] "new" (defS [Nda,Nno,Nni,Ntar] [Stem] `BS` NP [Ga]) (predSR 1 "世界最高"),
  -- BCCWJ語彙
  mylex ["死","し"] "BCCWJ" ((defS [V5s] [Stem] `BS` NP [Ga]) `BS` NP [Ni]) (verbSR 2 "死す"),
  mylex ["ユーカㇻ"] "BCCWJ" (NP [Nc]) (Con "ユーカラ"),
  mylex ["則ち"] "BCCWJ" (defS [Nemp] [Stem] `BS` NP [Ga]) (predSR 1 "すなわち")
  ]
