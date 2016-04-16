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
module Parser.Japanese.MyLexicon (
  emptyCategories,
  myLexicon
  ) where

import Prelude hiding (id)
import qualified Data.Text.Lazy as T
import Data.Ratio
import Parser.CombinatoryCategorialGrammar
import Logic.DependentTypes
import Parser.Japanese.Templates

ec :: T.Text -> T.Text -> Integer -> Cat -> (Preterm, [Signature]) -> Node
ec word num r c (s,sg) = Node {rs=EC, pf=word, cat=c, sem=s, daughters=[], score=(r % 100), source=num, sig=sg}
--ec pf source score cat sem sig = Node EC pf cat sem [] (score % 100) source sig

-- | A list of empty categories (i.e. lexical items whose PF is empty).
emptyCategories :: [Node]
emptyCategories = [
  -- 一段動詞活用語尾
  --lexicalitem "\\emp" "(132)" 100
  --            ((defS [V1] [Neg,Cont,ModM,NegL,EuphT]) `BS` (defS [V1] [Stem]))
  --            id,
  -- カ行変格活用動詞語幹
  --ec "come" "(154)" 100
  --            ((defS [VK] [Stem] `BS` NP [F[Ga]]) `BS` NP [F[Ni]])
  --            (verbSR 2 "来る"),
  -- サ行変格活用動詞語幹 -- とりあえずガヲ、ガヲトのパターンのみ。
  ec "do" "(156)" 100
              ((defS [VS] [Stem] `BS` NP [F[Ga]]) `BS` NP [F[O]])
              (verbSR 2 "する"),
  --lexicalitem "\\emp" "(156)" (100%100)
  --            (((defS [VS] [Stem] `BS` NP [Ga]) `BS` NP [F[O]]) `BS` defS [To])
  --            (Lam (Lam (App (Con "する") (Pair (Var 0) (Var 1))))),
  -- イ形容詞終止形活用語彙
  --ec "\\emp" "(175)" 100
  --            (S [F[Ai], F[Term], F[M], F[M], SF 1 [P,M], F[M], F[M]] `BS` S [F[Ai], F[Stem], F[M], F[M], SF 1 [P,M], F[M], F[M]])
  --            (id,[]),
  -- 判定詞語幹
  ec "be-pred." "(235a)" 100
              ((defS [Nda,Nno,Ntar] [Stem] `BS` NP [F [Ga]]) `BS` N)
              (id,[]),
  ec "be-ident." "(235b)" 100
              ((defS [Nda] [Stem] `BS` NP [F[Ga]]) `BS` NP [F[Nc]])
              ((Lam (Lam (Lam (Sigma (Eq (Con "entity") (Var 1) (Var 2)) (App (Var 1) (Var 0)))))),[]),
  -- サ変語幹→状詞語幹
  --ec "\\emp" "(262)" 99
  --            (defS [Nda,Nno] [Stem] `BS` defS [VSN] [Stem])
  --            (id,[]),
  -- サ変語幹→名詞
  ec "\\emp" "ss" 99
              ((((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Nc]])) `BS` NP [F[No]]) `BS` NP [F[No]]) `BS` ((defS [VS] [Stem] `BS` NP [F [Ga]]) `BS` NP [F [O]]))
              ((Lam (Lam (Lam (Lam (Lamvec (App (App (App (Var 4) (Var 3)) (Var 2)) (Lam (Appvec 1 (App (Var 2) (Var 0)))))))))),[]),
  -- 形式述語スル
  ec "do" "(380a)" 100
              (defS [VS,VSN] [Stem] `BS` S [F verb,F[Cont],F[M],F[M],F[M],F[M],F[P]])
              (id,[]),
  ec "do" "(380b)" 100
              (defS [VS,VSN] [Stem] `BS` S [F verb,F[Cont],F[P],F[P,M],F[P,M],F[M],F[P,M]])
              (id,[]),
  -- 補助動詞「くる」
  --ec "come" "(416)" 100
  --            (defS [VK] [Stem] `BS` defS verb [TeForm])
  --            (eventModifier "来る"),
  -- 推量のウ
  --ec "\\emp" "(349)" 100
  --            (S [SF 1 anyPos, F[Pre], SF 2 [P,M], SF 3 [P,M],SF 4 [P,M],F[M],F[M]] `BS` S [SF 1 anyPos, F[ModU], SF 2 [P,M],SF 3 [P,M],SF 4 [P,M],F[M],F[M]])
  --            (modalSR "ウ[MCN]"),
   -- 可能態
  ec "\\emp" "(652a)" 99
              ((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) `BS` (defS anyPos [VoE] `BS` NP [F[Ga]]))
              ((Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (Lam (App (App (Var 3) (Var 0)) (Var 1)))))))),
               [("可能",Type)]),
  ec "\\emp" "(652b)" 99
              (((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ni,Ga]]) `BS` NP [F[Ga]]) `BS` ((defS anyPos [VoE] `BS` NP [F[Ga]]) `BS` NP [F[O]]))
              ((Lam (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (Lam (App (App (App (Var 4) (Var 3)) (Var 0)) (Var 1))))))))),
               [("可能",Type)]),
  -- 状詞の副詞用法: \p.\q.\v.\c.qv(\e.pe ∧ ce)
  ec "\\emp" "(730)" 99
              ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` (defS [Nemp] [Stem] `BS` NP [F[Ga]]))
              ((Lam (Lam (Lam (App (Var 1) (Lam (App (App (Var 3) (Var 0)) (Lam (App (Var 2) (Var 1))))))))),[]),
  -- 空冠詞（存在量化）
  ec "∃ " "(544)" 99
              ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Nc]])) `SL` N)
              ((Lam (Lam (Lamvec (Sigma (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),[]),
  -- 空助詞
  ec "cm" "(515)" 60
              (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]]))
              argumentCM,
  -- pro1
  ec "pro" "(597)" 99
              (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Ga,O,Ni,To,No,Niyotte]]))
              ((Lam (App (Var 0) (Asp 1 (Con "entity")))),[]),
  -- pro2
  ec "pro" "(597)" 99
              (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Ga,O,Ni,To,No,Niyotte]]))
              ((Lam (App (Var 0) (Asp 2 (Con "entity")))),[]),
  -- pro3
  ec "pro" "(597)" 96
              (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Ga,No]]))
              ((Lam (App (Var 0) (Asp 3 (Con "entity")))),[]),
  -- 関係節化演算子(relativizer)
  ec "rel" "(670)" 99
              ((N `SL` N) `BS` (S [F anyPos, F[Attr], F[P,M],F[P,M],F[P,M],F[M],F[M]] `BS` NP [F[Ga,O,Ni,To]]))
              ((Lam (Lam (Lam (Lam (Sigma (App (App (Var 3) (Var 1)) (Lam Top)) (App (App (Var 3) (Var 2)) (Var 1))))))),[]),
  ec "rel-ext" "(670)+" 96
              ((N `SL` N) `BS` (S [F anyPos, F[Attr], F[P,M],F[P,M],F[P,M],F[M],F[M]]))
              ((Lam (Lam (Lam (Lam (Sigma (App (App (Var 2) (Var 1)) (Var 0)) (Sigma (App (Var 4) (Lam Top)) (Sigma (Pi (Con "event") (Pi (Con "entity") Type)) (App (App (Var 0) (Var 1)) (Var 4))))))))),[]),
  -- 連用節、テ節
  ec "\\emp" "new" 99
              ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` (S [F ([V5k, V5s, V5t, V5n, V5m, V5r, V5w, V5g, V5z, V5b, V5IKU, V5YUK, V5ARU, V5NAS, V5TOW, V1, VS, VSN, VZ]++adjective++nomPred), F[Cont], F[M],F[P,M],F[P,M],F[M],F[M]]))
              ((Lam (Lam (Lam (Sigma (App (Var 2) (Lam Top)) (App (Var 2) (Var 1)))))),[]),
  ec "\\emp" "new" 99
              ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` (S [F anyPos, F[TeForm], F[M],F[P,M],F[P,M],F[M],F[M]]))
              ((Lam (Lam (Lam (Sigma (App (Var 2) (Lam Top)) (App (Var 2) (Var 1)))))),[])
  -- ダロウ接続形を派生する空範疇
  -- lexicalitem "\\emp" "(354)" (99%100)
  --             (defS (verb++adjective) [ModD] `BS` defS (verb++adjective) [Term])
  --             id,
  -- lexicalitem "\\emp" "(354)" (99%100)
  --             (defS [Nda] [ModD] `BS` defS [Nda] [Stem])
  --             id,
  ]

{- Some Macros for adding lexical items to lexicon -}

mylex :: [T.Text] -> T.Text -> Cat -> (Preterm, [Signature]) -> [Node]
mylex wds num cat' (sem',sig') = [(lexicalitem wd num 100 cat' (sem',sig')) | wd <- wds ]

conjSuffix :: T.Text -> T.Text -> [FeatureValue] -> [FeatureValue] -> [Node]
conjSuffix wd num catpos catconj = [lexicalitem wd num 100 ((S ([SF 1 catpos, F catconj]++m5)) `BS` (S ([SF 1 catpos, F[Stem]]++m5))) (id,[])]

teidaiS :: Cat
teidaiS = S [SF 2 anyPos, SF 3 nonStem, SF 4 [P,M], SF 5 [P,M], SF 6 [P,M], F[M], F[M]]

-- | A list of (mostly functional) lexical items extracted from Bekki (2010).
myLexicon :: [Node]
myLexicon = concat $ [
  -- 格助詞
  -- argument:
  --mylex ["が"] "(524)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [Ga])) `BS` NP [Nc]) argumentCM,
  mylex ["を"] "(524)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[O]])) `BS` NP [F[Nc]]) argumentCM,
  mylex ["に","へ"] "(524)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Ni]])) `BS` NP [F[Nc]]) argumentCM,
  mylex ["によって","によっては"] "(524)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Niyotte]])) `BS` NP [F[Nc]]) argumentCM,
  mylex ["が","の"] "(531)?" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Ga,No]])) `BS` NP [F[Nc]]) argumentCM,
  -- 格助詞（の）
  mylex ["の"] "(531)+" ((N `SL` N) `BS` (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Nc]]))) 
               ((Lam (Lam (Lam (Lam (App (Var 3) (Lam (Sigma (App (App (Var 3) (Var 2)) (Var 1)) (App (App (Con "の[MCN]") (Var 1)) (Var 3))))))))),[]),
  -- adjunct:
  mylex ["と","とは","とも","とさえ"] "(524)+" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [F[Nc]]) (adjunctCM "ト"),
  mylex ["へ","へは","へも","へさえ"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [F[Nc]]) (adjunctCM "終点"),
  mylex ["で","では","でも","でさえ"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [F[Nc]]) (adjunctCM "場所"),
  mylex ["から","からは","からも"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [F[Nc]]) (adjunctCM "始点"),
  mylex ["まで","までは","までも"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [F[Nc]]) (adjunctCM "終点"),
  mylex ["より","よりは","よりも"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [F[Nc]]) (adjunctCM "起点"),
  mylex ["にて"] "(516)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [F[Nc]]) (adjunctCM "場所"),
  --
  mylex ["について","については","についても"] "" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [F[Nc]]) (adjunctCM "ニツイテ"),
  mylex ["にとって","にとっては","にとっても"] "" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [F[Nc]]) (adjunctCM "ニトッテ"),
  mylex ["において","においては","においても"] "" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [F[Nc]]) (adjunctCM "ニオイテ"),
  mylex ["として","としては","としても"] "" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [F[Nc]]) (adjunctCM "トシテ"),
  mylex ["に基づいて"] "" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [F[Nc]]) (adjunctCM "ニモトヅイテ"),
  mylex ["にあわせて"] "" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` NP [F[Nc]]) (adjunctCM "ニアワセテ"),
  -- 等位接続（連言）
  mylex ["と","や","に"] "new" CONJ andSR,
  mylex ["そして","および"] "new" CONJ andSR,
  mylex ["なり","やら","だの"] "new" CONJ andSR,
  mylex ["にくわえて","に加えて","ならびに","並びに","をはじめ","をはじめとして","を始め"] "new" CONJ andSR,
  -- 等位接続（選言）
  mylex ["か"] "new" CONJ orSR,
  mylex ["または","又は"] "new" CONJ orSR,
  mylex ["もしくは"] "new" CONJ orSR,
  mylex ["あるいは"] "new" CONJ orSR,
  mylex ["ないしは","ないし"] "new" CONJ orSR,
  -- 動詞活用語尾
  conjSuffix "か" "(112)" [V5k,V5IKU,V5YUK] [Neg,VoR,VoS,NegL],
  conjSuffix "き" "(112)" [V5k,V5IKU,V5YUK] [Cont,ModM],
  conjSuffix "く" "(112)" [V5k,V5IKU,V5YUK] [Term,Attr],
  conjSuffix "け" "(112)" [V5k,V5IKU,V5YUK] [Hyp,Imper,VoE],
  conjSuffix "こ" "(112)" [V5k,V5IKU,V5YUK] [ModU],
  --
  conjSuffix "さ" "(77)" [V5s] [Neg,VoR,VoS,NegL],
  conjSuffix "し" "(77)" [V5s] [Cont,ModM,EuphT],
  conjSuffix "す" "(77)" [V5s] [Term,Attr],
  conjSuffix "せ" "(77)" [V5s] [Hyp,Imper,VoE],
  conjSuffix "そ" "(77)" [V5s] [ModU],
  --
  conjSuffix "た" "(78)" [V5t] [Neg,VoR,VoS,NegL],
  conjSuffix "ち" "(78)" [V5t] [Cont,ModM],
  conjSuffix "つ" "(78)" [V5t] [Term,Attr],
  conjSuffix "て" "(78)" [V5t] [Hyp,Imper,VoE],
  conjSuffix "と" "(78)" [V5t] [ModU],
  --
  conjSuffix "な" "(79)" [V5n] [Neg,VoR,VoS,NegL],
  conjSuffix "に" "(79)" [V5n] [Cont,ModM],
  conjSuffix "ぬ" "(79)" [V5n] [Term,Attr],
  conjSuffix "ね" "(79)" [V5n] [Hyp,Imper,VoE],
  conjSuffix "の" "(79)" [V5n] [ModU],
  --
  conjSuffix "ま" "(80)" [V5m] [Neg,VoR,VoS,NegL],
  conjSuffix "み" "(80)" [V5m] [Cont,ModM],
  conjSuffix "む" "(80)" [V5m] [Term,Attr],
  conjSuffix "め" "(80)" [V5m] [Hyp,Imper,VoE],
  conjSuffix "も" "(80)" [V5m] [ModU],
  --
  conjSuffix "ら" "(125)" [V5r,V5NAS] [Neg],
  conjSuffix "ら" "(125)" [V5r,V5ARU,V5NAS] [VoR,VoS,NegL],
  conjSuffix "り" "(125)" [V5r,V5ARU,V5NAS] [Cont,ModM],
  conjSuffix "る" "(125)" [V5r,V1,V5ARU,V5NAS] [Term,Attr],
  conjSuffix "れ" "(125)" [V5r,V5ARU,V5NAS] [Hyp,Imper,VoE],
  conjSuffix "ろ" "(125)" [V5r,V5ARU,V5NAS] [ModU],
  --mylex ["ん"] "(81)" [S (F [V5r],F [Neg,Term,Attr],F [M,M,M,P,M]] `BS` defS [V5r] [Stem]) (id,[]),
  --
  conjSuffix "わ" "(130)" [V5w,V5TOW] [Neg,VoR,VoS,NegL],
  conjSuffix "い" "(130)" [V5w,V5TOW] [Cont,ModM],
  conjSuffix "う" "(130)" [V5w,V5TOW] [Term,Attr],
  conjSuffix "え" "(130)" [V5w,V5TOW] [Hyp,Imper,VoE],
  conjSuffix "お" "(130)" [V5w,V5TOW] [ModU],
  --
  conjSuffix "が" "(83)" [V5g] [Neg,VoR,VoS,NegL],
  conjSuffix "ぎ" "(83)" [V5g] [Cont,ModM],
  conjSuffix "ぐ" "(83)" [V5g] [Term,Attr],
  conjSuffix "げ" "(83)" [V5g] [Hyp,Imper,VoE],
  conjSuffix "ご" "(83)" [V5g] [ModU],
  --
  conjSuffix "ば" "(84)" [V5b] [Neg,VoR,VoS,NegL],
  conjSuffix "び" "(84)" [V5b] [Cont,ModM],
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
  mylex ["ん"] "(132)" (S ([F[V1], F[Neg,Term,Attr]]++mmmpm) `BS` defS [V1] [Stem]) (id,[]),
  -- 音便形
  conjSuffix "い" "(76)-" [V5k,V5g] [EuphT],
  conjSuffix "っ" "(78)-" [V5t,V5r,V5w,V5IKU,V5ARU,V5NAS] [EuphT],
  conjSuffix "ん" "(79)-" [V5n,V5m,V5b] [EuphD],
  conjSuffix "う" "(128)" [V5TOW] [EuphT],
  conjSuffix "い" "(123)" [V5NAS] [ModM,Imper], -- Contはナシ
  -- ナサル型活用動詞
  mylex ["なさ"] "(122)" (defS [V5NAS] [Stem] `BS` defS verb [Cont]) (id,[]),
  mylex ["なさ"] "(122)" (defS [V5NAS] [Stem] `BS` defS [VSN] [Stem]) (id,[]),
  mylex ["いらっしゃ"] "(122)" (defS [V5NAS] [Stem] `BS` NP [F[Ga]]) (verbSR 1 "来る"),
  mylex ["いらっしゃ"] "(122)" (defS [V5NAS] [Stem] `BS` defS anyPos [TeForm]) (eventModifier "来る"),
  mylex ["仰","おっしゃ"] "(122)" ((defS [V5NAS] [Stem] `BS` NP [F[Ga]]) `BS` Sbar [F[ToCL]]) (verbSR 2 "言う"),
  mylex ["下さ","くださ"] "(122)" (((defS [V5NAS] [Stem] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) `BS` NP [F[O]]) (verbSR 3 "クレル"),
  mylex ["下さ","くださ"] "(122)" (defS [V5NAS] [Stem] `BS` defS anyPos [TeForm]) (eventModifier "クレル"),
  mylex ["ござ","御座"] "(122)" (defS [V5NAS] [Stem] `BS` defS anyPos [TeForm]) (id,[]),
  -- 例外的な命令形
  mylex ["くれ"] "(149)" (((defS [V1] [Imper] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) `BS` NP [F[O]]) (verbSR 3 "くれる"),
  mylex ["くれ"] "(150)" (defS [V1] [Imper] `BS` S [F verb, F[TeForm], F[M],F[M],F[P,M],F[M],F[M]]) (id,[]),
  mylex ["射れ","いれ"] "(151)" ((defS [V5r] [Imper] `BS` NP [F[Ga]]) `BS` NP [F[O]]) (verbSR 2 "射る"),
  mylex ["蹴ろ","けろ"] "(152)" ((defS [V1] [Imper] `BS` NP [F[Ga]]) `BS` NP [F[O]]) (verbSR 2 "蹴る"),
  mylex ["捻ろ","ひねろ"] "(153)" ((defS [V1] [Imper] `BS` NP [F[Ga]]) `BS` NP [F[O]]) (verbSR 2 "捻る"),
  --- カ変動詞
  mylex ["来"] "(155)" ((defS [VK] [Neg,Cont,ModM,EuphT,NegL] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) (verbSR 2 "来る/くる"),
  mylex ["こ"] "(155)" ((defS [VK] [Neg,NegL] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) (verbSR 2 "来る/くる"),
  mylex ["き"] "(155)" ((defS [VK] [Cont,ModM,EuphT] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) (verbSR 2 "来る/くる"),
  mylex ["来ら","こら"] "(155)" ((defS [VK] [VoR] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) (verbSR 2 "来る/くる"),
  mylex ["来さ","こさ"] "(155)" ((defS [VK] [VoS] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) (verbSR 2 "来る/くる"),
  mylex ["来る","くる"] "(155)" ((defS [VK] [Term,Attr] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) (verbSR 2 "来る/くる"),
  mylex ["来れ","くれ"] "(155)" ((defS [VK] [Hyp] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) (verbSR 2 "来る/くる"),
  mylex ["来い","こい"] "(155)" ((defS [VK] [Imper] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) (verbSR 2 "来る/くる"),
  mylex ["来よ","こよ"] "(155)" ((defS [VK] [ModU] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) (verbSR 2 "来る/くる"),
  mylex ["来ん","くん"] "(155)" (S ([F[VK], F[Term,Attr]]++mmmpm) `BS` defS [VK] [Stem]) (id,[]),
  -- 補助動詞「くる」
  mylex ["来"] "(416)" ((defS [VK] [Neg,Cont,ModM,EuphT,NegL]) `BS` defS verb [TeForm]) (eventModifier "クル[ASP]"),
  mylex ["こ"] "(416)" ((defS [VK] [Neg,NegL]) `BS` defS verb [TeForm]) (eventModifier "クル[ASP]"),
  mylex ["き"] "(416)" ((defS [VK] [Cont,ModM,EuphT]) `BS` defS verb [TeForm]) (eventModifier "クル[ASP]"),
  mylex ["来ら","こら"] "(416)" ((defS [VK] [VoR]) `BS` defS verb [TeForm]) (eventModifier "クル[ASP]"),
  mylex ["来さ","こさ"] "(416)" ((defS [VK] [VoS]) `BS` defS verb [TeForm]) (eventModifier "クル[ASP]"),
  mylex ["来る","くる"] "(416)" ((defS [VK] [Term,Attr]) `BS` defS verb [TeForm]) (eventModifier "クル[ASP]"),
  mylex ["来れ","くれ"] "(416)" ((defS [VK] [Hyp]) `BS` defS verb [TeForm]) (eventModifier "クル[ASP]"),
  mylex ["来い","こい"] "(416)" ((defS [VK] [Imper]) `BS` defS verb [TeForm]) (eventModifier "クル[ASP]"),
  mylex ["来よ","こよ"] "(416)" ((defS [VK] [ModU]) `BS` defS verb [TeForm]) (eventModifier "クル[ASP]"),
  --- サ変動詞
  conjSuffix "さ" "(157)" [VS,VSN] [VoR,VoS],
  conjSuffix "し" "(157)" [VS,VSN] [Neg,Cont,ModM,EuphT],
  conjSuffix "する" "(157)" [VS,VSN] [Term,Attr],
  conjSuffix "すれ" "(157)" [VS,VSN] [Hyp],
  conjSuffix "しろ" "(157)" [VS,VSN] [Imper],
  conjSuffix "しよ" "(157)" [VS,VSN] [ModU],
  conjSuffix "せ" "(157)" [VS,VSN] [NegL],
  conjSuffix "す" "(157)" [VS,VSN] [Term],
  conjSuffix "せよ" "(157)" [VS,VSN] [Imper],
  conjSuffix "せい" "(157)" [VS,VSN] [Imper],
  mylex ["すん"] "(157)" (S ([SF 1 [VS,VSN], F[Term,Attr]]++mmmpm) `BS` S ([SF 1 [VS,VSN], F[Stem]]++m5)) (id,[]),
  --- ザ変動詞
  conjSuffix "じ" "(164)" [VZ] [Neg,NegL,Cont,ModM,EuphT],
  conjSuffix "ずる" "(164)" [VZ] [Term,Attr],
  conjSuffix "ずれ" "(164)" [VZ] [Hyp],
  conjSuffix "じろ" "(164)" [VZ] [Imper],
  conjSuffix "じよ" "(164)" [VZ] [ModU],
  conjSuffix "ぜ" "(164)" [VZ] [NegL],
  conjSuffix "ぜら" "(164)" [VZ] [VoR],
  conjSuffix "ず" "(164)" [VZ] [Term],
  conjSuffix "ぜよ" "(164)" [VZ] [Imper],
  --- ウル型活用動詞
  mylex ["得る","うる"] "(169)" ((defS [VURU] [Term,Attr] `BS` NP [F[Ga]]) `BS` NP [F[O]]) (verbSR 2 "得る"),
  -- 形容詞活用語尾
  --adverb = [Aauo, Ai, ANAS, ABES]
  mylex ["く"]  "(174)" (S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Cont], F[M],F[M],SF 2 [P,M],F[M],F[M]] `BS` 
                         S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Stem], F[M],F[M],SF 2 [P,M],F[M],F[M]]) (id,[]),
  mylex ["い","し"]  "(174)" (S [SF 1 [Aauo,Ai,ANAS,ATII], F[Term,Attr], F[M],F[M],SF 2 [P,M],F[M],F[M]] `BS` 
                             S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Stem], F[M],F[M],SF 2 [P,M],F[M],F[M]]) (id,[]), --- 「し」
  mylex ["けれ"] "(174)" (S [SF 1 [Aauo,Ai,ANAS,ATII], F[Hyp], F[M],F[M],SF 2 [P,M],F[M],F[M]] `BS` 
                         S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Stem], F[M],F[M],SF 2 [P,M],F[M],F[M]]) (id,[]),
  mylex ["かろ"] "(174)" (S [SF 1 [Aauo,Ai,ANAS,ATII], F[ModU], F[M],F[M],SF 2 [P,M],F[M],F[M]] `BS` 
                         S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Stem], F[M],F[M],SF 2 [P,M],F[M],F[M]]) (id,[]),
  mylex ["かっ"] "(174)" (S [SF 1 [Aauo,Ai,ANAS,ATII], F[EuphT], F[M],F[M],SF 2 [P,M],F[M],F[M]] `BS` 
                         S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Stem], F[M],F[M],SF 2 [P,M],F[M],F[M]]) (id,[]),
  mylex ["から"] "(175)" (S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[NegL], F[M],F[M],SF 2 [P,M],F[M],F[M]] `BS` 
                         S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Stem], F[M],F[M],SF 2 [P,M],F[M],F[M]]) (id,[]),
  mylex ["う"]   "(174)" (S [SF 1 [Aauo,Ai,ANAS,ATII], F[Cont], F[M],F[M],SF 2 [P,M],F[M],F[M]] `BS` 
                         S [SF 1 [Aauo,Ai,ANAS,ATII], F[UStem], F[M],F[M],SF 2 [P,M],F[M],F[M]]) (id,[]),
  mylex ["し"]   "(175)" (S [SF 1 [Aauo,ANAS,ABES], F[Term], F[M],F[M],SF 2 [P,M],F[M],F[M]] `BS` 
                         S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Stem], F[M],F[M],SF 2 [P,M],F[M],F[M]]) (id,[]),
  mylex ["き"]   "(175)" (S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Attr], F[M],F[M],SF 2 [P,M],F[M],F[M]] `BS` 
                          S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Stem], F[M],F[M],SF 2 [P,M],F[M],F[M]]) (id,[]),
  mylex ["かれ"] "(175)" (S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Imper], F[M],F[M],SF 2 [P,M],F[M],F[M]] `BS` 
                         S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Stem], F[M],F[M],SF 2 [P,M],F[M],F[M]]) (id,[]),
  -- ナシ型活用形容詞
  mylex ["良","よ"] "(173)" (defS [ANAS] [Stem,UStem] `BS` NP [F[Ga]]) (predSR 1 "良い"),
  mylex ["無","な"] "(173)" (S ([F[ANAS], F[Stem]]++mmpmm) `BS` NP [F[Ga]]) (predSR 1 "無い"), --  +nとした
  mylex ["無","な"] "(173)+" ((S ([F[ANAS], F[Stem]]++mmpmm) `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) (predSR 2 "無い"),
  mylex ["無","の"] "(173)" (S ([F[ANAS], F[UStem]]++mmpmm) `BS` NP [F[Ga]]) (predSR 1 "無い"), -- +nとした
  -- チイ形活用形容詞
  mylex ["弱っち","よわっち"] "(196)" (defS [ATII] [Stem] `BS` NP [F[Ga]]) (predSR 1 "弱っちい"),
  mylex ["ちゃち"]           "(196)" (defS [ATII] [Stem] `BS` NP [F[Ga]]) (predSR 1 "ちゃちい"),
  mylex ["ばばっち","ばばち"] "(196)" (defS [ATII] [Stem] `BS` NP [F[Ga]]) (predSR 1 "ばばちい"),
  mylex ["ぼろっち","ぼろち"] "(196)" (defS [ATII] [Stem] `BS` NP [F[Ga]]) (predSR 1 "ぼろちい"),
  mylex ["みみっち","みみち"] "(196)" (defS [ATII] [Stem] `BS` NP [F[Ga]]) (predSR 1 "みみっちい"),
  -- ベシ形活用形容詞
  mylex ["如","ごと"] "(199)" ((defS [ABES] [Stem] `BS` NP [F[Ga]]) `BS` NP [F[Ga,No]]) (verbSR 2 "如し"),
  mylex ["如","ごと"] "(199)" (defS [ABES] [Stem] `BS` defS anyPos [Attr]) (verbSR 2 "如し"),
  mylex ["べ"] "(200)" (defS [ABES] [Stem] `BS` defS verb [Term]) (modalSR "べし"),
  -- 形容詞派生形
  mylex ["さ","み"] "new" (N `BS` (defS adjective [Stem] `BS` NP [F[Ga]])) (id,[]),
  -- 状詞語幹
  conjSuffix "だ" "(218)" [Nda] [Term],
  conjSuffix "だっ" "(218)" [Nda] [EuphT],
  mylex ["です"] "(219)" (S ([F[Nda], F[Term]]++mpmmm) `BS` defS [Nda] [Stem]) (id,[]),
  mylex ["でし"] "(219)" (S ([F[Nda], F[EuphT]]++mpmmm) `BS` defS [Nda] [Stem]) (id,[]),
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
  mylex ["たり"] "(308)" (S [SF 1 anyPos, F[Cont] ,F[P], SF 2 [P,M], SF 3 [P,M],F[M],F[M]] `BS` S [SF 1 anyPos, F[EuphT], F[M],SF 2 [P,M],SF 3 [P,M],F[M],F[M]]) (eventModifier "タ[MCN]"),
  mylex ["た"] "(308)"  (S [SF 1 anyPos, F[Term,Attr], F[P],SF 2 [P,M],SF 3 [P,M],F[M],F[M]] `BS` S [SF 1 anyPos, F[EuphT], F[M],SF 2 [P,M],SF 3 [P,M],F[M],F[M]]) (eventModifier "タ[MCN]"),
  mylex ["たら"] "(308)" (S [SF 1 anyPos, F[Hyp], F[P],SF 2 [P,M],SF 3 [P,M],F[M],F[M]] `BS` S [SF 1 anyPos, F[EuphT], F[M],SF 2 [P,M],SF 3 [P,M],F[M],F[M]])      (eventModifier "タ[MCN]"),
  mylex ["たろ"] "(308)" (S [SF 1 anyPos, F[ModU], F[P],SF 2 [P,M],SF 3 [P,M],F[M],F[M]] `BS` S [SF 1 anyPos, F[EuphT], F[M],SF 2 [P,M],SF 3 [P,M],F[M],F[M]])     (eventModifier "タ[MCN]"),
  mylex ["だり"] "(309)" (S [SF 1 verb, F[Cont], F[P],F[M],SF 2 [P,M],F[M],F[M]] `BS` S [SF 1 verb, F[EuphD], F[M],F[M],SF 2 [P,M],F[M],F[M]])     (eventModifier "タ[MCN]"),
  mylex ["だ"] "(309)"   (S [SF 1 verb, F[Term,Attr], F[P],F[M],SF 2 [P,M],F[M],F[M]] `BS` S [SF 1 verb, F[EuphD], F[M],F[M],SF 2 [P,M],F[M],F[M]])  (eventModifier "タ[MCN]"),
  mylex ["だら"] "(309)" (S [SF 1 verb, F[Hyp], F[P],F[M],SF 2 [P,M],F[M],F[M]] `BS` S [SF 1 verb, F[EuphD], F[M],F[M],SF 2 [P,M],F[M],F[M]])      (eventModifier "タ[MCN]"),
  mylex ["だろ"] "(309)" (S [SF 1 verb, F[ModU], F[P],F[M],SF 2 [P,M],F[M],F[M]] `BS` S [SF 1 verb, F[EuphD], F[M],F[M],SF 2 [P,M],F[M],F[M]])     (eventModifier "タ[MCN]"),
  -- 助動詞（丁寧）
  mylex ["ます"]   "(310)" (S ([SF 1 verb, F[Term,Attr]]++mpmmm) `BS` S ([SF 1 verb, F[ModM]]++m5))   (id,[]),
  mylex ["ませ"]   "(310)" (S ([SF 1 verb, F[Imper]]++mpmmm) `BS` S ([SF 1 verb, F[ModM]]++m5))       (id,[]),
  mylex ["まし"]   "(310)" (S ([SF 1 verb, F[Imper,EuphT]]++mpmmm) `BS` S ([SF 1 verb, F[ModM]]++m5)) (id,[]),
  mylex ["ましょ"] "(310)" (S ([SF 1 verb, F[ModU]]++mpmmm) `BS` S ([SF 1 verb, F[ModM]]++m5))        (id,[]),
  mylex ["ませ"]   "(311)" (S ([SF 1 verb, F[NegL]]++mpmmm) `BS` S ([SF 1 verb, F[ModM]]++m5))        (id,[]),
  mylex ["まする"] "(311)" (S ([SF 1 verb, F[Term,Attr]]++mpmmm) `BS` S ([SF 1 verb, F[ModM]]++m5))  (id,[]),
  mylex ["ますれ"] "(311)" (S ([SF 1 verb, F[Hyp]]++mpmmm) `BS` S ([SF 1 verb, F[ModM]]++m5))        (id,[]),
  mylex ["ませい"] "(311)" (S ([SF 1 verb, F[Imper]]++mpmmm) `BS` S ([SF 1 verb, F[ModM]]++m5))      (id,[]),
  mylex ["です"]   "(318)" (S [SF 1 adjective, F[Term], SF 2 [P,M],F[P],F[M],F[M],F[M]] `BS` S [SF 1 adjective, F[Term],SF 2 [P,M],F[M],F[M],F[M],F[M]])   (id,[]),
  -- ません＋です
  mylex ["ませんで"]   "(321)" (S ([SF 1 verb, F[TeForm]]++mppmm) `BS` S ([SF 1 verb, F[ModM]]++m5))     negOperator,
  mylex ["ませんです"] "(321)" (S ([SF 1 verb, F[Term,Attr]]++mppmm) `BS` S ([SF 1 verb, F[ModM]]++m5)) negOperator,
  mylex ["ませんでし"] "(321)" (S ([SF 1 verb, F[EuphT]]++mppmm) `BS` S ([SF 1 verb, F[ModM]]++m5))     negOperator,
 -- 助動詞（否定）
  mylex ["ぬ","ん"] "(325)" (S [SF 1 anyPos, F[Term,Attr], F[M],SF 2 [P,M],F[P],F[M],F[M]] `BS` S [SF 1 anyPos, F[NegL], F[M],SF 2 [P,M],F[P,M],F[M],F[M]]) negOperator,
  mylex ["ね"]      "(325)" (S ([SF 1 anyPos, F[Hyp]]++mmpmm) `BS` S ([SF 1 anyPos, F[NegL]]++m5))          negOperator,
  mylex ["ざら"]     "(330)" (S ([SF 1 anyPos, F[NegL]]++mmpmm) `BS` S ([SF 1 anyPos, F[NegL]]++m5))        negOperator,
  mylex ["ずに"]     "(330)" (S ([SF 1 anyPos, F[NiForm]]++mmpmm) `BS` S ([SF 1 anyPos, F[NegL]]++m5))      negOperator,
  mylex ["ず"]      "(330)" (S ([SF 1 anyPos, F[Term,NiForm]]++mmpmm) `BS` S ([SF 1 anyPos, F[NegL]]++m5))  negOperator, -- NiFormを加えた
  mylex ["ざる"]    "(330)" (S ([SF 1 anyPos, F[Attr]]++mmpmm) `BS` S ([SF 1 anyPos, F[NegL]]++m5))         negOperator,
  mylex ["ざれ"]    "(330)" (S ([SF 1 anyPos, F[Hyp,Imper]]++mmpmm) `BS` S ([SF 1 anyPos, F[NegL]]++m5))                   negOperator,
  mylex ["ないで"]  "(335)" (S ([SF 1 verb, F[TeForm]]++mmpmm) `BS` S [SF 1 verb, F[Neg], F[M],F[M],F[P,M],F[P,M],F[M]])       negOperator,
  mylex ["んで"]    "(336)" (S [SF 1 verb, F[TeForm], F[M],SF 2 [P,M],F[P],F[M],F[M]] `BS` S [SF 1 verb, F[NegL], F[M],SF 2 [P,M],F[P,M],F[M],F[M]]) negOperator,
  mylex ["な"]     "(343)" (S [SF 1 verb, F[Imper], F[M],SF 2 [P,M],F[P],F[M],F[M]] `BS` S [SF 1 verb, F[Term], F[M],SF 2 [P,M],F[M],F[P,M],F[M]])       negOperator,
  mylex ["な"]     "(345)" (S ([SF 1 verb, F[Imper]]++m5) `BS` S [SF 1 verb, F[Cont], F[M],F[M],F[M],F[P,M],F[M]])                          (id,[]),
  mylex ["なかれ"] "(346)" (S ([SF 1 verb, F[Imper]]++mmpmm) `BS` S ([SF 1 verb, F[Term]]++m5))             negOperator,
   -- 助動詞（推量）
  mylex ["う"] "(349)" (S [SF 1 anyPos, F[Pre], SF 2 [P,M],SF 3 [P,M],SF 4 [P,M],F[M],F[M]] `BS` S [SF 1 anyPos, F[ModU], SF 2 [P,M],SF 3 [P,M],SF 4 [P,M],F[M],F[M]]) (modalSR "ウ[MCN]"),
  mylex ["ん"] "(353)" (T False 1 (S ([SF 1 anyPos, F[Pre]]++m5)) `BS` T False 1 (S ([SF 1 anyPos, F[NegL]]++m5))) (modalSR "ン[MCN]"),
  mylex ["だろう","であろう","だろ"] "(357)" (S [SF 1 (verb++adjective), F[Pre], SF 2 [P,M],F[M],F[M],F[M],F[M]] `BS` S [SF 1 (verb++adjective), F[Term], SF 2 [P,M],F[M],F[P,M],F[M],F[M]]) (modalSR "ダロウ[MCN]"), -- ここから作業再開 (3/8)
  mylex ["だろう","であろう","だろ"] "(357)" (S [F[Nda], F[Pre], SF 1 [P,M],F[M],F[M],F[M],F[M]] `BS` S [F[Nda], F[Stem], SF 1 [P,M],F[M],F[M],F[M],F[M]]) (modalSR "ダロウ[MCN]"),
  mylex ["でしょう","でしょ"] "(357)" (S [SF 1 (verb++adjective), F[Pre], SF 2 [P,M],F[P],F[M],F[M],F[M]] `BS` S [SF 1 (verb++adjective), F[Term], SF 2 [P,M],F[P,M],F[P,M],F[M],F[M]]) (modalSR "ダロウ[MCN]"),
  mylex ["でしょう","でしょ"] "(357)" (S [F[Nda], F[Pre], F[M],F[P],F[M],F[M],F[M]] `BS` S [F[Nda], F[Stem], F[M],F[P,M],F[P,M],F[M],F[M]]) (modalSR "ダロウ[MCN]"),
  mylex ["でしょう","でしょ"] "(357)+" (S [F[Nda], F[Pre], F[P],F[P],F[M],F[M],F[M]] `BS` S [F[Nda], F[Term], F[P],F[P,M],F[P,M],F[M],F[M]]) (modalSR "ダロウ[MCN]"),
  mylex ["まい"] "(359a)" (S [SF 1 verb, F[Pre], F[M],SF 2 [P,M],F[P],F[M],F[M]] `BS` S [SF 1 verb, F[Term], F[M],SF 2 [P,M],F[M],F[M],F[M]])
                         ((Lam (Lam (App (Con "ダロウ[MCN]") (Not (App (Var 1) (Var 0)))))),[]),
  mylex ["まい"] "(359b)" (S [SF 1 [V1,VK,VS,VSN,VZ], F[Pre], F[M],SF 2 [P,M],F[P],F[M],F[M]] `BS` S [SF 1 [V1,VK,VS,VSN,VZ], F[Neg], F[M],SF 2 [P,M],F[M],F[M],F[M]])
                         ((Lam (Lam (App (Con "ダロウ[MCN]") (Not (App (Var 1) (Var 0)))))),[]),
  mylex ["まい"] "(359c)" (S [SF 1 [VS,VSN,VZ], F[Pre], F[M],SF 2 [P,M],F[P],F[M],F[M]] `BS` S [SF 1 [VS,VSN,VZ], F[Term], F[M],SF 2 [P,M],F[M],F[M],F[M]])
                         ((Lam (Lam (App (Con "ダロウ[MCN]") (Not (App (Var 1) (Var 0)))))),[]),
  -- 動詞テ形
  mylex ["て"] "(369)" (S [SF 1 verb, F[TeForm], F[M],SF 2 [P,M],F[M],F[M],F[M]] `BS` S [SF 1 verb, F[EuphT], F[M],SF 2 [P,M],F[M],F[M],F[M]]) (id,[]),
  mylex ["で"] "(370)" (S ([SF 1 verb, F[TeForm]]++m5) `BS` S ([SF 1 verb, F[EuphD]]++m5)) (id,[]),
  mylex ["たって"] "(369)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` S [F verb, F[EuphT], F[M],F[P,M],F[M],F[M],F[M]]) (id,[]), --- CCG本から訂正 -- 意味表示要訂正
  mylex ["だって"] "(370)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` S [F verb, F[EuphD], F[M],F[M],F[M],F[M],F[M]]) (id,[]),          --- CCG本から訂正 -- 意味表示要訂正
  -- 形容詞テ形
  mylex ["て","って"] "(371)" (S ([SF 1 adjective, F[TeForm]]++m5) `BS` S ([SF 1 adjective, F[Cont]]++m5)) (id,[]),
  mylex ["たって"] "(371)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` defS adjective [Cont]) (id,[]),       --- CCG本から訂正 -- 意味表示要訂正
  -- 状詞テ形
  mylex ["で"] "(372)" (defS [Nda] [TeForm] `BS` defS [Nda] [Stem]) (id,[]),
  mylex ["だって"] "(372)" (S [F[Nda],F[TeForm],F[P],F[M],F[M],F[M],F[M]] `BS` defS [Nda] [Stem]) (id,[]),
  -- 動詞ニ形
  mylex ["に"] "(376)" (S ([SF 1 verb, F[NiForm]]++m5) `BS` S ([SF 1 verb, F[Cont]]++m5)) (id,[]),
  -- 状詞ニ形
  mylex ["に"] "(377)" (defS [Nda] [NiForm] `BS` defS [Nda] [Stem]) (id,[]),
  -- 動詞性接尾語
  -- 6.1.1 形式述語 
  mylex ["居","い"] "(381)" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` defS verb [TeForm,NiForm]) (eventModifier "テイル[ASP]"),
  mylex ["あ"] "(381)" (defS [V5ARU] [Stem] `BS` defS verb [TeForm]) (eventModifier "テアル[ASP]"),
  mylex ["あ"] "(381)" (defS [V5ARU] [Stem] `BS` defS adjective [Cont]) (eventModifier "テアル[ASP]"),
  mylex ["あ"] "(381)" (defS [V5ARU] [Stem] `BS` defS [Nda] [TeForm]) (id,[]),
  mylex ["な"] "(383)" ((defS [ANAS] [Stem] `BS` NP [F[Ga]]) `BS` (defS adjective [Cont]) `BS` NP [F[Ga]]) negOperator,
  mylex ["な"] "(383)" ((defS [ANAS] [Stem] `BS` NP [F[Ga]]) `BS` (defS [Nda] [TeForm] `BS` NP [F[Ga]])) negOperator,
  mylex ["な"] "(384)" ((defS [V5r] [Stem] `BS` NP [F[Ga]]) `BS` (defS adjective [Cont] `BS` NP [F[Ga]])) (intensionalEvent 1 "成る"),
  mylex ["な"] "(384)" ((defS [V5r] [Stem] `BS` NP [F[Ga]]) `BS` (defS [Nda] [NiForm] `BS` NP [F[Ga]])) (intensionalEvent 1 "成る"),
  -- い省略
  mylex ["て"] "(403)" (defS [V1] [Stem,Neg,ModM,NegL,EuphT] `BS` defS verb [EuphT]) (eventModifier "テイル[ASP]"), -- Contはナシ
  -- 取り立て（副助詞）
  --mylex ["は"] "(385)" (S anyPos nonStem [M,M,M,M,P] `BS` defS anyPos nonStem) (Lam (Lam 
  mylex ["は"] "(550)" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["には"] "(550)" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  --mylex ["とは"] "new" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [F[To]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["も"] "(385)" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["にも"] "(385)" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  --mylex ["とも"] "(385)" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [F[To]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["こそ"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["にこそ"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["さえ"] "(387)" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["にさえ","さえに"] "(387)" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["とさえ","さえと"] "(387)" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [F[To]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["だけ"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["にだけ","だけに"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["とだけ","だけと"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [F[To]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["ばかり","ばっかり","ばっか"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["にばかり","にばっかり","にばっか"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["ばかりに","ばっかりに","ばっかに"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["とばかり","とばっかり","とばっか"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [F[To]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["ばかりと","ばっかりと","ばっかと"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [F[To]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["のみ"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["にのみ","のみに"] "new" (((T True 1 anySExStem) `SL` ((T True 1 anySExStem) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  --mylex ["しか"]
  --mylex ["にしか"]
  --
  --mylex ["こそ"]
  --mylex ["こそが"]
  --mylex ["こそを"]
  --mylex ["にこそ"]
  --mylex ["なら"]
  mylex ["って"] "new" (((T True 1 teidaiS) `SL` ((T True 1 teidaiS) `BS` (NP [F[Ga]]))) `BS` (NP [F[Nc]])) argumentCM,
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
  mylex ["始め","はじめ"] "(412)" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` defS verb [Cont]) (eventModifier "始める"),
  mylex ["込","こ"] "(412)" (defS [V5m] [Stem] `BS` defS verb [Cont])                         (eventModifier "込む"),
  mylex ["出","だ"] "(412)" (defS [V5s] [Stem] `BS` defS verb [Cont])                         (eventModifier "出す"),
  mylex ["合","あ"] "(412)" (defS [V5w] [Stem] `BS` defS verb [Cont])                         (eventModifier "合う"),
  mylex ["続け","つづけ"] "(412)" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` defS verb [Cont]) (eventModifier "続ける"),
  mylex ["かけ"] "(412)" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` defS verb [Cont])         (eventModifier "かける"),
  mylex ["上げ","あげ"] "(412)" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` defS verb [Cont])   (eventModifier "上げる"),
  mylex ["切","き"] "(412)" (defS [V5r] [Stem] `BS` defS verb [Cont])                         (eventModifier "切る"),
  mylex ["付け","つけ"] "(412)" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` defS verb [Cont])   (eventModifier "付ける"),
  mylex ["過ぎ","すぎ"] "(412)" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` defS verb [Cont])   (eventModifier "過ぎる"),
  mylex ["あぐ"] "(412)" (defS [V5m] [Stem] `BS` defS verb [Cont])                            (eventModifier "あぐむ"),
  mylex ["かね"] "(412)" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` defS verb [Cont])         (eventModifier "かねる"),
  mylex ["やが"] "(412)" (defS [V5r] [Stem] `BS` defS verb [Cont])                            (eventModifier "やがる"),
  -- 6.1.4 補助動詞（テ形接続）
  mylex ["お"] "(416)" (defS [V5k] [Stem] `BS` defS verb [TeForm])        (eventModifier "テオク[ASP]"),
  mylex ["仕舞","しま"] "(416)" (defS [V5w] [Stem] `BS` defS verb [TeForm]) (eventModifier "テシマウ[ASP]"),
  mylex ["行","い"] "(416)" (defS [V5k] [Stem] `BS` defS verb [TeForm])    (eventModifier "テイク[ASP]"),
  mylex ["見","み"] "(416)" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` defS verb [TeForm])     (eventModifier "テミル[ASP]"),
  mylex ["見せ","みせ"] "(416)" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` defS verb [TeForm]) (eventModifier "テミセル[ASP]"),
  mylex ["ちゃ","ちま"] "(426)" (defS [V5w] [Stem] `BS` defS verb [EuphT]) (eventModifier "テシマウ[ASP]"),
  mylex ["じゃ","じま"] "(427)" (defS [V5w] [Stem] `BS` defS verb [EuphD]) (eventModifier "テシマウ[ASP]"),
  --mylex ["困","こま"] "(434a)" ((defS [V5r] [Stem] `BS` NP [F[Ga]]) `BS` S anyPos [TeForm]) (Lam p (Lam x (Lam c (Pi (App p (Lam Top)) (App (Con "困る") ()))))) -- ここまで
  --mylex ["拙","まず"] "(434b)" ((defS [Aauo] [Stem] `BS` NP [F[Ga]]) `BS` S anyPos [TeForm]) (Lam (Lam (Lam (Pi ))))
  -- 6.1.5 授受表現
  mylex ["上げ","あげ"] "(436)" ((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) `BS` (defS verb [TeForm] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (App (Con "アゲル") (Pair (Var 1) (App (App (Var 2) (Var 1)) (Var 0))))))), [("アゲル",Type)]),  -- Signature直す
  mylex ["貰","もら"] "(436)" (((defS [V5w] [Stem] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) `BS` (defS verb [TeForm] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (Lam (App (Con "モラウ") (Pair (Var 1) (Pair (Var 2) (App (App (Var 3) (Var 2)) (Var 0))))))))), [("モラウ",Type)]),
  -- 6.1.6 −がる
  mylex ["が"] "(443)" ((defS [V5r] [Stem] `BS` NP [F[Ga]]) `BS` (defS [Aauo,Ai,ANAS] [Stem] `BS` NP [F[Ga]])) (intensionalEvent 1 "ガル"),
  -- 6.1.7 −めく
  mylex ["め"] "(453)" ((defS [V5k] [Stem] `BS` NP [F[Ga]]) `BS` N) ((Lam (Lam (Lam (App (Con "メク") (Pair (Var 0) (App (App (Var 2) (Var 1)) (Var 0))))))), [("メク",Type)]), -- to be revised
  -- 形容詞性接尾語
  -- 6.2.1 ない
  mylex ["な"] "(455)" (S ([F[ANAS], F[Stem]]++mmpmm) `BS` S [F anyPos, F[Neg], F[M],F[M],F[P,M],F[P,M],F[M]]) negOperator,
  mylex ["無","な"] "(458)" ((S ([F[Aauo,ANAS], F[Stem]]++mmpmm) `BS` NP [F[Ni,Ga]]) `BS` NP [F[Ga]]) (predSR 1 "無い"),
  mylex ["ねえ","ねぇ","ねー"] "(455)" (S ([F[ANAS], F[Term,Attr]]++mmpmm) `BS` defS anyPos [Neg]) negOperator,
  mylex ["ねえ","ねぇ","ねー"] "(458)" (S ([F[Aauo,ANAS], F[Term,Attr]]++mmpmm) `BS` NP [F[Ga]]) (predSR 1 "無い"),
  mylex ["無し","なし","ナシ"] "(467)" (S ([F[Nda,Nno,Nni], F[Stem]]++mmpmm) `BS` NP [F[Ga]])    (predSR 1 "無い"),
  -- 6.2.2 たい
  mylex ["た"] "(474a)" ((defS [Aauo] [Stem] `BS` NP [F[Ga]]) `BS` (defS verb [Cont] `BS` NP [F[Ga]])) (intensionalState 1 "タイ"),
  mylex ["た"] "(474b)" (((defS [Aauo] [Stem] `BS` NP [F[Ni,Ga]]) `BS` NP [F[Ga]]) `BS` ((defS verb [Cont] `BS` NP [F[Ga]])) `BS` NP [F[O]]) (intensionalState 2 "たい"),
  mylex ["難","にく"] "(475a)" ((defS [Aauo] [Stem] `BS` NP [F[Ga]]) `BS` (defS verb [Cont] `BS` NP [F[Ga]])) (intensionalState 1 "ニクイ"),
  mylex ["難","にく"] "(475b)" (((defS [Aauo] [Stem] `BS` NP [F[Ni,Ga]]) `BS` NP [F[Ga]]) `BS` ((defS verb [Cont] `BS` NP [F[Ga]])) `BS` NP [F[O]]) (intensionalState 2 "難"),
  mylex ["易","やす"] "(476a)" ((defS [Aauo] [Stem] `BS` NP [F[Ga]]) `BS` (defS verb [Cont] `BS` NP [F[Ga]])) (intensionalState 1 "ヤスイ"),
  mylex ["易","やす"] "(476b)" (((defS [Aauo] [Stem] `BS` NP [F[Ni,Ga]]) `BS` NP [F[Ga]]) `BS` ((defS verb [Cont] `BS` NP [F[Ga]])) `BS` NP [F[O]]) (intensionalState 2 "易"),
  -- 6.2.3-6.2.6
  mylex ["らし"] "(478)" (defS [Ai] [Stem] `BS` defS verb [Term])      (modalSR "ラシイ[MCN]"),
  mylex ["らし"] "(478)" (defS [Ai] [Stem] `BS` defS adjective [Term]) (modalSR "ラシイ[MCN]"),
  mylex ["らし"] "(478)" (defS [Ai] [Stem] `BS` defS [Nda] [Stem])     (modalSR "ラシイ[MCN]"),
  mylex ["っぽ"] "(480)" (defS [Aauo] [Stem] `BS` defS [Aauo,Nda] [Stem]) (modalSR "ポイ[MCN]"),
  mylex ["くさ"] "(478)" (defS [Ai] [Stem] `BS` defS verb [Term])         (modalSR "クサイ[MCN]"),
  mylex ["くさ"] "(478)" (defS [Ai] [Stem] `BS` defS adjective [Term])    (modalSR "クサイ[MCN]"),
  mylex ["くさ"] "(478)" (defS [Ai] [Stem] `BS` defS [Nda] [Stem])       (modalSR "クサイ[MCN]"),
  mylex ["べき"] "(488)" (defS [Nda] [Attr] `BS` defS verb [Term])       (modalSR "ベキ[MCN]"),
  -- 状詞性接尾語
  mylex ["よう"] "(493)" (defS [Nda,Nna,Nni] [Stem] `BS` defS anyPos [Attr]) (modalSR "ヨウダ[MCN]"),
  mylex ["そう"] "(497)" (defS [Nda] [Stem] `BS` defS anyPos [Term])         (modalSR "ソウダ伝聞[MCN]"),
  mylex ["そう"] "(498)" (defS [Nda,Nna,Nni] [Stem] `BS` defS anyPos [ModS]) (modalSR "ソウダ推量[MCN]"),
  mylex ["がち"] "(506)" (defS [Nda,Nna,Nni] [Stem] `BS` defS anyPos [Cont]) (modalSR "ガチダ[MCN]"),
  mylex ["みたい"] "(507)" (defS [Nda,Nna,Nni] [Stem] `BS` defS verb [Term]) (modalSR "ミタイダ[MCN]"),
  mylex ["みたい"] "(507)" (defS [Nda,Nna,Nni] [Stem] `BS` defS adjective [Term]) (modalSR "ミタイダ[MCN]"),
  mylex ["みたい"] "(507)" (defS [Nda,Nna,Nni] [Stem] `BS` defS [Nda] [Stem]) (modalSR "ミタイダ[MCN]"),
  mylex ["的","てき"] "(508)" ((defS [Nda,Nna,Nni] [Stem] `BS` NP [F[Ga]]) `BS` NP [F[Nc]]) ((Lam (Lam (Eq (Con "Entity") (Var 0) (Var 1)))),[]),-- ??
  mylex ["的","てき"] "(508)" ((defS [Nda,Nna,Nni] [Stem] `BS` NP [F[Ga]]) `BS` N) ((Lam (Lam (App (Var 1) (Var 0)))), []),-- ??
  mylex ["気味","ぎみ"] "(509)" ((defS [Nda,Nna,Nno,Nni] [Stem] `BS` NP [F[Ga]]) `BS` N) ((Lam (Lam (App (Var 1) (Var 0)))),[]),-- ??
  mylex ["なの"] "(510)" (defS [Nda] [Stem] `BS` defS [Nda] [Stem])          (modalSR "ナノダ[MCN]"),
  mylex ["の","ん"] "(511)" (S [F[Nda],F[Stem],SF 1 [P,M],SF 2 [P,M],SF 3 [P,M],F[M],F[M]] `BS` S [F anyPos,F[Attr],SF 1 [P,M],SF 2 [P,M],SF 3 [P,M],F[P,M],F[M]])           (modalSR "ノダ[MCN]"),
  mylex ["筈","はず","ハズ"] "(511)" (defS [Nda] [Stem] `BS` defS anyPos [Attr]) (modalSR "ハズダ[MCN]"),
  mylex ["訳","わけ","ワケ"] "(511)" (defS [Nda] [Stem] `BS` defS anyPos [Attr]) (modalSR "ワケダ[MCN]"),
  mylex ["つもり"] "new" (defS [Nda] [Stem] `BS` defS anyPos [Attr])            (modalSR "ツモリ[MCN]"),
  mylex ["もの","もん"] "(511)" (defS [Nda] [Stem] `BS` defS anyPos [Attr])      (modalSR "モノダ[MCN]"),
  -- べきだ
  -- ふうだ
  -- ことだ
  -- ばかりだ、ばっかりだ、ばっかだ、
  -- 照応
  mylex ["これ","それ","あれ"] "new" (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Nc]])) 
        ((Lam (App (Var 0) (Asp 1 (Proj Fst (Sigma (Con "entity") (Not (App (Con "有生") (Var 0)))))))), [("有生",Pi (Con "entity") Type)]),
  mylex ["ここ","そこ","あそこ"] "(586)" (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Nc]])) 
        ((Lam (App (Var 0) (Asp 1 (Proj Fst (Sigma (Con "entity") (App (Con "場所") (Var 0))))))), [("場所",Pi (Con "entity") Type)]),
  mylex ["この","その","あの"] "(589)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Nc]])) `SL` N)
        ((Lam (Lam (Lamvec (Appvec 0 (App (Var 1) (Proj Fst (Asp 1 (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top)))))))))), []),
  -- 代名詞
  mylex ["彼","かれ","カレ"] "new" (NP [F[Nc]]) 
        ((Proj Fst (Asp 1 (Sigma (Con "entity") (App (Con "男") (Var 0))))), [("男",Pi (Con "entity") Type)]),
  mylex ["彼女","かのじょ","カノジョ"] "new" (NP [F[Nc]]) 
        ((Proj Fst (Asp 1 (Sigma (Con "entity") (App (Con "女") (Var 0))))), [("女",Pi (Con "entity") Type)]),
  -- 連体詞
  mylex ["こう"] "new" (anySExStem `SL` anySExStem) (eventModifier "こう"),
  mylex ["そう"] "new" (anySExStem `SL` anySExStem) (eventModifier "そう"),
  mylex ["ああ"] "new" (anySExStem `SL` anySExStem) (eventModifier "ああ"),
  mylex ["どう"] "new" (anySExStem `SL` anySExStem) (eventModifier "どう"),
  mylex ["このよう"] "new" (defS [Nna,Nni] [Stem] `BS` NP [F[Ga]]) (id,[]),
  mylex ["あのよう"] "new" (defS [Nna,Nni] [Stem] `BS` NP [F[Ga]]) (id,[]),
  mylex ["そのよう"] "new" (defS [Nna,Nni] [Stem] `BS` NP [F[Ga]]) (id,[]),
  mylex ["どのよう"] "new" (defS [Nna,Nni] [Stem] `BS` NP [F[Ga]]) (id,[]),
  mylex ["こういう"] "new" (N `SL` N) (id,[]),
  mylex ["ああいう"] "new" (N `SL` N) (id,[]),
  mylex ["そういう"] "new" (N `SL` N) (id,[]),
  mylex ["どういう"] "new" (N `SL` N) (id,[]),
  -- カ節
  mylex ["か","かどうか"] "(603)" (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Nc]]) `BS` defS (verb++adjective) [Term]) 
        ((Lam (Lam (App (Var 0) (App (Con "ドウカ") (Var 1))))),[]), 
  mylex ["か","かどうか"] "(603)" (T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Nc]]) `BS` defS [Nda] [Stem]) 
        ((Lam (Lam (App (Var 0) (App (Con "ドウカ") (Var 1))))),[]), 
  -- 態
  mylex ["れ"] "(607)" (((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) `BS` (defS anyPos [VoR] `BS` NP [F[Ga]])) 
        ((Lam (Lam (Lam (Lam (App (App (Var 3) (Var 2)) (Lam (Sigma (Con "event") (Sigma (App (App (App (Con "迷惑") (Var 1)) (Var 3)) (Var 0)) (App (Var 3) (Var 1)))))))))),[]),
  mylex ["れ"] "(608)" (((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) `BS` NP [F[Ni,Niyotte]]) `BS` ((defS anyPos [VoR] `BS` NP [F[Ga]]) `BS` NP [F[Ni,O]])) 
        ((Lam (Lam (Lam (Lam (App (App (App (Var 3) (Var 1)) (Var 2)) (Var 0)))))),[]),
  mylex ["せ"] "(629)" (((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) `BS` (defS anyPos [VoS] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (Lam (App (App (Var 3) (Var 2)) (Lam (Sigma (Con "event") (Sigma (App (App (App (Con "使役") (Var 1)) (Var 3)) (Var 0)) (App (Var 3) (Var 1)))))))))),[]),
  mylex ["せ"] "(635)" (((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) `BS` NP [F[O]]) `BS` (defS anyPos [VoS] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (Lam (App (App (Var 3) (Var 2)) (Lam (Sigma (Con "event") (Sigma (App (App (App (Con "使役") (Var 1)) (Var 3)) (Var 0)) (App (Var 3) (Var 1)))))))))),[]), 
  -- 以下、「可能」はstateを導入すべきか。
  mylex ["れ"] "(660a)" ((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) `BS` (defS [V1,VK] [VoR] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (Lam (App (App (Var 3) (Var 0)) (Var 1)))))))),[]),
  mylex ["れ"] "(660b)" (((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ni,Ga]]) `BS` NP [F[Ga]]) `BS` ((defS [V1,VK] [VoR] `BS` NP [F[Ga]]) `BS` NP [F[O]]))
        ((Lam (Lam (Lam (Lam (App (Con "可能") (Pair (Var 2) (Lam (App (App (App (Var 4) (Var 0)) (Var 2)) (Var 1))))))))),[]),
  mylex ["得","え"] "(661a)" ((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) `BS` (defS verb [Cont] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (App (App (Var 2) (Var 1)) (Var 0))))))),[]),
  mylex ["得","え"] "(661b)" (((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga,Ni]]) `BS` NP [F[Ga]]) `BS` ((defS verb [Cont] `BS` NP [F[Ga]]) `BS` NP [F[O]]))
        ((Lam (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (App (App (App (Var 3) (Var 2)) (Var 1)) (Var 0)))))))),[]),
  mylex ["得る","うる"] "(662a)" ((defS [VURU] [Term,Attr] `BS` NP [F[Ga]]) `BS` (defS verb [Cont] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (App (App (Var 2) (Var 1)) (Var 0))))))),[]),
  mylex ["得る","うる"] "(662b)" (((defS [VURU] [Term,Attr] `BS` NP [F[Ga,Ni]]) `BS` NP [F[Ga]]) `BS` ((defS verb [Cont] `BS` NP [F[Ga]]) `BS` NP [F[Ni,O]]))
        ((Lam (Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (App (App (App (Var 3) (Var 2)) (Var 1)) (Var 0)))))))),[]),
  mylex ["れ"] "(666)" (defS [V1] [VoE] `BS` defS [V1] [Stem]) (id,[]),
  mylex ["来れ","これ"] "(667)" (defS [VK] [VoE] `BS` defS [VK] [Stem]) (id,[]),
  mylex ["でき","出来"] "new" ((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga,Ni]]) `BS` (defS [VSN] [Stem] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (App (Con "可能") (Pair (Var 1) (Lam (App (App (Var 3) (Var 0)) (Var 1)))))))),[]),
  mylex ["でき","出来"] "new" ((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga,Ni]]) `BS` NP [F[Ga]])
        ((Lam (Lam (Lam (App (App (Con "可能") (Var 2)) (Var 1))))),[]),
  -- 接続詞
  mylex ["が"] "(711)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` S [F anyPos, F[Term], F[P,M],F[P,M],F[P,M],F[M],F[M]]) 
        ((Lam (Lam (Lam (Sigma (App (Var 2) (Lam Top)) (App (Var 2) (Var 1)))))),[]),
  mylex ["し"] "(713)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` S [F anyPos, F[Term], F[P,M],F[P,M],F[P,M],F[M],F[M]]) 
        ((Lam (Lam (Lam (Sigma (App (Var 2) (Lam Top)) (App (Var 2) (Var 1)))))),[]),
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
  mylex ["に"] "(728)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` (defS [Nni] [Stem] `BS` NP [F[Ga]])) 
        ((Lam (Lam (Lam (App (Var 1) (Lam (Sigma (App (App (Var 3) (Var 0)) (Lam Top)) (App (Var 2) (Var 1)))))))),[]),
  mylex ["と"] "(731)" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` (defS [Nto] [Stem] `BS` NP [F[Ga]])) 
        ((Lam (Lam (Lam (App (Var 1) (Lam (Sigma (App (App (Var 3) (Var 0)) (Lam Top)) (App (Var 2) (Var 1)))))))),[]),
  --mylex ["が"] "(711)" ((T True 1 anySExStem `SL` T True 1 anySExStem) `BS` defS anyPos [Term]) 
  --      (Lam (Lam (Lamvec (Lamvec (Sigma (Appvec 0 (Var 3)) (Appvec 1 (Appvec 2 (Var 3)))))))),
  -- Wh句
  mylex ["誰","だれ"] "new" (NP [F[Nc]]) (properNameSR "誰"),
  mylex ["何","なに"] "new" (NP [F[Nc]]) (properNameSR "何"),
  mylex ["何処","どこ"] "new" (NP [F[Nc]]) (properNameSR "どこ"),
  -- 従属節導入表現
  mylex ["と","とは","とも"] "new" (Sbar [F[ToCL]] `BS` S [F anyPos, F[Term,Imper,Pre],F[P,M],F[P,M],F[P,M],F[M],F[M]]) (id,[]),
  mylex ["ように","ようには","ようにも"] "new" (Sbar [F[YooniCL]] `BS` S [F anyPos, F[Attr], F[P,M],F[P,M],F[P,M],F[M],F[M]]) (id,[]),
  mylex ["という"] "new" ((N `SL` N) `BS` S [F anyPos, F[Term], F[P,M],F[P,M],F[P,M],F[M],F[M]]) 
  ((Lam (Lam (Lam (Lam (Sigma (App (Var 3) (Lam Top)) (App (App (Var 3) (Var 2)) (Lam (Sigma (App (App (Con "content") (Var 1)) (Var 3)) (App (Var 3) (Var 1)))))))))), [("content",nPlaceEventType 1)]),
  mylex ["とともに"] "new" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` S [F anyPos, F[Term], F[P,M], F[P,M], F[P,M], F[M], F[M]]) ((Lam (Lam (Lam (Sigma (App (Var 2) (Lam Top)) (App (Var 1) (Var 0)))))),[]),
  --- とする
  mylex ["と"] "new" ((defS [VS] [Stem]) `BS` (S [F anyPos, F[Pre], F[P,M],F[P,M],F[P,M],F[M],F[M]])) (modalSR "トスル[MCN]"),
  -- 連用節
  --mylex ["に"] "new" ((T True 1 anySExStem `SL` T True 1 anySExStem) `BS` (defS [Nni] [Stem])) (Lam (Lam (Sigma (Var 1) (Var 1)))),
  -- 条件節
  mylex ["ば"] "new" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` (S [F anyPos,F[NegL,Hyp],F[P,M],F[P,M],F[P,M],F[M],F[M]])) 
        ((Lam (Lam (Lam (Pi (App (Var 2) (Lam Top)) (App (Var 2) (Var 1)))))),[]),
  mylex ["と"] "new" ((T False 1 anySExStem `SL` T False 1 anySExStem) `BS` (S [F anyPos,F[Term],F[P,M],F[P,M],F[P,M],F[M],F[M]])) 
        ((Lam (Lam (Lam (Pi (App (Var 2) (Lam Top)) (App (Var 2) (Var 1)))))),[]),
  -- 終助詞
  mylex ["か"] "new" (S [SF 1 anyPos, F[Term], F[P,M],F[P,M],F[P,M],F[M],F[M]] `BS` S [SF 1 anyPos, F[Term], F[P,M],F[P,M],F[P,M],F[M],F[M]]) (id,[]),
  --mylex ["ね","ねえ","ねー","ネ"] "new" (defS anyPos, F[Term]] `BS` defS anyPos [Term]) (id,[]),
  --mylex ["よ","ヨ"] "new" (defS anyPos [Term] `BS` defS anyPos [Term]) (id,[]),
  --mylex ["さ","さあ","さー","サ"] "new" (defS anyPos [Term] `BS` defS anyPos [Term]) (id,[]),
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
  -- 括弧
  --mylex ["「","（","(","『","《","〈","【","［","[","−","-"] "new" LPAREN Unit,
  --mylex ["」","）",")","』","》","〉","】","］","]","−","-"] "new" RPAREN Unit,
  mylex ["(","[","−","-"] "new" LPAREN (Unit, []),
  mylex [")","]","−","-"] "new" RPAREN (Unit, []),
  -- 形式名詞
  mylex ["こと","事"] "new" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Nc]])) `BS` (S [F anyPos, F[Attr], F[P,M],F[P,M],F[P,M],F[M],F[M]])) ((Lam (Lam (Lamvec (Sigma (Con "entity") (Sigma (Sigma (Con "state") (App (App (Con "こと") (Var 1 )) (Var 0))) (Sigma (App (App (Con "content") (App (Var 4) (Lam Top))) (Var 1)) (Appvec 3 (App (Var 4) (Var 2))))))))),[("こと",Type),("content",Type)]),
  mylex ["の"] "new" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Nc]])) `BS` (S [F anyPos, F[Attr], F[P,M],F[P,M],F[P,M],F[M],F[M]])) ((Lam (Lam (Lamvec (Sigma (Con "entity") (Sigma (Sigma (Con "state") (App (App (Con "の") (Var 1 )) (Var 0))) (Sigma (App (App (Con "content") (App (Var 4) (Lam Top))) (Var 1)) (Appvec 3 (App (Var 4) (Var 2))))))))),[("の",Type),("content",Type)]),
  mylex ["ことな","事な"] "new" (S [F[ANAS],F[Stem],F[M],F[M],F[P],F[M],F[M]] `BS` S [F anyPos,F [Attr],F[M],F[M],F[P,M],F[M],F[M]]) negOperator,
  -- 量化表現：Q-no N
  mylex ["すべての","あらゆる","一人一人の","各","各々の","それぞれの"] "(534)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Nc]])) `SL` N) 
        ((Lam (Lam (Lamvec (Pi (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),[]),
  mylex ["一人の","或る","ある","何人かの","数人の"] "(534)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Nc]])) `SL` N) 
        ((Lam (Lam (Lamvec (Sigma (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),[]),
  -- 量化表現：N-no Q
  mylex ["の一人一人","のそれぞれ","のすべて","の全員","の誰も"] "(535)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Nc]])) `BS` N) 
        ((Lam (Lam (Lamvec (Pi (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),[]),
  mylex ["の一人","の何人か","の数人","の誰か"] "(535)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Nc]])) `BS` N) 
        ((Lam (Lam (Lamvec (Sigma (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),[]),
  -- 量化表現：Q N
  mylex ["全員"] "(536)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Nc]])) `BS` N) 
        ((Lam (Lam (Lamvec (Pi (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),[]),
  mylex ["一人"] "(536)" ((T True 1 anySExStem `SL` (T True 1 anySExStem `BS` NP [F[Nc]])) `BS` N) 
        ((Lam (Lam (Lamvec (Sigma (Sigma (Con "entity") (App (App (Var 3) (Var 0)) (Lam Top))) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),[]),
  -- 遊離数量詞
  -- mylex ["全員","みな","誰も","すべて","それぞれ"]
  -- mylex ["一人","誰か"]
  -- 存在動詞
  mylex ["い"] "new" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) ((Lam (Lam (Sigma (Con "entity") (Sigma (Eq (Con "entity") (Var 0) (Var 2)) (App (Var 2) (Var 0)))))),[]),
  mylex ["あ"] "new" (defS [V5ARU] [Stem] `BS` NP [F[Ga]]) ((Lam (Lam (Sigma (Con "entity") (Sigma (Eq (Con "entity") (Var 0) (Var 2)) (App (Var 2) (Var 0)))))),[]),
  -- 状詞
  mylex ["多分"] "(Adv)" (defS [Nna,Nni] [Stem] `BS` NP [F[Ga]]) (predSR 1 "多分"),
  mylex ["多分"] "(Adv)" (defS [Nemp] [Stem] `BS` NP [F[Ga]]) (modalSR "タブン"),
  -- 引用のト形式の状詞
  mylex ["我先に"] "(Adv)" (defS [Nto] [Stem] `BS` NP [F[Ga]]) (predSR 1 "我先に"),
  mylex ["やれやれ"] "(Adv)" (defS [Nto] [Stem] `BS` NP [F[Ga]]) (predSR 1 "やれやれ"),
  mylex ["あれこれ"] "(Adv)" (defS [Nto] [Stem] `BS` NP [F[Ga]]) (predSR 1 "あれこれ"),
  mylex ["やいのやいの"] "(Adv)" (defS [Nto] [Stem] `BS` NP [F[Ga]]) (predSR 1 "やいのやいの"),
  mylex ["これでもか"] "(Adv)" (defS [Nto,Nemp] [Stem] `BS` NP [F[Ga]]) (predSR 1 "これでもか"),
  mylex ["これでもかこれでもか"] "(Adv)" (defS [Nto] [Stem] `BS` NP [F[Ga]]) (predSR 1 "これでもかこれでもか"),
  -- JSeM語彙
  mylex ["世界最高"] "new" (defS [Nda,Nno,Nni,Ntar] [Stem] `BS` NP [F[Ga]]) (predSR 1 "世界最高"),
  -- BCCWJ語彙
  mylex ["死","し"] "BCCWJ" ((defS [V5s] [Stem] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) (verbSR 2 "死す"),
  mylex ["ユーカㇻ"] "BCCWJ" (NP [F[Nc]]) (properNameSR "ユーカラ"),
  mylex ["則ち"] "BCCWJ" (defS [Nemp] [Stem] `BS` NP [F[Ga]]) (predSR 1 "すなわち"),
  mylex ["事実"] "BCCWJ" N (commonNounSR "事実")
  ]