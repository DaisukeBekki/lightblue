{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

{-|
Module      : Parser.Japanese.MyLexicon
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

The lexicon for functional lexical items in Japanese.
-}
module Parser.Language.Japanese.MyLexicon (
  emptyCategories,
  myLexicon,
  verblex
  ) where

import Prelude hiding (id)
import qualified Data.Text.Lazy as T
import Data.Ratio
import Parser.CCG
import Parser.Language.Japanese.Templates
import qualified DTS.DTTdeBruijn as DTT --lightblue
import DTS.UDTTdeBruijn as UDTT hiding (sig) --lightblue

type DTTpreterm = DTT.Preterm
type UDTTpreterm = UDTT.Preterm
type Signature = DTT.Signature

terminator :: UDTT.Preterm
terminator = UDTT.Ann (UDTT.Lam UDTT.Top) (DTT.Pi DTT.Entity DTT.Type)

-- | defines a lexical entry for an empty category
ec :: T.Text -> T.Text -> Integer -> Cat -> (UDTTpreterm, Signature) -> Node
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
  -- サ行変格活用動詞語幹 -- とりあえずガヲ、ガヲトのパターンのみ。→サ変名詞のみに修正
  --ec "do" "(156)" 100
  --            ((defS [VS] [Stem] `BS` NP [F[Ga]]) `BS` defS [VSN] [Stem])
  --            ((Lam (Lam (Lam (Sigma event (App (App (App (Con "する") (App (Var 3) (Var 1))) (Var 2)) (Var 0)))))), [("する",Pi Type (Pi entity Type))]),
  --lexicalitem "\\emp" "(156)" (100%100)
  --            (((defS [VS] [Stem] `BS` NP [Ga]) `BS` NP [F[O]]) `BS` defS [To])
  --            (Lam (Lam (App (Con "する") (Pair (Var 0) (Var 1))))),
  -- イ形容詞終止形活用語彙
  --ec "\\emp" "(175)" 100
  --            (S [F[Ai], F[Term], F[M], F[M], SF 1 [P,M], F[M], F[M]] `BS` S [F[Ai], F[Stem], F[M], F[M], SF 1 [P,M], F[M], F[M]])
  --            (id,[]),
  -- 判定詞語幹
  ec "be-pred." "(235a)" 100
              ((defS [Nda,Nna,Nno,Ntar] [NStem] `BS` NP [F [Ga]]) `BS` N)
              (id,[]),
  ec "be-ident." "(235b)" 99
              ((defS [Nda,Nna] [NStem] `BS` NP [F[Ga]]) `BS` NP [F[Nc]])
              ((Lam (Lam (Lam (Sigma (Eq Entity (Var 1) (Var 2)) (App (Var 1) (Var 0)))))),[]),
  -- サ変語幹→状詞語幹
  --ec "\\emp" "(262)" 99
  --            (defS [Nda,Nno] [NStem] `BS` defS [VSN] [Stem])
  --            (id,[]),
  -- サ変語幹→名詞
  ec "\\emp" "ss" 99
              ((((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `BS` NP [F[No]]) `BS` NP [F[No]]) `BS` ((defS [VSN] [Stem] `BS` NP [F [Ga]]) `BS` NP [F [O]]))
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
  --            (modalSR "＃ウ"),
   -- 可能態
  ec "\\emp" "(652a)" 99
              ((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) `BS` (defS anyPos [VoE] `BS` NP [F[Ga]]))
              ((Lam (Lam (Lam (App (App (Con "＃可能") (Lam (App (App (Var 3) (Var 0)) (Var 1)))) (Var 1)) ))),
               [("＃可能",(DTT.Pi (DTT.Pi DTT.Entity DTT.Type) (DTT.Pi DTT.Entity DTT.Type)))]),
  ec "\\emp" "(652b)" 99
              (((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ni,Ga]]) `BS` NP [F[Ga]]) `BS` ((defS anyPos [VoE] `BS` NP [F[Ga]]) `BS` NP [F[O]]))
              ((Lam (Lam (Lam (Lam (App (App (Con "＃可能") (Lam (App (App (Var 4) (Var 3)) (Var 0)))) (Var 1)))))),
               [("＃可能",(DTT.Pi (DTT.Pi DTT.Entity DTT.Type) (DTT.Pi DTT.Entity DTT.Type)))]),
  -- 状詞の副詞用法: ✕\p.\q.\v.\c.qv(\e.pe ∧ ce)
  --                \p.\q.\c.q(\e.(x:entity) × (＃様態(e,x) × (px(λx.T) × ce)))
  ec "\\emp" "(730)" 100
              ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (defS [Nemp] [NStem] `BS` NP [F[Ga]]))
              ((Lam (Lam (Lam (App (Var 1) (Lam (Sigma Entity (Sigma (App (App (Con "＃様態") (Var 0)) (Var 1)) (Sigma (App (App (Var 5) (Var 1)) terminator) (App (Var 4) (Var 3)))))))))), 
              [("＃様態", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type))]),
  -- 形容詞の様態副詞用法: 
  ec "\\emp" "(730)+" 100
              ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (defS [Aauo,Ai,ANAS,ATII] [Cont] `BS` NP[F[Ga]]))
              ((Lam (Lam (Lam (App (Var 1) (Lam (Sigma Entity (Sigma (App (App (Con "＃様態") (Var 0)) (Var 1)) (Sigma (App (App (Var 5) (Var 1)) terminator) (App (Var 4) (Var 3)))))))))), 
              [("＃様態", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type))]),
  -- 空冠詞（存在量化）
  ec "∃ " "(544)" 99
              ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `SL` N)
              ((Lam (Lam (Lamvec (Sigma (Sigma Entity (App (App (Var 3) (Var 0)) terminator)) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),[]),
  -- 空助詞 (as one of the last resorts)
  ec "cm" "(515)" 50
              (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]]))
              argumentCM,
  -- pro1
  ec "pro" "(597)" 99
              (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Ga,O,Ni,To,No,Niyotte]]))
              ((Lam (App (Var 0) (Asp Entity))),[]),
  -- pro2
  ec "pro" "(597)" 98
              (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Ga,O,Ni,To,No,Niyotte]]))
              ((Lam (App (Var 0) (Asp Entity))),[]),
  -- pro3
  ec "pro" "(597)" 97
              (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Ga,No]]))
              ((Lam (App (Var 0) (Asp Entity))),[]),
  -- 関係節化演算子(relativizer)
  ec "rel" "(670)" 99
              ((N `SL` N) `BS` (S [F anyPos, F[Attr], F[P,M],F[P,M],F[P,M],F[M],F[M]] `BS` NP [F[Ga,O,Ni,To]]))
              ((Lam (Lam (Lam (Lam (Sigma (App (App (Var 3) (Var 1)) terminator) (App (App (Var 3) (Var 2)) (Var 1))))))),[]),
  ec "rel-ext" "(670)+" 96
              ((N `SL` N) `BS` (S [F anyPos, F[Attr], F[P,M],F[P,M],F[P,M],F[M],F[M]]))
              ((Lam (Lam (Lam (Lam (Sigma (App (App (Var 2) (Var 1)) (Var 0)) (Sigma (App (Var 4) terminator) (Sigma (Pi Entity (Pi Entity Type)) (App (App (Var 0) (Var 1)) (Var 4))))))))),[]),
  -- 連用節、ヨウニ節、テ節
  ec "cont-mod" "new" 99
              ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (S [F ([V5k, V5s, V5t, V5n, V5m, V5r, V5w, V5g, V5z, V5b, V5IKU, V5YUK, V5ARU, V5NAS, V5TOW, V1, VS, VSN, VZ]++adjective++nomPred), F[Cont], F[M],F[P,M],F[P,M],F[M],F[M]])) (conjunctionSR "cont"),
  ec "yooni-mod" "new" 99
              ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` Sbar [F[YooniCL]]) (conjunctionSR "yooni"),
  ec "te-mod" "new" 99
              ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (S [F anyPos, F[TeForm], F[M],F[P,M],F[P,M],F[M],F[M]])) (conjunctionSR "te"),
  --ec "te-mod" "new" 80 -- 倒置
  --            ((T False 1 (S [F anyPos,F[Term,Pre,Imper],F[P,M],F[P,M],F[P,M],F[M],F[M]]) `BS` T False 1 (S [F anyPos,F[Term,Pre,Imper],F[P,M],F[P,M],F[P,M],F[M],F[M]])) `SL` (S [F anyPos, F[TeForm], F[M],F[P,M],F[P,M],F[M],F[M]])) (conjunctionSR "te"),
  ec "ni-neg-mod" "new" 99
              ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (S [F anyPos, F[NiForm], F[M],F[M],F[P],F[M],F[M]])) (conjunctionSR "ni-neg")
  --ec "ni-neg-mod" "new" 80 -- 倒置
  --            ((T False 1 (S [F anyPos,F[Term,Pre,Imper],F[P,M],F[P,M],F[P,M],F[M],F[M]]) `BS` T False 1 (S [F anyPos,F[Term,Pre,Imper],F[P,M],F[P,M],F[P,M],F[M],F[M]])) `SL` (S [F anyPos, F[NiForm], F[M],F[M],F[P],F[M],F[M]])) (conjunctionSR "ni-neg"),
  -- ダロウ接続形を派生する空範疇
  -- lexicalitem "\\emp" "(354)" (99%100)
  --             (defS (verb++adjective) [ModD] `BS` defS (verb++adjective) [Term])
  --             id,
  -- lexicalitem "\\emp" "(354)" (99%100)
  --             (defS [Nda] [ModD] `BS` defS [Nda] [NStem])
  --             id,
  ]

{- Some Macros for adding lexical items to lexicon -}

-- | 語彙項目登録用マクロ
mylex :: [T.Text] -> T.Text -> Cat -> (UDTT.Preterm, Signature) -> [Node]
mylex wds num cat' (sem',sig') = [(lexicalitem wd num 100 cat' (sem',sig')) | wd <- wds ]

mylex' :: [T.Text] -> T.Text -> Integer -> Cat -> (UDTT.Preterm, Signature) -> [Node]
mylex' wds num sco cat' (sem',sig') = [(lexicalitem wd num sco cat' (sem',sig')) | wd <- wds ]

verblex :: [T.Text] -> T.Text -> [FeatureValue] -> [FeatureValue] -> T.Text -> T.Text -> DTT.Preterm -> [Node]
verblex wds num posF conjF daihyo cf evt = [(lexicalitem wd num 100 (verbCat cf posF conjF) (verbSR daihyo evt cf))| wd <- wds ] 

-- | 活用語尾登録用マクロ
conjSuffix :: T.Text -> T.Text -> [FeatureValue] -> [FeatureValue] -> [Node]
conjSuffix wd num catpos catconj = [lexicalitem wd num 100 ((S ([SF 1 catpos, F catconj]++m5)) `BS` (S ([SF 1 catpos, F[Stem]]++m5))) (id,[])]

conjNSuffix :: T.Text -> T.Text -> [FeatureValue] -> [FeatureValue] -> [Node]
conjNSuffix wd num catpos catconj = [lexicalitem wd num 100 ((S ([SF 1 catpos, F catconj]++m5)) `BS` (S ([SF 1 catpos, F[NStem]]++m5))) (id,[])]

-- | A list of (mostly functional) lexical items extracted from Bekki (2010).
myLexicon :: [Node]
myLexicon = concat $ [
  -- 格助詞
  -- argument:
  mylex ["が"] "(524)" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Ga]])) `BS` NP [F[Nc]]) argumentCM,
  mylex ["を"] "(524)" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[O]])) `BS` NP [F[Nc]]) argumentCM,
  mylex ["に","へ"] "(524)" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Ni]])) `BS` NP [F[Nc]]) argumentCM,
  mylex ["によって","によっては"] "(524)" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Niyotte]])) `BS` NP [F[Nc]]) argumentCM,
  mylex' ["の"] "(531)?" 80 ((T True 1 N `SL` (T True 1 N `BS` NP [F[No]])) `BS` NP [F[Nc]]) argumentCM,
  -- 格助詞（の） T/(T\NPnc)/N\NPnc: \x.\n.\p.\vec.Sigma y:entity (Sigma ny(λT) (Sigma の(x,y) (py vec)))
  mylex ["の"] "(531)+" (((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `SL` N) `BS` NP[F[Nc]])
               ((Lam (Lam (Lam (Lamvec (Sigma Entity (Sigma (App (App (Var 3) (Var 0)) terminator) (Sigma (App (App (Con "＃ノ") (Var 5)) (Var 1)) (Appvec 3 (App (Var 4) (Var 2)))))))))) ,[("＃ノ", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type))]),
  mylex' ["が"] "(531)+" 60 (((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `SL` N) `BS` NP[F[Nc]])
               ((Lam (Lam (Lam (Lamvec (Sigma Entity (Sigma (App (App (Var 3) (Var 0)) terminator) (Sigma (App (App (Con "＃ノ") (Var 5)) (Var 1)) (Appvec 3 (App (Var 4) (Var 2)))))))))) ,[("＃ノ", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type))]),
  -- adjunct:
  mylex ["と","とは","とも","とさえ","とすら"] "(524)+" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` NP [F[Nc]]) (adjunctCM "＃ト"),
  mylex ["へ","へは","へも","へさえ","へと"] "(516)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` NP [F[Nc]]) (adjunctCM "＃へ"), -- 「へと」
  mylex ["で","では","でも","でさえ","ですら","じゃ","じゃあ"] "(516)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` NP [F[Nc]]) (adjunctCM "＃デ"),
  mylex ["から","からは","からも"] "(516)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` NP [F[Nc]]) (adjunctCM "＃カラ"),
  mylex ["まで","までは","までも"] "(516)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` NP [F[Nc]]) (adjunctCM "＃マデ"),
  mylex ["より","よりは","よりも"] "(516)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` NP [F[Nc]]) (adjunctCM "＃ヨリ"),
  mylex ["にて"] "(516)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` NP [F[Nc]]) (adjunctCM "＃場所"),
  ---
  mylex ["との"] "new" ((N `SL` N) `BS` (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (adjunctNM "＃ト"),
  mylex ["への"] "new" ((N `SL` N) `BS` (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (adjunctNM "＃ヘ"),
  mylex ["での"] "new" ((N `SL` N) `BS` (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (adjunctNM "＃デ"),
  mylex ["からの"] "new" ((N `SL` N) `BS` (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (adjunctNM "＃カラ"),
  mylex ["までの"] "new" ((N `SL` N) `BS` (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (adjunctNM "＃マデ"),
  -- についての、にとっての、においての（における）、としての、に基づいての、にあわせた
  --
  mylex ["について","については","についても"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` NP [F[Nc]]) (adjunctCM "＃ニツイテ"),
  mylex ["にとって","にとっては","にとっても"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` NP [F[Nc]]) (adjunctCM "＃ニトッテ"),
  mylex ["において","においては","においても"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` NP [F[Nc]]) (adjunctCM "＃ニオイテ"),
  mylex ["として","としては","としても"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` NP [F[Nc]]) (adjunctCM "＃トシテ"),
  mylex ["に基づいて"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` NP [F[Nc]]) (adjunctCM "＃ニモトヅイテ"),
  mylex ["にあわせて"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` NP [F[Nc]]) (adjunctCM "＃ニアワセテ"),
  mylex ["にしては"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` NP [F[Nc]]) (adjunctCM "＃ニシテハ"),
  --
  mylex ["についての"] "new" ((N `SL` N) `BS` (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (adjunctNM "＃ニツイテ"),
  mylex ["にとっての"] "new" ((N `SL` N) `BS` (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (adjunctNM "＃ニトッテ"),
  mylex ["においての","における"] "new" ((N `SL` N) `BS` (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (adjunctNM "＃ニオイテ"),
  mylex ["としての"] "new" ((N `SL` N) `BS` (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (adjunctNM "＃トシテ"),
  mylex ["に基づいた"] "new" ((N `SL` N) `BS` (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (adjunctNM "＃ニモトヅイテ"),
  mylex ["にあわせた"] "new" ((N `SL` N) `BS` (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (adjunctNM "＃ニアワセテ"),
  -- 等位接続
  mylex ["と","や","・","かつ","かつまた","そして","および","及び","なり","やら","だの","とか","にくわえて","に加えて","ならびに","並びに","をはじめ","をはじめとして","を始め"] "new" CONJ andSR,
  mylex ["か","または","又は","また","又","もしくは","若しくは","あるいは","或いは","ないしは","乃至は","ないし","乃至"] "new" CONJ orSR,
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
  mylex ["ん"] "(81)" (S [F[V5r],F[Neg,Term,Attr],F[M],F[M],F[M],F[P],F[M]] `BS` defS [V5r] [Stem]) (id,[]),
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
  --conjSuffix "い" "(132)" [V1] [Imper],
  mylex ["ん"] "(132)" (S ([F[V1], F[Neg,Term,Attr]]++mmmpm) `BS` defS [V1] [Stem]) (id,[]),
  -- 音便形
  conjSuffix "い" "(76)-" [V5k] [EuphT],
  conjSuffix "い" "(76)-" [V5g] [EuphD],
  conjSuffix "っ" "(78)-" [V5t,V5r,V5w,V5IKU,V5ARU,V5NAS] [EuphT],
  conjSuffix "ん" "(79)-" [V5n,V5m,V5b] [EuphD],
  conjSuffix "う" "(128)" [V5TOW] [EuphT],
  conjSuffix "い" "(123)" [V5NAS] [ModM,Imper], -- Contはナシ
  -- イク、ユク
  verblex ["行","い","逝"] "(103)" [V5IKU] [Stem] "行く/いく" "ガニ" event,
  verblex ["行","ゆ","逝"] "(108)" [V5YUK] [Stem] "行く/ゆく" "ガニ" event,
  verblex ["道行","道ゆ"] "(108)" [V5YUK] [Stem] "道行く/みちゆく" "ガ" event,
  verblex ["心行","心ゆ"] "(108)" [V5YUK] [Stem] "心ゆく/こころゆく" "ガ" event,
  -- ナサル型活用動詞
  mylex ["なさ"] "(122)" (defS [V5NAS] [Stem] `BS` defS verb [Cont]) (id,[]),
  mylex ["なさ"] "(122)" (defS [V5NAS] [Stem] `BS` defS [VSN] [Stem]) (id,[]),
  verblex ["いらっしゃ"] "(122)" [V5NAS] [Stem] "来る/くる" "ガ" event,
  mylex ["いらっしゃ"] "(122)" (defS [V5NAS] [Stem] `BS` defS anyPos [TeForm]) (eventModifier "来る/くる"),
  verblex ["仰","おっしゃ"] "(122)" [V5NAS] [Stem] "言う/いう" "ガト" event,
  verblex ["下さ","くださ"] "(122)" [V5NAS] [Stem] "くれる/くれる" "ガニヲ" event,
  mylex ["下さ","くださ"] "(122)" (defS [V5NAS] [Stem] `BS` defS anyPos [TeForm]) (eventModifier "くれる/くれる"),
  mylex ["ござ","御座"] "(122)" (defS [V5NAS] [Stem] `BS` defS anyPos [TeForm]) (id,[]),
  mylex ["ござ","御座"] "new" (defS [V5NAS] [Stem] `BS` defS [Aauo,Ai,ANAS,ATII] [Cont]) (id,[]),
  -- 例外的な命令形
  verblex ["くれ"] "(149)" [V1] [Imper] "くれる/くれる" "ガニヲ" event,
  mylex ["くれ"] "(150)" (defS [V1] [Imper] `BS` S [F verb, F[TeForm], F[M],F[M],F[P,M],F[M],F[M]]) (id,[]),
  verblex ["射れ","いれ"] "(151)" [V5r] [Imper] "射る/いる" "ガヲ" event,
  verblex ["蹴ろ","けろ"] "(152)" [V1] [Imper] "蹴る/ける" "ガヲ" event,
  verblex ["捻ろ","ひねろ"] "(153)" [V1] [Imper] "捻る/ひねる" "ガヲ" event,
  --- カ変動詞
  verblex ["来","やって来"] "(155)" [VK] [Neg,Cont,ModM,EuphT,NegL] "来る/くる" "ガニ" event,
  verblex ["こ","やってこ"] "(155)" [VK] [Neg,NegL] "来る/くる" "ガニ" event,
  verblex ["き","やってき"] "(155)" [VK] [Cont,ModM,EuphT] "来る/くる" "ガニ" event,
  verblex ["来ら","こら","やって来ら","やってこら"] "(155)" [VK] [VoR] "来る/くる" "ガニ" event,
  verblex ["来さ","こさ","やって来さ","やってこさ"] "(155)" [VK] [VoS] "来る/くる" "ガニ" event,
  verblex ["来る","くる","やって来る","やってくる"] "(155)" [VK] [Term,Attr] "来る/くる" "ガニ" event,
  verblex ["来れ","くれ","やって来れ","やってくれ"] "(155)" [VK] [Hyp] "来る/くる" "ガニ" event,
  verblex ["来い","こい","やって来い","やってこい"] "(155)" [VK] [Imper] "来る/くる" "ガニ" event,
  verblex ["来よ","こよ","やって来よ","やってこよ"] "(155)" [VK] [ModU] "来る/くる" "ガニ" event,
  mylex ["来ん","くん","やって来ん","やってくん"] "(155)" (S ([F[VK], F[Term,Attr]]++mmmpm) `BS` defS [VK] [Stem]) (id,[]),
  -- 補助動詞「くる」
  mylex ["来"] "(416)" ((defS [VK] [Neg,Cont,ModM,EuphT,NegL]) `BS` defS verb [TeForm]) (eventModifier "＃クル"),
  mylex ["こ"] "(416)" ((defS [VK] [Neg,NegL]) `BS` defS verb [TeForm]) (eventModifier "＃クル"),
  mylex ["き"] "(416)" ((defS [VK] [Cont,ModM,EuphT]) `BS` defS verb [TeForm]) (eventModifier "＃クル"),
  mylex ["来ら","こら"] "(416)" ((defS [VK] [VoR]) `BS` defS verb [TeForm]) (eventModifier "＃クル"),
  mylex ["来さ","こさ"] "(416)" ((defS [VK] [VoS]) `BS` defS verb [TeForm]) (eventModifier "＃クル"),
  mylex ["来る","くる"] "(416)" ((defS [VK] [Term,Attr]) `BS` defS verb [TeForm]) (eventModifier "＃クル"),
  mylex ["来れ","くれ"] "(416)" ((defS [VK] [Hyp]) `BS` defS verb [TeForm]) (eventModifier "＃クル"),
  mylex ["来い","こい"] "(416)" ((defS [VK] [Imper]) `BS` defS verb [TeForm]) (eventModifier "＃クル"),
  mylex ["来よ","こよ"] "(416)" ((defS [VK] [ModU]) `BS` defS verb [TeForm]) (eventModifier "＃クル"),
 --- サ変動詞
  verblex ["さ"] "(157)" [VS] [VoR,VoS] "する/する" "ガヲ" event,
  verblex ["し"] "(157)" [VS] [Neg,Cont,ModM,EuphT] "する/する" "ガヲ" event,
  verblex ["する"] "(157)" [VS] [Term,Attr] "する/する" "ガヲ" event,
  verblex ["すれ"] "(157)" [VS] [Hyp] "する/する" "ガヲ" event,
  verblex ["しろ"] "(157)" [VS] [Imper] "する/する" "ガヲ" event,
  verblex ["しよ"] "(157)" [VS] [ModU] "する/する" "ガヲ" event,
  verblex ["せ"] "(157)" [VS] [NegL] "する/する" "ガヲ" event,
  verblex ["す"] "(157)" [VS] [Term] "する/する" "ガヲ" event,
  verblex ["せよ"] "(157)" [VS] [Imper] "する/する" "ガヲ" event,
  verblex ["せい"] "(157)" [VS] [Imper] "する/する" "ガヲ" event,
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
  verblex ["得る","うる"] "(169)" [VURU] [Term,Attr] "得る/うる" "ガヲ" event,
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
                         S [SF 1 [Aauo,Ai,ANAS,ATII], F[Stem,UStem], F[M],F[M],SF 2 [P,M],F[M],F[M]]) (id,[]),
  mylex ["し"]   "(175)" (S [SF 1 [Aauo,ANAS,ABES], F[Term], F[M],F[M],SF 2 [P,M],F[M],F[M]] `BS` 
                         S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Stem], F[M],F[M],SF 2 [P,M],F[M],F[M]]) (id,[]),
  mylex ["き"]   "(175)" (S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Attr], F[M],F[M],SF 2 [P,M],F[M],F[M]] `BS` 
                          S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Stem], F[M],F[M],SF 2 [P,M],F[M],F[M]]) (id,[]),
  mylex ["かれ"] "(175)" (S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Imper], F[M],F[M],SF 2 [P,M],F[M],F[M]] `BS` 
                         S [SF 1 [Aauo,Ai,ANAS,ATII,ABES], F[Stem], F[M],F[M],SF 2 [P,M],F[M],F[M]]) (id,[]),
  -- ナシ型活用形容詞
  mylex ["良","よ"] "(173)" (defS [ANAS] [Stem,UStem] `BS` NP [F[Ga]]) (predSR 1 "良い/よい"),
  mylex ["無","な"] "(173)" (S ([F[ANAS], F[Stem]]++mmpmm) `BS` NP [F[Ga]]) (predSR 1 "無い/ない"), --  +nとした。「太郎しか財布にお金が無い」
  mylex ["無","な"] "(173)+" ((S ([F[ANAS], F[Stem]]++mmpmm) `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) (predSR 2 "無い/ない"),
  mylex ["無","の"] "(173)" (S ([F[ANAS], F[UStem]]++mmpmm) `BS` NP [F[Ga]]) (predSR 1 "無い/ない"), -- +nとした
  -- チイ形活用形容詞
  mylex ["弱っち","よわっち"] "(196)" (defS [ATII] [Stem] `BS` NP [F[Ga]]) (predSR 1 "弱っちい/よわっちい"),
  mylex ["ちゃち"]           "(196)" (defS [ATII] [Stem] `BS` NP [F[Ga]]) (predSR 1 "ちゃちい/ちゃちい"),
  mylex ["ばばっち","ばばち"] "(196)" (defS [ATII] [Stem] `BS` NP [F[Ga]]) (predSR 1 "ばばちい/ばばちい"),
  mylex ["ぼろっち","ぼろち"] "(196)" (defS [ATII] [Stem] `BS` NP [F[Ga]]) (predSR 1 "ぼろちい/ぼろちい"),
  mylex ["みみっち","みみち"] "(196)" (defS [ATII] [Stem] `BS` NP [F[Ga]]) (predSR 1 "みみっちい/みみっちい"),
  -- ベシ形活用形容詞
  verblex ["如","ごと"] "(199)" [ABES] [Stem] "如し/ごとし" "ガガ#ガノ" state,
  mylex ["如","ごと"] "(199)" (defS [ABES] [Stem] `BS` defS anyPos [Attr]) (verbSR "如し/ごとし" DTT.Entity "ガト"),--SR要修正
  mylex ["べ"] "(200)" (defS [ABES] [Stem] `BS` defS verb [Term]) (modalSR "べし/べし"),
  -- 形容詞派生形
  mylex ["さ","み"] "new" (N `BS` (defS adjective [Stem] `BS` NP [F[Ga]])) (id,[]),
  -- 状詞語幹
  conjNSuffix "だ" "(218)" [Nda] [NTerm],
  conjNSuffix "だっ" "(218)" [Nda] [EuphT],
  mylex ["です"] "(219)" (S ([F[Nda], F[NTerm]]++mpmmm) `BS` defS [Nda] [NStem]) (id,[]),
  mylex ["でし"] "(219)" (S ([F[Nda], F[EuphT]]++mpmmm) `BS` defS [Nda] [NStem]) (id,[]),
  conjNSuffix "な" "(220)" [Nna] [Attr],
  conjNSuffix "の" "(220)" [Nno] [Attr],
  conjNSuffix "なら" "(221)" [Nda] [NegL],
  conjNSuffix "なり" "(221)" [Nda] [Term],
  conjNSuffix "なる" "(221)" [Nda] [Attr],
  conjNSuffix "なれ" "(221)" [Nda] [Imper],
  conjNSuffix "たら" "(222)" [Ntar] [NegL],
  conjNSuffix "たり" "(222)" [Ntar] [Term],
  conjNSuffix "たる" "(222)" [Ntar] [Attr],
  conjNSuffix "たれ" "(222)" [Ntar] [Imper],
  -- 状詞活用する形容詞
  mylex ["大き","おおき"] "(258)" (defS [Nna] [NStem] `BS` NP [F[Ga]]) (predSR 1 "大きな/おおきな"),
  mylex ["小さ","ちいさ"] "(258)" (defS [Nna] [NStem] `BS` NP [F[Ga]]) (predSR 1 "小さな/ちいさな"),
  mylex ["可笑し","おかし"] "(258)" (defS [Nna] [NStem] `BS` NP [F[Ga]]) (predSR 1 "可笑しな/おかしな"),
  -- 「同じ」の連体用法
  mylex ["同じ","おなじ"] "new" (N `SL` N) (nominalModifier "同じ/おなじ"),
  -- 助動詞（過去）
  mylex ["たり"] "(308)" (S [SF 1 anyPos, F[Cont] ,F[P], SF 2 [P,M], SF 3 [P,M],F[M],F[M]] `BS` S [SF 1 anyPos, F[EuphT], F[M],SF 2 [P,M],SF 3 [P,M],F[M],F[M]]) (eventModifier "＃タ"),
  mylex ["た"] "(308)"  (S [SF 1 anyPos, F[Term,Attr], F[P],SF 2 [P,M],SF 3 [P,M],F[M],F[M]] `BS` S [SF 1 anyPos, F[EuphT], F[M],SF 2 [P,M],SF 3 [P,M],F[M],F[M]]) (eventModifier "＃タ"),
  mylex ["たら"] "(308)" (S [SF 1 anyPos, F[Hyp], F[P],SF 2 [P,M],SF 3 [P,M],F[M],F[M]] `BS` S [SF 1 anyPos, F[EuphT], F[M],SF 2 [P,M],SF 3 [P,M],F[M],F[M]])      (eventModifier "＃タ"),
  mylex ["たろ"] "(308)" (S [SF 1 anyPos, F[ModU], F[P],SF 2 [P,M],SF 3 [P,M],F[M],F[M]] `BS` S [SF 1 anyPos, F[EuphT], F[M],SF 2 [P,M],SF 3 [P,M],F[M],F[M]])     (eventModifier "＃タ"),
  mylex ["だり"] "(309)" (S [SF 1 verb, F[Cont], F[P],F[M],SF 2 [P,M],F[M],F[M]] `BS` S [SF 1 verb, F[EuphD], F[M],F[M],SF 2 [P,M],F[M],F[M]])     (eventModifier "＃タ"),
  mylex ["だ"] "(309)"   (S [SF 1 verb, F[Term,Attr], F[P],F[M],SF 2 [P,M],F[M],F[M]] `BS` S [SF 1 verb, F[EuphD], F[M],F[M],SF 2 [P,M],F[M],F[M]])  (eventModifier "＃タ"),
  mylex ["だら"] "(309)" (S [SF 1 verb, F[Hyp], F[P],F[M],SF 2 [P,M],F[M],F[M]] `BS` S [SF 1 verb, F[EuphD], F[M],F[M],SF 2 [P,M],F[M],F[M]])      (eventModifier "＃タ"),
  mylex ["だろ"] "(309)" (S [SF 1 verb, F[ModU], F[P],F[M],SF 2 [P,M],F[M],F[M]] `BS` S [SF 1 verb, F[EuphD], F[M],F[M],SF 2 [P,M],F[M],F[M]])     (eventModifier "＃タ"),
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
  mylex ["う"] "(349)" (S [SF 1 anyPos, F[Pre], SF 2 [P,M],SF 3 [P,M],SF 4 [P,M],F[M],F[M]] `BS` S [SF 1 anyPos, F[ModU], SF 2 [P,M],SF 3 [P,M],SF 4 [P,M],F[M],F[M]]) (modalSR "＃ウ"),
  mylex ["ん"] "(353)" (T False 1 (S ([SF 1 anyPos, F[Pre]]++m5)) `BS` T False 1 (S ([SF 1 anyPos, F[NegL]]++m5))) (modalSR "＃ン"),
  mylex ["だろう","であろう","だろ"] "(357)" (S [SF 1 anyPos, F[Pre], SF 2 [P,M],F[M],F[M],F[M],F[M]] `BS` S [SF 1 anyPos, F[Term,NStem], SF 2 [P,M],F[M],F[P,M],F[M],F[M]]) (modalSR "＃ダロウ"), -- ここから作業再開 (3/8)
  mylex ["でしょう","でしょ"] "(357)" (S [SF 1 anyPos, F[Pre], SF 2 [P,M],F[P],F[M],F[M],F[M]] `BS` S [SF 1 anyPos, F[Term,NStem], SF 2 [P,M],F[P,M],F[P,M],F[M],F[M]]) (modalSR "＃ダロウ"),
  mylex ["まい"] "(359a)" (S [SF 1 verb, F[Pre], F[M],SF 2 [P,M],F[P],F[M],F[M]] `BS` S [SF 1 verb, F[Term], F[M],SF 2 [P,M],F[M],F[M],F[M]])
                         ((Lam (Lam (App (Con "＃ダロウ") (Not (App (Var 1) (Var 0)))))),[]),
  mylex ["まい"] "(359b)" (S [SF 1 [V1,VK,VS,VSN,VZ], F[Pre], F[M],SF 2 [P,M],F[P],F[M],F[M]] `BS` S [SF 1 [V1,VK,VS,VSN,VZ], F[Neg], F[M],SF 2 [P,M],F[M],F[M],F[M]])
                         ((Lam (Lam (App (Con "＃ダロウ") (Not (App (Var 1) (Var 0)))))),[]),
  mylex ["まい"] "(359c)" (S [SF 1 [VS,VSN,VZ], F[Pre], F[M],SF 2 [P,M],F[P],F[M],F[M]] `BS` S [SF 1 [VS,VSN,VZ], F[Term], F[M],SF 2 [P,M],F[M],F[M],F[M]])
                         ((Lam (Lam (App (Con "＃ダロウ") (Not (App (Var 1) (Var 0)))))),[]),
  -- 動詞テ形
  mylex ["て"] "(369)" (S [SF 1 verb, F[TeForm], F[M],SF 2 [P,M],F[M],F[M],F[M]] `BS` S [SF 1 verb, F[EuphT], F[M],SF 2 [P,M],F[M],F[M],F[M]]) (id,[]),
  mylex ["で"] "(370)" (S ([SF 1 verb, F[TeForm]]++m5) `BS` S ([SF 1 verb, F[EuphD]]++m5)) (id,[]),
  mylex ["たって"] "(369)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` S [F verb, F[EuphT], F[M],F[P,M],F[M],F[M],F[M]]) (id,[]), --- CCG本から訂正 -- 意味表示要訂正
  mylex ["だって"] "(370)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` S [F verb, F[EuphD], F[M],F[M],F[M],F[M],F[M]]) (id,[]),          --- CCG本から訂正 -- 意味表示要訂正
  -- 形容詞テ形
  mylex ["て","って"] "(371)" (S ([SF 1 adjective, F[TeForm]]++m5) `BS` S ([SF 1 adjective, F[Cont]]++m5)) (id,[]),
  mylex ["たって"] "(371)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` defS adjective [Cont]) (id,[]),       --- CCG本から訂正 -- 意味表示要訂正
  -- 状詞テ形
  mylex ["で","では","でも"] "(372)" (defS [Nda] [TeForm] `BS` defS [Nda] [NStem]) (id,[]),
  mylex ["だって"] "(372)" (S [F[Nda],F[TeForm],F[P],F[M],F[M],F[M],F[M]] `BS` defS [Nda] [NStem]) (id,[]),
  -- 動詞ニ形
  mylex ["に"] "(376)" (S ([SF 1 verb, F[NiForm]]++m5) `BS` S ([SF 1 verb, F[Cont]]++m5)) (id,[]),
  -- 状詞ニ形
  mylex ["に"] "(377)" (defS [Nda] [NiForm] `BS` defS [Nda] [NStem]) (id,[]),
  -- 動詞性接尾語
  -- 6.1.1 形式述語 
  mylex ["居","い"] "(381)" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` defS verb [TeForm,NiForm]) (eventModifier "＃テイル"),
  mylex ["あ"] "(381)" (defS [V5ARU] [Stem] `BS` defS verb [TeForm]) (eventModifier "＃テアル"),
  mylex ["あ"] "(381)" (defS [V5ARU] [Stem] `BS` defS adjective [Cont]) (eventModifier "＃テアル"),
  mylex ["あ"] "(381)" (defS [V5ARU] [Stem] `BS` defS [Nda] [TeForm]) (id,[]),
  mylex ["お"] "(381*)" (defS [V5k] [Stem] `BS` defS verb [TeForm]) (eventModifier "＃テオク"),
  mylex ["な"] "(383)" ((S [F[ANAS],F[Stem],F[M],F[M],F[P],F[M],F[M]] `BS` NP [F[Ga]]) `BS` (defS adjective [Cont]) `BS` NP [F[Ga]]) negOperator2,
  mylex ["な"] "(383)" ((S [F[ANAS],F[Stem],F[M],F[M],F[P],F[M],F[M]] `BS` NP [F[Ga]]) `BS` (defS [Nda] [TeForm] `BS` NP [F[Ga]])) negOperator2,
  mylex ["な"] "(384)" ((defS [V5r] [Stem] `BS` NP [F[Ga]]) `BS` (defS adjective [Cont] `BS` NP [F[Ga]])) (intensionalEvent 1 "成る/なる"),
  mylex ["な"] "(384)" ((defS [V5r] [Stem] `BS` NP [F[Ga]]) `BS` (defS [Nda] [NiForm] `BS` NP [F[Ga]])) (intensionalEvent 1 "成る/なる"),
  -- い省略
  mylex ["て"] "(403)" (defS [V1] [Stem,Neg,ModM,NegL,EuphT] `BS` defS verb [EuphT]) (eventModifier "＃テイル"), -- Contはナシ
  -- 取り立て（副助詞）
  --mylex ["は"] "(385)" (S anyPos nonStem [M,M,M,M,P] `BS` defS anyPos nonStem) (Lam (Lam 
  mylex ["は"] "(550)" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["には"] "(550)" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  --mylex ["とは"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[To]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["も"] "(385)" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["にも"] "(385)" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  --mylex ["とも"] "(385)" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[To]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["こそ"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["にこそ"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["さえ"] "(387)" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["にさえ","さえに"] "(387)" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["すら"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["にすら"] "(387)" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["だけ"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga,O,Nc]]))) `BS` (NP [F[Nc]])) 
    ((Lam (Lam (Lamvec (Sigma (Appvec 0 (App (Var 1) (Var 2))) (Pi Entity (Pi (Appvec 2 (App (Var 3) (Var 0))) (Eq Entity (Var 5) (Var 1)))))))), []),
  mylex ["にだけ","だけに"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["とだけ","だけと"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[To]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["ばかり","ばっかり","ばっか"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["にばかり","にばっかり","にばっか"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["ばかりに","ばっかりに","ばっかに"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["とばかり","とばっかり","とばっか"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[To]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["ばかりと","ばっかりと","ばっかと"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[To]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["のみ"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["にのみ","のみに"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["しか"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["にしか"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["こそ"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["こそが"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["こそを"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["にこそ"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["なら"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["になら"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["って"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["ったら"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga]]))) `BS` (NP [F[Nc]])) argumentCM,
  --
  mylex ["など","なんか","なんて","なんぞ"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["などを","をなど","をなんか","をなんて","をなんぞ"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["などに","になど","になんか","になんて","になんぞ"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  ---
  mylex ["くらい"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga,O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["くらいが","くらいは"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["くらいを"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[O]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["くらいに"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ni]]))) `BS` (NP [F[Nc]])) argumentCM,
  mylex ["くらいの"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[No]]))) `BS` (NP [F[Nc]])) argumentCM,
  ---
  mylex ["とて"] "new" (((T True 1 modifiableS) `SL` ((T True 1 modifiableS) `BS` (NP [F[Ga]]))) `BS` (NP [F[Nc]])) argumentCM,
  --mylex ["まで"] -- 〜してまで
  --mylex ["どころ"]
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
  mylex ["過ぎ","すぎ"] "(412)+" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` defS (adjective++[Nda]) [Stem,NStem])   (eventModifier "過ぎる"),
  mylex ["あぐ"] "(412)" (defS [V5m] [Stem] `BS` defS verb [Cont])                            (eventModifier "あぐむ"),
  mylex ["かね"] "(412)" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` defS verb [Cont])         (eventModifier "かねる"),
  mylex ["やが"] "(412)" (defS [V5r] [Stem] `BS` defS verb [Cont])                            (eventModifier "やがる"),
  -- mylex ["入れ","容れ"] 
  -- 6.1.4 補助動詞（テ形接続）
  mylex ["お"] "(416)" (defS [V5k] [Stem] `BS` defS verb [TeForm])        (eventModifier "＃テオク"),
  mylex ["仕舞","しま"] "(416)" (defS [V5w] [Stem] `BS` defS verb [TeForm]) (eventModifier "＃テシマウ"),
  mylex ["行","い"] "(416)" (defS [V5IKU] [Stem] `BS` defS verb [TeForm])    (eventModifier "＃テイク"),
  mylex ["見","み"] "(416)" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` defS verb [TeForm])     (eventModifier "＃テミル"),
  mylex ["見せ","みせ"] "(416)" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` defS verb [TeForm]) (eventModifier "＃テミセル"),
  mylex ["ちゃ","ちま"] "(426)" (defS [V5w] [Stem] `BS` defS verb [EuphT]) (eventModifier "＃テシマウ"),
  mylex ["じゃ","じま"] "(427)" (defS [V5w] [Stem] `BS` defS verb [EuphD]) (eventModifier "＃テシマウ"),
  --mylex ["困","こま"] "(434a)" ((defS [V5r] [Stem] `BS` NP [F[Ga]]) `BS` S anyPos [TeForm]) (Lam p (Lam x (Lam c (Pi (App p terminator) (App (Con "困る") ()))))) -- ここまで
  --mylex ["拙","まず"] "(434b)" ((defS [Aauo] [Stem] `BS` NP [F[Ga]]) `BS` S anyPos [TeForm]) (Lam (Lam (Lam (Pi ))))
  -- 6.1.5 授受表現
  mylex ["上げ","あげ"] "(436)" ((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) `BS` (defS verb [TeForm] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (App (App (Con "＃アゲル") (App (App (Var 2) (Var 1)) (Var 0))) (Var 1))))), [("＃アゲル", DTT.Pi DTT.Type (DTT.Pi DTT.Entity DTT.Type))]),  -- Signature直す
  mylex ["貰","もら"] "(436)" (((defS [V5w] [Stem] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) `BS` (defS verb [TeForm] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (Lam (App (App (App (Con "＃モラウ") (App (App (Var 3) (Var 2)) (Var 0))) (Var 2)) (Var 1)))))) , [("＃モラウ", DTT.Pi DTT.Type (DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type)))]),
  mylex ["頂","いただ"] "new" (((defS [V5k] [Stem] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) `BS` (defS verb [TeForm] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (Lam (App (App (App (Con "＃モラウ") (App (App (Var 3) (Var 2)) (Var 0))) (Var 2)) (Var 1)))))) , [("＃モラウ", DTT.Pi DTT.Type (DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type)))]),
  -- 6.1.6 −がる
  mylex ["が"] "(443)" ((defS [V5r] [Stem] `BS` NP [F[Ga]]) `BS` (defS [Aauo,Ai,ANAS] [Stem] `BS` NP [F[Ga]])) (intensionalEvent 1 "＃ガル"),
  -- 6.1.7 −めく
  mylex ["め"] "(453)" ((defS [V5k] [Stem] `BS` NP [F[Ga]]) `BS` N) ((Lam (Lam (Lam (App (App (Con "＃メク") (App (App (Var 2) (Var 1)) (Var 0))) (Var 1))))), [("＃メク", DTT.Pi DTT.Type (DTT.Pi DTT.Entity DTT.Type))]), -- to be revised
  -- 形容詞性接尾語
  -- 6.2.1 ない
  mylex ["な"] "(455)" (S ([F[ANAS], F[Stem]]++mmpmm) `BS` S [F anyPos, F[Neg], F[M],F[M],F[P,M],F[P,M],F[M]]) negOperator,
  mylex ["無","な"] "(458)" ((S ([F[Aauo,ANAS], F[Stem]]++mmpmm) `BS` NP [F[Ni,Ga]]) `BS` NP [F[Ga]]) (predSR 2 "無い/ない"),
  mylex ["ねえ","ねぇ","ねー"] "(455)" (S ([F[ANAS], F[Term,Attr]]++mmpmm) `BS` defS anyPos [Neg]) negOperator,
  mylex ["ねえ","ねぇ","ねー"] "(458)" (S ([F[Aauo,ANAS], F[Term,Attr]]++mmpmm) `BS` NP [F[Ga]]) (predSR 1 "無い/ない"),
  mylex ["無し","なし","ナシ"] "(467)" (S ([F[Nda,Nno,Nni], F[NStem]]++mmpmm) `BS` NP [F[Ga]])    (predSR 1 "無い/ない"),
  -- 6.2.2 たい
  mylex ["た"] "(474a)" ((defS [Aauo] [Stem] `BS` NP [F[Ga]]) `BS` (defS verb [Cont] `BS` NP [F[Ga]])) (intensionalState 1 "＃タイ"),
  mylex ["た"] "(474b)" (((defS [Aauo] [Stem] `BS` NP [F[Ni,Ga]]) `BS` NP [F[Ga]]) `BS` ((defS verb [Cont] `BS` NP [F[Ga]])) `BS` NP [F[O]]) (intensionalState 2 "たい"),
  mylex ["難","にく"] "(475a)" ((defS [Aauo] [Stem] `BS` NP [F[Ga]]) `BS` (defS verb [Cont] `BS` NP [F[Ga]])) (intensionalState 1 "＃ニクイ"),
  mylex ["難","にく"] "(475b)" (((defS [Aauo] [Stem] `BS` NP [F[Ni,Ga]]) `BS` NP [F[Ga]]) `BS` ((defS verb [Cont] `BS` NP [F[Ga]])) `BS` NP [F[O]]) (intensionalState 2 "難"),
  mylex ["易","やす"] "(476a)" ((defS [Aauo] [Stem] `BS` NP [F[Ga]]) `BS` (defS verb [Cont] `BS` NP [F[Ga]])) (intensionalState 1 "＃ヤスイ"),
  mylex ["易","やす"] "(476b)" (((defS [Aauo] [Stem] `BS` NP [F[Ni,Ga]]) `BS` NP [F[Ga]]) `BS` ((defS verb [Cont] `BS` NP [F[Ga]])) `BS` NP [F[O]]) (intensionalState 2 "易"),
  -- 6.2.3-6.2.6
  mylex ["らし"] "(478)" (defS [Ai] [Stem] `BS` S [F anyPos,F[Term,NStem],F[P,M],F[M],F[P,M],F[M],F[M]])      (modalSR "＃ラシイ"),
  mylex ["らし"] "(478)" (defS [Ai] [Stem] `BS` defS [Nda] [NStem])     (modalSR "＃ラシイ"),
  mylex ["っぽ"] "(480)" (defS [Aauo] [Stem] `BS` defS [Aauo,Nda] [Stem,NStem]) (modalSR "＃ポイ"),
  mylex ["くさ"] "(478)" (defS [Ai] [Stem] `BS` S [F anyPos,F[Term,NStem],F[P,M],F[M],F[P,M],F[M],F[M]])         (modalSR "＃クサイ"),
  mylex ["べき"] "(488)" (defS [Nda] [Attr] `BS` defS verb [Term])       (modalSR "＃ベキ"),
  mylex ["かもしれな"] "new" (defS [Aauo] [Stem] `BS` S [F anyPos,F[Term,NStem],F[P,M],F[P,M],F[P,M],F[M],F[M]]) (modalSR "＃カモシレナイ"),
  -- 状詞性接尾語
  mylex ["よう"] "(493)" (defS [Nda,Nna,Nni] [NStem] `BS` S [F anyPos,F[Attr],F[P,M],F[M],F[P,M],F[M],F[M]]) (modalSR "＃ヨウダ"),
  mylex ["そう"] "(497)" (defS [Nda] [NStem] `BS` S [F anyPos,F[Term],F[P,M],F[M],F[P,M],F[M],F[M]])         (modalSR "＃ソウダ伝聞"),
  mylex ["そう"] "(498)" (defS [Nda,Nna,Nni] [NStem] `BS` defS anyPos [ModS]) (modalSR "＃ソウダ推量"),
  mylex ["がち"] "(506)" (defS [Nda,Nna,Nni] [NStem] `BS` defS anyPos [Cont]) (modalSR "＃ガチダ"),
  mylex ["みたい"] "(507)" (defS [Nda,Nna,Nni] [NStem] `BS` defS verb [Term]) (modalSR "＃ミタイダ"),
  mylex ["みたい"] "(507)" (defS [Nda,Nna,Nni] [NStem] `BS` defS adjective [Term]) (modalSR "＃ミタイダ"),
  mylex ["みたい"] "(507)" (defS [Nda,Nna,Nni] [NStem] `BS` defS [Nda] [NStem]) (modalSR "＃ミタイダ"),
  mylex ["的","てき"] "(508)" ((defS [Nda,Nna,Nni] [NStem] `BS` NP [F[Ga]]) `BS` NP [F[Nc]]) ((Lam (Lam (Eq Entity (Var 0) (Var 1)))),[]),-- ??
  mylex ["的","てき"] "(508)" ((defS [Nda,Nna,Nni] [NStem] `BS` NP [F[Ga]]) `BS` N) ((Lam (Lam (App (Var 1) (Var 0)))), []),-- ??
  mylex ["気味","ぎみ"] "(509)" ((defS [Nda,Nna,Nno,Nni] [NStem] `BS` NP [F[Ga]]) `BS` N) ((Lam (Lam (App (Var 1) (Var 0)))),[]),-- ??
  --mylex ["なの"] "(510)" (defS [Nda] [NStem] `BS` defS [Nda] [NStem])          (modalSR "＃ナノダ"),
  mylex ["の","ん"] "(511)" (S [F[Nda], F[NStem],F[M],F[M],F[M],F[M],F[M]] `BS` S [F anyPos,F[Attr],F[P,M],F[P,M],F[P,M],F[P,M],F[M]]) (modalSR "＃ノダ"),
  mylex ["筈","はず","ハズ"] "(511)" (defS [Nda] [NStem] `BS` S [F anyPos,F[Attr],F[P,M],F[M],F[P,M],F[M],F[M]]) (modalSR "＃ハズダ"),
  mylex ["訳","わけ","ワケ"] "(511)" (defS [Nda] [NStem] `BS` S [F anyPos,F[Attr],F[P,M],F[M],F[P,M],F[M],F[M]]) (modalSR "＃ワケダ"),
  mylex ["つもり"] "new"            (defS [Nda] [NStem] `BS` S [F anyPos,F[Attr],F[P,M],F[M],F[P,M],F[M],F[M]])            (modalSR "＃ツモリダ"),
  mylex ["もの","もん"] "(511)"      (defS [Nda] [NStem] `BS` S [F anyPos,F[Attr],F[P,M],F[M],F[P,M],F[M],F[M]])      (modalSR "＃モノダ"),
  mylex ["等","など"] "new" (defS [Nda,Nno] [NStem] `BS` S [F anyPos,F[Term,NStem],F[P,M],F[M],F[P,M],F[M],F[M]]) (modalSR "＃ナド"),
  mylex ["べき"] "new"      (defS [Nda] [NStem] `BS` S [F anyPos,F[Attr],F[M],F[M],F[M],F[M],F[M]]) (modalSR "＃ベキダ"),
  mylex ["風","ふう"] "new"      (defS [Nda] [NStem] `BS` S [F anyPos,F[Attr],F[P,M],F[M],F[P,M],F[M],F[M]]) (modalSR "＃フウダ"),
  mylex ["こと"] "new"      (defS [Nda] [NStem] `BS` S [F anyPos,F[Attr],F[P,M],F[P,M],F[P,M],F[M],F[M]]) (modalSR "＃コトダ"),
  mylex ["ばかり","ばっかり","ばっか"] "new" (defS [Nda] [NStem] `BS` S [F anyPos,F[Attr],F[P,M],F[M],F[M],F[M],F[M]]) (modalSR "＃バカリダ"),
  -- 照応代名詞
  mylex ["これ","それ","あれ","どれ"] "new" (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) 
        ((Lam (App (Var 0) (Proj Fst (Asp (Sigma Entity (Not (App (Con "＃有生") (Var 0)))))))), [("＃有生", DTT.Pi DTT.Entity DTT.Type)]),
  mylex ["ここ","そこ","あそこ","どこ"] "(586)" (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) 
        ((Lam (App (Var 0) (Proj Fst (Asp (Sigma Entity (App (Con "＃場所") (Var 0))))))), [("＃場所", DTT.Pi DTT.Entity DTT.Type)]),
  mylex ["この","その","あの","どの"] "(589)" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `SL` N)
        ((Lam (Lam (Lamvec (Appvec 0 (App (Var 1) (Proj Fst (Asp (Sigma Entity (App (App (Var 3) (Var 0)) terminator))))))))), []),
  mylex ["こんな","そんな","あんな","どんな"] "(589)" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `SL` N)
        ((Lam (Lam (Lamvec (Appvec 0 (App (Var 1) (Proj Fst (Asp (Sigma Entity (App (App (Var 3) (Var 0)) terminator))))))))), []),
  mylex ["彼","かれ","カレ"] "new" (NP [F[Nc]]) 
        ((Proj Fst (Asp (Sigma Entity (App (Con "＃男") (Var 0))))), [("＃男", DTT.Pi DTT.Entity DTT.Type)]),
  mylex ["彼女","かのじょ","カノジョ"] "new" (NP [F[Nc]]) 
        ((Proj Fst (Asp (Sigma Entity (App (Con "＃女") (Var 0))))), [("＃女", DTT.Pi DTT.Entity DTT.Type)]),
  -- 連体詞
  mylex ["こう"] "new" (modifiableS `SL` modifiableS) (eventModifier "＃こう"),
  mylex ["そう"] "new" (modifiableS `SL` modifiableS) (eventModifier "＃そう"),
  mylex ["ああ"] "new" (modifiableS `SL` modifiableS) (eventModifier "＃ああ"),
  mylex ["どう"] "new" (modifiableS `SL` modifiableS) (eventModifier "＃どう"),
  mylex ["このよう"] "new" (defS [Nna,Nni] [NStem] `BS` NP [F[Ga]]) (predSR 1 "＃このよう"),
  mylex ["そのよう"] "new" (defS [Nna,Nni] [NStem] `BS` NP [F[Ga]]) (predSR 1 "＃そのよう"),
  mylex ["あのよう"] "new" (defS [Nna,Nni] [NStem] `BS` NP [F[Ga]]) (predSR 1 "＃あのよう"),
  mylex ["どのよう"] "new" (defS [Nna,Nni] [NStem] `BS` NP [F[Ga]]) (predSR 1 "＃どのよう"),
  mylex ["こういう"] "new" (N `SL` N) (nominalModifier "＃こういう"),
  mylex ["そういう"] "new" (N `SL` N) (nominalModifier "＃そういう"),
  mylex ["ああいう"] "new" (N `SL` N) (nominalModifier "＃あういう"),
  mylex ["どういう"] "new" (N `SL` N) (nominalModifier "＃どういう"),
  -- カ節
  mylex ["か","かどうか"] "(603)" (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]) `BS` defS anyPos [Term,NStem]) 
        --((Lam (Lam (App (Var 0) (App (Con "＃ドウカ") (Var 1))))),[("＃ドウカ",Pi entity Type)]), 
        (Lam (Lam (Lamvec (Sigma Entity (Sigma (App (App (Con "＃ドウカ") (App (Var 3) terminator)) (Var 0)) (Appvec 2 (App (Var 3) (Var 1))))))), [("＃ドウカ", DTT.Pi DTT.Entity (DTT.Pi DTT.Type DTT.Type))]),
  -- 受動態・使役態
  mylex ["れ"] "(607)" (((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) `BS` (defS anyPos [VoR] `BS` NP [F[Ga]])) 
        ((Lam (Lam (Lam (Lam (App (App (Var 3) (Var 2)) (Lam (Sigma Entity (Sigma (App (App (App (Con "＃迷惑") (Var 1)) (Var 3)) (Var 0)) (App (Var 3) (Var 2)))))))))),[("＃迷惑", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type)))]),
  mylex ["れ"] "(608)" (((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) `BS` NP [F[Ni,Niyotte]]) `BS` ((defS anyPos [VoR] `BS` NP [F[Ga]]) `BS` NP [F[Ni,O]])) 
        ((Lam (Lam (Lam (Lam (App (App (App (Var 3) (Var 1)) (Var 2)) (Var 0)))))),[]),
  mylex ["せ"] "(629)" (((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) `BS` NP [F[Ni]]) `BS` (defS anyPos [VoS] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (Lam (App (App (Var 3) (Var 2)) (Lam (Sigma Entity (Sigma (App (App (App (Con "＃使役") (Var 1)) (Var 3)) (Var 0)) (App (Var 3) (Var 2)))))))))),[("＃使役", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type)))]),
  mylex ["せ"] "(635)" (((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) `BS` NP [F[O]]) `BS` (defS anyPos [VoS] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (Lam (App (App (Var 3) (Var 2)) (Lam (Sigma Entity (Sigma (App (App (App (Con "＃使役") (Var 1)) (Var 3)) (Var 0)) (App (Var 3) (Var 2)))))))))),[("＃使役", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type)))]), 
  -- 自発態
  verblex ["思われ","おもわれ"] "new" [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] "思われる/おもわれる" "ニト" state,
  verblex ["感じられ"] "new" [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] "感じられる/かんじられる" "ニト" state,
  verblex ["悔やまれ","くやまれ"] "new" [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] "悔やまれる/くやまれる" "ガニ" state,
  verblex ["偲ばれ","しのばれ"] "new" [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] "偲ばれる/しのばれる" "ガニ" state,
  verblex ["思い出され","おもいだされ"] "new" [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] "思い出される/おもいだされる" "ガニ" state,
  verblex ["考えられ"] "new" [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] "考えられる/かんがえられる" "ト" state,
  verblex ["予想され"] "new" [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] "予想される/よそうされる" "ト" state,
  verblex ["見受けられ","見うけられ"] "new" [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] "見受けられる/みうけられる" "ト" state,
  verblex ["悔やまれ","くやまれ"] "new" [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] "悔やまれる/くやまれる" "ト" state,
  verblex ["見られ","みられ"] "new" [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] "見られる/みられる" "ガニ" state,
  verblex ["見え","みえ"] "new" [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] "見える/みえる" "ガニ" state,
  verblex ["聞こえ","きこえ"] "new" [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] "聞こえる/きこえる" "ガニ" state,
  -- 以下、「可能」はstateを導入すべきか。
  mylex ["れ"] "(660a)" ((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) `BS` (defS [V1,VK] [VoR] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (App (App (Con "＃可能") (Lam (App (App (Var 3) (Var 0)) (Var 1)))) (Var 1))))),[("＃可能",(DTT.Pi (DTT.Pi DTT.Entity DTT.Type) (DTT.Pi DTT.Entity DTT.Type)))]),
  mylex ["れ"] "(660b)" (((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ni,Ga]]) `BS` NP [F[Ga]]) `BS` ((defS [V1,VK] [VoR] `BS` NP [F[Ga]]) `BS` NP [F[O]]))
        ((Lam (Lam (Lam (Lam (App (App (Con "＃可能") (Lam (App (App (App (Var 4) (Var 0)) (Var 2)) (Var 1)))) (Var 2)))))),[("＃可能",(DTT.Pi (DTT.Pi DTT.Entity DTT.Type) (DTT.Pi DTT.Entity DTT.Type)))]),
  mylex ["得","え"] "(661a)" ((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) `BS` (defS verb [Cont] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (App (App (Con "＃可能") (App (App (Var 2) (Var 1)) (Var 0))) (Var 1))))),[("＃可能",(DTT.Pi (DTT.Pi DTT.Entity DTT.Type) (DTT.Pi DTT.Entity DTT.Type)))]),
  mylex ["得","え"] "(661b)" (((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga,Ni]]) `BS` NP [F[Ga]]) `BS` ((defS verb [Cont] `BS` NP [F[Ga]]) `BS` NP [F[O]]))
        ((Lam (Lam (Lam (Lam (App (App (Con "＃可能") (App (App (App (Var 3) (Var 2)) (Var 1)) (Var 0))) (Var 1)))))),[("＃可能",(DTT.Pi (DTT.Pi DTT.Entity DTT.Type) (DTT.Pi DTT.Entity DTT.Type)))]),
  mylex ["得る","うる"] "(662a)" ((defS [VURU] [Term,Attr] `BS` NP [F[Ga]]) `BS` (defS verb [Cont] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (App (App (Con "＃可能") (App (App (Var 2) (Var 1)) (Var 0))) (Var 1))))),[("＃可能",(DTT.Pi (DTT.Pi DTT.Entity DTT.Type) (DTT.Pi DTT.Entity DTT.Type)))]),
  mylex ["得る","うる"] "(662b)" (((defS [VURU] [Term,Attr] `BS` NP [F[Ga,Ni]]) `BS` NP [F[Ga]]) `BS` ((defS verb [Cont] `BS` NP [F[Ga]]) `BS` NP [F[Ni,O]]))
        ((Lam (Lam (Lam (Lam (App (App (Con "＃可能") (App (App (App (Var 3) (Var 2)) (Var 1)) (Var 0))) (Var 1)))))),[("＃可能",(DTT.Pi (DTT.Pi DTT.Entity DTT.Type) (DTT.Pi DTT.Entity DTT.Type)))]),
  mylex ["れ"] "(666)" (defS [V1] [VoE] `BS` defS [V1] [Stem]) (id,[]),
  mylex ["来れ","これ"] "(667)" (defS [VK] [VoE] `BS` defS [VK] [Stem]) (id,[]),
  mylex ["でき","出来"] "new" ((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga,Ni]]) `BS` (defS [VSN] [Stem] `BS` NP [F[Ga]]))
        ((Lam (Lam (Lam (App (App (Con "＃可能") (Lam (App (App (Var 3) (Var 0)) (Var 1)))) (Var 1))))),[("＃可能",(DTT.Pi (DTT.Pi DTT.Entity DTT.Type) (DTT.Pi DTT.Entity DTT.Type)))]),
  mylex ["でき","出来"] "new" ((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga,Ni]]) `BS` NP [F[Ga]])
        ((Lam (Lam (Lam (App (App (Con "＃可能") (Var 2)) (Var 1))))),[("＃可能",(DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type)))]),
  -- 比較級
  mylex ["より"] "(comp)" (((T False 1 (S [SF 2 anyPos, SF 3 nonStem, SF 4 [P,M],SF 5 [P,M],SF 6 [P,M],F[M],F[M]]) `BS` NP [F[Ga]]) `SL` (T False 1 (S [SF 2 anyPos, SF 3 nonStem, SF 4 [P,M],SF 5 [P,M],SF 6 [P,M],F[M],F[M]]) `BS` NP [F[Ga]])) `BS` NP [F[Nc]])
        (Lam (Lam (Lam (Lam (App (App (Var 2) (Var 3)) (Lam (App (App (Var 3) (Var 2)) (Lam (Sigma (App (App (Con "＃ヨリ") (Var 1)) (Var 0)) (App (Var 3) (Var 1)))))))))),
         [("＃ヨリ", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type))]),
  mylex ["ほど"] "(comp)" (((T False 1 (S [SF 2 anyPos, SF 3 nonStem, SF 4 [P,M],SF 5 [P,M],SF 6 [P],F[M],F[M]]) `BS` NP [F[Ga]]) `SL` (T False 1 (S [SF 2 anyPos, SF 3 nonStem, SF 4 [P,M],SF 5 [P,M],SF 6 [P],F[M],F[M]]) `BS` NP [F[Ga]])) `BS` NP [F[Nc]])
        (Lam (Lam (Lam (Lam (App (App (Var 2) (Var 3)) (Lam (App (App (Var 3) (Var 2)) (Lam (Sigma (App (App (Con "＃比較") (Var 1)) (Var 0)) (App (Var 3) (Var 1)))))))))),
         [("＃比較", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type))]),
  -- 接続詞
  mylex ["が"] "(711)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` S [F anyPos, F[Term], F[P,M],F[P,M],F[P,M],F[M],F[M]]) 
        (conjunctionSR "ga"),
  mylex ["し"] "(713)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` S [F anyPos, F[Term], F[P,M],F[P,M],F[P,M],F[M],F[M]]) 
        (conjunctionSR "si"),
  mylex ["のに"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` S [F anyPos, F[Attr], F[P,M],F[P,M],F[P,M],F[M],F[M]]) 
        (conjunctionSR "noni"),
  mylex ["けど","けれど","けども","けれども"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` S [F anyPos, F[Term,NTerm], F[P,M],F[P,M],F[P,M],F[M],F[M]]) 
        (conjunctionSR "kedo"),
  --mylex ["と"] "new" (S [F anyPos,F[Term],SF 1 [P,M],SF 2 [P,M],SF 3 [P,M],F[M],F[M]] `SL` S [F anyPos,F[Term],SF 1 [P,M],SF 2 [P,M],SF 3 [P,M],F[M],F[M]]) (id,[]),
  -- そして
  -- それとも
  -- それに
  -- まで
  -- より
  --（名詞：副詞的名詞？？）
  mylex ["まま"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` S [F anyPos, F[Attr], F[P,M],F[P,M],F[P,M],F[M],F[M]]) 
        ((Lam (Lam (Lam (Sigma (App (Var 2) terminator) (App (Var 2) (Var 1)))))),[]),
  -- きり,っきり（エントリなし）
  -- コントロール構文
  mylex ["ながら"] "new" (((T False 1 modifiableS `BS` NP[F[Ga]]) `SL` (T False 1 modifiableS `BS` NP[F[Ga]])) `BS` (S [F verb, F[Cont], F[M],F[M],F[M],F[M],F[M]] `BS` NP[F[Ga]])) 
        ((Lam (Lam (Lam (Lam (Sigma (App (App (Var 3) (Var 1)) terminator) (App (App (Var 3) (Var 2)) (Var 1))))))), []),
  mylex ["でもって"] "new" (((T False 1 modifiableS `BS` NP[F[Ga]]) `SL` (T False 1 modifiableS `BS` NP[F[Ga]])) `BS` (defS nomPred [NStem] `BS` NP[F[Ga]]))
        ((Lam (Lam (Lam (Lam (Sigma (App (App (Var 3) (Var 1)) terminator) (App (App (Var 3) (Var 2)) (Var 1))))))), []),
  -- つつ、がてら
  -- やいなや
  -- ど,ども
  -- きや
  -- 状詞の副詞用法
  mylex ["に"] "(728)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (defS [Nni] [NStem] `BS` NP [F[Ga]])) 
        ((Lam (Lam (Lam (App (Var 1) (Lam (Sigma Entity (Sigma (App (App (Con "＃様態") (Var 0)) (Var 1)) (Sigma (App (App (Var 5) (Var 1)) terminator) (App (Var 4) (Var 3)))))))))), [("＃様態", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type))]), -- これと「のに」の兼ね合い
  mylex ["と","とも","とだけ","とばかり"] "(731)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (defS [Nto] [NStem] `BS` NP [F[Ga]])) 
        ((Lam (Lam (Lam (App (Var 1) (Lam (Sigma Entity (Sigma (App (App (Con "＃様態") (Var 0)) (Var 1)) (Sigma (App (App (Var 5) (Var 1)) terminator) (App (Var 4) (Var 3)))))))))), [("＃様態", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type))]),
  --mylex ["たる"] "(73x)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (defS [Ntar] [NStem] `BS` NP [F[Ga]])) 
  --      ((Lam (Lam (Lam (App (Var 1) (Lam (Sigma entity (Sigma (App (App (Con "＃様態") (Var 0)) (Var 1)) (Sigma (App (App (Var 5) (Var 1)) terminator) (App (Var 4) (Var 3)))))))))), [("＃様態",Pi entity (Pi event Type))]),
  --mylex ["が"] "(711)" ((T True 1 modifiableS `SL` T True 1 modifiableS) `BS` defS anyPos [Term]) 
  --      (Lam (Lam (Lamvec (Lamvec (Sigma (Appvec 0 (Var 3)) (Appvec 1 (Appvec 2 (Var 3)))))))),
  -- Wh句
  mylex ["誰","だれ"] "new" (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) (properNameSR "誰/だれ"),
  mylex ["何","なに","なん"] "new" (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) (properNameSR "何/なに"),
  mylex ["何処","どこ"] "new" (T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) (properNameSR "何処/どこ"),
  mylex ["何故","なぜ"] "new" (defS [Nda,Nemp] [NStem] `BS` NP [F[Ga]]) (predSR 1 "何故/なぜ"),
  -- 従属節導入表現
  --mylex ["に"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (defS [Nni] [NStem])) (conjunctionSR "ni"),
  mylex ["と","とは","とも","とさえ"] "new" (Sbar [F[ToCL]] `BS` S [F anyPos, F[Term,Imper,Pre,NStem,NTerm],F[P,M],F[P,M],F[P,M],F[M],F[M]]) (id,[]), -- ダロウ接続形なので後で正確に書きなおす -- この辺の接続条件要検証 -- 非過去のみ
  mylex ["ように","ようには","ようにも"] "new" (Sbar [F[YooniCL]] `BS` S [F anyPos, F[Attr], F[P,M],F[P,M],F[P,M],F[M],F[M]]) (id,[]),
  mylex ["という","といわれる"] "toiu" (defS [V5w] [Term,Attr] `BS` S [F anyPos, F[Term,Pre,NStem,NTerm,Imper], F[P,M],F[P,M],F[P,M],F[M],F[M]]) (modalSR "＃トイウ"),
  --mylex ["という"] "toiu1" ((N `SL` N) `BS` S [F anyPos, F[Term,Pre,NStem,NTerm,Imper], F[P,M],F[P,M],F[P,M],F[M],F[M]]) 
  --((Lam (Lam (Lam (Lam (Sigma (App (Var 3) terminator) (App (App (Var 3) (Var 2)) (Lam (Sigma (App (App (Con "＃内容") (Var 1)) (Var 3)) (App (Var 3) (Var 1)))))))))), [("＃内容",nPlaceEventType 1)]),
  mylex ["という"] "toiu3" ((N `SL` N) `BS` N) (Lam (Lam (Lam (Lam (Sigma (App (App (Var 3) (Var 1)) terminator) (App (App (Var 3) (Var 2)) (Var 1)))))),[]),
  mylex ["とともに"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` S [F anyPos, F[Term,NTerm], F[P,M], F[P,M], F[P,M], F[M], F[M]]) ((Lam (Lam (Lam (Sigma (App (Var 2) terminator) (App (Var 1) (Var 0)))))),[]),
  mylex ["のを"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` S [F anyPos, F[Attr], F[P,M], F[P,M], F[P,M], F[P,M], F[M]]) ((Lam (Lam (Lam (App (Var 2) (Lam (Sigma (App (Var 1) (Var 0)) (App (App (Con "＃トコロ") (Var 0)) (Var 1)))))))),[("＃トコロ", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type))]),
  mylex ["だけで"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` S [F anyPos, F[Attr], F[P,M], F[P,M], F[P,M], F[M], F[M]]) ((Lam (Lam (Lam (Sigma (App (Var 2) terminator) (App (Var 1) (Var 0)))))),[]),
  mylex ["ものの"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` S [F anyPos, F[Attr], F[P,M], F[P,M], F[P,M], F[M], F[M]]) ((Lam (Lam (Lam (Sigma (App (Var 2) terminator) (App (Var 1) (Var 0)))))),[]),
  -- 理由節
  mylex ["から"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` S [F anyPos, F[Term,NTerm], F[P,M],F[P,M],F[P,M],F[M],F[M]]) 
        (conjunctionSR "kara"),
        --((Lam (Lam (Lam (Sigma (App (Var 2) terminator) (Sigma (App (Var 2) (Var 1)) ()))))),[]),
  mylex ["ので"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` S [F anyPos, F[Attr], F[P,M],F[P,M],F[P,M],F[M],F[M]]) 
        (conjunctionSR "node"),
        --((Lam (Lam (Lam (Sigma (App (Var 2) terminator) (App (Var 2) (Var 1)))))),[]),
  mylex ["ため","ために"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` S [F anyPos, F[Attr], F[P,M],F[P,M],F[P,M],F[M],F[M]]) 
        (conjunctionSR "tame"),
  -- 条件節
  mylex ["ば"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (S [F anyPos,F[NegL,Hyp],F[P,M],F[P,M],F[P,M],F[M],F[M]])) 
        ((Lam (Lam (Lam (Pi (App (Var 2) terminator) (App (Var 2) (Var 1)))))),[]),
  mylex ["ど"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (S [F anyPos,F[Hyp],F[M],F[P,M],F[P,M],F[M],F[M]])) 
        ((Lam (Lam (Lam (Pi (App (Var 2) terminator) (App (Var 2) (Var 1)))))),[]),
  mylex ["なら","ならば"] "(779a)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (S [F anyPos,F[Term,NStem],F[P,M],F[P,M],F[P,M],F[M],F[M]])) 
        ((Lam (Lam (Lam (Pi (App (Var 2) terminator) (App (Var 2) (Var 1)))))),[]),
  mylex ["たら","たらば"] "(780)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (S [F anyPos,F[EuphT],F[M],F[P,M],F[P,M],F[M],F[M]])) 
        ((Lam (Lam (Lam (Pi (App (Var 2) terminator) (App (Var 2) (Var 1)))))),[]),
  mylex ["だら","だらば"] "(781)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (S [F anyPos,F[EuphD],F[M],F[P,M],F[P,M],F[M],F[M]])) 
        ((Lam (Lam (Lam (Pi (App (Var 2) terminator) (App (Var 2) (Var 1)))))),[]),
  mylex ["と"] "(785)" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (S [F anyPos,F[Term,NTerm],F[M],F[P,M],F[P,M],F[M],F[M]])) 
        ((Lam (Lam (Lam (Pi (App (Var 2) terminator) (App (Var 2) (Var 1)))))),[]), -- 非過去のみ
  mylex ["にせよ"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (S [F anyPos,F[Term,NStem],F[P,M],F[M],F[P,M],F[M],F[M]])) 
        ((Lam (Lam (Lam (Pi (App (Var 2) terminator) (App (Var 2) (Var 1)))))),[]),
  mylex ["ても"] "new" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (S [F adjective,F[Cont],F[M],F[M],F[P,M],F[M],F[M]])) 
        ((Lam (Lam (Lam (Pi (App (Var 2) terminator) (App (Var 2) (Var 1)))))),[]),
  -- 「走っても間に合わない」「飛んでも間に合わない」「学生でもいい」は？
  -- きゃ、しゃ、ちゃ、にゃ、みゃ、りゃ、や、ぎゃ、びゃ、来りゃ、くりゃ、すりゃ、ずりゃ、
  --- とする
  mylex ["と"] "new" ((defS [VS] [Stem]) `BS` (S [F anyPos, F[Pre], F[P,M],F[P,M],F[P,M],F[M],F[M]])) (modalSR "＃トスル"),
  -- 終助詞
  mylex ["か"] "new" (S [SF 1 anyPos, F[Term], F[P,M],F[P,M],F[P,M],F[M],F[M]] `BS` S [SF 1 anyPos, F[Term,NStem], F[P,M],F[P,M],F[P,M],F[M],F[M]]) (id,[]),
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
  -- とは
  -- 括弧
  --mylex ["「","（","(","『","《","〈","【","［","[","−","-"] "new" LPAREN Unit,
  --mylex ["」","）",")","』","》","〉","】","］","]","−","-"] "new" RPAREN Unit,
  mylex ["(","[","−","-"] "new" LPAREN (Unit, []),
  mylex [")","]","−","-"] "new" RPAREN (Unit, []),
  -- 形式名詞
  mylex ["こと","事"] "new" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `BS` (S [F anyPos, F[Attr], F[P,M],F[P,M],F[P,M],F[M],F[M]])) ((Lam (Lam (Lamvec (Sigma Entity (Sigma (Sigma Entity (App (App (Con "＃コト") (Var 1 )) (Var 0))) (Sigma (App (App (Con "＃内容") (App (Var 4) terminator)) (Var 1)) (Appvec 3 (App (Var 4) (Var 2))))))))),[("＃コト", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type)),("＃内容",DTT.Pi DTT.Type (DTT.Pi DTT.Entity DTT.Type))]),
  mylex ["の"] "new" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `BS` (S [F anyPos, F[Attr], F[P,M],F[P,M],F[P,M],F[M],F[M]])) ((Lam (Lam (Lamvec (Sigma Entity (Sigma (Sigma Entity (App (App (Con "＃ノ") (Var 1 )) (Var 0))) (Sigma (App (App (Con "＃内容") (App (Var 4) terminator)) (Var 1)) (Appvec 3 (App (Var 4) (Var 2))))))))),[("＃ノ", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type)),("＃内容", DTT.Pi DTT.Type (DTT.Pi DTT.Entity DTT.Type))]),
  mylex ["の"] "cleft" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `BS` (S [F anyPos, F[Attr], F[P,M],F[P,M],F[P,M],F[M],F[M]] `BS` NP[F[Ga,O,Ni]])) ((Lam (Lam (Lamvec (Sigma Entity (Sigma (App (App (Var 3) (Var 0)) terminator) (Appvec 2 (App (Var 3) (Var 1)))))))),[]),
  mylex ["ことな","事な"] "new" (S [F[ANAS],F[Stem],F[M],F[M],F[P],F[M],F[M]] `BS` S [F anyPos,F [Attr],F[M],F[M],F[P,M],F[M],F[M]]) negOperator,
  --
  -- 量化表現：Q-no N
  mylex ["すべての","あらゆる","一人一人の","各","各々の","それぞれの"] "(534)" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `SL` N) 
        ((Lam (Lam (Lamvec (Pi (Sigma Entity (App (App (Var 3) (Var 0)) terminator)) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),[]),
  mylex ["一人の","或る","ある","何人かの","数人の"] "(534)" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `SL` N) 
        ((Lam (Lam (Lamvec (Sigma (Sigma Entity (App (App (Var 3) (Var 0)) terminator)) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),[]),
  -- 量化表現：N-no Q
  mylex ["の一人一人","のそれぞれ","のすべて","の全員","の誰も"] "(535)" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `BS` N) 
        ((Lam (Lam (Lamvec (Pi (Sigma Entity (App (App (Var 3) (Var 0)) terminator)) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),[]),
  mylex ["の一人","の何人か","の数人","の誰か"] "(535)" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `BS` N) 
        ((Lam (Lam (Lamvec (Sigma (Sigma Entity (App (App (Var 3) (Var 0)) terminator)) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),[]),
  -- 量化表現：N Q
  mylex ["全員"] "(536)" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `BS` N) 
        ((Lam (Lam (Lamvec (Pi (Sigma Entity (App (App (Var 3) (Var 0)) terminator)) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),[]),
  mylex ["一人"] "(536)" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `BS` N) 
        ((Lam (Lam (Lamvec (Sigma (Sigma Entity (App (App (Var 3) (Var 0)) terminator)) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),[]),
  -- その他GQ
  mylex ["半数以上の","半数の"] "(GQ)" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `SL` N) (generalizedQuantifierSR "半数"),  
  mylex ["の半数以上","の半数","半数","半数以上"] "(GQ)" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `BS` N) (generalizedQuantifierSR "半数"),  
  mylex ["ほとんどの"] "(GQ)" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `SL` N) (generalizedQuantifierSR "ほとんど"),  
  mylex ["のほとんど"] "(GQ)" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `BS` N) (generalizedQuantifierSR "ほとんど"),  
  -- NPI
  mylex ["ろく","碌","ロク"] "new" (defS [Nna,Nni] [NStem] `BS` NP [F[Ga]]) (predSR 1 "ろく/ろく"), -- NPIなので後でちゃんと効かせる
  -- 遊離数量詞
  -- mylex ["全員","みな","誰も","すべて","それぞれ"]
  -- mylex ["一人","誰か"]
  -- 存在動詞 \x.\z.\c.Sigma (s:state) (Sigma (存在(s,x)) (Sigma (場所(s,z)) cs)), 存在,場所
  --mylex ["い"] "new" ((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP[F[Ni]]) `BS` NP [F[Ga]]) ((Lam (Lam (Sigma entity (Sigma (Eq entity (Var 0) (Var 2)) (App (Var 2) (Var 0)))))),[]),
  --mylex ["あ"] "new" ((defS [V5ARU] [Stem] `BS` NP[F[Ni]]) `BS` NP [F[Ga]]) ((Lam (Lam (Sigma entity (Sigma (Eq entity (Var 0) (Var 2)) (App (Var 2) (Var 0)))))),[]),
  -- 存在の「いる」「ある」
  mylex ["い"] "new" (defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP [F[Ga]]) ((Lam terminator),[]),
  mylex ["あ"] "new" (defS [V5ARU] [Stem] `BS` NP [F[Ga]]) ((Lam terminator),[]),
  -- 場所の「いる」「ある」
  mylex ["い"] "new" ((defS [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] `BS` NP[F[Ni]]) `BS` NP [F[Ga]]) 
        ((Lam (Lam (Lam (Sigma Entity (Sigma (App (App (Con "＃存在") (Var 3)) (Var 0)) (Sigma (App (App (Con "＃場所") (Var 3)) (Var 1)) (App (Var 3) (Var 2)))))))),[("＃存在", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type)),("＃場所", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type))]),
  mylex ["あ"] "new" ((defS [V5ARU] [Stem] `BS` NP[F[Ni]]) `BS` NP [F[Ga]]) 
        ((Lam (Lam (Lam (Sigma Entity (Sigma (App (App (Con "＃存在") (Var 3)) (Var 0)) (Sigma (App (App (Con "＃場所") (Var 3)) (Var 1)) (App (Var 3) (Var 2)))))))),[("＃存在", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type)),("＃場所", DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type))]),
  -- 状詞
  mylex ["多分"] "(Adv)" (defS [Nna,Nni] [NStem] `BS` NP [F[Ga]]) (predSR 1 "多分/たぶん"),
  mylex ["多分"] "(Adv)" (defS [Nemp] [NStem] `BS` NP [F[Ga]]) (modalSR "多分/たぶん"),
  mylex' ["とこ"] "new" 90 (defS [Nda] [NStem] `BS` NP [F[Ga]]) (predSR 1 "ところ/ところ"),
  -- 慣用的副詞
  mylex ["どういうわけか"] "new" (T False 1 modifiableS `SL` T False 1 modifiableS) (id,[]),
  mylex ["それはさておき"] "new" (T False 1 modifiableS `SL` T False 1 modifiableS) (id,[]),
  -- 引用のト形式の状詞
  mylex ["我先に"] "(Adv)" (defS [Nto] [NStem] `BS` NP [F[Ga]]) (predSR 1 "我先に/われさきに"),
  mylex ["やれやれ"] "(Adv)" (defS [Nto] [NStem] `BS` NP [F[Ga]]) (predSR 1 "やれやれ/やれやれ"),
  mylex ["あれこれ"] "(Adv)" (defS [Nto] [NStem] `BS` NP [F[Ga]]) (predSR 1 "あれこれ/あれこれ"),
  mylex ["やいのやいの"] "(Adv)" (defS [Nto] [NStem] `BS` NP [F[Ga]]) (predSR 1 "やいのやいの/やいのやいの"),
  mylex ["これでもか"] "(Adv)" (defS [Nto,Nemp] [NStem] `BS` NP [F[Ga]]) (predSR 1 "これでもか/これでもか"),
  mylex ["これでもかこれでもか"] "(Adv)" (defS [Nto] [NStem] `BS` NP [F[Ga]]) (predSR 1 "これでもかこれでもか"),
  -- JSeM用追加語彙項目
  mylex ["世界最高"] "new" (defS [Nda,Nno,Nni,Ntar] [NStem] `BS` NP [F[Ga]]) (predSR 1 "世界最高/せかいさいこう"),
  -- BCCWJ用追加語彙項目
  -- PB10_00047
  verblex ["死"] "PB10-3" [V5s] [Stem] "死す/しす" "ガニ" event,
  --mylex ["古代"] "PB10-4" (NP[F[Nc]] `BS` NP[F[Nc]]) ((Con "古代"),[("古代",Pi entity entity)]),
  mylex ["日本古代"] "PB10-4" (NP[F[Nc]]) ((Con "日本古代/にほんこだい"),[("日本古代/にほんこだい", DTT.Entity)]),
  mylex ["第一"] "PB10-10" (defS [Nda,Nno,Nni,Nemp,Ntar] [NStem] `BS` NP[F[Ga]]) (predSR 1 "第一/だいいち"),
  mylex ["もののふ"] "PB10-10" N (commonNounSR "武士/もののふ"),
  verblex ["突き落"] "PB10-10" [V5s] [Stem] "突き落とす/つきおとす" "ガヲ" event,
  mylex ["則ち"] "PB10-11" (defS [Nemp] [NStem] `BS` NP [F[Ga]]) (predSR 1 "すなわち/すなわち"),
  mylex ["まっ先"] "PB10-11" (defS [Nno,Nni] [NStem] `BS` NP [F[Ga]]) (predSR 1 "真っ先/まっさき"),
  verblex ["引返","引き返","引きかえ","ひき返","ひきかえ"] "PB10-11" [V5s] [Stem] "引き返す/ひきかえす" "ガ" event,
  mylex ["あいだ"] "PB10-11" N (commonNounSR "間/あいだ"),
  mylex ["からから"] "PB10-12" (defS [Nda,Nno,Nemp,Nto] [NStem] `BS` NP[F[Ga]]) (predSR 1 "カラカラ/からから"),
  verblex ["殪"] "PB10-13" [V5s] [Stem] "殪す/のこす" "ガヲ" event,
  verblex ["沃え"] "PB10-14" [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] "沃える/こえる" "ガ" state,
  verblex ["名を得"] "PB10-15" [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] "名を得る/なをえる" "ガニ" event,
  verblex ["押っ取り篭め","おっとりこめ"] "PB10-16" [V1] [Stem,Neg,Cont,ModM,NegL,EuphT] "押っ取り篭め/おっとりこめ" "ガヲ" event,
  mylex ["ずっぷり"] "PB10-18" (defS [Nda,Nemp,Nto] [NStem] `BS` NP[F[Ga]]) (predSR 1 "ずっぷり/ずっぷり"),
  mylex ["まっすぐ"] "PB10-18" (defS [Nda,Nna,Nno,Nni,Nemp] [NStem] `BS` NP[F[Ga]]) (predSR 1 "真っ直ぐ/まっすぐ"),
  mylex ["疾"] "PB10-23" (defS [Aauo] [Stem] `BS` NP[F[Ga]]) (predSR 1 "疾い/はやい"),
  verblex ["退","しさ","すさ"] "PB10-23" [V5r] [Stem] "退る/しさる" "ガ" event,
  verblex ["くずおれ去"] "PB10-23" [V5r] [Stem] "くずおれ去る/くずおれさる" "ガ" event,
  mylex ["ユーカㇻ"] "PB10-25" (NP [F[Nc]]) (properNameSR "ユーカラ/ユーカラ"),
  mylex ["先"] "PB10-25" (defS [Nda,Nno,Nni] [NStem] `BS` NP [F[Ga]]) (predSR 1 "先/さき"),
  mylex ["あまり"] "PB10-26" (defS [Nno,Nni] [NStem] `BS` NP[F[Ga]]) (predSR 1 "あまり/あまり"),
  mylex ["アイヌユーカㇻ"] "PB10-30" (NP [F[Nc]]) (properNameSR "アイヌユーカラ/アイヌユーカラ"),
  mylex ["とっく"] "PB10-39" (defS [Nni] [NStem] `BS` NP[F[Ga]]) (predSR 1 "疾っく/とっく"),
  mylex ["とっくの昔"] "PB10-39" (defS [Nda,Nni,Nno] [NStem] `BS` NP[F[Ga]]) (predSR 1 "疾っくの昔/とっくのむかし"),
  mylex ["不死"] "PB10-44" N (commonNounSR "不死/ふし"),
  mylex ["不死"] "PB10-44" (defS [Nda,Nno] [NStem] `BS` NP[F[Ga]]) (predSR 1 "不死/ふし"),
  -- PB12_00001
  verblex ["思い付","思いつ","おもい付","おもいつ"] "PB12-26" [V5k] [Stem] "思い付く/おもいつく" "ガヲ" DTT.Entity, -- 二次アスペクトは規則で生成？
  mylex ["暇","ひま","ヒマ"] "PB12-37" N (commonNounSR "暇/ひま"), -- ナ形容詞のエントリは存在
  mylex ["三分の一"] "PB12-51" N (commonNounSR "三分の一/１／３"), --- これは後で数詞をちゃんとやる。
  mylex ["必要"] "PB12-67" N (commonNounSR "必要/ひつよう"), -- ナ形容詞のエントリ有り
  mylex ["時には"] "PB12-86" (defS [Nemp] [NStem] `BS` NP[F[Ga]]) (commonNounSR "時には/ときには"), -- 「時に」は副詞エントリがあるが意味が異なる
  mylex ["真っ暗闇"] "PB12-86" N (commonNounSR "真っ暗闇/まっくらやみ"), -- 「真っ」は接頭語のエントリ有り？
  -- PN
  mylex ["☎"] "PN" (NP [F[Nc]]) (properNameSR "電話番号"),
  -- ABCbank
  mylex ["ところ"] "(ABC)" (defS [Nda,Nni,Nemp] [NStem] `BS` S [F anyPos,F[Attr],F[P,M],F[M],F[P,M],F[M],F[M]]) (modalSR "＃トコロ"),
  mylex ["イエス"] "PN" (NP [F[Nc]]) (properNameSR "イエス")
  ]
