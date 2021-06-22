module DTS.Prover_daido.Judgement
( Judgement(..),
  UJudgement(..),
  TEnv,
  TUEnv,
  SUEnv,
  Tree(..),
  UTree (..),
  Label(..),
  ULabel(..),
  getTerm,
  getType,
  getTypeU,
  getTermU,
  getList,
  utreeToTeX,
  utreeToMathML,
  treeToMathML,
  treeToTeX
) where

import qualified DTS.UDTT as UD           -- UDTT
import qualified DTS.DTT as DT            -- DTT
import qualified Data.Text.Lazy as T      -- text
import qualified Data.List as L           -- base
import qualified Control.Applicative as M -- base
import qualified Control.Monad as M       -- base
import qualified DTS.UDTTwithName as VN
import Interface.Text
import Interface.TeX
import Interface.HTML


-- TUEnv : UDTTの型環境の型
-- | haddock
type TUEnv = [UD.Preterm]

-- TEnv : DTTの型環境の型
-- | haddock
type TEnv = [DT.Preterm]

-- | SUEnv : UDTTのシグネチャの型
type SUEnv = UD.Signature

-- | getList : 環境 env の中で変数 var の値をすべて返す
getList :: (Eq k, Show k) => [(k, v)] -> k -> [v]
getList [] key = []
getList ((k,v):xs) key
  | key == k  = v:(getList xs key)
  | otherwise = getList xs key


-- | UJudgement : UDTTのジャッジメントの定義
data UJudgement =
  UJudgement TUEnv UD.Preterm UD.Preterm
    deriving (Eq, Show)

instance Typeset UJudgement where
  toTeX (UJudgement env preM preA) =
    toTeX UD.Judgment {UD.context = env, UD.term = preM, UD.typ = preA}

instance MathML UJudgement where
  toMathML (UJudgement env preM preA) =
    toMathML UD.Judgment {UD.context = env, UD.term = preM, UD.typ = preA}


-- | Judgement : DTTのジャッジメントの定義
data Judgement =
  Judgement TEnv DT.Preterm DT.Preterm
    deriving (Eq, Show)

instance Typeset Judgement where
  toTeX (Judgement env preM preA) =
    let uenv = map DT.toUDTT env
        preM' = DT.toUDTT preM
        preA' = DT.toUDTT preA in
    toTeX UD.Judgment {UD.context = uenv, UD.term = preM', UD.typ = preA'}

instance MathML Judgement where
  toMathML (Judgement env preM preA) =
    let uenv = map DT.toUDTT env
        preM' = DT.toUDTT preM
        preA' = DT.toUDTT preA in
    toMathML UD.Judgment {UD.context = uenv, UD.term = preM', UD.typ = preA'}

-- | 規則
data Label =
   CHK   -- (CHK) rule
 | CON   -- (CON) rule
 | VAR   -- (VAR) rule
 | TypeF  -- (typeF) rule
 | PiF    -- (Pi F) rule
 | PiI    -- (Pi I) rule
 | PiE    -- (Pi E) rule
 | SigF   -- (Sig F) rule
 | SigI   -- (Sig I) rule
 | SigE   -- (Sig E) rule
 | NotF   -- (Not F) rule
 | NotI   -- (Not I) rule
 | NotE   -- (Not E) rule
 | TopF   -- (TF) rule
 | TopI   -- (TI) rule
 | BotF   -- (Bot F) rule
 | DREL   -- (DRel) rule
 deriving (Eq)

instance Show Label where
  show label =
    case label of
     CHK   -> "CHK"
     CON   -> "CON"
     VAR   -> "VAR"
     TypeF  -> "typeF"
     PiF    -> "Pi F"
     PiI    -> "Pi I"
     PiE    -> "Pi E"
     SigF   -> "Sig F"
     SigI   -> "Sig I"
     SigE   -> "Sig E"
     NotF   -> "Not F"
     NotI   -> "Not I"
     NotE   -> "Not E"
     TopF   -> "TF"
     TopI   -> "TI"
     BotF   -> "Bot F"
     DREL   -> "DRel"

labelToMathML :: Label -> String
labelToMathML label =
    case label of
     PiF    -> "&Pi;F"
     PiI    -> "&Pi;I"
     PiE    -> "&Pi;E"
     SigF   -> "&Sigma;F"
     SigI   -> "&Sigma;I"
     SigE   -> "&Sigma;E"
     NotF   -> "<not/>F"
     NotI   -> "<not/>I"
     NotE   -> "<not/>E"
     TopF   -> "&top;F"
     TopI   -> "&top;I"
     BotF   -> "&bot;F"
     _ -> show label

labelToTeX :: Label -> String
labelToTeX label =
    case label of
     PiF    -> "\\Pi F"
     PiI    -> "\\Pi I"
     PiE    -> "\\Pi E"
     SigF   -> "\\Sigma F"
     SigI   -> "\\Sigma I"
     SigE   -> "\\Sigma E"
     NotF   -> "\\neg F"
     NotI   -> "\\neg I"
     NotE   -> "\\neg E"
     TopF   -> "\\top F"
     TopI   -> "\\top I"
     BotF   -> "\\bot F"
     _ -> show label

data ULabel = ASP' | L Label deriving (Eq) -- (@) rule

instance Show ULabel where
  show ulabel =
    case ulabel of
     ASP'     -> "@"
     L label    -> show label

ulabelToMathML :: ULabel -> String
ulabelToMathML ulabel =
    case ulabel of
     L label    -> labelToMathML label
     ASP' -> show ulabel

ulabelToTeX :: ULabel -> String
ulabelToTeX ulabel =
    case ulabel of
     L label    -> labelToTeX label
     ASP' -> show ulabel

-- | UTree  : UDTT用の木構造
data UTree  a = UT ULabel a [UTree  a] | UError' a T.Text deriving (Eq, Show)

-- | utreeToTeX : UDTTのTree用のtoTeX関数
utreeToTeX  :: (UTree  UJudgement) -> T.Text
utreeToTeX (UT label downside upside) =
  T.pack ("\\nd[(\\underline{"++ ulabelToTeX label ++ "})]{") `T.append` (toTeX downside) `T.append` T.pack "}{" `T.append`  (if null upside then T.pack "" else T.init $ T.concat$ map (\tree -> utreeToTeX tree `T.append`  T.pack "&") upside) `T.append` T.pack "}"
utreebaseToTex (UError' judgement msg) =
  T.pack "\\nd[(Error)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` msg `T.append` T.pack "}"

utreeToMathML :: (UTree  UJudgement) -> T.Text
utreeToMathML (UT label downside upside) =
  case length upside of
    0 ->
      T.concat [
        T.pack ("<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>("++ulabelToMathML label++")</p></mi><mfrac linethickness='2px'>"),
        T.pack "<mi color='White'> Empty </mi>",
        toMathML downside,
        T.pack "</mfrac></mrow>"
        ]
    1 ->
        T.concat [
        T.pack ("<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>("++ulabelToMathML label++")</p></mi><mfrac linethickness='2px'>"),
        utreeToMathML $head upside,
        toMathML downside,
        T.pack "</mi></mrow>"
        ]
    2 ->
        T.concat [
          T.pack ("<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>("++ ulabelToMathML label ++")</p></mi><mfrac linethickness='2px'>"),
          T.pack "<mrow>",
          utreeToMathML $head upside,
          T.pack " ",
          utreeToMathML $upside !! 1 ,
          T.pack "</mrow>",
          toMathML downside,
          T.pack "</mfrac></mrow>"
          ]
    _ ->
      T.pack ""
utreeToMathML (UError' judgement msg) =
   T.concat [
     T.pack "<mrow><mi fontsize='0.8'>(Error)</mi><mfrac linethickness='2px'>",
     msg,
     toMathML judgement,
     T.pack "</mfrac></mrow>"
     ]

-- | Tree : DTT用の木構造
data Tree a = T Label a [Tree a] | Error' a T.Text deriving (Eq, Show)

-- | treeToTeX : DTTのTree用のtoTeX関数
treeToTeX  :: (Tree Judgement) -> T.Text
treeToTeX (T label downside upside) =
  T.pack ("\\nd[("++ labelToTeX label ++ ")]{") `T.append` (toTeX downside) `T.append` T.pack "}{" `T.append`  (if null upside then T.pack "" else T.init $ T.concat$ map (\tree -> treeToTeX tree `T.append`  T.pack "&") upside) `T.append` T.pack "}"
treebaseToTex (Error' judgement msg) =
  T.pack "\\nd[(Error)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` msg `T.append` T.pack "}"

treeToMathML :: (Tree Judgement) -> T.Text
treeToMathML (T label downside upside) =
  case length upside of
    0 ->
      T.concat [
        T.pack ("<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>("++labelToMathML label++")</p></mi><mfrac linethickness='2px'>"),
        T.pack "<mi color='White'> Empty </mi>",
        toMathML downside,
        T.pack "</mfrac></mrow>"
        ]
    1 ->
        T.concat [
        T.pack ("<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>("++labelToMathML label++")</p></mi><mfrac linethickness='2px'>"),
        treeToMathML $head upside,
        toMathML downside,
        T.pack "</mi></mrow>"
        ]
    2 ->
        T.concat [
          T.pack ("<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>("++ labelToMathML label ++")</p></mi><mfrac linethickness='2px'>"),
          T.pack "<mrow>",
          treeToMathML $head upside,
          T.pack " ",
          treeToMathML $upside !! 1 ,
          T.pack "</mrow>",
          toMathML downside,
          T.pack "</mfrac></mrow>"
          ]
    _ ->
      T.pack ""
treeToMathML (Error' judgement msg) =
   T.concat [
     T.pack "<mrow><mi fontsize='0.8'>(Error)</mi><mfrac linethickness='2px'>",
     msg,
     toMathML judgement,
     T.pack "</mfrac></mrow>"
     ]

-- | getTypeU : UDTTの証明木の一番下のTypeを取り出す
getTypeU :: (UTree  UJudgement) -> [UD.Preterm]
getTypeU (UT ulabel (UJudgement env preM preA) upside) = [preA]
getTypeU (UError' (UJudgement env preM preA) text) = [preA]

-- | getTermU : UDTTの証明木の一番下のTermを取り出す
getTermU :: (UTree  UJudgement) -> [UD.Preterm]
getTermU (UT ulabel (UJudgement env preM preA) upside) = [preM]
getTermU (UError' (UJudgement env preM preA) text) = [preM]

-- | getType : DTTの証明木の一番下のTypeを取り出す
getType :: (Tree Judgement) -> [DT.Preterm]
getType (T label (Judgement env preM preA) upside) =  [preA]
getType (Error' (Judgement env preM preA) text) = [preA]

-- | getTerm : DTTの証明木の一番下のTermを取り出す
getTerm :: (Tree Judgement) -> [DT.Preterm]
getTerm (T label (Judgement env preM preA) upside) =  [preM]
getTerm (Error' (Judgement env preM preA) text) = [preM]

-- | printGammaU : UDTTの環境を出力する関数
printGammaU :: TUEnv -> T.Text
printGammaU [] = T.pack ""
printGammaU [x] = toTeX x
printGammaU (x:xs) = (toTeX x) `T.append` T.pack ", " `T.append` (printGammaU xs)

-- | printGamma : DTTの環境を出力する関数
printGamma :: TEnv -> T.Text
printGamma [] = T.pack ""
printGamma [x] = toTeX x
printGamma (x:xs) = (toTeX x) `T.append` T.pack ", " `T.append` (printGamma xs)
