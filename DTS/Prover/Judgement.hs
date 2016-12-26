module DTS.Prover.Judgement
( Judgement(..),
  UJudgement(..),
  TEnv,
  TUEnv,
  SUEnv,
  Tree(..),
  UTree(..),
  getTerm,
  getType,
  getTypeU,
  getTermU,
  getList,
  utreeToTeX,
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

-- SUEnv : UDTTのシグネチャの型
type SUEnv = [UD.Signature]

-- getList : 環境 env の中で変数 var の値をすべて返す
getList :: (Eq k, Show k) => [(k, v)] -> k -> [v]
getList [] key = []
getList ((k,v):xs) key
  | key == k  = v:(getList xs key)
  | otherwise = getList xs key

-- -- UJudgement : UDTTのジャッジメントの定義
data UJudgement =
  UJudgement TUEnv UD.Preterm UD.Preterm
    deriving (Eq, Show)

instance Typeset UJudgement where
  toTeX (UJudgement env preM preA) = (printGammaU env) `T.append` T.pack "\\vdash" `T.append` (toTeX preM) `T.append` T.pack " : " `T.append` (toTeX preA)

instance MathML UJudgement where
  toMathML (UJudgement env preM preA) = T.pack "これから"


-- Judgement : DTTのジャッジメントの定義
data Judgement =
  Judgement TEnv DT.Preterm DT.Preterm
    deriving (Eq, Show)

instance Typeset Judgement where
  toTeX (Judgement env preM preA) = (printGamma env) `T.append` T.pack "\\vdash" `T.append` (toTeX preM) `T.append` T.pack " : " `T.append` (toTeX preA)

instance MathML Judgement where
  toMathML (Judgement env preM preA) = T.pack "これから"


-- UTree : UDTT用の木構造
data UTree a = 
   UEmpty
 | UCHK a (UTree a)
 | UCON a
 | UVAR a
 | UTypeF a
 | ASP a (UTree a) (UTree a)
 | UPiF a (UTree a) (UTree a)
 | UPiI a (UTree a) (UTree a)
 | UPiE a (UTree a) (UTree a)
 | USigF a (UTree a) (UTree a)
 | USigI a (UTree a) (UTree a)
 | USigE a (UTree a)
 | UTopF a
 | UTopI a
 | UBotF a
   deriving (Eq, Show)

-- utreeToTeX : UDTTのTree用のtoTeX関数
utreeToTeX :: (UTree UJudgement) -> T.Text
utreeToTeX UEmpty = T.pack ""
utreeToTeX (UCHK judgement overTree) = 
  T.pack "\\nd[(\\underline{CHK})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (utreeToTeX overTree) `T.append` T.pack "}"
utreeToTeX (UCON judgement) = 
  T.pack "\\nd[(\\underline{CON})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
utreeToTeX (UVAR judgement) = 
  T.pack "\\nd[(\\underline{VAR})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
utreeToTeX (UTypeF judgement) = 
  T.pack "\\nd[(\\underline{typeF})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
utreeToTeX (ASP judgement leftTree rightTree) = 
  T.pack "\\nd[(@)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (utreeToTeX leftTree) `T.append` T.pack "&" `T.append` (utreeToTeX rightTree) `T.append` T.pack "}"
utreeToTeX (UPiF judgement leftTree rightTree) = 
  T.pack "\\nd[(\\underline{\\Pi F})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (utreeToTeX leftTree) `T.append` T.pack "&" `T.append` (utreeToTeX rightTree) `T.append` T.pack "}"
utreeToTeX (UPiI judgement leftTree rightTree) = 
  T.pack "\\nd[(\\underline{\\Pi I})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (utreeToTeX leftTree) `T.append` T.pack "&" `T.append` (utreeToTeX rightTree) `T.append` T.pack "}"
utreeToTeX (UPiE judgement leftTree rightTree) = 
  T.pack "\\nd[(\\underline{\\Pi E})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (utreeToTeX leftTree) `T.append` T.pack "&" `T.append` (utreeToTeX rightTree) `T.append` T.pack "}"
utreeToTeX (USigF judgement leftTree rightTree) = 
  T.pack "\\nd[(\\underline{\\Sigma F})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (utreeToTeX leftTree) `T.append` T.pack "&" `T.append` (utreeToTeX rightTree) `T.append` T.pack "}"
utreeToTeX (USigI judgement leftTree rightTree) = 
  T.pack "\\nd[(\\underline{\\Sigma I})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (utreeToTeX leftTree) `T.append` T.pack "&" `T.append` (utreeToTeX rightTree) `T.append` T.pack "}"
utreeToTeX (USigE judgement overTree) = 
  T.pack "\\nd[(\\underline{\\Sigma E})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (utreeToTeX overTree) `T.append` T.pack "}"
utreeToTeX (UTopF judgement) = 
  T.pack "\\nd[(\\underline{\\top F})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
utreeToTeX (UTopI judgement) = 
  T.pack "\\nd[(\\underline{\\top I})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
utreeToTeX (UBotF judgement) = 
  T.pack "\\nd[(\\underline{\\bot F})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"

-- Typesetのinstanceにはできなさそう。
-- おそらく (UTree a) の a が悪い?
{-
instance Typeset (UTree UJudgement) where
  toTeX UEmpty = T.pack ""
  toTeX (UCHK judgement overTree) = toTeX judgement
    (T.pack "\\nd[(\\underline{CHK})]{") `T.append` (toTeX judgement) `T.append` (T.pack "}{") `T.append` (toTeX overTree) `T.append` (T.pack "}")
-}

-- Tree : DTT用の木構造
data Tree a =
   Empty
 | CHK a (Tree a)
 | CON a
 | VAR a
 | TypeF a
 | PiF a (Tree a) (Tree a)
 | PiI a (Tree a) (Tree a)
 | PiE a (Tree a) (Tree a)
 | SigF a (Tree a) (Tree a)
 | SigI a (Tree a) (Tree a)
 | SigE a (Tree a)
 | TopF a
 | TopI a
 | BotF a
   deriving (Eq, Show)

-- treeToTeX : DTTのTree用のtoTeX関数
treeToTeX :: (Tree Judgement) -> T.Text
treeToTeX Empty = T.pack ""
treeToTeX (CHK judgement overTree) = 
  T.pack "\\nd[(CHK)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (treeToTeX overTree) `T.append` T.pack "}"
treeToTeX (CON judgement) = 
  T.pack "\\nd[(CON)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
treeToTeX (VAR judgement) = 
  T.pack "\\nd[(VAR)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
treeToTeX (TypeF judgement) = 
  T.pack "\\nd[(typeF)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
treeToTeX (PiF judgement leftTree rightTree) = 
  T.pack "\\nd[(\\Pi F)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (treeToTeX leftTree) `T.append` T.pack "&" `T.append` (treeToTeX rightTree) `T.append` T.pack "}"
treeToTeX (PiI judgement leftTree rightTree) = 
  T.pack "\\nd[(\\Pi I)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (treeToTeX leftTree) `T.append` T.pack "&" `T.append` (treeToTeX rightTree) `T.append` T.pack "}"
treeToTeX (PiE judgement leftTree rightTree) = 
  T.pack "\\nd[(\\Pi E)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (treeToTeX leftTree) `T.append` T.pack "&" `T.append` (treeToTeX rightTree) `T.append` T.pack "}"
treeToTeX (SigF judgement leftTree rightTree) = 
  T.pack "\\nd[(\\Sigma F)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (treeToTeX leftTree) `T.append` T.pack "&" `T.append` (treeToTeX rightTree) `T.append` T.pack "}"
treeToTeX (SigI judgement leftTree rightTree) = 
  T.pack "\\nd[(\\Sigma I)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (treeToTeX leftTree) `T.append` T.pack "&" `T.append` (treeToTeX rightTree) `T.append` T.pack "}"
treeToTeX (SigE judgement overTree) = 
  T.pack "\\nd[(\\Sigma E)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (treeToTeX overTree) `T.append` T.pack "}"
treeToTeX (TopF judgement) = 
  T.pack "\\nd[(\\top F)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
treeToTeX (TopI judgement) = 
  T.pack "\\nd[(\\top I)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
treeToTeX (BotF judgement) = 
  T.pack "\\nd[(\\bot F)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"



-- getTypeU : UDTTの証明木の一番下のTypeを取り出す
getTypeU :: (UTree UJudgement) -> [UD.Preterm]
getTypeU UEmpty = []
getTypeU (UCHK (UJudgement env preM preA) over) = [preA]
getTypeU (UCON (UJudgement env preCON preA)) = [preA]
getTypeU (UVAR (UJudgement env preVAR preM)) = [preM]
getTypeU (ASP (UJudgement env preAsp preA) left right) = [preA]
getTypeU (UTypeF (UJudgement env preT preK)) = [preK]
getTypeU (UPiF (UJudgement env preP preS) left right) = [preS]
getTypeU (UPiI (UJudgement env preL preP) left right) = [preP]
getTypeU (UPiE (UJudgement env preMN preB) left right) = [preB]
getTypeU (USigF (UJudgement env preP preS) left right) = [preS]
getTypeU (USigI (UJudgement env preP preS) left right) = [preS]
getTypeU (USigE (UJudgement env preP preS) over) = [preS]
getTypeU (UTopF (UJudgement env preTop preT)) = [preT]
getTypeU (UTopI (UJudgement env preUnit preTop)) = [preTop]
getTypeU (UBotF (UJudgement env preBot preT)) = [preT]


-- getTypeU : UDTTの証明木の一番下のTypeを取り出す
getType :: (Tree Judgement) -> [DT.Preterm]
getType Empty = []
getType (CHK (Judgement env preM preA) over) = [preA]
getType (CON (Judgement env preCON preA)) = [preA]
getType (VAR (Judgement env preVAR preM)) = [preM]
getType (TypeF (Judgement env preT preK)) = [preK]
getType (PiF (Judgement env preP preS) left right) = [preS]
getType (PiI (Judgement env preL preP) left right) = [preP]
getType (PiE (Judgement env preMN preB) left right) = [preB]
getType (SigF (Judgement env preP preS) left right) = [preS]
getType (SigI (Judgement env preP preS) left right) = [preS]
getType (SigE (Judgement env preP preS) over) = [preS]
getType (TopF (Judgement env preTop preT)) = [preT]
getType (TopI (Judgement env preUnit preTop)) = [preTop]
getType (BotF (Judgement env preBot preT)) = [preT]


-- getTermU : UDTTの証明木の一番下のTermを取り出す
getTermU :: (UTree UJudgement) -> [UD.Preterm]
getTermU UEmpty = []
getTermU (UCHK (UJudgement env preM preA) over) = [preM]
getTermU (UCON (UJudgement env preCON preA)) = [preCON]
getTermU (UVAR (UJudgement env preVAR preM)) = [preVAR]
getTermU (ASP (UJudgement env preAsp preA) left right) = [preAsp]
getTermU (UTypeF (UJudgement env preT preK)) = [preT]
getTermU (UPiF (UJudgement env preP preS) left right) = [preP]
getTermU (UPiI (UJudgement env preL preP) left right) = [preL]
getTermU (UPiE (UJudgement env preMN preB) left right) = [preMN]
getTermU (USigF (UJudgement env preP preS) left right) = [preP]
getTermU (USigI (UJudgement env preP preS) left right) = [preP]
getTermU (USigE (UJudgement env preP preS) over) = [preP]
getTermU (UTopF (UJudgement env preTop preT)) = [preTop]
getTermU (UTopI (UJudgement env preUnit preTop)) = [preUnit]
getTermU (UBotF (UJudgement env preBot preT)) = [preBot]


-- getTerm : DTTの証明木の一番下のTermを取り出す
getTerm :: (Tree Judgement) -> [DT.Preterm]
getTerm Empty = []
getTerm (CHK (Judgement env preM preA) over) = [preM]
getTerm (CON (Judgement env preCON preA)) = [preCON]
getTerm (VAR (Judgement env preVAR preM)) = [preVAR]
getTerm (TypeF (Judgement env preT preK)) = [preT]
getTerm (PiF (Judgement env preP preS) left right) = [preP]
getTerm (PiI (Judgement env preL preP) left right) = [preL]
getTerm (PiE (Judgement env preMN preB) left right) = [preMN]
getTerm (SigF (Judgement env preP preS) left right) = [preP]
getTerm (SigI (Judgement env preP preS) left right) = [preP]
getTerm (SigE (Judgement env preP preS) over) = [preP]
getTerm (TopF (Judgement env preTop preT)) = [preTop]
getTerm (TopI (Judgement env preUnit preTop)) = [preUnit]
getTerm (BotF (Judgement env preBot preT)) = [preBot]


-- printGammaU : UDTTの環境を出力する関数
printGammaU :: TUEnv -> T.Text
printGammaU [] = T.pack ""
printGammaU [x] = toTeX x
printGammaU (x:xs) = (toTeX x) `T.append` T.pack ", " `T.append` (printGammaU xs)

-- printGamma : DTTの環境を出力する関数
printGamma :: TEnv -> T.Text
printGamma [] = T.pack ""
printGamma [x] = toTeX x
printGamma (x:xs) = (toTeX x) `T.append` T.pack ", " `T.append` (printGamma xs)