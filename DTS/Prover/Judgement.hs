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
  utreeToMathML,
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
  toTeX (UJudgement env preM preA) = 
    toTeX UD.Judgment {UD.context = env, UD.term = preM, UD.typ = preA}

instance MathML UJudgement where
  toMathML (UJudgement env preM preA) = 
    toMathML UD.Judgment {UD.context = env, UD.term = preM, UD.typ = preA}


-- Judgement : DTTのジャッジメントの定義
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


-- UTree : UDTT用の木構造
data UTree a = 
   UCHK a (UTree a)                -- (CHK) rule
 | UCON a                          -- (CON) rule
 | UVAR a                          -- (VAR) rule
 | UTypeF a                        -- (typeF) rule
 | ASP a (UTree a) (UTree a)       -- (@) rule
 | UPiF a (UTree a) (UTree a)      -- (Pi F) rule
 | UPiI a (UTree a) (UTree a)      -- (Pi I) rule
 | UPiE a (UTree a) (UTree a)      -- (Pi E) rule
 | USigF a (UTree a) (UTree a)     -- (Sig F) rule
 | USigI a (UTree a) (UTree a)     -- (Sig I) rule
 | USigE a (UTree a)               -- (Sig E) rule
 | UNotF a (UTree a)               -- (Not F) rule
 | UNotI a (UTree a) (UTree a)     -- (Not I) rule
 | UNotE a (UTree a) (UTree a)     -- (Not E) rule
 | UTopF a                         -- (TF) rule
 | UTopI a                         -- (TI) rule
 | UBotF a                         -- (Bot F) rule
 | UDREL a                         -- (DRel) rule
 | UError a T.Text                 -- for debug
 deriving (Eq, Show)
--  UError a (UTree a) T.Text       -- for debug


-- utreeToTeX : UDTTのTree用のtoTeX関数
utreeToTeX :: (UTree UJudgement) -> T.Text
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
utreeToTeX (UNotF judgement overTree) = 
  T.pack "\\nd[(\\underline{\\neg F})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (utreeToTeX overTree) `T.append` T.pack "}"
utreeToTeX (UNotI judgement leftTree rightTree) = 
  T.pack "\\nd[(\\underline{\\neg I})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (utreeToTeX leftTree) `T.append` T.pack "&" `T.append` (utreeToTeX rightTree) `T.append` T.pack "}"
utreeToTeX (UNotE judgement leftTree rightTree) = 
  T.pack "\\nd[(\\underline{\\neg E})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (utreeToTeX leftTree) `T.append` T.pack "&" `T.append` (utreeToTeX rightTree) `T.append` T.pack "}"
utreeToTeX (UTopF judgement) = 
  T.pack "\\nd[(\\underline{\\top F})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
utreeToTeX (UTopI judgement) = 
  T.pack "\\nd[(\\underline{\\top I})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
utreeToTeX (UBotF judgement) = 
  T.pack "\\nd[(\\underline{\\bot F})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
utreeToTeX (UDREL judgement) = 
  T.pack "\\nd[(\\underline{DRel})]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
utreeToTeX (UError judgement text) = 
  T.pack "\\nd[(Error)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` text `T.append` T.pack "}"


-- utreeToMathML : UDTTのTree用のtoMathML関数
utreeToMathML :: (UTree UJudgement) -> T.Text
utreeToMathML (UCHK judgement overTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(CHK)</p></mi><mfrac linethickness='2px'>",
  utreeToMathML overTree,
  toMathML judgement,
  T.pack "</mi></mrow>"
  ]
utreeToMathML (UCON judgement) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(CON)</p></mi><mfrac linethickness='2px'>",
  T.pack "<mi color='White'> Empty </mi>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
utreeToMathML (UVAR judgement) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(VAR)</p></mi><mfrac linethickness='2px'>",
  T.pack "<mi color='White'> Empty </mi>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
utreeToMathML (UTypeF judgement) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(typeF)</p></mi><mfrac linethickness='2px'>",
  T.pack "<mi color='White'> Empty </mi>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
utreeToMathML (UPiF judgement leftTree rightTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(&Pi;F)</p></mi><mfrac linethickness='2px'>",
  T.pack "<mrow>",
  utreeToMathML leftTree,
  T.pack " ",
  utreeToMathML rightTree,
  T.pack "</mrow>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
utreeToMathML (UPiI judgement leftTree rightTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(&Pi;I)</p></mi><mfrac linethickness='2px'>",
  T.pack "<mrow>",
  utreeToMathML leftTree,
  T.pack " ",
  utreeToMathML rightTree,
  T.pack "</mrow>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
utreeToMathML (UPiE judgement leftTree rightTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(&Pi;E)</p></mi><mfrac linethickness='2px'>",
  T.pack "<mrow>",
  utreeToMathML leftTree,
  T.pack " ",
  utreeToMathML rightTree,
  T.pack "</mrow>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
utreeToMathML (USigF judgement leftTree rightTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(&Sigma;F)</p></mi><mfrac linethickness='2px'>",
  T.pack "<mrow>",
  utreeToMathML leftTree,
  T.pack " ",
  utreeToMathML rightTree,
  T.pack "</mrow>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
utreeToMathML (USigI judgement leftTree rightTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(&Sigma;I)</p></mi><mfrac linethickness='2px'>",
  T.pack "<mrow>",
  utreeToMathML leftTree,
  T.pack " ",
  utreeToMathML rightTree,
  T.pack "</mrow>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
utreeToMathML (USigE judgement overTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(&Sigma;E)</p></mi><mfrac linethickness='2px'>",
  utreeToMathML overTree,
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
utreeToMathML (UNotF judgement overTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(<not/>F)</p></mi><mfrac linethickness='2px'>",
  utreeToMathML overTree,
  toMathML judgement,
  T.pack "</mi></mrow>"
  ]
utreeToMathML (UNotI judgement leftTree rightTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(<not/>I)</p></mi><mfrac linethickness='2px'>",
  T.pack "<mrow>",
  utreeToMathML leftTree,
  T.pack " ",
  utreeToMathML rightTree,
  T.pack "</mrow>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
utreeToMathML (UNotE judgement leftTree rightTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(<not/>E)</p></mi><mfrac linethickness='2px'>",
  T.pack "<mrow>",
  utreeToMathML leftTree,
  T.pack " ",
  utreeToMathML rightTree,
  T.pack "</mrow>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
utreeToMathML (UTopF judgement) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(&top;F)</p></mi><mfrac linethickness='2px'>",
  T.pack "<mi color='White'> Empty </mi>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
utreeToMathML (UTopI judgement) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(&top;I)</p></mi><mfrac linethickness='2px'>",
  T.pack "<mi color='White'> Empty </mi>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
utreeToMathML (UBotF judgement) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(&bot;F)</p></mi><mfrac linethickness='2px'>",
  T.pack "<mi color='White'> Empty </mi>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
utreeToMathML (ASP judgement leftTree rightTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(@)</mi><mfrac linethickness='2px'>",
  T.pack "<mrow>",
  utreeToMathML leftTree,
  T.pack " ",
  utreeToMathML rightTree,
  T.pack "</mrow>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
utreeToMathML (UDREL judgement) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>(DRel)</p></mi><mfrac linethickness='2px'>",
  T.pack "<mi color='White'> Empty </mi>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
utreeToMathML (UError judgement text) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(Error)</mi><mfrac linethickness='2px'>",
  text,
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]




-- Tree : DTT用の木構造
data Tree a =
   CHK a (Tree a)
 | CON a
 | VAR a
 | TypeF a
 | PiF a (Tree a) (Tree a)
 | PiI a (Tree a) (Tree a)
 | PiE a (Tree a) (Tree a)
 | SigF a (Tree a) (Tree a)
 | SigI a (Tree a) (Tree a)
 | SigE a (Tree a)
 | NotF a (Tree a)
 | NotI a (Tree a) (Tree a)
 | NotE a (Tree a) (Tree a)
 | TopF a
 | TopI a
 | BotF a
 | DREL a
 | Error a T.Text
-- Error a (Tree a) T.Text
 deriving (Eq, Show)

-- treeToTeX : DTTのTree用のtoTeX関数
treeToTeX :: (Tree Judgement) -> T.Text
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
treeToTeX (NotF judgement overTree) = 
  T.pack "\\nd[(\\neg F)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (treeToTeX overTree) `T.append` T.pack "}"
treeToTeX (NotI judgement leftTree rightTree) = 
  T.pack "\\nd[(\\neg I)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (treeToTeX leftTree) `T.append` T.pack "&" `T.append` (treeToTeX rightTree) `T.append` T.pack "}"
treeToTeX (NotE judgement leftTree rightTree) = 
  T.pack "\\nd[(\\neg E)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` (treeToTeX leftTree) `T.append` T.pack "&" `T.append` (treeToTeX rightTree) `T.append` T.pack "}"
treeToTeX (TopF judgement) = 
  T.pack "\\nd[(\\top F)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
treeToTeX (TopI judgement) = 
  T.pack "\\nd[(\\top I)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
treeToTeX (BotF judgement) = 
  T.pack "\\nd[(\\bot F)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
treeToTeX (DREL judgement) = 
  T.pack "\\nd[(DRel)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{}"
treeToTeX (Error judgement text) = 
  T.pack "\\nd[(Error)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` text `T.append` T.pack "}"


-- treeToMathML : DTTのTree用のtoMathML関数
treeToMathML :: (Tree Judgement) -> T.Text
treeToMathML (CHK judgement overTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(CHK)</mi><mfrac linethickness='2px'>",
  treeToMathML overTree,
  toMathML judgement,
  T.pack "</mi></mrow>"
  ]
treeToMathML (CON judgement) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(CON)</mi><mfrac linethickness='2px'>",
  T.pack "<mi color='White'> Empty </mi>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
treeToMathML (VAR judgement) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(VAR)</mi><mfrac linethickness='2px'>",
  T.pack "<mi color='White'> Empty </mi>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
treeToMathML (TypeF judgement) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(typeF)</mi><mfrac linethickness='2px'>",
  T.pack "<mi color='White'> Empty </mi>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
treeToMathML (PiF judgement leftTree rightTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(&Pi;F)</mi><mfrac linethickness='2px'>",
  T.pack "<mrow>",
  treeToMathML leftTree,
  T.pack " ",
  treeToMathML rightTree,
  T.pack "</mrow>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
treeToMathML (PiI judgement leftTree rightTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(&Pi;I)</mi><mfrac linethickness='2px'>",
  T.pack "<mrow>",
  treeToMathML leftTree,
  T.pack " ",
  treeToMathML rightTree,
  T.pack "</mrow>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
treeToMathML (PiE judgement leftTree rightTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(&Pi;E)</mi><mfrac linethickness='2px'>",
  T.pack "<mrow>",
  treeToMathML leftTree,
  T.pack " ",
  treeToMathML rightTree,
  T.pack "</mrow>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
treeToMathML (SigF judgement leftTree rightTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(&Sigma;F)</mi><mfrac linethickness='2px'>",
  T.pack "<mrow>",
  treeToMathML leftTree,
  T.pack " ",
  treeToMathML rightTree,
  T.pack "</mrow>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
treeToMathML (SigI judgement leftTree rightTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(&Sigma;I)</mi><mfrac linethickness='2px'>",
  T.pack "<mrow>",
  treeToMathML leftTree,
  T.pack " ",
  treeToMathML rightTree,
  T.pack "</mrow>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
treeToMathML (SigE judgement overTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(&Sigma;E)</mi><mfrac linethickness='2px'>",
  treeToMathML overTree,
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
treeToMathML (NotF judgement overTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(<not/>F)</mi><mfrac linethickness='2px'>",
  treeToMathML overTree,
  toMathML judgement,
  T.pack "</mi></mrow>"
  ]
treeToMathML (NotI judgement leftTree rightTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(<not/>I)</mi><mfrac linethickness='2px'>",
  T.pack "<mrow>",
  treeToMathML leftTree,
  T.pack " ",
  treeToMathML rightTree,
  T.pack "</mrow>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
treeToMathML (NotE judgement leftTree rightTree) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(<not/>E)</mi><mfrac linethickness='2px'>",
  T.pack "<mrow>",
  treeToMathML leftTree,
  T.pack " ",
  treeToMathML rightTree,
  T.pack "</mrow>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
treeToMathML (TopF judgement) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(&top;F)</mi><mfrac linethickness='2px'>",
  T.pack "<mi color='White'> Empty </mi>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
treeToMathML (TopI judgement) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(&top;I)</mi><mfrac linethickness='2px'>",
  T.pack "<mi color='White'> Empty </mi>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
treeToMathML (BotF judgement) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(&bot;F)</mi><mfrac linethickness='2px'>",
  T.pack "<mi color='White'> Empty </mi>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
treeToMathML (DREL judgement) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(DRel)</mi><mfrac linethickness='2px'>",
  T.pack "<mi color='White'> Empty </mi>",
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]
treeToMathML (Error judgement text) = T.concat [
  T.pack "<mrow><mi fontsize='0.8'>(Error)</mi><mfrac linethickness='2px'>",
  text,
  toMathML judgement,
  T.pack "</mfrac></mrow>"
  ]


-- getTypeU : UDTTの証明木の一番下のTypeを取り出す
getTypeU :: (UTree UJudgement) -> [UD.Preterm]
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
getTypeU (UNotF (UJudgement env preP preS) over) = [preS]
getTypeU (UNotI (UJudgement env preP preS) left right) = [preS]
getTypeU (UNotE (UJudgement env preP preS) left right) = [preS]
getTypeU (UTopF (UJudgement env preTop preT)) = [preT]
getTypeU (UTopI (UJudgement env preUnit preTop)) = [preTop]
getTypeU (UBotF (UJudgement env preBot preT)) = [preT]
getTypeU (UDREL (UJudgement env preBot preT)) = [preT]
getTypeU (UError (UJudgement env preBot preT) text) = [preT]


-- getType : DTTの証明木の一番下のTypeを取り出す
getType :: (Tree Judgement) -> [DT.Preterm]
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
getType (NotF (Judgement env preP preS) over) = [preS]
getType (NotI (Judgement env preP preS) left right) = [preS]
getType (NotE (Judgement env preP preS) left right) = [preS]
getType (TopF (Judgement env preTop preT)) = [preT]
getType (TopI (Judgement env preUnit preTop)) = [preTop]
getType (BotF (Judgement env preBot preT)) = [preT]
getType (DREL (Judgement env preBot preT)) = [preT]
getType (Error (Judgement env preBot preT) text) = [preT]


-- getTermU : UDTTの証明木の一番下のTermを取り出す
getTermU :: (UTree UJudgement) -> [UD.Preterm]
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
getTermU (UNotF (UJudgement env preP preS) over) = [preP]
getTermU (UNotI (UJudgement env preP preS) left right) = [preP]
getTermU (UNotE (UJudgement env preP preS) left right) = [preP]
getTermU (UTopF (UJudgement env preTop preT)) = [preTop]
getTermU (UTopI (UJudgement env preUnit preTop)) = [preUnit]
getTermU (UBotF (UJudgement env preBot preT)) = [preBot]
getTermU (UDREL (UJudgement env preBot preT)) = [preBot]
getTermU (UError (UJudgement env preBot preT) text) = [preBot]


-- getTerm : DTTの証明木の一番下のTermを取り出す
getTerm :: (Tree Judgement) -> [DT.Preterm]
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
getTerm (NotF (Judgement env preP preS) over) = [preP]
getTerm (NotI (Judgement env preP preS) left right) = [preP]
getTerm (NotE (Judgement env preP preS) left right) = [preP]
getTerm (TopF (Judgement env preTop preT)) = [preTop]
getTerm (TopI (Judgement env preUnit preTop)) = [preUnit]
getTerm (BotF (Judgement env preBot preT)) = [preBot]
getTerm (DREL (Judgement env preBot preT)) = [preBot]
getTerm (Error (Judgement env preBot preT) text) = [preBot]


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