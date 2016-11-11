{-|
Module      : TypeChecker
Description : A Typechecker for DTS
Copyright   : Miho Sato
Licence     : All right reserved
Maintainer  : Miho Sato <satoh.miho@is.ocha.ac.jp>
Stability   : beta
-}
module DTS.Prover.TypeChecker 
( aspElim,
  typeCheckU,
  typeInferU
) where

{-
できたら公開する
aspElim,
  typeCheckU,
  typeInferU,
  proofSearch
-}

import qualified DTS.UDTT as UD           -- UDTT
import qualified DTS.DTT as DT            -- DTT
import qualified Data.Text.Lazy as T      -- text
import qualified Data.List as L           -- base
import qualified Control.Applicative as M -- base
import qualified Control.Monad as M       -- base
import qualified DTS.UDTTwithName as VN
import Interface.Text
import Interface.TeX


-- TUEnv : UDTTの型環境の型
type TUEnv = [UD.Preterm]

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


-- @-Elimination
aspElim :: (UTree UJudgement) -> [UTree UJudgement]
aspElim tree = [tree]


-- typeCheckU : UDTTの型チェック
typeCheckU :: TUEnv -> SUEnv -> UD.Preterm -> UD.Preterm -> [UTree UJudgement]
-- (ΠI) rule
typeCheckU typeEnv sig (UD.Lam preM) (UD.Pi preA preB) = do
  leftTree <- (typeCheckU typeEnv sig preA (UD.Type))
                ++ (typeCheckU typeEnv sig preA (UD.Kind))
  newleftTree <- aspElim leftTree
  newA <- getTermU newleftTree
  let preA' = UD.betaReduce newA
  rightTree <- typeCheckU (preA':typeEnv) sig preM preB
  return (UPiI (UJudgement typeEnv (UD.Lam preM) (UD.Pi preA preB)) leftTree rightTree)
-- (ΣI) rule
typeCheckU typeEnv sig (UD.Pair preM preN) (UD.Sigma preA preB) = do
  leftTree <- typeCheckU typeEnv sig preM preA
  newleftTree <- aspElim leftTree
  newM <- getTermU newleftTree
  let preB' = UD.subst preB (UD.betaReduce newM) 0
  rightTree <- typeCheckU typeEnv sig preN preB'
  return (USigI (UJudgement typeEnv (UD.Pair preM preN) (UD.Sigma preA preB)) leftTree rightTree)
-- (CHK) rule
typeCheckU typeEnv sig preE value = do
  overTree <- typeInferU typeEnv sig preE
  resultType <- getTypeU overTree
  M.guard (value == resultType)
  return (UCHK (UJudgement typeEnv preE value) overTree)


-- typeInferU : UDTTの型推論
typeInferU :: TUEnv -> SUEnv -> UD.Preterm -> [UTree UJudgement]
-- (typeF) rule
typeInferU typeEnv _ UD.Type = do
  return (UTypeF (UJudgement typeEnv UD.Type UD.Kind))
-- (VAR) rule
typeInferU typeEnv _ (UD.Var k) = do
  let varType = typeEnv !! k
  return (UVAR (UJudgement typeEnv (UD.Var k) varType))
-- (CON) rule
typeInferU typeEnv sig (UD.Con text) = do
  conType <- getList sig text
  return (UCON (UJudgement typeEnv (UD.Con text) conType))
-- (TopF) rule
typeInferU typeEnv _ UD.Top = do
  return (UTopF (UJudgement typeEnv UD.Top UD.Type))
-- (TopI) rule
typeInferU typeEnv _ UD.Unit = do
  return (UTopI (UJudgement typeEnv UD.Unit UD.Top))
-- (BotF) rule
typeInferU typeEnv _ UD.Bot = do
  return (UBotF (UJudgement typeEnv UD.Bot UD.Type))
-- (ΠF) rule
typeInferU typeEnv sig (UD.Pi preA preB) = do
  leftTree <- (typeCheckU typeEnv sig preA UD.Type)
                ++ (typeCheckU typeEnv sig preA UD.Kind)
  newleftTree <- aspElim leftTree
  newA <- getTermU newleftTree
  let preA' = UD.betaReduce newA
  rightTree <- (typeCheckU (preA':typeEnv) sig preB UD.Type)
                 ++ (typeCheckU (preA':typeEnv) sig preB UD.Kind)
  ansType <- getTypeU rightTree
  return (UPiF (UJudgement typeEnv (UD.Pi preA preB) ansType) leftTree rightTree)
-- (ΠE) rule
typeInferU typeEnv sig (UD.App preM preN) = do
  leftTree <- typeInferU typeEnv sig preM
  funcType <- getTypeU leftTree
  case funcType of
    (UD.Pi preA preB) -> do
       rightTree <- typeCheckU typeEnv sig preN preA
       let preB' = UD.subst preB preN 0
       return (UPiE (UJudgement typeEnv (UD.App preM preN) preB') leftTree rightTree)
    otherwise -> []
-- (ΣF) rule
typeInferU typeEnv sig (UD.Sigma preA preB) = do
  leftTree <- (typeCheckU typeEnv sig preA UD.Type)
                ++ (typeCheckU typeEnv sig preA UD.Kind)
  newleftTree <- aspElim leftTree
  newA <- getTermU newleftTree
  let preA' = UD.betaReduce newA
  rightTree <- (typeCheckU (preA':typeEnv) sig preB UD.Type)
                 ++ (typeCheckU (preA':typeEnv) sig preB UD.Kind)
  ansType <- getTypeU rightTree
  return (USigF (UJudgement typeEnv (UD.Sigma preA preB) ansType) leftTree rightTree)
-- (ΣE) rule
typeInferU typeEnv sig (UD.Proj selector preM) =
  if selector == UD.Fst
  then do overTree <- typeInferU typeEnv sig preM
          bodyType <- getTypeU overTree
          case bodyType of
            (UD.Sigma preA preB) ->
               return (USigE (UJudgement typeEnv (UD.Proj UD.Fst preM) preA) overTree)
            otherwise -> []
  else do overTree <- typeInferU typeEnv sig preM
          bodyType <- getTypeU overTree
          case bodyType of
            (UD.Sigma preA preB) ->
               return (USigE (UJudgement typeEnv (UD.Proj UD.Snd preM) preA) overTree)
            otherwise -> []
-- (Asp) rule
typeInferU _ _ _ = []