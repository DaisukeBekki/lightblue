{-# LANGUAGE DoAndIfThenElse #-}

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
  typeInferU,
  proofSearch,
  dismantle,
  dismantleSig,
  changeSig,
  execute
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
import DTS.Prover.Judgement





-- transP : UDTTの項をDTTの項に変換する関数
-- (Asp)は変換先がないので、[]が返る(?)
transP :: UD.Preterm -> [DT.Preterm]
transP (UD.Var k) = [DT.Var k]
transP (UD.Con text) = [DT.Con text]
transP (UD.Lam preM) = do
  preterm <- transP preM
  return (DT.Lam preterm)
transP (UD.App preM preN) = do
  pretermM <- transP preM
  pretermN <- transP preN
  return (DT.App pretermM pretermN)
transP (UD.Pair preM preN) = do
  pretermM <- transP preM
  pretermN <- transP preN
  return (DT.Pair pretermM pretermN)
transP (UD.Proj selector preM) = do
  preterm <- transP preM
  if selector == UD.Fst
    then do return (DT.Proj DT.Fst preterm)
    else do return (DT.Proj DT.Snd preterm)
transP UD.Type = [DT.Type]
transP UD.Kind = [DT.Kind]
transP UD.Top = [DT.Top]
transP UD.Bot = [DT.Bot]
transP UD.Unit = [DT.Unit]
transP (UD.Pi preA preB) = do
  pretermA <- transP preA
  pretermB <- transP preB
  return (DT.Pi pretermA pretermB)
transP (UD.Sigma preA preB) = do
  pretermA <- transP preA
  pretermB <- transP preB
  return (DT.Sigma pretermA pretermB)
transP (UD.Not preM) = 
  transP (UD.Pi preM UD.Bot)
transP (UD.Asp n preM) = []
transP (UD.DRel i t preM preN) = do
  pretermA <- transP preM
  pretermB <- transP preN
  return (DT.DRel i t pretermA pretermB)
transP preterm = []


-- repositP : DTTの項をUDTTの項に変換する関数
repositP :: DT.Preterm -> UD.Preterm
repositP = DT.toUDTT


-- transE : TUEnv(UDTTの環境)をTEnv(DTTの環境)に変換する関数
-- 内部でtransPを使う
transE :: TUEnv -> TEnv
transE [] = []
transE (udtt:rest) = 
  case transP udtt of
    [] -> (DT.Con (T.pack "miss")):(transE rest)
    term -> term ++ (transE rest)

-- @-Elimination
aspElim :: (UTree UJudgement) -> [Tree Judgement]
-- (@)規則
aspElim (ASP (UJudgement uenv (UD.Asp n preA) preA2) left right) = 
  aspElim right
-- (Type)規則
aspElim (UTypeF (UJudgement uenv UD.Type UD.Kind)) = do
  return (TypeF (Judgement (transE uenv) DT.Type DT.Kind))
-- (CON)規則 ※ transP preA の部分が悩ましい
aspElim (UCON (UJudgement uenv (UD.Con text) preA)) = do
  preA' <- transP preA
  return (CON (Judgement (transE uenv) (DT.Con text) preA'))
-- (VAR)規則 ※ transP preM の部分が悩ましい
aspElim (UVAR (UJudgement uenv preV preM)) = do
  preM' <- transP preM
  preV' <- transP preV
  return (VAR (Judgement (transE uenv) preV' preM'))
-- (TopF)規則
aspElim (UTopF (UJudgement uenv UD.Top UD.Type)) = do
  return (TopF (Judgement (transE uenv) DT.Top DT.Type))
-- (TopI)規則
aspElim (UTopI (UJudgement uenv UD.Unit UD.Top)) = do
  return (TopI (Judgement (transE uenv) DT.Unit DT.Top))
-- (BotF)規則
aspElim (UBotF (UJudgement uenv UD.Bot UD.Type)) = do
  return (BotF (Judgement (transE uenv) DT.Bot DT.Type))
-- (ΠF)規則
aspElim (UPiF (UJudgement uenv (UD.Pi preA preB) sort) left right) = do
  resultL <- aspElim left
  resultR <- aspElim right
  preA' <- getTerm resultL
  preB' <- getTerm resultR
  sort2 <- getType resultR
  return (PiF (Judgement (transE uenv) (DT.Pi preA' preB') sort2) resultL resultR)
-- (ΠI)規則
aspElim (UPiI (UJudgement uenv (UD.Lam preM) (UD.Pi preA preB)) left right) = do
  resultL <- aspElim left
  resultR <- aspElim right
  preA' <- getTerm resultL
  preM' <- getTerm resultR
  preB' <- getType resultR
  return (PiI (Judgement (transE uenv) (DT.Lam preM') (DT.Pi preA' preB')) resultL resultR)
-- (ΠE)規則 ※ 最後のB''を返すところ??? 計算して返すのだろうか??
aspElim (UPiE (UJudgement uenv (UD.App preM preN) preterm) left right) = do
  resultL <- aspElim left
  resultR <- aspElim right
  preM' <- getTerm resultL
  preN' <- getTerm resultR
  pre' <- transP preterm
  return (PiE (Judgement (transE uenv) (DT.App preM' preN') pre') resultL resultR)
-- (ΣF)規則
aspElim (USigF (UJudgement uenv (UD.Sigma preA preB) sort) left right) = do
  resultL <- aspElim left
  resultR <- aspElim right
  preA' <- getTerm resultL
  preB' <- getTerm resultR
  sort2 <- getType resultR
  return (SigF (Judgement (transE uenv) (DT.Sigma preA' preB') sort2) resultL resultR)
-- (ΣI)規則 ※ 最後の、B''を返すところ???
aspElim (USigI (UJudgement uenv (UD.Pair preM preN) (UD.Sigma preA preB)) left right) = do
  resultL <- aspElim left
  resultR <- aspElim right
  preM' <- getTerm resultL
  preN' <- getTerm resultR
  preA' <- getType resultL
  preB' <- getType resultR
  return (SigI (Judgement (transE uenv) (DT.Pair preM' preN') (DT.Sigma preA' preB')) resultL resultR)
-- (ΣE)規則
aspElim (USigE (UJudgement uenv (UD.Proj selector preM) preA) over) = do
  case selector of
    UD.Fst -> do
      resultO <- aspElim over
      preM' <- getTerm resultO
      typeS <- getType resultO
      case typeS of
        DT.Sigma preA' preB' -> return (SigE (Judgement (transE uenv) (DT.Proj DT.Fst preM') (preA')) resultO)
        otherwise -> []
    UD.Snd -> do
      resultO <- aspElim over
      preM' <- getTerm resultO
      typeS <- getType resultO
      case typeS of
        DT.Sigma preA' preB' -> return (SigE (Judgement (transE uenv) (DT.Proj DT.Snd preM') (preB')) resultO)
        otherwise -> []
-- (CHK)規則
aspElim (UCHK (UJudgement uenv preM preA) over) = do
  resultO <- aspElim over
  preM' <- getTerm resultO
  preA' <- getType resultO
  return (CHK (Judgement (transE uenv) preM' preA') resultO)
-- (DRel)規則
aspElim (UDREL (UJudgement uenv (UD.DRel i t preM preN) typ)) = do
  preM' <- transP preM
  preN' <- transP preN
  typ' <- transP typ
  return (DREL (Judgement (transE uenv) (DT.DRel i t preM' preN') typ'))
-- (Error)
aspElim (UError (UJudgement uenv pretermA pretermB) text) = []
{-
aspElim (UError (UJudgement uenv pretermA pretermB) text) = do
  preA' <- transP pretermA
  preB' <- transP pretermB
  return (Error (Judgement (transE uenv) preA' preB') text)
-}
{-
--  resultO <- aspElim over
  preA' <- transP pretermA
  preB' <- transP pretermB
  let tree = CON (Judgement [] (DT.Con $ T.pack "error") (DT.Con $ T.pack "error"))
  return (Error (Judgement (transE uenv) preA' preB') tree text)
-}
{-
aspElim (UError (UJudgement uenv pretermA pretermB) over text) = do
  resultO <- aspElim over
  preA' <- transP pretermA
  preB' <- transP pretermB
  return (Error (Judgement (transE uenv) preA' preB') resultO text)
-}  
--aspElim tree = [tree]



-- typeCheckU : UDTTの型チェック
typeCheckU :: TUEnv -> SUEnv -> UD.Preterm -> UD.Preterm -> [UTree UJudgement]
-- (ΠI) rule
typeCheckU typeEnv sig (UD.Lam preM) (UD.Pi preA preB) = do
  leftTree <- (typeCheckU typeEnv sig preA (UD.Type))
                ++ (typeCheckU typeEnv sig preA (UD.Kind))
  newleftTree <- aspElim leftTree
  newA <- getTerm newleftTree
  let preA' = UD.betaReduce $ repositP newA
  rightTree <- typeCheckU (preA':typeEnv) sig preM preB
  return (UPiI (UJudgement typeEnv (UD.Lam preM) (UD.Pi preA preB)) leftTree rightTree)
-- (ΣI) rule
typeCheckU typeEnv sig (UD.Pair preM preN) (UD.Sigma preA preB) = do
  leftTree <- typeCheckU typeEnv sig preM preA
  newleftTree <- aspElim leftTree
  newM <- getTerm newleftTree
  let preB' = UD.subst preB (UD.betaReduce $ repositP newM) 0
  rightTree <- typeCheckU typeEnv sig preN preB'
  return (USigI (UJudgement typeEnv (UD.Pair preM preN) (UD.Sigma preA preB)) leftTree rightTree)
-- (CHK) rule
typeCheckU typeEnv sig preE value = do
  overTree <- typeInferU typeEnv sig preE
  resultType <- getTypeU overTree
  if value == resultType
    then do return (UCHK (UJudgement typeEnv preE value) overTree)
    else if value == UD.Kind
      then []
      else do return (UError (UJudgement typeEnv value resultType) (T.pack "does not match type")) 
--    else do return (UError (UJudgement typeEnv value resultType) (T.pack "does not match type"))
{-
typeCheckU typeEnv sig preE value = do
  overTree <- typeInferU typeEnv sig preE
  resultType <- getTypeU overTree
  M.guard (value == resultType)
  return (UCHK (UJudgement typeEnv preE value) overTree)
-}


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
typeInferU typeEnv sig (UD.Con text) = 
  let conTypes = getList sig text in
  if conTypes == []
  then do return (UCON (UJudgement typeEnv (UD.Con text) (UD.Con $ T.pack "is not exist.")))
  else do conType <- conTypes
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
  newA <- getTerm newleftTree
  let preA' = UD.betaReduce $ repositP newA
  rightTree <- (typeCheckU (preA':typeEnv) sig preB UD.Type)
                 ++ (typeCheckU (preA':typeEnv) sig preB UD.Kind)
  ansType <- getTypeU rightTree
  return (UPiF (UJudgement typeEnv (UD.Pi preA preB) ansType) leftTree rightTree)
-- (ΠF) rule (Not の場合)
typeInferU typeEnv sig (UD.Not preM) = 
  typeInferU typeEnv sig (UD.Pi preM UD.Bot)
-- (ΠE) rule
typeInferU typeEnv sig (UD.App preM preN) = do
  leftTree <- typeInferU typeEnv sig preM
  funcType <- getTypeU leftTree
  case funcType of
    (UD.Pi preA preB) -> do
       rightTree <- typeCheckU typeEnv sig preN preA
       let preB' = UD.betaReduce $ UD.shiftIndices (UD.subst preB (UD.shiftIndices preN 1 0) 0) (-1) 0
       return (UPiE (UJudgement typeEnv (UD.App preM preN) preB') leftTree rightTree)
    otherwise -> do return (UError (UJudgement typeEnv (UD.App preM preN) (UD.Con $ T.pack "???")) (T.pack "Not a function"))
-- (ΣF) rule
typeInferU typeEnv sig (UD.Sigma preA preB) = do
  leftTree <- (typeCheckU typeEnv sig preA UD.Type)
                ++ (typeCheckU typeEnv sig preA UD.Kind)
  newleftTree <- aspElim leftTree
  newA <- getTerm newleftTree
  let preA' = UD.betaReduce $ repositP newA
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
            otherwise -> do return (UError (UJudgement typeEnv (UD.Proj UD.Fst preM) (UD.Con $ T.pack "???")) (T.pack "Not a Sigma type"))
  else do overTree <- typeInferU typeEnv sig preM
          bodyType <- getTypeU overTree
          case bodyType of
            (UD.Sigma preA preB) -> do
               let preB' = UD.betaReduce $ UD.shiftIndices (UD.subst preB (UD.shiftIndices (UD.Proj UD.Fst preM) 1 0) 0) (-1) 0
               return (USigE (UJudgement typeEnv (UD.Proj UD.Snd preM) preB') overTree)
            otherwise -> do return (UError (UJudgement typeEnv (UD.Proj UD.Snd preM) (UD.Con $ T.pack "???")) (T.pack "Not a Sigma type"))
-- UD.subst preB (UD.Proj UD.Fst preM) 0
-- (Asp) rule
typeInferU typeEnv sig (UD.Asp i preA) = do
  leftTree <- (typeCheckU typeEnv sig preA UD.Type)
                ++ (typeCheckU typeEnv sig preA UD.Kind)
  newleftTree <- aspElim leftTree
  newA <- getTerm newleftTree
  let preA' = UD.betaReduce $ repositP newA
  ansTree <- proofSearch typeEnv sig preA'
  return (ASP (UJudgement typeEnv (UD.Asp i preA) preA) leftTree ansTree)
-- (DRel) rule
typeInferU typeEnv _ (UD.DRel i t preM preN) = do
  return (UDREL (UJudgement typeEnv (UD.DRel i t preM preN) UD.Type))


-- Proof Search
proofSearch :: TUEnv -> SUEnv -> UD.Preterm -> [UTree UJudgement]
proofSearch typeEnv sig preterm = do
  let candidatesA = (dismantle typeEnv typeEnv [])
                       ++ (dismantleSig (changeSig sig []) []) ++ [(UD.Unit, UD.Top)]
  let candidatesB = execute (changeSig sig []) candidatesA []
  let candidatesC = execute (changeTenv typeEnv typeEnv []) candidatesA []
  let candidates = candidatesA ++ candidatesB ++ candidatesC
  let ansTerms = searchType candidates preterm
  if ansTerms == []
    then do let envs = show candidates
            return (UError (UJudgement typeEnv (UD.Con $ T.pack "???") preterm) (T.pack "fail: proofSearch." `T.append` T.pack envs))
    else do ansTerm <- ansTerms
            typeCheckU typeEnv sig ansTerm preterm
--let results = show candidates
--let overTree = (UCON (UJudgement [] (UD.Con $ T.pack "?") (UD.Con $ T.pack "?")))

-- searchType : ProofSearchのための補助関数
-- 与えた型をもつ項をリストのなかから探し、それを全て返す
searchType :: (Eq v) => [(k, v)] -> v -> [k]
searchType [] _ = []
searchType ((tr, ty1):xs) ty2 
  | ty1 == ty2 = tr:(searchType xs ty2)
  | otherwise  = searchType xs ty2


-- dismantle : 型環境の中からSigma型を見つけて投射をかける
dismantle :: TUEnv -> TUEnv -> [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)]
dismantle _ [] result = result
dismantle env (preterm:xs) result = 
  case preterm of
    (UD.Sigma preA preB) -> 
      let index = L.elemIndex preterm env in
      case index of
        Just k -> let preB' = UD.shiftIndices (UD.subst preB (UD.shiftIndices (UD.Proj UD.Fst (UD.Var k)) 1 0) 0) (-1) 0 in
                  dismantle env (preB':preA:xs) ((UD.Proj UD.Snd (UD.Var k), preB'):(UD.Proj UD.Fst (UD.Var k), preA):(UD.Var k, preterm):result)
        Nothing -> let newTerm = search result preterm in
                   case newTerm of
                     Just term -> let preB' = UD.shiftIndices (UD.subst preB (UD.shiftIndices (UD.Proj UD.Fst term) 1 0) 0) (-1) 0 in
                                  dismantle env (preB':preA:xs) ((UD.Proj UD.Snd term, preB'):(UD.Proj UD.Fst term, preA):result)
                     Nothing -> dismantle env xs result
    otherwise -> dismantle env xs result
-- UD.subst preB (UD.Proj UD.Fst term) 0
-- UD.subst preB (UD.Proj UD.Fst (UD.Var k)) 0

-- execute : シグネチャの中からPi型を見つけて投射をかける
-- シグネチャはあらかじめchangeSigで型を変換しておく
execute :: [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)]
execute [] _ result = result
execute ((v, preterm):xs) env result = 
  case preterm of
    (UD.Pi preA preB) -> 
      let termAs = searchType env preA in
      let anslist = do {p <- termAs;
                        return (make v preB p)} in
      execute (anslist ++ xs) env ((v, preterm):(anslist ++ result))
    otherwise -> execute xs env ((v, preterm):result)

-- executeT : 型環境の中からPi型を見つけて投射をかける
-- 型環境はあらかじめchangeTenvで型を変換しておく
--executeT :: [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)]
--execute


-- search : dismantleの補助関数
-- resultの中からSigma型の項を探す
search :: [(UD.Preterm, UD.Preterm)] -> UD.Preterm -> Maybe UD.Preterm
search [] _ = Nothing
search ((tm, ty):xs) sigma = 
  if ty == sigma
  then Just tm
  else search xs sigma


-- dismantleSig : シグネチャの中からSigma型を見つけて投射をかける
dismantleSig :: [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)]
dismantleSig [] result = result
dismantleSig ((term, preterm):xs) result = 
  case preterm of
    (UD.Sigma preA preB) -> 
      let preB' = UD.shiftIndices (UD.subst preB (UD.shiftIndices (UD.Proj UD.Fst term) 1 0) 0) (-1) 0 in
      dismantleSig ((UD.Proj UD.Fst term, preA):(UD.Proj UD.Snd term, preB'):xs) ((term, preterm):result)
    otherwise -> dismantleSig xs ((term, preterm):result)


-- changeSig : dismantleSigの補助関数
-- シグネチャの型を変更
changeSig :: SUEnv -> [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)]
changeSig [] result = result
changeSig ((text, preterm):xs) result = (UD.Con text, preterm):(changeSig xs result)


-- changeTenv : executeTの補助関数
-- 型環境の型を変更
changeTenv :: TUEnv -> TUEnv -> [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)]
changeTenv _ [] result = result
changeTenv env (preterm:xs) result = 
  let index = L.elemIndex preterm env in
  case index of
    Just k -> let term = (UD.Var k) in
              changeTenv env xs ((term, preterm):result)
    otherwise -> changeTenv env xs result


-- searchIndex : executeの補助関数
-- 型環境の中から型Aを持つ項(変数)のIndexを全て返す
searchIndex :: UD.Preterm -> TUEnv -> TUEnv -> [Int] -> [Int]
searchIndex _ [] _ result = result
searchIndex preA (x:xs) env result = 
  let index = L.elemIndex preA env in
  case index of
    Just k -> searchIndex preA xs env (k:result)
    Nothing -> searchIndex preA xs env result


-- make : executeの補助関数
-- Pi型の項の、関数適用した結果の項と型を返す
make :: UD.Preterm -> UD.Preterm -> UD.Preterm -> (UD.Preterm, UD.Preterm)
make v preB p = (UD.App v p, UD.shiftIndices (UD.subst preB (UD.shiftIndices p 1 0) 0) (-1) 0)


-- sigToMathML : Debug用関数
-- candidatesの中身をmathML形式で表示
sigToMathML :: [(UD.Preterm, UD.Preterm)] -> T.Text
sigToMathML [] = T.pack " "
sigToMathML ((term, typ):rest) = 
  (toMathML term) `T.append` (T.pack " : ") `T.append` (toMathML typ) `T.append` (T.pack ", ") `T.append` (sigToMathML rest)


