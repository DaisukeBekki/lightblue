{-#OPTIONS -Wall#-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : DTS.Prover.TypeChecker
Copyright   : Miho Sato, Hinari Daido
Licence     : All right reserved
Maintainer  : Miho Sato <satoh.miho@is.ocha.ac.jp>, Hinari Daido <daido.hinari@is.ocha.ac.jp>
Stability   : beta

A Typechecker and a theorem prover for DTS.
-}
module DTS.Prover_daido.TypeChecker
( aspElim,
  typeCheckU,
  typeInferU,
  proofSearch,
  dismantle,
  dismantleSig,
  changeSig,
  execute,
  repositP,
  searchIndex
) where

import qualified DTS.UDTT as UD           -- UDTT
import qualified DTS.DTT as DT            -- DTT
import qualified Data.Text.Lazy as T      -- text
import qualified Data.List as L           -- base
import Interface.Text
import DTS.Prover_daido.Judgement

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
transP (UD.Not preM) = do
  pretermM <- transP preM
  return (DT.Not pretermM)
transP (UD.Asp _ _) = []
transP _ = []


-- | repositP : DTTの項をUDTTの項に変換する関数
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

-- | @-Elimination
aspElim :: (UTree UJudgement) -> [Tree Judgement]
-- (@)規則
aspElim (UT ASP' _ [_,right])  = aspElim right
aspElim (UT (L label) (UJudgement uenv preV preM) upside) =
  if (label == TypeF && (preV /= UD.Type || preM /= UD.Kind))
      || (label == CON&& (case preV of UD.Con _ -> False ; _ -> True))
      || (label == TopF && (preV /= UD.Top || preM /= UD.Type))
      || (label == TopI && (preV /= UD.Unit || preM /= UD.Top))
      || (label == BotF && (preV /= UD.Bot || preM /= UD.Type))
      || (label == PiF && (case preV of UD.Pi _ _-> False;_->True))
      || (label == PiI && (case (preV,preM) of (UD.Lam _,UD.Pi _ _)-> False;_->True))
      || (label == PiE && (case preV of UD.App _ _-> False;_->True))
      || (label == SigF && (case preV of UD.Sigma _ _-> False;_->True))
      || (label == SigI && (case (preV,preM) of (UD.Pair _ _, UD.Sigma _ _)-> False;_->True))
      || (label == SigE && (case preV of UD.Proj _ _-> False;_->True))
      || (label == NotF && (case preV of UD.Not _ -> False;_->True))
      || (label == NotI && (case (preV,preM) of (UD.Lam _, UD.Not _)-> False;_->True))
      || (label == NotE && (case (preV,preM) of (UD.App _ _, UD.Bot)-> False;_->True))
  then
    []
  else
    do
      let upside' = map (head . aspElim ) upside
      let upsideTerms' = map (head . getTerm) upside'
      let upsideTypes' = map (head . getType) upside'
      let termTypePair =
            if null upside
              then
                Just (head $transP preV,head $ transP preM)
              else
                case label of
                  PiF -> Just (DT.Pi (upsideTerms' !! 0) (upsideTerms' !! 1), head $ getType $upside' !! 1)
                  PiI -> Just (DT.Lam $upsideTerms' !! 1,DT.Pi (upsideTerms' !! 0) (upsideTerms' !! 1))
                  PiE -> Just (DT.App (upsideTerms' !! 0) (upsideTerms' !! 1),head $transP preM)
                  SigF -> Just (DT.Sigma (upsideTerms' !! 0) (upsideTerms' !! 1),head $ getType $upside' !! 1)
                  SigI -> Just (DT.Pair (upsideTerms' !! 0) (upsideTerms' !! 1),DT.Sigma (upsideTypes' !! 0) (upsideTypes' !! 1))
                  SigE ->
                      case (head upsideTypes',preV) of
                        (DT.Sigma preA _, UD.Proj UD.Fst _) ->
                          Just (DT.Proj DT.Fst $head upsideTerms',preA)
                        (DT.Sigma _ preB, UD.Proj UD.Snd _) ->
                          Just (DT.Proj DT.Snd $head upsideTerms',preB)
                        _ -> Nothing
                  NotF -> Just (DT.Not $head upsideTerms',head upsideTypes')
                  NotI -> Just (DT.Lam $upsideTerms' !! 1, DT.Not $ upsideTerms' !! 0)
                  NotE -> Just (DT.App (upsideTerms' !! 0) (upsideTerms' !! 1), DT.Bot)
                  CHK->
                    Just (head upsideTerms',head upsideTypes')
                  _ -> Nothing
      case termTypePair of
        Just (jterm,jtype) -> return $ T label (Judgement (transE uenv) jterm jtype) upside'
        Nothing -> []
--(Error)
aspElim (UError' _ _) = []
-- otherwise
aspElim _ = []

-- | typeCheckU : UDTTの型チェック
typeCheckU :: TUEnv -> SUEnv -> UD.Preterm -> UD.Preterm -> [UTree UJudgement]
-- (ΠI) rule
typeCheckU typeEnv sig (UD.Lam preM) (UD.Pi preA preB) = do
  leftTree <- (typeCheckU typeEnv sig preA (UD.Type))
                ++ (typeCheckU typeEnv sig preA (UD.Kind))
  newleftTree <- aspElim leftTree
  newA <- getTerm newleftTree
  let preA' = UD.betaReduce $ repositP newA
  rightTree <- typeCheckU (preA':typeEnv) sig preM preB
  return (UT (L PiI) (UJudgement typeEnv (UD.Lam preM) (UD.Pi preA preB)) [leftTree,rightTree])
-- (NotI) rule (
typeCheckU typeEnv sig (UD.Lam preM) (UD.Not preterm) = do
  leftTree <- (typeCheckU typeEnv sig preterm UD.Type)
                ++ (typeCheckU typeEnv sig preterm UD.Kind)
  newleftTree <- aspElim leftTree
  newA <- getTerm newleftTree
  let preA' = UD.betaReduce $ repositP newA
  rightTree <- typeCheckU (preA':typeEnv) sig preM UD.Bot
  return (UT (L NotI) (UJudgement typeEnv (UD.Lam preM) (UD.Not preterm)) [leftTree,rightTree])
-- (ΣI) rule
typeCheckU typeEnv sig (UD.Pair preM preN) (UD.Sigma preA preB) = do
  leftTree <- typeCheckU typeEnv sig preM preA
  newleftTree <- aspElim leftTree
  newM <- getTerm newleftTree
  let preB' = UD.shiftIndices (UD.subst preB (UD.shiftIndices (repositP newM) 1 0) 0) (-1) 0
--  let preB' = UD.subst preB (UD.betaReduce $ repositP newM) 0
  rightTree <- typeCheckU typeEnv sig preN preB'
  return (UT (L SigI) (UJudgement typeEnv (UD.Pair preM preN) (UD.Sigma preA preB)) [leftTree,rightTree])
-- (CHK) rule
typeCheckU typeEnv sig preE value = do
  overTree <- typeInferU typeEnv sig preE
  resultType <- getTypeU overTree
  if value == resultType
    then do return (UT (L CHK) (UJudgement typeEnv preE value) [overTree])
    else if value == UD.Kind
      then []
      else do return (UError' (UJudgement typeEnv value resultType) (T.pack "does not match type"))

-- | typeInferU : UDTTの型推論
typeInferU :: TUEnv -> SUEnv -> UD.Preterm -> [UTree UJudgement]
-- (typeF) rule
typeInferU typeEnv _ UD.Type =
  return $ UT (L TypeF) (UJudgement typeEnv UD.Type UD.Kind) []
-- (VAR) rule
typeInferU typeEnv _ (UD.Var k) = do
  let varType = typeEnv !! k
  return $UT (L VAR) (UJudgement typeEnv (UD.Var k) varType) []
-- (CON) rule
typeInferU typeEnv sig (UD.Con text) =
  let conTypes = getList sig text in
  if null conTypes
  then return (UT (L CON) (UJudgement typeEnv (UD.Con text) (UD.Con $ T.pack "is not exist.")) [] )
  else do conType <- conTypes
          return (UT (L CON) (UJudgement typeEnv (UD.Con text) conType) [])
-- (TopF) rule
typeInferU typeEnv _ UD.Top =
  return (UT (L TopF) (UJudgement typeEnv UD.Top UD.Type) [])
-- (TopI) rule
typeInferU typeEnv _ UD.Unit =
  return (UT (L TopI) (UJudgement typeEnv UD.Unit UD.Top) [])
-- (BotF) rule
typeInferU typeEnv _ UD.Bot =
  return (UT (L BotF) (UJudgement typeEnv UD.Bot UD.Type) [])
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
  return (UT (L PiF) (UJudgement typeEnv (UD.Pi preA preB) ansType) [leftTree,rightTree])
-- (Not F) rule
typeInferU typeEnv sig (UD.Not preM) = do
  overTree <- typeInferU typeEnv sig preM
  typ <- getTypeU overTree
  return (UT (L NotF) (UJudgement typeEnv (UD.Not preM) typ) [overTree])
-- (ΠE) rule, (Not E) rule
typeInferU typeEnv sig (UD.App preM preN) = do
  leftTree <- typeInferU typeEnv sig preM
  funcType <- getTypeU leftTree
  case funcType of
    (UD.Pi preA preB) -> do
       rightTree <- typeCheckU typeEnv sig preN preA
       let preB' = UD.betaReduce $ UD.shiftIndices (UD.subst preB (UD.shiftIndices preN 1 0) 0) (-1) 0
       return (UT (L PiE) (UJudgement typeEnv (UD.App preM preN) preB') [leftTree,rightTree])
    (UD.Not preA) -> do
       rightTree <- typeCheckU typeEnv sig preN preA
       return (UT (L NotE) (UJudgement typeEnv (UD.App preM preN) UD.Bot) [leftTree,rightTree])
    _ -> return (UError' (UJudgement typeEnv (UD.App preM preN) (UD.Con $ T.pack "???")) (T.pack "Not a function"))
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
  return (UT (L SigF) (UJudgement typeEnv (UD.Sigma preA preB) ansType) [leftTree,rightTree])
-- (ΣE) rule
typeInferU typeEnv sig (UD.Proj selector preM) =
  if selector == UD.Fst
  then do overTree <- typeInferU typeEnv sig preM
          bodyType <- getTypeU overTree
          case bodyType of
            (UD.Sigma preA _) ->
               return (UT (L SigE) (UJudgement typeEnv (UD.Proj UD.Fst preM) preA) [overTree])
            _ -> return (UError' (UJudgement typeEnv (UD.Proj UD.Fst preM) (UD.Con $ T.pack "???")) (T.pack "Not a Sigma type"))
  else do overTree <- typeInferU typeEnv sig preM
          bodyType <- getTypeU overTree
          case bodyType of
            (UD.Sigma _ preB) -> do
               let preB' = UD.betaReduce $ UD.shiftIndices (UD.subst preB (UD.shiftIndices (UD.Proj UD.Fst preM) 1 0) 0) (-1) 0
               return (UT (L SigE) (UJudgement typeEnv (UD.Proj UD.Snd preM) preB') [overTree])
            _ -> do return (UError' (UJudgement typeEnv (UD.Proj UD.Snd preM) (UD.Con $ T.pack "???")) (T.pack "Not a Sigma type"))
-- UD.subst preB (UD.Proj UD.Fst preM) 0
-- (Asp) rule
typeInferU typeEnv sig (UD.Asp i preA) = do
  leftTree <- (typeCheckU typeEnv sig preA UD.Type)
                ++ (typeCheckU typeEnv sig preA UD.Kind)
  newleftTree <- aspElim leftTree
  newA <- getTerm newleftTree
  let preA' = UD.betaReduce $ repositP newA
  ansTree <- proofSearch typeEnv sig preA'
  return (UT ASP' (UJudgement typeEnv (UD.Asp i preA) preA) [leftTree,ansTree])
-- (DRel) rule
--typeInferU typeEnv _ (UD.DRel i t preM preN) = do
--  return (UDREL (UJudgement typeEnv (UD.DRel i t preM preN) UD.Type))
-- otherwise
typeInferU _ _ _ = []


proofSearch :: TUEnv -> SUEnv -> UD.Preterm -> [UTree UJudgement]
proofSearch typeEnv sig preterm = do
  let candidatesA = (dismantle typeEnv typeEnv [])
                       ++ (dismantleSig (changeSig sig []) []) ++ [(UD.Unit, UD.Top)]
  let candidatesB = execute (changeSig sig []) candidatesA []
  let candidatesC = execute (changeTenv typeEnv typeEnv []) candidatesA []
  let candidates = candidatesA ++ candidatesB ++ candidatesC
  let ansTerms = searchType candidates preterm
  case (ansTerms, preterm) of
    -- バラすだけではだめで、型がSigmaの場合(Sigma I)
    ([], UD.Sigma _ _) -> sigIntro typeEnv sig candidates preterm
    -- バラすだけではだめで、型がPiの場合(Pi I)
    ([], UD.Pi _ _) -> piIntro typeEnv sig preterm
    -- 失敗なら
    ([], _) -> do
      let envs = T.intercalate "," $ map (\(x,y) -> T.concat ["(",toText x,",",toText y,")"]) candidates
      return (UError' (UJudgement typeEnv (UD.Con $ T.pack "???") preterm) (T.pack "fail: proofSearch." `T.append` envs))
    -- 証明項があるなら
    (pTerms, _) -> do
      ansTerm <- pTerms
      typeCheckU typeEnv sig ansTerm preterm

-- searchType : ProofSearchのための補助関数
-- 与えた型をもつ項をリストのなかから探し、それを全て返す
searchType :: (Eq v) => [(k, v)] -> v -> [k]
searchType [] _ = []
searchType ((tr, ty1):xs) ty2
  | ty1 == ty2 = tr:(searchType xs ty2)
  | otherwise  = searchType xs ty2

-- sigIntro : (Sigma I)規則にしたがって証明探索する関数
sigIntro :: TUEnv -> SUEnv -> [(UD.Preterm, UD.Preterm)] -> UD.Preterm -> [UTree UJudgement]
sigIntro typeEnv sig candidates (preterm@(UD.Sigma preM preN)) = do
  termM <- searchType candidates preM
  leftTree <- typeCheckU typeEnv sig termM preM
  newleftTree <- aspElim leftTree
  newM <- getTerm newleftTree
  let newM' = UD.betaReduce $ UD.shiftIndices (UD.subst preN (UD.shiftIndices (repositP newM) 1 0) 0) (-1) 0
--UD.subst preN (repositP newM) 0
  termN <- searchType candidates newM'
  typeCheckU typeEnv sig (UD.Pair termM termN) preterm
sigIntro _ _ _ _ = []

-- piIntro : (Pi I)規則にしたがって証明探索する関数
piIntro :: TUEnv -> SUEnv -> UD.Preterm -> [UTree UJudgement]
piIntro typeEnv sig (preterm@(UD.Pi preA preB)) = do
  leftTree <- (typeCheckU typeEnv sig preA UD.Type)
                 ++ (typeCheckU typeEnv sig preA UD.Kind)
  newleftTree <- aspElim leftTree
  newA <- getTerm newleftTree
  let preA' = UD.betaReduce $ repositP newA
  let newEnv = (preA':typeEnv)
  let candidatesA = (dismantle newEnv newEnv [])
                       ++ (dismantleSig (changeSig sig []) [])
  let candidatesB = execute (changeSig sig []) candidatesA []
  let candidates = candidatesA ++ candidatesB
  termM <- searchType candidates preB
  typeCheckU typeEnv sig (UD.Lam termM) preterm
piIntro _ _ _ = []


-- | dismantle : 型環境の中からSigma型を見つけて投射をかける
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
    _ -> dismantle env xs result
-- UD.subst preB (UD.Proj UD.Fst term) 0
-- UD.subst preB (UD.Proj UD.Fst (UD.Var k)) 0

-- | execute : シグネチャの中からPi型を見つけて投射をかける
-- | シグネチャはあらかじめchangeSigで型を変換しておく
execute :: [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)]
execute [] _ result = result
execute ((v, preterm):xs) env result =
  case preterm of
    (UD.Pi preA preB) ->
      let termAs = searchType env preA in
      let anslist = do {p <- termAs;
                        return (make v preB p)} in
      execute (anslist ++ xs) env ((v, preterm):(anslist ++ result))
    _ -> execute xs env ((v, preterm):result)


-- search : dismantleの補助関数
-- resultの中からSigma型の項を探す
search :: [(UD.Preterm, UD.Preterm)] -> UD.Preterm -> Maybe UD.Preterm
search [] _ = Nothing
search ((tm, ty):xs) sigma =
  if ty == sigma
  then Just tm
  else search xs sigma


-- | dismantleSig : シグネチャの中からSigma型を見つけて投射をかける
dismantleSig :: [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)]
dismantleSig [] result = result
dismantleSig ((term, preterm):xs) result =
  case preterm of
    (UD.Sigma preA preB) ->
      let preB' = UD.shiftIndices (UD.subst preB (UD.shiftIndices (UD.Proj UD.Fst term) 1 0) 0) (-1) 0 in
      dismantleSig ((UD.Proj UD.Fst term, preA):(UD.Proj UD.Snd term, preB'):xs) ((term, preterm):result)
    _ -> dismantleSig xs ((term, preterm):result)


-- | changeSig : dismantleSigの補助関数
-- | シグネチャの型を変更
changeSig :: SUEnv -> [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)]
changeSig [] result = result
changeSig ((text, preterm):xs) result = (UD.Con text, preterm):(changeSig xs result)


-- | changeTenv : executeTの補助関数
-- | 型環境の型を変更
changeTenv :: TUEnv -> TUEnv -> [(UD.Preterm, UD.Preterm)] -> [(UD.Preterm, UD.Preterm)]
changeTenv _ [] result = result
changeTenv env (preterm:xs) result =
  let index = L.elemIndex preterm env in
  case index of
    Just k -> let term = (UD.Var k) in
              changeTenv env xs ((term, preterm):result)
    _ -> changeTenv env xs result


-- | searchIndex : executeの補助関数
-- | 型環境の中から型Aを持つ項(変数)のIndexを全て返す
searchIndex :: UD.Preterm -> TUEnv -> TUEnv -> [Int] -> [Int]
searchIndex _ [] _ result = result
searchIndex preA (_:xs) env result =
  let index = L.elemIndex preA env in
  case index of
    Just k -> searchIndex preA xs env (k:result)
    Nothing -> searchIndex preA xs env result

-- | make : executeの補助関数
-- | Pi型の項の、関数適用した結果の項と型を返す
make :: UD.Preterm -> UD.Preterm -> UD.Preterm -> (UD.Preterm, UD.Preterm)
make v preB p = (UD.App v p, UD.shiftIndices (UD.subst preB (UD.shiftIndices p 1 0) 0) (-1) 0)
