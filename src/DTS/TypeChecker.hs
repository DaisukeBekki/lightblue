{-# LANGUAGE RecordWildCards #-}

{-|
Copyright   : (c) Daisuke Bekki, 2023
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Type checker for Underspecified Dependent Type Theory (Bekki forthcoming).
-}
module DTS.TypeChecker (
  sequentialTypeCheck
  , nullProver
  , typeInfer
  , typeCheck
  ) where

import Data.List (lookup)    --base
import Control.Monad (guard) --base
import qualified Data.Text.Lazy as T --text
--import Control.Monad.Except (throwError)
import Interface.Text (SimpleText(..))
import Interface.Tree (Tree(..),node)
import qualified DTS.UDTTdeBruijn as U
import qualified DTS.UDTTvarName as V
import DTS.Labels (UDTT)
import qualified DTS.QueryTypes as QT

-- | executes type check to a list of UDTT preterms
sequentialTypeCheck :: QT.TypeChecker -> QT.Prover -> U.Signature -> [U.Preterm UDTT] -> [U.Context]
sequentialTypeCheck _ _ _ [] = []
sequentialTypeCheck typchecker prover sig (uterm:uterms) = do
  prevContext <- sequentialTypeCheck typchecker prover sig uterms
  typeCheckResult <- typchecker prover $ QT.TypeCheckQuery sig prevContext uterm U.Type
  return ((U.term $ node typeCheckResult):prevContext)

-- | A skeltal prover 
nullProver :: QT.Prover
nullProver _ _ = []

-- Prover -> Prover -> TypeCheckQuery U.UDTT -> Maybe TypeCheckResult
-- TypeCheckQuery U.UDTT = TypeCheckQuery U.Signature U.Context (U.Preterm U.UDTT) (U.Preterm U.DTT)
-- TypeCheckResult = [Tree (U.Judgment U.DTT) UDTTrule] 

-- | The default type inference for UDTT
typeInfer :: QT.TypeInfer
typeInfer prover QT.TypeInferQuery{..} = case trm of -- sig:Signature, ctx:Context, trm:Preterm DTT, typ:Preterm DTT
  U.Var i -> do
    guard $ i < length ctx
    return $ Tree QT.Var (U.Judgment sig ctx (U.Var i) (ctx!!i)) []
  U.Con t -> do
    case lookup t sig of
      Just termA -> return $ Tree QT.Con (U.Judgment sig ctx (U.Con t) termA) []
      Nothing -> fail $ "No constant symbol " ++ (T.unpack t)
  U.Type -> return $ Tree QT.TypeF (U.Judgment sig ctx U.Type U.Kind) []
  U.Bot -> return $ Tree QT.EnumF (U.Judgment sig ctx U.Bot U.Type) []
  U.Top -> return $ Tree QT.EnumF (U.Judgment sig ctx U.Top U.Type) []
  U.Unit -> return $ Tree QT.EnumI (U.Judgment sig ctx U.Unit U.Top) []
  U.Entity -> return $ Tree QT.EnumF (U.Judgment sig ctx U.Entity U.Type) []
  U.Pi termA termB -> do
    diagramA <- typeCheck prover (QT.TypeCheckQuery sig ctx termA U.Type)
    let termA' = U.term $ node diagramA
    diagramB <- typeCheck prover (QT.TypeCheckQuery sig (termA':ctx) termB U.Type)
    let termB' = U.term $ node diagramB
    return $ Tree QT.PiF (U.Judgment sig ctx (U.Pi termA' termB') (U.typ $ node diagramB)) [diagramA,diagramB]
  U.App termM termN -> do
    diagramM <- typeInfer prover $ QT.TypeInferQuery sig ctx termM
    let termM' = U.term $ node diagramM
        (U.Pi termA termB) = U.typ $ node diagramM
    diagramN <- typeCheck prover $ QT.TypeCheckQuery sig ctx termN termA
    let termN' = U.term $ node diagramN
        termB' = U.betaReduce $ U.subst termB termN' 0
    return $ Tree QT.PiE (U.Judgment sig ctx (U.App termM' termN') termB') [diagramM, diagramN]
  U.Sigma termA termB -> do
    diagramA <- typeCheck prover $ QT.TypeCheckQuery sig ctx termA U.Type
    let termA' = U.term $ node diagramA
    diagramB <- typeCheck prover $ QT.TypeCheckQuery sig (termA':ctx) termB U.Type
    let termB' = U.term $ node diagramB
    return $ Tree QT.SigmaF (U.Judgment sig ctx (U.Sigma termA' termB') (U.typ $ node diagramB)) [diagramA,diagramB]
  U.Proj U.Fst termM -> do
    diagramM <- typeInfer prover $ QT.TypeInferQuery sig ctx termM
    let termM' = U.term $ node diagramM
        (U.Sigma termA _) = U.typ $ node diagramM
    return $ Tree QT.SigmaE (U.Judgment sig ctx (U.Proj U.Fst termM') termA) [diagramM]
  U.Proj U.Snd termM -> do
    diagramM <- typeInfer prover $ QT.TypeInferQuery sig ctx termM
    let termM' = U.term $ node diagramM
        (U.Sigma _ termB) = U.typ $ node diagramM
        termB' = U.betaReduce $ U.subst termB (U.Proj U.Fst termM') 0
    return $ Tree QT.Conv (U.Judgment sig ctx (U.Proj U.Snd termM') termB') [diagramM]
  -- | For debug
  termM -> return $ Tree QT.Con (U.Judgment sig ctx (U.Con (toText termM)) U.Type) []

-- | The default type checker for UDTT
typeCheck :: QT.TypeChecker
typeCheck prover QT.TypeCheckQuery{..} = case (trm,typ) of -- sig:Signature, ctx:Context, trm:Preterm DTT, typ:Preterm DTT
  (U.Lam termM, U.Pi termA termB) -> do
    diagramA <- typeCheck prover $ QT.TypeCheckQuery sig ctx (U.toUDTT termA) U.Type
    let termA' = U.term $ node diagramA
    diagramM <- typeCheck prover $ QT.TypeCheckQuery sig (termA':ctx) termM termB
    let termM' = U.term $ node diagramM
        termB' = U.typ $ node diagramM
    return $ Tree QT.PiI (U.Judgment sig ctx (U.Lam termM') (U.Pi termA' termB')) [diagramA, diagramM]
  (U.Pair termM termN, U.Sigma termA termB) -> do
    diagramM <- typeCheck prover $ QT.TypeCheckQuery sig ctx termM termA
    let termM' = U.term $ node diagramM
        termA' = U.typ $ node diagramM
        termB' = U.betaReduce $ U.subst termB termM' 0
    diagramN <- typeCheck prover $ QT.TypeCheckQuery sig ctx termN termB'
    let termN' = U.term $ node diagramN
    return $ Tree QT.SigmaE (U.Judgment sig ctx (U.Pair termM' termN') (U.Sigma termA' termB')) [diagramM,diagramN]
  _ -> do
    diagramM <- typeInfer prover $ QT.TypeInferQuery sig ctx trm
    let termA' = U.typ $ node diagramM
    guard $ termA' == typ
    return diagramM
