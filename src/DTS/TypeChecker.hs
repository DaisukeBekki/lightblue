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
import Control.Applicative (empty)   --base
import Control.Monad (guard,when)    --base
import qualified Data.Text.Lazy as T --text
--import Control.Monad.Except (throwError)
import Interface.Text (SimpleText(..))
import Interface.Tree (Tree(..),node)
import qualified DTS.UDTTdeBruijn as U
import qualified DTS.UDTTvarName as V
import DTS.Labels (UDTT,DTT)
import qualified DTS.QueryTypes as QT

-- | executes type check to a list of UDTT preterms
-- | returns as many possible DTT context
sequentialTypeCheck :: QT.TypeChecker -> QT.Prover -> U.Signature -> [U.Preterm UDTT] -> QT.ListEx [U.Preterm DTT]
sequentialTypeCheck _ _ _ [] = empty
sequentialTypeCheck typchecker prover sig (uterm:uterms) = do
  prevContext <- sequentialTypeCheck typchecker prover sig uterms
  typeCheckTree <- typchecker prover $ QT.TypeCheckQuery sig prevContext uterm U.Type
  return $ (U.term $ node typeCheckTree):prevContext

-- | A skeltal prover 
nullProver :: QT.Prover
nullProver _ _ = []

-- | The default type inference for UDTT
typeInfer :: QT.TypeInfer
typeInfer prover QT.TypeInferQuery{..} = case trm of -- sig:Signature, ctx:Context, trm:Preterm DTT, typ:Preterm DTT
  U.Var i -> do
    when (i >= length ctx) $ QT.exception $ T.concat [T.pack (show i), " is out of bound in the context: ", toText ctx]
    return $ Tree QT.Var (U.Judgment sig ctx (U.Var i) (ctx!!i)) []
  U.Con t -> do
    case lookup t sig of
      Just termA -> return $ Tree QT.Con (U.Judgment sig ctx (U.Con t) termA) []
      Nothing -> QT.exception $ T.concat ["No constant symbol for ", t]
  U.Type -> return $ Tree QT.TypeF (U.Judgment sig ctx U.Type U.Kind) []
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
    return $ Tree QT.SigmaE (U.Judgment sig ctx (U.Proj U.Snd termM') termB') [diagramM]
  U.Disj termA termB -> do
    diagramA <- typeCheck prover $ QT.TypeCheckQuery sig ctx termA U.Type
    let termA' = U.term $ node diagramA
    diagramB <- typeCheck prover $ QT.TypeCheckQuery sig (termA':ctx) termB U.Type
    let termB' = U.term $ node diagramB
    return $ Tree QT.DisjF (U.Judgment sig ctx (U.Sigma termA' termB') (U.typ $ node diagramB)) [diagramA,diagramB]
  U.Unpack termP termL termM termN -> do
    diagramL <- typeInfer prover $ QT.TypeInferQuery sig ctx termL
    let termL' = U.term $ node diagramL
        (U.Disj termA termB) = U.typ $ node diagramL
    diagramP <- typeCheck prover $ QT.TypeCheckQuery sig ctx termP (U.Pi (U.Disj termA termB) U.Type)
    let termP' = U.term $ node diagramP
        termPL = U.betaReduce $ U.App termP' termL'
        termPM = U.betaReduce $ U.App termP' (U.Iota U.Fst (U.Var 0))
        termPN = U.betaReduce $ U.App termP' (U.Iota U.Fst (U.Var 0))
    diagramM <- typeCheck prover $ QT.TypeCheckQuery sig ctx termM (U.Pi termA termPM)
    diagramN <- typeCheck prover $ QT.TypeCheckQuery sig ctx termM (U.Pi termB termPN)
    let termM' = U.term $ node diagramM
        termN' = U.term $ node diagramN
    return $ Tree QT.DisjE (U.Judgment sig ctx (U.Unpack termP' termL' termM' termN') termPL) [diagramL,diagramP,diagramM,diagramN]
  U.Bot -> return $ Tree QT.EnumF (U.Judgment sig ctx U.Bot U.Type) []
  U.Top -> return $ Tree QT.EnumF (U.Judgment sig ctx U.Top U.Type) []
  U.Unit -> return $ Tree QT.EnumI (U.Judgment sig ctx U.Unit U.Top) []
  U.Entity -> return $ Tree QT.EnumF (U.Judgment sig ctx U.Entity U.Type) []
  U.Eq termA termM termN -> do
    diagramA <- typeCheck prover $ QT.TypeCheckQuery sig ctx termA U.Type
    let termA' = U.term $ node diagramA
    diagramM <- typeCheck prover $ QT.TypeCheckQuery sig ctx termM termA'
    let termM' = U.term $ node diagramM
    diagramN <- typeCheck prover $ QT.TypeCheckQuery sig ctx termN termA'
    let termN' = U.term $ node diagramN
    return $ Tree QT.IqF (U.Judgment sig ctx (U.Eq termA' termM' termN') U.Type) [diagramA,diagramM,diagramN]
  U.Refl termA termM -> do
    diagramA <- typeCheck prover $ QT.TypeCheckQuery sig ctx termA U.Type
    let termA' = U.term $ node diagramA
    diagramM <- typeCheck prover $ QT.TypeCheckQuery sig ctx termM termA'
    let termM' = U.term $ node diagramM
    return $ Tree QT.IqI (U.Judgment sig ctx (U.Refl termA' termM') (U.Eq termA' termM' termM')) [diagramA, diagramM]
  --U.Idpeel
  U.Nat -> return $ Tree QT.NatF (U.Judgment sig ctx U.Nat U.Type) []
  U.Zero -> return $ Tree QT.NatI (U.Judgment sig ctx U.Zero U.Nat) []
  U.Succ termN -> do
    diagramN <- typeCheck prover $ QT.TypeCheckQuery sig ctx termN U.Nat
    let termN' = U.term $ node diagramN
    return $ Tree QT.NatI (U.Judgment sig ctx (U.Succ termN') U.Nat) [diagramN]
  --U.Natrec
  {-
  U.Asp termA termB -> do
    diagramA <- typeCheck $ prover $ QT.TypeCheckQuery sig ctx termA U.Type
    let termA' = U.term $ node diagramA
    diagramQ <- prover (QT.ProofSearchSetting Nothing Nothing QT.Intuitionistic)
                       (QT.ProofSearchQuery sig ctx termA')
    let termM = U.term $ node diagramQ
        termB' = U.betaReduce $ U.subst termB termM 0
    typeCheck $ prover $ QT.TypeCheckQuery sig ctx termB' U.Type
  -}
  -- | type inference fails
  termM -> QT.exception $ T.concat [toText termM, " is not an inferable term."]

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
  (U.Iota U.Fst termM, U.Disj termA termB) -> do
    diagramM <- typeCheck prover $ QT.TypeCheckQuery sig ctx termM termA
    let termM' = U.term $ node diagramM
        termA' = U.typ $ node diagramM
    return $ Tree QT.DisjI (U.Judgment sig ctx (U.Iota U.Fst termM') (U.Disj termA' termB)) [diagramM]
  (U.Iota U.Snd termN, U.Disj termA termB) -> do
    diagramN <- typeCheck prover $ QT.TypeCheckQuery sig ctx termN termB
    let termN' = U.term $ node diagramN
        termB' = U.typ $ node diagramN
    return $ Tree QT.DisjI (U.Judgment sig ctx (U.Iota U.Snd termN') (U.Disj termA termB')) [diagramN]
  _ -> do
    diagramM <- typeInfer prover $ QT.TypeInferQuery sig ctx trm
    let termA' = U.typ $ node diagramM
    guard $ termA' == typ
    return diagramM
