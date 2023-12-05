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
  , uDTTtypeCheck
  ) where

import Control.Monad.Except (throwError)
import Interface.Tree (Tree(..),node)
import qualified DTS.UDTTdeBruijn as U
import qualified DTS.UDTTvarName as V
import DTS.Labels (UDTT)
import DTS.QueryTypes as TQ

-- | executes type check to a list of UDTT preterms
sequentialTypeCheck :: TypeChecker -> Prover -> U.Signature -> [U.Preterm UDTT] -> [U.Context]
sequentialTypeCheck _ _ _ [] = []
sequentialTypeCheck typchecker prover sig (uterm:uterms) = do
  prevContext <- sequentialTypeCheck typchecker prover sig uterms
  typeCheckResult <- typchecker prover $ TypeCheckQuery sig prevContext uterm U.Type
  return ((U.term $ node typeCheckResult):prevContext)

nullProver :: Prover
nullProver ProofSearchSetting{..} ProofSearchQuery{..} = []

-- | The type checker of UDTT
-- |   Prover -> Prover -> TypeCheckQuery U.UDTT -> Maybe TypeCheckResult
-- |   TypeCheckQuery U.UDTT = TypeCheckQuery U.Signature U.Context (U.Preterm U.UDTT) (U.Preterm U.DTT)
-- |   TypeCheckResult = [Tree (U.Judgment U.DTT) UDTTrule] 
uDTTtypeCheck :: TypeChecker
uDTTtypeCheck prover TypeCheckQuery{..} = [] --case (trm,typ) of -- sig:Signature, ctx:Context, trm:Preterm DTT, typ:Preterm DTT
--  (U.Lam termM, U.Pi termA termB) -> do
--    leftTree <- uDTTtypeCheck prover (TypeCheckQuery sig ctx (U.toUDTT termA) (U.Type))
--    rightTree <- 
--    return $ Tree TQ.runeName (U.Judgment ) [leftTree,rightTree]
--  Var i -> if i < length ctx
--             then 
--             else []
--           
--  tree

--uDTTtypeInfer :: TypeCheck
