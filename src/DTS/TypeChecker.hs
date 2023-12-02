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
  , typeCheck
  ) where

import Interface.Tree (node)
import qualified DTS.UDTTdeBruijn as U
import DTS.TypeQuery 

-- | executes type check to a list of UDTT preterms
sequentialTypeCheck :: U.Signature -> [U.Preterm U.UDTT] -> [U.Context]
sequentialTypeCheck _ [] = []
sequentialTypeCheck sig (uterm:uterms) = do
  prevContext <- sequentialTypeCheck sig uterms
  typeCheckResult <- typeCheck nullProver $ TypeCheckQuery sig prevContext uterm U.Type
  return ((U.term $ node typeCheckResult):prevContext)

nullProver :: Prover
nullProver ProofSearchSetting{..} ProofSearchQuery{..} = []

typeCheck :: TypeChecker -- | Prover -> TypeCheckQuery U.UDTT -> TypeCheckResult
typeCheck prover TypeCheckQuery{..} = [] --case trm of -- sig:Signature, ctx:Context, trm:Preterm DTT, typ:Preterm DTT
--  Var i -> if i < length ctx
--             then 
--             else []
--           
--  tree

--typeInfer :: 
