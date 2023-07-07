{-# LANGUAGE RecordWildCards #-}

{-|
Module      : DTS.Prover.Diag.Prover
Copyright   : Daisuke Bekki
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

A Prover-Interfaces for DTS.
-}

module DTS.Prover.Diag.Prover (
  defaultTypeCheck
  , defaultProofSearch
  , checkFelicity
  , sequentialTypeCheck
  ) where

import qualified DTS.UDTT as UD
import qualified DTS.Prover.Diag.TypeChecker as Ty
import qualified DTS.Prover.Diag.Judgement as Ty

-- | type check with the default signature = entity:type, evt:type
defaultTypeCheck :: UD.Signature -> UD.Context -> UD.Preterm -> UD.Preterm -> [Ty.UTree  Ty.UJudgement]
defaultTypeCheck sig cont term typ = Ty.typeCheckU cont (("evt",UD.Type):("entity",UD.Type):sig) term typ

-- | proof search with the default signature = entity:type, evt:type
defaultProofSearch :: UD.Signature -> UD.Context -> UD.Preterm -> [Ty.UTree  Ty.UJudgement]
defaultProofSearch sig cont typ = Ty.proofSearch cont (("evt",UD.Type):("entity",UD.Type):sig) typ

-- | checks felicity condition
checkFelicity :: UD.Signature -> [UD.Preterm] -> UD.Preterm -> [Ty.UTree  Ty.UJudgement]
checkFelicity sig cont term = defaultTypeCheck sig cont term (UD.Type)

-- | executes type check to a context
sequentialTypeCheck :: UD.Signature -> [UD.Preterm] -> [UD.Preterm]
sequentialTypeCheck sig = foldr (\sr cont -> let result = do
                                                          t1 <- checkFelicity sig cont sr;
                                                          t2 <- Ty.aspElim t1;
                                                          t3 <- Ty.getTerm t2
                                                          return $ Ty.repositP t3 in
                                             if null result
                                                then (UD.Con "Typecheck or aspElim failed"):cont
                                                else (head result):cont
                                ) []

