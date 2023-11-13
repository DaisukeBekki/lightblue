{-|
Copyright   : (c) Daisuke Bekki, 2023
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Type checker for Underspecified Dependent Type Theory (Bekki forthcoming).
-}
module DTS.TypeChecker (
  -- * Type system of UDTT
  RuleName(..)
  , typeChecker
  , sequentialTypeChecker
  ) where

import qualified DTS.UDTTdeBruijn as U
import DTS.Query (TypeChecker, Prover)

data RuleName = Var | Con | Typ | PiF | PiI | PiE | SigmaF | SigmaI | SigmaE | DisjF | DisjI | DisjE | EnumF | EnumI | EnumE | IqF | IqI | IqE | NumF | NumI | NumE deriving (Eq, Show, Read)


typeChecker :: TypeChecker -- | TypeCheckQuery U.UDTT -> TypeCheckResult
typeChecker _ _ = []

sequentialTypeChecker :: [U.Context U.UDTT] -> [U.Context U.DTT] 
sequentialTypeChecker _ = []
