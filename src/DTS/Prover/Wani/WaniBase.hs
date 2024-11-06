{-# LANGUAGE OverloadedStrings #-}
{-|
  Module      : DTS.Wani.WaniBase
  Definitions for wani
-}
module DTS.Prover.Wani.WaniBase (
   -- * Parameters
    ATerm,
    AType,
    Depth,
    ProofMode(..),
    Status(..),
    Setting(..),
    Result(..),
    -- * Defaults
    statusDef,
    settingDef,
    resultDef,
    -- * Rules
    DeduceRule,
    TypecheckRule,
    -- * Functions
    mergeResult,
    mergeStatus,
    -- ** Debug functions
    debugLogWithTerm,
    debugLog
) where

import qualified DTS.DTTdeBruijn as DdB  -- UDTT
import qualified DTS.Prover.Wani.Arrowterm as A
import qualified Interface.Tree as UDT

import qualified Data.Text.Lazy as T 
import qualified Data.List as L 
import qualified Debug.Trace as D

type ATerm = A.Arrowterm
type AType = A.Arrowterm
type Depth = Int

type DeduceRule = A.SAEnv -> A.AEnv -> AType -> Depth -> Setting -> Result
type TypecheckRule = A.SAEnv -> A.AEnv -> ATerm -> AType -> Depth -> Setting -> Result

data ProofMode = Plain | WithDNE | WithEFQ deriving (Show,Eq)

data Status = Status 
  {failedlst :: [(A.Context,ATerm,AType)], -- ^ Once wani failed to @typecheck@, wani add the tuple to this list
   deduceNgLst :: [(A.Context,AType)], -- ^ Once wani failed to @deduce@, wani add the pair to this list
   usedMaxDepth ::Depth, 
   allProof :: Bool -- ^ In the bottom of the tree, one proof is enough to judge whether the hypo is true or not.
  }deriving (Show,Eq)

data Setting = Setting 
  {mode :: ProofMode,
   falsum :: Bool,
   maxdepth :: Depth,
   maxtime :: Int,
   debug :: Bool,
   sStatus :: Status} deriving (Show,Eq)

data Result = Result
  {trees :: [UDT.Tree A.Arrowrule A.AJudgment],
   errMsg :: T.Text,
   rStatus :: Status} deriving (Show,Eq)

mergeResult :: Result -> Result -> Result
mergeResult rs1 rs2 =
  Result{trees = (trees rs1) ++ (trees rs2),errMsg =  (T.append (errMsg rs1)  (errMsg rs2)),rStatus = mergeStatus (rStatus rs1) (rStatus rs2)}

mergeStatus :: Status -> Status -> Status
mergeStatus st1 st2 =
  Status {failedlst = L.nub(concatMap failedlst [st1,st2]),usedMaxDepth = maximum (map usedMaxDepth [st1,st2]),deduceNgLst=L.nub(concatMap deduceNgLst [st1,st2]),allProof = (allProof st1) || (allProof st2)}

statusDef :: Status
statusDef = Status{failedlst=[],usedMaxDepth = 0,deduceNgLst=[],allProof = False}

settingDef :: Setting
settingDef = Setting{mode = Plain,falsum = True,maxdepth = 9,maxtime = 100000,debug = False,sStatus = statusDef}

resultDef :: Result
resultDef = Result{trees = [],errMsg = "",rStatus = statusDef}

debugLog :: A.Context -> AType -> Depth -> Setting -> T.Text -> a -> a
debugLog con = debugLogWithTerm con (A.Conclusion $ DdB.Con $T.pack "?")

debugLogWithTerm :: A.Context -> ATerm -> AType -> Depth -> Setting -> T.Text -> a -> a
debugLogWithTerm (sig,var) term target depth setting label answer=
  if debug setting
    then
      D.trace
        ((if allProof (sStatus setting) then "all " else "")++  L.replicate (2*depth) ' ' ++ show depth ++ " " ++ T.unpack label ++ " " ++ show (A.AJudgment sig var term target)++ " ")
        answer
    else answer

