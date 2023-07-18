{-# LANGUAGE GADTs #-}

module DTS.QueryTypes (
  -- * DTT type checking
  DTTtypeChecker
  , DTTtypeCheckQuery(..)
  , DTTtypeCheckResult(..)
  -- * DTT proof search
  , DTTprover
  , ProofSearchQuery(..)
  , ProofSearchResult(..)
  -- * UDTT type checking
  , UDTTtypeCheker
  , UDTTtypeCheckQuery(..)
  , UDTTtypeCheckResult(..)
  ) where

import qualified Data.Text as T
import Interface.Tree
import qualified DTS.UDTT as UDTT
import qualified DTS.DTT as DTT

-- | Type checking in DTT

type DTTtypeChecker = DTTtypeCheckQuery -> DTTtypeCheckResult

data DTTtypeCheckQuery = DTTtypeCheckQuery DTT.Signature DTT.Context DTT.Preterm DTT.Preterm deriving (Eq, Show)
data DTTtypeCheckResult = DTTtypeCheckResult [Tree DTT.Judgment] deriving (Eq, Show)

signatureChecker :: DTTtypeChecker -> DTT.Signature -> Bool
signatureChecker = signatureCheckerLoop []
  where signatureCheckerLoop _ _ [] = True
        signatureCheckerLoop prevSig typeChecker (sig:sigs) =
          case typeChecker of
            [] -> False
            (Tree (Judgment)):_ -> True
          &&
          signatureCheckerLoop (new:prevSig) typeChecker sigs

contextChecker :: Context -> Bool
contextChecker = contextCheckerLoop []
  where contextCheckerLoop _ [] = True
        contextCheckerLoop prevCon (con:cons) =



-- | Proof Search in DTT

type DTTprover = ProofSearchSetting -> ProofSearchQuery -> ProofSearchResult

data LogicSystem = Intuitionistic | Classical deriving (Eq, Show)
data ProofSearchSetting = ProofSearchSetting (Maybe Int) (Maybe LogicSystem) deriving (Eq, Show)
data ProofSearchQuery = ProofSearchQuery DTT.Signature DTT.Context DTT.Preterm deriving (Eq, Show)

instance MathML ProofSearchQuery where
  toMathML (DTTproofSearchQuery sig cont term typ) = -- | prints a proof search query in MathML
    let (vcontext', vtyp')
        = UDTT.initializeIndex $ do
                            (varnames,vcontext) <- fromDeBruijnContextLoop psContext
                            vtyp <- fromDeBruijn varnames psTyp
                            return (vcontext, vtyp)
    in T.concat ["<mrow>", toMathML vcontext', "<mo>&vdash;</mo><mo>?</mo><mo>:</mo>", toMathML vtyp', "</mrow>"]

data ProofSearchResult = ProofSearchResult {
  diagrams :: [Tree DTT.Judgment]
  , maxDepth :: Maybe Int
  } deriving (Eq, Show)

-- | Type checking in UDTT

type UDTTtypeChecker = UDTTtypeCheckQuery -> UDTTtypeCheckResult

data UDTTtypeCheckQuery = UDTTtypeCheckQuery DTT.Signature UDTT.Context UDTT.Preterm UDTT.Preterm DTTProver deriving (Eq, Show)
data UDTTtypeCheckResult = UDTTtypeCheckResult [Tree DTT.Judgment] deriving (Eq, Show)

