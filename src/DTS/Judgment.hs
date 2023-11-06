{-# LANGUAGE DuplicateRecordFields #-}

module DTS.Judgment (
  Judgment(..)
  -- * Type system of DTT
  , TypingRuleName(..)
  -- * DTT type checking
  --, TypeChecker
  --, TypeCheckQuery(..)
  --, TypeCheckResult(..)
  --, signatureChecker
  --, contextChecker
  -- * DTT proof search
  --, Prover
  --, ProofSearchQuery(..)
  --, ProofSearchResult(..)
  ) where

import qualified Data.Text.Lazy as T
import Interface.Text
import Interface.TeX
import Interface.HTML
import Interface.Tree
import qualified DTS.UDTT         as UDTT
--import qualified DTS.UDTTJudgment as UDTT
import qualified DTS.DTT          as DTT

-- | The data type for a DTT judgment
data Judgment = Judgment {
  sig :: DTT.Signature -- ^ A signature \sigma
  , ctx :: DTT.Context -- ^ A context \Gamma in \Gamma \vdash M:A
  , trm :: DTT.Preterm -- ^ A term M in \Gamma \vdash M:A
  , typ :: DTT.Preterm -- ^ A type A in \Gamma \vdash M:A
  } deriving (Eq, Show)

data TypingRuleName = Var | Con | Typ | PiF | PiI | PiE | SigmaF | SigmaI | SigmaE | DisjF | DisjI | DisjE | EnumF | EnumI | EnumE | IqF | IqI | IqE | NumF | NumI | NumE deriving (Eq, Show, Read)

-- | Type checking in DTT

data UDTTTypeCheckQuery = TypeCheckQuery {
  sig :: DTT.Signature
  , ctx :: DTT.Context
  , trm :: UDTT.Preterm
  , typ :: DTT.Preterm
  } deriving (Eq, Show)

--data TypeCheckResult = Diagrams [Tree Judgment RuleName] deriving (Eq, Show)
--type UDTTTypeChecker = UDTTTypeCheckQuery -> TypeCheckResult

{-
signatureChecker :: TypeChecker -> DTT.Signature -> Bool
signatureChecker = signatureCheckerLoop []
  where signatureCheckerLoop _ _ [] = True
        signatureCheckerLoop typeChecker prevSig (sig:sigs) =
          case typeChecker of
            [] -> False
            (Tree rulename node dtrs):_ -> True
          &&
          signatureCheckerLoop typeChecker (new:prevSig) sigs

contextChecker :: Context -> Bool
contextChecker = contextCheckerLoop []
  where contextCheckerLoop _ [] = True
        contextCheckerLoop prevCon (con:cons) =
-}

-- | Proof Search in DTT

data LogicSystem = Intuitionistic | Classical deriving (Eq, Show)

data ProofSearchSetting = ProofSearchSetting {
  maxDepth :: Maybe Int
  , system :: Maybe LogicSystem
  } deriving (Eq, Show)

data ProofSearchQuery = ProofSearchQuery {
  sig :: DTT.Signature
  , ctx :: DTT.Context
  , typ :: DTT.Preterm
  } deriving (Eq, Show)

--type Prover = ProofSearchSetting -> ProofSearchQuery -> ProofSearchResult

instance MathML ProofSearchQuery where
  toMathML (ProofSearchQuery _ cont typ) = -- | prints a proof search query in MathML
    let (vcontext', vtyp') = UDTT.initializeIndex $ do
                               (varnames,vcontext) <- UDTT.fromDeBruijnContextLoop $ DTT.toUDTTcontext cont
                               vtyp <- UDTT.fromDeBruijn varnames $ DTT.toUDTT typ
                               return (vcontext, vtyp)
    in T.concat ["<mrow>", toMathML vcontext', "<mo>&vdash;</mo><mo>?</mo><mo>:</mo>", toMathML vtyp', "</mrow>"]

--data ProofSearchResult = Diagrams [Tree Judgment RuleName] deriving (Eq, Show)


