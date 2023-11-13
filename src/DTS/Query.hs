{-# LANGUAGE DuplicateRecordFields #-}

module DTS.Query (
  -- * DTT type checking
  TypeCheckQuery(..)
  , TypeCheckResult(..)
  , TypeChecker
  --, signatureChecker
  --, contextChecker
  -- * DTT proof search
  , LogicSystem(..)
  , ProofSearchSetting(..)
  , ProofSearchQuery(..)
  , ProofSearchResult(..)
  , Prover
  ) where

import qualified Data.Text.Lazy as T
import Interface.Text
import Interface.TeX
import Interface.HTML
import Interface.Tree
import qualified DTS.UDTTdeBruijn as U

-- | Type checking in DTT

data TypeCheckQuery a = TypeCheckQuery {
  sig :: U.Signature
  , ctx :: U.Context
  , trm :: U.Preterm a
  , typ :: U.Preterm U.DTT
  } deriving (Eq, Show)

data TypeCheckResult = TypeCheckResult [Tree (U.Judgment U.DTT) TypingRuleName] deriving (Eq)

type TypeChecker a = TypeCheckQuery a -> Prover -> TypeCheckResult

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
  sig :: U.Signature
  , ctx :: U.Context
  , typ :: U.Preterm U.DTT
  } deriving (Eq, Show)

data ProofSearchLabel = FOUND | NOTFOUND deriving (Eq, Show)

data ProofSearchResult = ProofSearchResult [Tree (U.Judgment U.DTT) RuleName] deriving (Eq)

type Prover = ProofSearchSetting -> ProofSearchQuery -> ProofSearchResult

{-
instance MathML ProofSearchQuery where
  toMathML (ProofSearchQuery _ cont typ) = -- | prints a proof search query in MathML
    let (vcontext', vtyp') = UDTT.initializeIndex $ do
                               (varnames,vcontext) <- UDTT.fromDeBruijnContextLoop $ DTT.toUDTTcontext cont
                               vtyp <- UDTT.fromDeBruijn varnames $ DTT.toUDTT typ
                               return (vcontext, vtyp)
    in T.concat ["<mrow>", toMathML vcontext', "<mo>&vdash;</mo><mo>?</mo><mo>:</mo>", toMathML vtyp', "</mrow>"]
-}

--data ProofSearchResult = Diagrams [Tree Judgment RuleName] deriving (Eq, Show)


