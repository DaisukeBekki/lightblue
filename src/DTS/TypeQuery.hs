{-# LANGUAGE DuplicateRecordFields, TypeSynonymInstances, FlexibleInstances #-}

module DTS.TypeQuery (
  -- * Type system of UDTT
  UDTTrule(..)
  -- * UDTT type check
  , TypeCheckQuery(..)
  , TypeCheckResult(..)
  , TypeChecker
  -- * DTT proof search
  , LogicSystem(..)
  , ProofSearchSetting(..)
  , ProofSearchQuery(..)
  , ProofSearchResult(..)
  , Prover
  ) where

import qualified Data.Text.Lazy as T --text
import Interface.Text
import Interface.TeX
import Interface.HTML
import Interface.Tree
import qualified DTS.UDTTdeBruijn as U
import qualified DTS.UDTTvarName as VN
import DTS.Labels (UDTT,DTT)

data UDTTrule = Var | Con | Typ | PiF | PiI | PiE | SigmaF | SigmaI | SigmaE | DisjF | DisjI | DisjE | EnumF | EnumI | EnumE | IqF | IqI | IqE | NumF | NumI | NumE deriving (Eq, Show, Read)

-- | Type checking in DTT

data TypeCheckQuery a = TypeCheckQuery {
  sig :: U.Signature
  , ctx :: U.Context
  , trm :: U.Preterm a
  , typ :: U.Preterm DTT
  } deriving (Eq, Show)

type TypeCheckResult = [Tree (U.Judgment DTT) UDTTrule]

type TypeChecker = Prover -> TypeCheckQuery UDTT -> TypeCheckResult

data TypeInferQuery a = TypeInferQuery {
  sig :: U.Signature
  , ctx :: U.Context
  , trm :: U.Preterm a
  } deriving (Eq, Show)

-- | Proof Search in DTT

data LogicSystem = Intuitionistic | Classical deriving (Eq, Show)

data ProofSearchSetting = ProofSearchSetting {
  maxDepth :: Maybe Int
  , system :: Maybe LogicSystem
  } deriving (Eq, Show)

data ProofSearchQuery = ProofSearchQuery {
  sig :: U.Signature
  , ctx :: U.Context
  , typ :: U.Preterm DTT
  } deriving (Eq, Show)

type ProofSearchResult = [Tree (U.Judgment DTT) UDTTrule]

type Prover = ProofSearchSetting -> ProofSearchQuery -> ProofSearchResult

{-
instance MathML ProofSearchResult where
  toMathML results =
    let n = length results in
    T.concat $ map (\(i,r) ->
      T.concat [
        "<mrow>",
        T.pack $ show i,
        "th result out of ",
        T.pack $ show n,
        "</mrow><mrow>",
        toMathML $ U.fromDeBruijnJudgment r,
        "</mrow>"
        ]) $ zip [1..] results
-}

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
