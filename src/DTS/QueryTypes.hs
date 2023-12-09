{-# LANGUAGE DuplicateRecordFields, TypeSynonymInstances, FlexibleInstances #-}

module DTS.QueryTypes (
  -- * Type system of UDTT
  DTTrule(..)
  -- * UDTT type check
  , TypeCheckQuery(..)
  , TypeCheckResult(..)
  , TypeChecker
  , TypeInferQuery(..)
  , TypeInfer
  -- * DTT proof search
  , LogicSystem(..)
  , ProofSearchSetting(..)
  , ProofSearchQuery(..)
  , ProofSearchResult(..)
  , Prover
  ) where

import Data.Bifunctor (second)       --base
import qualified Data.Text.Lazy as T --text
import Interface.Text
import Interface.TeX
import Interface.HTML
import Interface.Tree
import qualified DTS.UDTTdeBruijn as U
import qualified DTS.UDTTvarName as VN
import DTS.Labels (UDTT,DTT)

data DTTrule = Var | Con | TypeF | Conv | WK | PiF | PiI | PiE | SigmaF | SigmaI | SigmaE | DisjF | DisjI | DisjE | EnumF | EnumI | EnumE | IqF | IqI | IqE | NatF | NatI | NatE deriving (Eq, Show, Read)

instance SimpleText DTTrule where
  toText = T.pack . show
instance Typeset DTTrule where
  toTeX = T.pack . show
instance MathML DTTrule where
  toMathML rule = T.concat [
    "<mi>",
    case rule of
      PiF -> "ΠF"
      PiI -> "ΠI"
      PiE -> "ΠE"
      SigmaF -> "ΣF"
      SigmaI -> "ΣI"
      SigmaE -> "ΣE"
      DisjF -> "+F"
      DisjI -> "+I"
      DisjE -> "+E"
      EnumF -> "{}F"
      EnumI -> "{}I"
      EnumE -> "{}E"
      IqF -> "=F"
      IqI -> "=I"
      IqE -> "=E"
      NatF -> "NatF"
      NatI -> "NatI"
      NatE -> "NatE"
      _ -> T.pack $ show rule,
    "</mi>"
    ]

-- | Type checking in DTT

data TypeCheckQuery = TypeCheckQuery {
  sig :: U.Signature
  , ctx :: U.Context
  , trm :: U.Preterm UDTT
  , typ :: U.Preterm DTT
  } deriving (Eq, Show)

type TypeCheckResult = [Tree DTTrule (U.Judgment DTT)]

type TypeChecker = Prover -> TypeCheckQuery -> TypeCheckResult

data TypeInferQuery = TypeInferQuery {
  sig :: U.Signature
  , ctx :: U.Context
  , trm :: U.Preterm UDTT
  } deriving (Eq, Show)

type TypeInfer = Prover -> TypeInferQuery -> TypeCheckResult

-- | Proof Search in DTT

data LogicSystem = Intuitionistic | Classical deriving (Eq, Show)

data ProofSearchSetting = ProofSearchSetting {
  maxDepth :: Maybe Int
  , maxTime :: Maybe Int
  , system :: Maybe LogicSystem
  } deriving (Eq, Show)

data ProofSearchQuery = ProofSearchQuery {
  sig :: U.Signature
  , ctx :: U.Context
  , typ :: U.Preterm DTT
  } deriving (Eq, Show)

type ProofSearchResult = [Tree DTTrule (U.Judgment DTT)]

type Prover = ProofSearchSetting -> ProofSearchQuery -> ProofSearchResult

instance MathML ProofSearchResult where
  toMathML results =
    let n = length results in
    T.concat $ map (\(i,diagram) ->
      T.concat [
        "<mrow>",
        T.pack $ show i,
        "th result out of ",
        T.pack $ show n,
        "</mrow><mrow>",
        toMathML $ second U.fromDeBruijnJudgment diagram,
        "</mrow>"
        ]) $ zip [1..] results

{-
data TypeCheckError = IndexOutOfBounds

instance Show TypeCheckError where
  show IndexOutOfBounds = "Index out of bounds"

instance SimpleText TypeCheckError where
  toText = T.pack . show
instance Typeset TypeCheckError where
  toTeX = T.pack . show
instance MathML TypeCheckError where
  toMathML = T.pack . show
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

--data ProofSearchResult = Diagrams [Tree RuleName Judgment] deriving (Eq, Show)

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
