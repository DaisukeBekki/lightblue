{-# LANGUAGE DuplicateRecordFields, TypeSynonymInstances, FlexibleInstances, RecordWildCards, DeriveGeneric, DeriveAnyClass #-}

module DTS.QueryTypes (
  -- * Type system of UDTT
  DTTrule(..)
  -- * type query
  , DTTProofDiagram
  , TypeChecker
  , TypeInferer
  , LogicSystem(..)
  , ProofSearchSetting(..)
  , ProverBuilder
  , Prover
  -- * ListEx monad
  --, ListEx(..)
  --, exception
  --, record
  ) where

--import Data.Bifunctor (second)       --base
import qualified GHC.Generics   as G   --base
import Data.Store (Store(..))          --store
import ListT (ListT)                   --list-t
import qualified Data.Text.Lazy as T   --text
import Interface.Text
import Interface.TeX
import Interface.HTML
import Interface.Tree
import qualified DTS.DTTdeBruijn as DTTdB
import qualified DTS.UDTTdeBruijn as UDTTdB
import DTS.GeneralTypeQuery (GeneralTypeQuery(..))

-- BOTF?
data DTTrule = Var | Con | TypeF | Conv | WK | PiF | PiI | PiE | SigmaF | SigmaI | SigmaE | DisjF | DisjI | DisjE | EnumF | EnumI | EnumE | IqF | IqI | IqE | NatF | NatI | NatE deriving (Eq, Show, Read, G.Generic, Store, Enum, Bounded, Ord)

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

type DTTProofDiagram = Tree DTTrule (DTTdB.Judgment)

type TypeChecker = Prover -> Bool -> UDTTdB.TypeCheckQuery -> ListT IO DTTProofDiagram

type TypeInferer = Prover -> Bool -> UDTTdB.TypeInferQuery -> ListT IO DTTProofDiagram

data LogicSystem = Intuitionistic | Classical deriving (Eq, Show)

data ProofSearchSetting = ProofSearchSetting {
  maxDepth :: Maybe Int
  , maxTime :: Maybe Int
  , logicSystem :: Maybe LogicSystem
  } deriving (Eq, Show)

type Prover = DTTdB.ProofSearchQuery -> ListT IO DTTProofDiagram

type ProverBuilder = ProofSearchSetting -> Prover
 