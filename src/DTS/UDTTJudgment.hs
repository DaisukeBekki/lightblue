{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}

module DTS.UDTTJudgment (
  Judgment(..)
  , fromDeBruijnJudgment
  , RuleName(..)
  -- * UDTT type checking
  , TypeCheckQuery(..)
  , TypeCheckResult(..)
  , TypeChecker
  ) where

import qualified Data.Text.Lazy as T
import Interface.Text (SimpleText(..))
import Interface.TeX (Typeset(..))
import Interface.HTML (MathML(..))
import Interface.Tree (Tree(..))
import qualified DTS.UDTT as UDTT
import qualified DTS.DTT as DTT
import qualified DTS.DTTJudgment as DTT
import qualified DTS.UDTTwithName as VN

-- | The data type for a judgment
-- | - UDTTのsignatureにはDTTのpretermしか現れない
data Judgment = Judgment {
  sig :: DTT.Signature
  , ctx :: DTT.Context -- ^ A context \Gamma in \Gamma \vdash M:A
  , trm :: UDTT.Preterm   -- ^ A term M in \Gamma \vdash M:A
  , typ :: DTT.Preterm     -- ^ A type A in \Gamma \vdash M:A
  } deriving (Eq, Show)

instance SimpleText Judgment where
  toText = toText . fromDeBruijnJudgment

instance Typeset Judgment where
  toTeX = toTeX . fromDeBruijnJudgment

instance MathML Judgment where
  toMathML = toMathML . fromDeBruijnJudgment

toUDTTJudgment :: DTT.Judgment -> Judgment
toUDTTJudgment (DTT.Judgment s c tm ty) = Judgment s c (DTT.toUDTT tm) ty

instance SimpleText DTT.Judgment where
  toText = toText . toUDTTJudgment

instance Typeset DTT.Judgment where
  toTeX = toTeX . toUDTTJudgment

instance MathML DTT.Judgment where
  toMathML = toMathML . toUDTTJudgment

-- | translates a judgment in de Bruijn notation into one with variable names
fromDeBruijnJudgment :: Judgment -> VN.Judgment
fromDeBruijnJudgment Judgment{..} =
  let (vsig', vcontext', vterm', vtyp')
        = UDTT.initializeIndex $ do
                            (varnames,vcontext) <- UDTT.fromDeBruijnContextLoop $ DTT.toUDTTcontext ctx
                            vterm <- UDTT.fromDeBruijn varnames trm
                            vtyp <- UDTT.fromDeBruijn varnames $ DTT.toUDTT typ
                            return (sig, vcontext, vterm, vtyp)
  in VN.Judgment vsig' vcontext' vterm' vtyp'

data RuleName = Var | Con | Typ | Asp | PiF | PiI | PiE | SigmaF | SigmaI | SigmaE | DisjF | DisjI | DisjE | EnumF | EnumI | EnumE | IqF | IqI | IqE | NumF | NumI | NumE deriving (Eq, Show, Read)

instance SimpleText RuleName where
  toText = T.pack . show

instance MathML RuleName where
  toMathML Var = "<mo>VAR</mo>"
  toMathML _ = "<err>"

instance Typeset RuleName where
  toTeX Var = "\\mathit{Var}"
  toTeX _ = ""

-- | Type checking in UDTT

data TypeCheckQuery = TypeCheckQuery {
  sig :: DTT.Signature
  , ctx :: UDTT.Context
  , trm :: UDTT.Preterm
  , typ :: UDTT.Preterm
  , prover :: DTT.Prover
  } 

data TypeCheckResult = TypeCheckResult {
  diagrams :: [Tree DTT.Judgment DTT.RuleName]
  } deriving (Eq, Show)

type TypeChecker = TypeCheckQuery -> TypeCheckResult

