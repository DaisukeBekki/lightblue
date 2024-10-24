{-# LANGUAGE DuplicateRecordFields, TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}

module DTS.GeneralTypeQuery (
  -- * type query
  QueryGoal(..)
  , GeneralTypeQuery(..)
  ) where

import qualified Data.Text.Lazy    as T --text
import Interface.Text
import Interface.TeX
import Interface.HTML

-- | (general) Query data type (mainly for printing)

data QueryGoal a = Term a | Question deriving (Eq)

instance (SimpleText a) => Show (QueryGoal a) where 
  show = T.unpack . toText
instance (SimpleText a) => SimpleText (QueryGoal a) where
  toText (Term t) = toText t
  toText Question = "?"
instance (Typeset a) => Typeset (QueryGoal a) where
  toTeX (Term t) = toTeX t
  toTeX Question = "?"
instance (MathML a) => MathML (QueryGoal a) where
  toMathML (Term t) = toMathML t
  toMathML Question = "<mi>?</mi>"

data GeneralTypeQuery a b c d = GeneralTypeQuery a b (QueryGoal c) (QueryGoal d) deriving (Eq)

instance (SimpleText a, SimpleText b, SimpleText c, SimpleText d) => Show (GeneralTypeQuery a b c d) where 
  show = T.unpack . toText
instance (SimpleText a, SimpleText b, SimpleText c, SimpleText d) => SimpleText (GeneralTypeQuery a b c d) where
  toText (GeneralTypeQuery _ cxt trm typ) = T.concat [toText cxt, " |- ", toText trm, ":", toText typ]
instance (Typeset a, Typeset b, Typeset c, Typeset d) => Typeset (GeneralTypeQuery a b c d) where
  toTeX (GeneralTypeQuery _ cxt trm typ) = T.concat [toTeX cxt, "{\\vdash}", toTeX trm, "{:}", toTeX typ]
instance (MathML a, MathML b, MathML c, MathML d) => MathML (GeneralTypeQuery a b c d) where
  toMathML (GeneralTypeQuery _ cxt trm typ) = T.concat ["<mrow>", toMathML cxt, "<mo>&vdash;</mo>", toMathML trm, "<mo>:</mo>", toMathML typ, "</mrow>"]


