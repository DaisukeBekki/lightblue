{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

{-|
Description : Dependent Type Theory (in de Bruijn index)
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

-}
module DTS.DTT (
  -- * Types
  Preterm(..),
  Selector(..),
  Signature,
  printSignature,
  toUDTT,
  toDTT
  ) where

import qualified Data.Text.Lazy as T      -- text
--import qualified Data.List as L           -- base
--import qualified Control.Applicative as M -- base
--import qualified Control.Monad as M       -- base
import qualified DTS.UDTT as UDTT
--import qualified DTS.UDTTwithName as VN
import Interface.Text
import Interface.TeX
import Interface.HTML

-- | 'Proj' 'Fst' m is the first projection of m, 
-- while 'Proj' 'Snd' m is the second projection of m.
data Selector = Fst | Snd deriving (Eq, Show)

-- | translates a selector into either 1 or 2.
instance SimpleText Selector where
  toText Fst = "1"
  toText Snd = "2"

-- | `Fst` is translated into \"1\", and `Snd` is into \"2\".
instance Typeset Selector where
  toTeX = toText

-- | Preterms of Underspecified Dependent Type Theory (DTT).
data Preterm =
  Var Int |               -- ^ Variables
  Con T.Text |            -- ^ Constant symbols
  Type |                  -- ^ The sort \"type\"
  Kind |                  -- ^ The sort \"kind\"
  Pi Preterm Preterm |    -- ^ Dependent function types (or Pi types)
  Not Preterm |           -- ^ Negations
  Lam Preterm |           -- ^ Lambda abstractions
  App Preterm Preterm |   -- ^ Function Applications
  Sigma Preterm Preterm | -- ^ Dependent product types (or Sigma types)
  Pair Preterm Preterm |  -- ^ Pairs
  Proj Selector Preterm | -- ^ (First and second) Projections
  Unit |                  -- ^ The unit term (of type Top)
  Top |                   -- ^ The top type
  Bot |                   -- ^ The bottom type
  Nat |                   -- ^ Natural number type (Nat)
  Zero |                  -- ^ 0 (of type Nat)
  Succ Preterm |          -- ^ The successor function
  Natrec Preterm Preterm Preterm | -- ^ natrec
  Eq Preterm Preterm Preterm |     -- ^ Intensional equality types
  Refl Preterm Preterm |           -- ^ refl
  Idpeel Preterm Preterm |         -- ^ idpeel
  DRel Int T.Text Preterm Preterm  -- ^ Discourse relations
  deriving (Eq)

instance Show Preterm where
  show = show . toUDTT

-- | translates a DTS preterm into a simple text notation.
instance SimpleText Preterm where
  toText = toText . toUDTT

-- | translates a DTS preterm into a tex source code.
instance Typeset Preterm where
  toTeX = toTeX . toUDTT

-- | translates a DTS preterm into a MathML notation.
instance MathML Preterm where
  toMathML = toMathML . toUDTT

-- | A type of an element of a type signature, that is, a list of pairs of a preterm and a type.
-- ex. [entity:type, state:type, event:type, student:entity->type]
type Signature = (T.Text,Preterm)

instance SimpleText Signature where
  toText = toText . toUDTTsig

-- | prints a signature in text.
printSignature :: [Signature] -> T.Text
printSignature = UDTT.printSignature . (map toUDTTsig)

-- | DTT to UDTT
toUDTT :: Preterm -> UDTT.Preterm
toUDTT preterm = case preterm of
  Var i -> UDTT.Var i
  Con t -> UDTT.Con t
  Type -> UDTT.Type
  Kind -> UDTT.Kind
  Pi a b -> UDTT.Pi (toUDTT a) (toUDTT b)
  Not a  -> UDTT.Not (toUDTT a)
  Lam m  -> UDTT.Lam (toUDTT m)
  App m n -> UDTT.App (toUDTT m) (toUDTT n)
  Sigma a b -> UDTT.Sigma (toUDTT a) (toUDTT b)
  Pair m n  -> UDTT.Pair (toUDTT m) (toUDTT n)
  Proj s m  -> UDTT.Proj (toUDTTselector s) (toUDTT m)
  Unit    -> UDTT.Unit
  Top     -> UDTT.Top
  Bot     -> UDTT.Bot
  Nat     -> UDTT.Nat
  Zero    -> UDTT.Zero
  Succ n  -> UDTT.Succ (toUDTT n)
  Natrec e f n -> UDTT.Natrec (toUDTT e) (toUDTT f) (toUDTT n)
  Eq a m n     -> UDTT.Eq (toUDTT a) (toUDTT m) (toUDTT n)
  Refl a m     -> UDTT.Refl (toUDTT a) (toUDTT m)
  Idpeel m n   -> UDTT.Idpeel (toUDTT m) (toUDTT n)
  DRel i t m n -> UDTT.DRel i t (toUDTT m) (toUDTT n)

toUDTTselector :: Selector -> UDTT.Selector
toUDTTselector Fst = UDTT.Fst
toUDTTselector Snd = UDTT.Snd

toUDTTsig :: Signature -> UDTT.Signature
toUDTTsig (cname,typ) = (cname, toUDTT typ)

-- | UDTT to DTT
toDTT :: UDTT.Preterm -> Preterm
toDTT preterm = case preterm of
  UDTT.Var i -> Var i
  UDTT.Con t -> Con t
  UDTT.Type  -> Type
  UDTT.Kind  -> Kind
  UDTT.Pi a b -> Pi (toDTT a) (toDTT b)
  UDTT.Not a   -> Not (toDTT a)
  UDTT.Lam m   -> Lam (toDTT m)
  UDTT.App m n -> App (toDTT m) (toDTT n)
  UDTT.Sigma a b -> Sigma (toDTT a) (toDTT b)
  UDTT.Pair m n  -> Pair (toDTT m) (toDTT n)
  UDTT.Proj s m  -> Proj (toDTTselector s) (toDTT m)
  UDTT.Asp _ _   -> Top
  UDTT.Lamvec m   -> toDTT m
  UDTT.Appvec _ m -> toDTT m
  UDTT.Unit   -> Unit
  UDTT.Top    -> Top
  UDTT.Bot    -> Bot
  UDTT.Nat    -> Nat
  UDTT.Zero   -> Zero
  UDTT.Succ n  -> Succ (toDTT n)
  UDTT.Natrec e f n -> Natrec (toDTT e) (toDTT f) (toDTT n)
  UDTT.Eq a m n     -> Eq (toDTT a) (toDTT m) (toDTT n)
  UDTT.Refl a m     -> Refl (toDTT a) (toDTT m)
  UDTT.Idpeel m n   -> Idpeel (toDTT m) (toDTT n)
  UDTT.DRel _ _ _ _ -> Top

toDTTselector :: UDTT.Selector -> Selector
toDTTselector UDTT.Fst = Fst
toDTTselector UDTT.Snd = Snd
