{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Underspecified Dependent Type Theory (with variable names)
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

-}
module DTS.DependentTypesWVN (
  -- * Types
  Preterm(..),
  Selector(..),
  ) where

import qualified Data.Text.Lazy as T
import DTS.SimpleText

-- | Preterms of Underspecified Dependent Type Theory (UDTT).
data Preterm =
  Var T.Text |               -- ^ Variable
  Con T.Text |            -- ^ Constant symbol
  Type |                  -- ^ The sort \"type\"
  Kind |                  -- ^ The sort \"kind\"
  Pi T.Text Preterm Preterm |    -- ^ Dependent function type (or Pi type)
  Not Preterm |           -- ^ Negation
  Lam T.Text Preterm |           -- ^ Lambda abstraction
  App Preterm Preterm |   -- ^ Function Application
  Sigma T.Text Preterm Preterm | -- ^ Dependent product type (or Sigma type)
  Pair Preterm Preterm |  -- ^ Pair
  Proj Selector Preterm | -- ^ (First and second) Projections
  Lamvec T.Text Preterm |        -- ^ Variable-length lambda abstraction
  Appvec T.Text Preterm |    -- ^ Variable-length function application
  Unit |                  -- ^ The unit term (of type Top)
  Top |                   -- ^ The top type
  Bot |                   -- ^ The bottom type
  Asp Int Preterm |       -- ^ The asperand term (or Underspesified term)
  Nat |                   -- ^ The natural number type (Nat)
  Zero |                  -- ^ 0 (of type Nat)
  Succ Preterm |          -- ^ The successor function
  Natrec Preterm Preterm Preterm | -- ^ natrec
  Eq Preterm Preterm Preterm |     -- ^ Intensional equality type
  Refl Preterm Preterm |           -- ^ refl
  Idpeel Preterm Preterm           -- ^ idpeel
    deriving (Eq)

-- | 'Proj' 'Fst' m is the first projection of m, while 'Proj' 'Snd' m is the second projection of m.
data Selector = Fst | Snd
  deriving (Eq, Show)

-- | translates a selector into either 1 or 2.
instance SimpleText Selector where
  toText Fst = "1"  -- `Proj` `Fst` m is the first projection of m
  toText Snd = "2" -- `Proj` `Snd` m is the second projection of m

instance Show Preterm where
  show = T.unpack . toText

instance SimpleText Preterm where
  toText preterm = case preterm of
    Var vname -> vname
    Con cname -> cname
    Type -> "type"
    Kind -> "kind"
    Pi vname a b -> T.concat ["(", vname, ":", toText a, ")→ ", toText b]
    Not a -> T.concat["¬", toText a]
    Lam vname m -> T.concat ["λ", vname, ".", toText m]
    App (App (Con cname) y) x -> 
      T.concat [cname, "(", toText x, ",", toText y,")"]
    App (App (App (Con cname) z) y) x -> 
      T.concat [cname, "(", toText x, ",", toText y,",",toText z,")"]
    App (App (App (App (Con cname) u) z) y) x -> 
      T.concat [cname, "(", toText x, ",", toText y,",",toText z,",", toText u, ")"]
    App m n -> T.concat [toText m, "(", toText n, ")"]
    Sigma vname a b -> case b of 
                         Top -> T.concat ["(", toText a, ")"]
                         _   -> T.concat ["(", vname, ":", toText a, ")× ", toText b]
    Pair m n  -> T.concat ["(", toText m, ",", toText n, ")"]
    Proj s m  -> T.concat ["π", toText s, "(", toText m, ")"]
    Lamvec vname m  -> T.concat ["λ", vname, "+.", toText m]
    Appvec vname m -> T.concat ["(", toText m, " ", vname, "+)"]
    Unit       -> "()"
    Top        -> "T"
    Bot        -> "⊥"
    Asp j m    -> T.concat["@", T.pack (show j), ":", toText m]
    Nat    -> "N"
    Zero   -> "0"
    Succ n -> T.concat ["s", toText n]
    Natrec n e f -> T.concat ["natrec(", toText n, ",", toText e, ",", toText f, ")"]
    Eq a m n -> T.concat [toText m, "=[", toText a, "]", toText n]
    Refl a m -> T.concat ["refl", toText a, "(", toText m, ")"]
    Idpeel m n -> T.concat ["idpeel(", toText m, ",", toText n, ")"]


