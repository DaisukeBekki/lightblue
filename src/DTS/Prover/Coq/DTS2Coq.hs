{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : DTS.Prover.Coq.DTS2Coq
Description : 
Copyright   : 
Licence     : 
Maintainer  : 
Stability   : beta
-}

module DTS.DTS2Coq (
  sigmaElimination,
  dts2coq
  ) where

import qualified Data.Text.Lazy as T
import qualified DTS.UDTT as DTS

sigmaElimination :: DTS.Preterm -> DTS.Preterm
sigmaElimiantion preterm = case preterm of
    Var i   -> Var i
    Con c   -> Con c
    Type    -> Type
    Kind    -> Kind
    Pi a b  -> Pi a b 
    Not m   -> Not m
    Lam m   -> Lam m
    App m n -> App m n
    Sigma a b  -> Sigma a b
    Pair m n   -> Pair m n
    Proj s m   -> Proj s m
    Lamvec m   -> Lamvec m
    Appvec i m -> Appvec i m
    Unit  -> Unit
    Top   -> Top
    Bot   -> Bot
    Asp i m -> Asp i m
    Nat   -> Nat
    Zero  -> Zero
    Succ n -> Succ n
    Natrec n e f -> Natrec n e f
    Eq a m n -> Eq a m n
    Refl a m -> Refl a m
    Idpeel m n -> Idpell m n

dts2coq :: DTS.Preterm -> T.Text
dts2coq preterm = case preterm of
    Var i   -> T.pack (show i)
    Con c   -> c
    Type    -> "type"
    Kind    -> "kind"
    Pi a b  -> T.concat["(Π ", toText a, ")", toText b]
    Not m   -> T.concat["¬", toText m]
    Lam m   -> T.concat["λ.", toText m]
    App m n -> T.concat["(", toText m, " ", toText n, ")"]
    Sigma a b  -> T.concat["(Σ ", toText a, ")", toText b]
    Pair m n   -> T.concat["(", toText m, ",", toText n, ")"]
    Proj s m   -> T.concat["π", toText s, "(", toText m, ")"] 
    Lamvec m   -> T.concat ["λ+.", toText m]
    Appvec i m -> T.concat ["(", toText m, " ", T.pack (show i), "+)"]
    Unit  -> "()"
    Top   -> "T"
    Bot   -> "⊥"
    Asp i m -> T.concat["@", T.pack (show i), ":", toText m]
    Nat   -> "N"
    Zero  -> "0"
    Succ n -> T.concat ["s", toText n]
    Natrec n e f -> T.concat ["natrec(", toText n, ",", toText e, ",", toText f, ")"]
    Eq a m n -> T.concat [toText m, "=[", toText a, "]", toText n]
    Refl a m -> T.concat ["refl", toText a, "(", toText m, ")"]
    Idpeel m n -> T.concat ["idpeel(", toText m, ",", toText n, ")"]
