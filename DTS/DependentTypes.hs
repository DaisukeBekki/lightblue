{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

{-|
Description : Underspecified Dependent Type Theory (in de Bruijn index)
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Implementation of Underspecified Dependent Type Theory (Bekki forthcoming).

-}
module DTS.DependentTypes (
  -- * Types
  Preterm(..),
  Selector(..),
  Signature,
  printSignatures,
  toTextDeBruijn,
  -- * De Bruijn notation <-> Variable-name notation
  fromDeBruijn,
  toDeBruijn,
  -- * Syntactic Operations
  subst,
  addLambda,
  deleteLambda,
  -- * Computations
  betaReduce,
  add,
  multiply,
  -- * Utility
  currying
  ) where

import qualified Data.Text.Lazy as T
import qualified Data.List as L
import qualified DTS.DependentTypesWVN as DTSWVN
import Interface.Text

-- | Preterms of Underspecified Dependent Type Theory (UDTT).
data Preterm =
  Var Int |               -- ^ Variable
  Con T.Text |            -- ^ Constant symbol
  Type |                  -- ^ The sort \"type\"
  Kind |                  -- ^ The sort \"kind\"
  Pi Preterm Preterm |    -- ^ Dependent function type (or Pi type)
  Not Preterm |           -- ^ Negation
  Lam Preterm |           -- ^ Lambda abstraction
  App Preterm Preterm |   -- ^ Function Application
  Sigma Preterm Preterm | -- ^ Dependent product type (or Sigma type)
  Pair Preterm Preterm |  -- ^ Pair
  Proj Selector Preterm | -- ^ (First and second) Projections
  Lamvec Preterm |        -- ^ Variable-length lambda abstraction
  Appvec Int Preterm |    -- ^ Variable-length function application
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
    deriving (Eq, Show)

-- | translates a term into a simple text notation.
instance SimpleText Preterm where
  toText = toText . fromDeBruijn

-- | 'Proj' 'Fst' m is the first projection of m, while 'Proj' 'Snd' m is the second projection of m.
data Selector = Fst | Snd
  deriving (Eq, Show)

-- | translates a selector into either 1 or 2.
instance SimpleText Selector where
  toText Fst = "1"  -- `Proj` `Fst` m is the first projection of m
  toText Snd = "2" -- `Proj` `Snd` m is the second projection of m

-- | A type of an element of a type signature, that is, a list of pairs of a preterm and a type.
-- ex. [entity:type, state:type, event:type, student:entity->type]
type Signature = (T.Text,Preterm)

instance SimpleText Signature where
  toText (cname,typ) = T.concat $ [toText (Con cname), ":", toText typ]

-- | prints a signature in text.
printSignatures :: [Signature] -> T.Text
printSignatures sigs = T.concat ["[", (T.intercalate ", " $ map toText sigs), "]"]

-- | prints a preterm in text, in the De Bruijn style.
toTextDeBruijn :: Preterm -> T.Text
toTextDeBruijn preterm = case preterm of
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

fromDeBruijn :: Preterm -> DTSWVN.Preterm
fromDeBruijn = fromDeBruijn2 [] 0

fromDeBruijn2 :: [T.Text] -> Int -> Preterm -> DTSWVN.Preterm
fromDeBruijn2 vnames i preterm = case preterm of
  Var j -> if j < length vnames
                  then DTSWVN.Var (vnames!!j)
                  else DTSWVN.Var $ T.concat ["error: var ",T.pack (show j), " in ", T.pack (show vnames)]
  Con cname -> DTSWVN.Con cname
  Type -> DTSWVN.Type
  Kind -> DTSWVN.Kind
  Pi a b -> 
    let vname = case a of
                  Con cname | cname == "entity" -> T.concat ["x", T.pack (show i)] 
                                | cname == "event" -> T.concat ["e", T.pack (show i)] 
                                | cname == "state" -> T.concat ["s", T.pack (show i)] 
                  Type -> T.concat ["p", T.pack (show i)] 
                  Kind -> T.concat ["p", T.pack (show i)] 
                  App _ _ -> T.concat ["u", T.pack (show i)] 
                  Sigma _ _ -> T.concat ["u", T.pack (show i)] 
                  Pi _ _ -> T.concat ["u", T.pack (show i)] 
                  Not _  -> T.concat ["u", T.pack (show i)] 
                  Appvec _ _ -> T.concat ["u", T.pack (show i)] 
                  Eq _ _ _ -> T.concat ["s", T.pack (show i)] 
                  Nat -> T.concat ["k", T.pack (show i)] 
                  _ -> T.concat ["x", T.pack (show i)] in
    DTSWVN.Pi vname (fromDeBruijn2 vnames (i+1) a) (fromDeBruijn2 (vname:vnames) (i+1) b)
  Not a   -> DTSWVN.Not (fromDeBruijn2 vnames i a)
  Lam m   -> 
    let vname = case m of
                  Sigma _ _ -> T.concat ["x", T.pack (show i)] 
                  Pi _ _ -> T.concat ["x", T.pack (show i)] 
                  _ -> T.concat ["x", T.pack (show i)] in
    DTSWVN.Lam vname (fromDeBruijn2 (vname:vnames) (i+1) m)
  App m n -> DTSWVN.App (fromDeBruijn2 vnames i m) (fromDeBruijn2 vnames i n)
  Sigma a b -> 
    let vname = case a of
                  Con cname | cname == "entity" -> T.concat ["x", T.pack (show i)] 
                                | cname == "event" -> T.concat ["e", T.pack (show i)] 
                                | cname == "state" -> T.concat ["s", T.pack (show i)] 
                  App _ _ -> T.concat ["u", T.pack (show i)] 
                  Sigma _ _ -> T.concat ["u", T.pack (show i)] 
                  Pi _ _ -> T.concat ["u", T.pack (show i)] 
                  Not _  -> T.concat ["u", T.pack (show i)] 
                  Appvec _ _ -> T.concat ["u", T.pack (show i)] 
                  Type -> T.concat ["p", T.pack (show i)] 
                  Kind -> T.concat ["p", T.pack (show i)] 
                  Eq _ _ _ -> T.concat ["s", T.pack (show i)] 
                  Nat -> T.concat ["k", T.pack (show i)] 
                  _ -> T.concat ["x", T.pack (show i)] in
    DTSWVN.Sigma vname (fromDeBruijn2 vnames (i+1) a) (fromDeBruijn2 (vname:vnames) (i+1) b)
  Pair m n  -> DTSWVN.Pair (fromDeBruijn2 vnames i m) (fromDeBruijn2 vnames i n)
  Proj s m  -> case s of
                 Fst -> DTSWVN.Proj DTSWVN.Fst (fromDeBruijn2 vnames i m)
                 Snd -> DTSWVN.Proj DTSWVN.Snd (fromDeBruijn2 vnames i m)
  Lamvec m  -> let vname = T.concat ["x", T.pack (show i)] in
                   DTSWVN.Lamvec vname (fromDeBruijn2 (vname:vnames) (i+1) m)
  Appvec j m -> let vname = if j < (length vnames) 
                                   then vnames!!j
                                   else T.concat ["error: var+ ", T.pack (show j)] in
                    DTSWVN.Appvec vname (fromDeBruijn2 (vname:vnames) i m)
  Unit       -> DTSWVN.Unit
  Top        -> DTSWVN.Top
  Bot        -> DTSWVN.Bot
  Asp j m    -> DTSWVN.Asp j (fromDeBruijn2 vnames i m)
  Nat    -> DTSWVN.Nat
  Zero   -> DTSWVN.Zero
  Succ n -> DTSWVN.Succ (fromDeBruijn2 vnames i n)
  Natrec n e f -> DTSWVN.Natrec (fromDeBruijn2 vnames i n) (fromDeBruijn2 vnames i e) (fromDeBruijn2 vnames i f)
  Eq a m n -> DTSWVN.Eq (fromDeBruijn2 vnames i a) (fromDeBruijn2 vnames i m) (fromDeBruijn2 vnames i n)
  Refl a m -> DTSWVN.Refl (fromDeBruijn2 vnames i a) (fromDeBruijn2 vnames i m)
  Idpeel m n -> DTSWVN.Idpeel (fromDeBruijn2 vnames i m) (fromDeBruijn2 vnames i n)

toDeBruijn :: DTSWVN.Preterm -> Preterm
toDeBruijn = toDeBruijn2 []

toDeBruijn2 :: [T.Text] -> DTSWVN.Preterm -> Preterm
toDeBruijn2 vnames preterm = case preterm of
  DTSWVN.Var vname -> case L.elemIndex vname vnames of
                        Just i -> Var i
                        Nothing -> Type
  DTSWVN.Con cname -> Con cname
  DTSWVN.Type -> Type
  DTSWVN.Kind -> Kind
  DTSWVN.Pi vname a b -> Pi (toDeBruijn2 (vname:vnames) a) (toDeBruijn2 (vname:vnames) b)
  DTSWVN.Not a -> Not (toDeBruijn2 vnames a)
  DTSWVN.Lam vname m -> Lam (toDeBruijn2 (vname:vnames) m)
  DTSWVN.App m n -> App (toDeBruijn2 vnames m) (toDeBruijn2 vnames n)
  DTSWVN.Sigma vname a b -> Sigma (toDeBruijn2 (vname:vnames) a) (toDeBruijn2 (vname:vnames) b)
  DTSWVN.Pair m n -> Pair (toDeBruijn2 vnames m) (toDeBruijn2 vnames n)
  _ -> Type

{-
-- | translates a term, given a context (=a list of variable names), into a non-de-Bruijn notation (i.e. each variable is printed with a variable name).
toTextWithVN :: [T.Text] -> Preterm -> T.Text
toTextWithVN varlist term = toTextWithVNLoop varlist term 0

-- | 
toTextWithVNLoop :: [T.Text] -> Preterm -> Int -> T.Text
toTextWithVNLoop vlist preterm i = case preterm of
  Var j -> if j < (length vlist)
           then vlist!!j
           else  T.concat ["error: var ",T.pack (show j), " in ", T.pack (show vlist)]
  Con c -> c
  Type  -> "type"
  Kind  -> "kind"
  Pi a b  -> let varname = case a of
                             Con "entity" -> T.concat ["x", T.pack (show i)] 
                             Con "event" -> T.concat ["e", T.pack (show i)] 
                             Con "state" -> T.concat ["s", T.pack (show i)] 
                             App _ _ -> T.concat ["u", T.pack (show i)] 
                             Sigma _ _ -> T.concat ["u", T.pack (show i)] 
                             Pi _ _ -> T.concat ["u", T.pack (show i)] 
                             Not _  -> T.concat ["u", T.pack (show i)] 
                             Appvec _ _ -> T.concat ["u", T.pack (show i)] 
                             Type -> T.concat ["p", T.pack (show i)] 
                             Kind -> T.concat ["p", T.pack (show i)] 
                             Eq _ _ _ -> T.concat ["s", T.pack (show i)] 
                             Nat -> T.concat ["k", T.pack (show i)] 
                             _ -> T.concat ["x", T.pack (show i)] in
             T.concat ["(", varname, ":", toTextWithVNLoop vlist a (i+1), ")→ ", toTextWithVNLoop (varname:vlist) b (i+1)]
  Not a   -> T.concat["¬", toTextWithVNLoop vlist a i]
  Lam m   -> let varname = case m of
                              Sigma _ _ -> T.concat ["x", T.pack (show i)] 
                              Pi _ _ -> T.concat ["x", T.pack (show i)] 
                              _ -> T.concat ["x", T.pack (show i)] in
             T.concat ["λ", varname, ".", toTextWithVNLoop (varname:vlist) m (i+1)]
  (App (App (Con c) y) x) -> T.concat [c, "(", toTextWithVNLoop vlist x i, ",", toTextWithVNLoop vlist y i,")"]
  (App (App (App (Con c) z) y) x) -> T.concat [c, "(", toTextWithVNLoop vlist x i, ",", toTextWithVNLoop vlist y i,",",toTextWithVNLoop vlist z i,")"]
  (App (App (App (App (Con c) u) z) y) x) -> T.concat [c, "(", toTextWithVNLoop vlist x i, ",", toTextWithVNLoop vlist y i,",",toTextWithVNLoop vlist z i,",", toTextWithVNLoop vlist u i, ")"]
  App m n -> T.concat [toTextWithVNLoop vlist m i, "(", toTextWithVNLoop vlist n i, ")"]
  Sigma a b -> let varname = case a of
                               Con "entity" -> T.concat ["x", T.pack (show i)] 
                               Con "event" -> T.concat ["e", T.pack (show i)] 
                               Con "state" -> T.concat ["s", T.pack (show i)] 
                               App _ _ -> T.concat ["u", T.pack (show i)] 
                               Sigma _ _ -> T.concat ["u", T.pack (show i)] 
                               Pi _ _ -> T.concat ["u", T.pack (show i)] 
                               Not _  -> T.concat ["u", T.pack (show i)] 
                               Appvec _ _ -> T.concat ["u", T.pack (show i)] 
                               Type -> T.concat ["p", T.pack (show i)] 
                               Kind -> T.concat ["p", T.pack (show i)] 
                               Eq _ _ _ -> T.concat ["s", T.pack (show i)] 
                               Nat -> T.concat ["k", T.pack (show i)] 
                               _ -> T.concat ["x", T.pack (show i)] in
               case b of 
                 Top -> T.concat ["(", toTextWithVNLoop vlist a (i+1), ")"]
                 _   -> T.concat ["(", varname, ":", toTextWithVNLoop vlist a (i+1), ")× ", toTextWithVNLoop (varname:vlist) b (i+1)]
  Pair m n  -> T.concat ["(", toTextWithVNLoop vlist m i, ",", toTextWithVNLoop vlist n i, ")"]
  Proj s m  -> T.concat ["π", toText s, "(", toTextWithVNLoop vlist m i, ")"]
  Lamvec m  -> let varname = T.concat ["x", T.pack (show i)] in
               T.concat ["λ", varname, "+.", toTextWithVNLoop (varname:vlist) m (i+1)]
  Appvec j m -> if j < (length vlist)
                then T.concat ["(", toTextWithVNLoop vlist m i, " ", vlist!!j, "+)"]
                else T.concat ["(", toTextWithVNLoop vlist m i, " error: var+ ", T.pack (show j), "+)"]
  Unit       -> "()"
  Top        -> "T"
  Bot        -> "⊥"
  Asp j m    -> T.concat["@", T.pack (show j), ":", toTextWithVNLoop vlist m i]
  Nat    -> "N"
  Zero   -> "0"
  Succ n -> T.concat ["s", toTextWithVNLoop vlist n i]
  Natrec n e f -> T.concat ["natrec(", toTextWithVNLoop vlist n i, ",", toTextWithVNLoop vlist e i, ",", toTextWithVNLoop vlist f i, ")"]
  Eq a m n -> T.concat [toTextWithVNLoop vlist m i, "=[", toTextWithVNLoop vlist a i, "]", toTextWithVNLoop vlist n i]
  Refl a m -> T.concat ["refl", toTextWithVNLoop vlist a i, "(", toTextWithVNLoop vlist m i, ")"]
  Idpeel m n -> T.concat ["idpeel(", toTextWithVNLoop vlist m i, ",", toTextWithVNLoop vlist n i, ")"]
-}

-- | Substitution of the variable i in a preterm M with a preterm L
--   "subst M L i" = M[L/i]
subst :: Preterm -> Preterm -> Int -> Preterm
subst preterm l i = case preterm of
  Var j  -> if i == j
            then l
            else (Var j)
  Con c  -> Con c
  Type   -> Type
  Kind   -> Kind
  Pi a b -> Pi (subst a l i) (subst b (shiftIndices l 1 0) (i+1))
  Not m  -> Not (subst m l i)
  Lam m      -> Lam (subst m (shiftIndices l 1 0) (i+1))
  App m n    -> App (subst m l i) (subst n l i)
  Sigma a b  -> Sigma (subst a l i) (subst b (shiftIndices l 1 0) (i+1))
  Pair m n   -> Pair (subst m l i) (subst n l i)
  Proj s m   -> Proj s (subst m l i)
  Lamvec m   -> Lamvec (subst m (shiftIndices l 1 0) (i+1))
  Appvec j m -> Appvec j (subst m l i)
  Unit    -> Unit
  Top     -> Top
  Bot     -> Bot
  Asp j m -> Asp j (subst m l i)
  Nat     -> Nat
  Zero    -> Zero
  Succ n  -> Succ (subst n l i)
  Natrec n e f -> Natrec (subst n l i) (subst e l i) (subst f l i)
  Eq a m n -> Eq (subst a l i) (subst m l i) (subst n l i)
  Refl a m -> Refl (subst a l i) (subst m l i)
  Idpeel m n -> Idpeel (subst m l i) (subst n l i)

-- | shiftIndices m d i
--   add d to all the indices more than i within m (=d-place shift)
shiftIndices :: Preterm -> Int -> Int -> Preterm
shiftIndices preterm d i = case preterm of
  Var j      -> if j >= i 
                then Var (j+d)
                else Var j
  Pi a b     -> Pi (shiftIndices a d i) (shiftIndices b d (i+1))
  Not m      -> Not (shiftIndices m d (i+1))
  Lam m      -> Lam (shiftIndices m d (i+1))
  App m n    -> App (shiftIndices m d i) (shiftIndices n d i)
  Sigma a b  -> Sigma (shiftIndices a d i) (shiftIndices b d (i+1))
  Pair m n   -> Pair (shiftIndices m d i) (shiftIndices n d i)
  Proj s m   -> Proj s (shiftIndices m d i)
  Lamvec m   -> Lamvec (shiftIndices m d (i+1))
  Appvec j m -> if j >= i
                then Appvec (j+d) (shiftIndices m d i)
                else Appvec j (shiftIndices m d i)
  Asp j m    -> Asp j (shiftIndices m d i)
  Succ n     -> Succ (shiftIndices n d i)
  Natrec n e f -> Natrec (shiftIndices n d i) (shiftIndices e d i) (shiftIndices f d i)
  Eq a m n   -> Eq (shiftIndices a d i) (shiftIndices m d i) (shiftIndices n d i)
  Refl a m   -> Refl (shiftIndices a d i) (shiftIndices m d i)
  Idpeel m n -> Idpeel (shiftIndices m d i) (shiftIndices n d i)
  t -> t

-- | Beta reduction
betaReduce :: Preterm -> Preterm
betaReduce preterm = case preterm of
  Var i  -> Var i
  Con c  -> Con c
  Type   -> Type
  Kind   -> Kind
  Pi a b -> Pi (betaReduce a) (betaReduce b)
  Not a  -> Not (betaReduce a)
  Lam m  -> Lam (betaReduce m)
  App m n -> case betaReduce m of
    Lam v -> betaReduce (shiftIndices (subst v (shiftIndices n 1 0) 0) (-1) 0)
    e -> App e (betaReduce n)
  Sigma a b -> Sigma (betaReduce a) (betaReduce b)
  Pair m n  -> Pair (betaReduce m) (betaReduce n)
  Proj s m  -> case betaReduce m of
    Pair x y -> case s of
                  Fst -> x
                  Snd -> y
    e -> Proj s e
  Lamvec m   -> Lamvec (betaReduce m)
  Appvec i m -> Appvec i (betaReduce m)
  Unit -> Unit
  Top  -> Top
  Bot  -> Bot
  Asp i m -> Asp i (betaReduce m)
  Nat  -> Nat
  Zero -> Zero
  Succ n -> Succ (betaReduce n)
  Natrec n e f -> case betaReduce n of
                    Zero -> betaReduce e
                    Succ m -> betaReduce $ (App (App f m) (Natrec m e f))
                    m -> Natrec m (betaReduce e) (betaReduce f) -- Con $ T.concat ["Error in beta-reduction of Natrec: ", toText n]
  Eq a m n -> Eq (betaReduce a) (betaReduce m) (betaReduce n)
  Refl a m -> Refl (betaReduce a) (betaReduce m)
  Idpeel m n -> case betaReduce m of
                  Refl _ m' -> betaReduce $ (App n m')
                  m' -> Idpeel m' (betaReduce n)

-- | adds two preterms (of type `Nat`).
add :: Preterm -> Preterm -> Preterm
add m n = Natrec m n (Lam (Lam (Succ (Var 0))))

-- | multiplies two preterms (of type `Nat').
multiply :: Preterm -> Preterm -> Preterm
multiply m n = Natrec m Zero (Lam (Lam (add n (Var 0))))

-- | addLambda i preterm: the first subroutine for 'transvec' function.
-- this function takes an index and a preterm, transforms the latter in a way that the Var/Appvec with an index j that is equal or greater than i 
-- Ex.
-- addLambda 1 (Appvec 0 m) = Appvec 1 (addLambda 1 m)
-- addLambda 0 (Appvec 0 m) = Appvec 0 (App () (Var 1))
addLambda :: Int -> Preterm -> Preterm
addLambda i preterm = case preterm of
  Var j | j > i     -> Var (j+1)
        | j < i     -> Var j
        | otherwise -> Con $ T.concat [" Error: var ", T.pack (show j)]
  Pi a b     -> Pi (addLambda i a) (addLambda (i+1) b)
  Not a      -> Not (addLambda (i+1) a)
  Lam m      -> Lam (addLambda (i+1) m)
  App m n    -> App (addLambda i m) (addLambda i n)
  Sigma a b  -> Sigma (addLambda i a) (addLambda (i+1) b)
  Pair m n   -> Pair (addLambda i m) (addLambda i n)
  Proj s m   -> Proj s (addLambda i m)
  Lamvec m   -> Lamvec (addLambda (i+1) m)
  Appvec j m | j > i     -> Appvec (j+1) (addLambda i m)
             | j < i     -> Appvec j (addLambda i m)
             | otherwise -> Appvec j (App (addLambda i m) (Var (j+1)))
  Asp j m    -> Asp j (addLambda i m)
  t -> t

-- | deleteLambda i preterm: the second subroutine for 'transvec' function.
-- this function takes an index and a preterm, transforms the latter in a way that the Var/Appvec with an index j 
deleteLambda :: Int -> Preterm -> Preterm
deleteLambda i preterm = case preterm of
  Var j | j > i     -> Var (j-1)
        | j < i     -> Var j
        | otherwise -> Con $ T.concat ["Error: var ", T.pack (show j)]
  Pi a b     -> Pi (deleteLambda i a) (deleteLambda (i+1) b)
  Not a      -> Not (deleteLambda (i+1) a)
  Lam m      -> Lam (deleteLambda (i+1) m)
  App m n    -> App (deleteLambda i m) (deleteLambda i n)
  Sigma a b  -> Sigma (deleteLambda i a) (deleteLambda (i+1) b)
  Pair m n   -> Pair (deleteLambda i m) (deleteLambda i n)
  Proj s m   -> Proj s (deleteLambda i m)
  Lamvec m   -> Lamvec (deleteLambda (i+1) m)
  Appvec j m | j > i     -> Appvec (j-1) (deleteLambda i m)
             | j < i     -> Appvec j (deleteLambda i m)
             | otherwise -> deleteLambda i m
  Asp j m    -> Asp j (deleteLambda i m)
  t -> t

currying :: [Preterm] -> Preterm -> Preterm
currying [] preterm = App preterm (Lam Top)
currying (p:ps) preterm = Pi (App p (Lam Top)) (currying ps preterm)
