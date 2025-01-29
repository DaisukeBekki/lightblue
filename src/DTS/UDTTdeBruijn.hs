{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass, TemplateHaskell #-}

{-|
Copyright   : (c) Daisuke Bekki, 2024
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Implementation of Underspecified Dependent Type Theory (Bekki forthcoming).
-}

module DTS.UDTTdeBruijn (
  -- * Terms and Types
  Selector(..)
  , Preterm(..)
  -- * General Syntactic Operations
  , subst
  , shiftIndices
  -- * Computations
  , betaReduce
  , strongBetaReduce
  , sigmaElimination
  , add
  , multiply
  -- * Variable Vectors
  , addLambda
  , deleteLambda
  , replaceLambda
  -- * Conversion btw. UDTT and DTT
  , toUDTT
  , toDTT
  -- * Judgment
  -- , Signature
  -- , Context
  , Judgment(..)
  , toUDTTJudgment
  , TypeCheckQuery
  , TypeInferQuery(..)
  ) where

import qualified GHC.Generics as G    --base
import qualified Data.Text.Lazy as T  --text
import Data.Store (Store(..))         --store
import Data.Store.TH (makeStore)      --store
import Interface.Text                 --lightblue
import Interface.TeX                  --lightblue
import Interface.HTML                 --lightblue
import DTS.GeneralTypeQuery           --lightblue
import qualified DTS.DTTdeBruijn  as DTTdB  --lightblue
-- import qualified DTS.UDTTwithName as UDTTwN --lightblue

-- | 'Proj' 'Fst' m is the first projection of m, while 'Proj' 'Snd' m is the second projection of m.
data Selector = Fst | Snd deriving (Eq, Show, G.Generic, Store)

-- | Print a selector as "1" or "2".
instance SimpleText Selector where
  toText Fst = "1"
  toText Snd = "2"

-- instance Typeset Selector where
--   toTeX = toText

-- instance MathML Selector where
--   toMathML Fst = "<mn>1</mn>"  -- `Proj` `Fst` m is the first projection of m
--   toMathML Snd = "<mn>2</mn>" -- `Proj` `Snd` m is the second projection of m

-- | Preterms of Underspecified Dependent Type Theory (UDTT).
data Preterm = 
  -- | Basic Preterms
  Var Int                       -- ^ Variables
  | Con T.Text                  -- ^ Constant symbols
  | Type                        -- ^ The sort \"type\"
  | Kind                        -- ^ The sort \"kind\"
  -- | Pi Types
  | Pi Preterm Preterm          -- ^ Pi types
  | Lam Preterm                 -- ^ Lambda abstractions
  | App Preterm Preterm         -- ^ Function Applications
  | Not Preterm                 -- ^ Negations
  -- | Sigma Types
  | Sigma Preterm Preterm       -- ^ Sigma types
  | Pair Preterm Preterm        -- ^ Pairs
  | Proj Selector Preterm       -- ^ (First and second) Projections
  -- | Disjoint Union Types
  | Disj Preterm Preterm        -- ^ Disjoint Union types
  | Iota Selector Preterm       -- ^ (FIrst and second) Injections
  | Unpack Preterm Preterm Preterm Preterm  -- ^ Unpack P L M N
  -- | Enumeration Types
  | Bot                          -- ^ The bottom type
  | Unit                         -- ^ The unit term (of type Top)
  | Top                          -- ^ The top type
  | Entity                       -- ^ The entity type
  -- | Natural Number Types
  | Nat                          -- ^ Natural number type (Nat)
  | Zero                         -- ^ 0 (of type Nat)
  | Succ Preterm                 -- ^ The successor function
  | Natrec Preterm Preterm Preterm Preterm -- ^ Natrec
  -- | Intensional Equality Types
  | Eq Preterm Preterm Preterm   -- ^ Intensional equality types
  | Refl Preterm Preterm         -- ^ refl
  | Idpeel Preterm Preterm Preterm -- ^ idpeel
  -- | UDTT extensions
  | Asp Preterm                 -- ^ Underspesified terms
  | Lamvec Preterm              -- ^ Lambda abstractions of a variable vector
  | Appvec Int Preterm          -- ^ Function applications of a variable vector
  | Ann Preterm DTTdB.Preterm         -- ^ Type annotation
  -- | ToDo: add First Universe
  deriving (Eq, G.Generic)

instance Store Preterm

-- makeStore ''T.Text

instance Show Preterm where
  show = T.unpack . toText

-- | translates a preterm into a simple text notation.
instance SimpleText Preterm where
  toText preterm = case preterm of
    Var i   -> T.pack (show i)
    Con c   -> c
    Type    -> "type"
    Kind    -> "kind"
    Pi a b  -> case b of
                 Bot -> T.concat ["¬", toText a]
                 b' -> T.concat ["(Π ", toText a, ")", toText b']
    Lam m   -> T.concat ["λ.", toText m]
    App m n -> T.concat ["(", toText m, " ", toText n, ")"]
    Not m   -> T.concat ["¬", toText m]
    Sigma a b  -> T.concat ["(Σ ", toText a, ")", toText b]
    Pair m n   -> T.concat ["(", toText m, ",", toText n, ")"]
    Proj s m   -> T.concat ["π", toText s, "(", toText m, ")"]
    Disj a b -> T.concat [toText a, " + ", toText b] 
    Iota s m -> T.concat ["ι", toText s, "(", toText m, ")"]
    Unpack p l m n -> T.concat ["unpack(", toText p, ",", toText l, ",", toText m, ",", toText n, ")"]
    Bot   -> "⊥"
    Unit  -> "()"
    Top   -> "T"
    Entity -> "entity"
    Nat   -> "N"
    Zero  -> "0"
    Succ n -> T.concat ["s", toText n]
    Natrec p n e f -> T.concat ["natrec(", toText p, ",", toText n, ",", toText e, ",", toText f, ")"]
    Eq a m n -> T.concat [toText m, "=[", toText a, "]", toText n]
    Refl a m -> T.concat ["refl", toText a, "(", toText m, ")"]
    Idpeel p e r -> T.concat ["idpeel(", toText p, ",", toText e, ",", toText r, ")"]
    Asp m -> T.concat["@", toText m]
    Lamvec m   -> T.concat ["λ+.", toText m]
    Appvec i m -> T.concat ["(", toText m, " ", T.pack (show i), "+)"]
    Ann m a -> T.concat ["(", toText m, "::", toText a, ")"]

-- | translates a DTS preterm into a tex source code.
instance Typeset Preterm where
  toTeX = toText

-- | translates a DTS preterm into a MathML notation.
instance MathML Preterm where
  toMathML = toText

{- Syntactic Operations -}

-- | Substitution of the variable i in a preterm M with a preterm L
--   "subst M L i" = M[L/i]
subst :: Preterm -> Preterm -> Int -> Preterm
subst preterm l i = case preterm of
  Var j  -> if i == j
               then l
               else Var j
  Con c  -> Con c
  Type   -> Type
  Kind   -> Kind
  Pi a b -> Pi (subst a l i) (subst b (shiftIndices l 1 0) (i+1))
  Lam m      -> Lam (subst m (shiftIndices l 1 0) (i+1))
  App m n    -> App (subst m l i) (subst n l i)
  Not m  -> Not (subst m l i)
  Sigma a b  -> Sigma (subst a l i) (subst b (shiftIndices l 1 0) (i+1))
  Pair m n   -> Pair (subst m l i) (subst n l i)
  Proj s m   -> Proj s (subst m l i)
  Disj a b   -> Disj (subst a l i) (subst b l i)
  Iota s m   -> Iota s (subst m l i)
  Unpack p h m n -> Unpack (subst p l i) (subst h l i) (subst m l i) (subst n l i)
  Bot        -> Bot
  Unit       -> Unit
  Top        -> Top
  Entity     -> Entity
  Nat        -> Nat
  Zero       -> Zero
  Succ n     -> Succ (subst n l i)
  Natrec p n e f -> Natrec (subst p l i) (subst n l i) (subst e l i) (subst f l i)
  Eq a m n   -> Eq (subst a l i) (subst m l i) (subst n l i)
  Refl a m   -> Refl (subst a l i) (subst m l i)
  Idpeel p e r -> Idpeel (subst p l i) (subst e l i) (subst r l i)
  Asp m      -> Asp (subst m l i)
  Lamvec m   -> Lamvec (subst m (shiftIndices l 1 0) (i+1))
  Appvec j m -> Appvec j (subst m l i)
  Ann m a    -> Ann (subst m l i) a

-- | shiftIndices m d i
-- add d to all the indices that is greater than or equal to i within m (=d-place shift)
shiftIndices :: Preterm -> Int -> Int -> Preterm
shiftIndices preterm d i = case preterm of
  Var j      -> if j >= i
                   then Var (j+d)
                   else Var j
  Pi a b     -> Pi (shiftIndices a d i) (shiftIndices b d (i+1))
  Lam m      -> Lam (shiftIndices m d (i+1))
  App m n    -> App (shiftIndices m d i) (shiftIndices n d i)
  Not m      -> Not (shiftIndices m d i)
  Sigma a b  -> Sigma (shiftIndices a d i) (shiftIndices b d (i+1))
  Pair m n   -> Pair (shiftIndices m d i) (shiftIndices n d i)
  Proj s m   -> Proj s (shiftIndices m d i)
  Disj a b   -> Disj (shiftIndices a d i) (shiftIndices b d i)
  Iota s m   -> Iota s (shiftIndices m d i)
  Unpack p l m n -> Unpack (shiftIndices p d i) (shiftIndices l d i) (shiftIndices m d i) (shiftIndices n d i)
  Succ n     -> Succ (shiftIndices n d i)
  Natrec p n e f -> Natrec (shiftIndices p d i) (shiftIndices n d i) (shiftIndices e d i) (shiftIndices f d i)
  Eq a m n   -> Eq (shiftIndices a d i) (shiftIndices m d i) (shiftIndices n d i)
  Refl a m   -> Refl (shiftIndices a d i) (shiftIndices m d i)
  Idpeel p e r -> Idpeel (shiftIndices p d i) (shiftIndices e d i) (shiftIndices r d i)
  Asp m    -> Asp (shiftIndices m d i)
  Lamvec m   -> Lamvec (shiftIndices m d (i+1))
  Appvec j m -> if j >= i
                   then Appvec (j+d) (shiftIndices m d i)
                   else Appvec j (shiftIndices m d i)
  Ann m a    -> Ann (shiftIndices m d i) a
  m -> m
  

{- Computations -}

-- | Beta reduction
betaReduce :: Preterm -> Preterm
betaReduce preterm = case preterm of
  Var i  -> Var i
  Con c  -> Con c
  Type   -> Type
  Kind   -> Kind
  Pi a b -> Pi (betaReduce a) (betaReduce b)
  Lam m  -> Lam (betaReduce m)
  App m n -> case betaReduce m of
    Lam v -> betaReduce (shiftIndices (subst v (shiftIndices n 1 0) 0) (-1) 0)
    (Ann (Lam v) _) -> betaReduce (shiftIndices (subst v (shiftIndices n 1 0) 0) (-1) 0)
    e -> App e (betaReduce n)
  Not a  -> Not (betaReduce a)
  Sigma a b -> Sigma (betaReduce a) (betaReduce b)
  Pair m n  -> Pair (betaReduce m) (betaReduce n)
  Proj s m  -> case betaReduce m of
    Pair x y -> case s of Fst -> x; Snd -> y
    (Ann (Pair x y) _) -> case s of Fst -> x; Snd -> y
    e -> Proj s e
  Disj a b -> Disj (betaReduce a) (betaReduce b)
  Iota s m -> Iota s (betaReduce m)
  Unpack p l m n -> case betaReduce l of
    Iota Fst v -> betaReduce $ App m v
    Iota Snd v -> betaReduce $ App n v
    l' -> Unpack (betaReduce p) l' (betaReduce m) (betaReduce n)
  Bot  -> Bot
  Unit -> Unit
  Top  -> Top
  Entity -> Entity
  Nat  -> Nat
  Zero -> Zero
  Succ n -> Succ (betaReduce n)
  Natrec p n e f -> case betaReduce n of
                      Zero -> betaReduce e
                      Succ m -> betaReduce $ (App (App f m) (Natrec (betaReduce p) m (betaReduce e) (betaReduce f)))
                      m -> Natrec (betaReduce p) m (betaReduce e) (betaReduce f) -- Con $ T.concat ["Error in beta-reduction of Natrec: ", toText n]
  Eq a m n -> Eq (betaReduce a) (betaReduce m) (betaReduce n)
  Refl a m -> Refl (betaReduce a) (betaReduce m)
  Idpeel p e r -> case betaReduce e of
                  Refl _ m -> betaReduce $ (App r m)
                  e' -> Idpeel (betaReduce p) e' (betaReduce r)
  Asp m -> Asp (betaReduce m)
  Lamvec m   -> Lamvec (betaReduce m)
  Appvec i m -> Appvec i (betaReduce m)
  Ann m a -> Ann (betaReduce m) a

-- | strong Beta reduction
strongBetaReduce :: Int -> Preterm -> Preterm
strongBetaReduce t preterm = case preterm of
  Var i  -> Var i
  Con c  -> Con c
  Type   -> Type
  Kind   -> Kind
  Pi a b -> Pi (strongBetaReduce 0 a) (strongBetaReduce 0 b)
  Lam m  -> if t > 0
               then Lam (strongBetaReduce (t-1) m)
               else Lam (strongBetaReduce 0 m)
  App m n -> case strongBetaReduce (t+1) m of
    Lam v -> strongBetaReduce t (shiftIndices (subst v (shiftIndices n 1 0) 0) (-1) 0)
    (Ann (Lam v) _) -> strongBetaReduce t (shiftIndices (subst v (shiftIndices n 1 0) 0) (-1) 0)
    e -> App e (strongBetaReduce 0 n)
  Not a  -> Not (strongBetaReduce 0 a)
  Sigma a b -> Sigma (strongBetaReduce 0 a) (strongBetaReduce 0 b)
  Pair m n  -> Pair (strongBetaReduce 0 m) (strongBetaReduce 0 n)
  Proj s m  -> case strongBetaReduce 0 m of
    Pair x y -> case s of Fst -> x; Snd -> y
    (Ann (Pair x y) _) -> case s of Fst -> x; Snd -> y
    e -> Proj s e
  Disj a b -> Disj (strongBetaReduce 0 a) (strongBetaReduce 0 b)
  Iota s m -> Iota s (strongBetaReduce 0 m)
  Unpack p l m n -> case strongBetaReduce 0 l of
                      Iota Fst v -> strongBetaReduce 0 $ App m v
                      Iota Snd v -> strongBetaReduce 0 $ App n v
                      l' -> Unpack (strongBetaReduce 0 p) l' (strongBetaReduce 0 m) (strongBetaReduce 0 n)
  Bot  -> Bot
  Unit -> Unit
  Top  -> Top
  Entity -> Entity
  Nat  -> Nat
  Zero -> Zero
  Succ n -> Succ (strongBetaReduce 0 n)
  Natrec p n e f -> case strongBetaReduce 0 n of
                      Zero -> strongBetaReduce 0 e
                      Succ m -> strongBetaReduce 0 $ (App (App f m) (Natrec (strongBetaReduce 0 p) m (strongBetaReduce 0 e) (strongBetaReduce 0 f)))
                      m -> Natrec (strongBetaReduce 0 p) m (strongBetaReduce 0 e) (strongBetaReduce 0 f) -- Con $ T.concat ["Error in  beta-reduction of Natrec: ", toText n]
  Eq a m n -> Eq (strongBetaReduce 0 a) (strongBetaReduce 0 m) (strongBetaReduce 0 n)
  Refl a m -> Refl (strongBetaReduce 0 a) (strongBetaReduce 0 m)
  Idpeel p e r -> case strongBetaReduce 0 e of
                    Refl _ m -> strongBetaReduce 0 (App r m)
                    e' -> Idpeel (strongBetaReduce 0 p) e' (strongBetaReduce 0 r)
  Asp m -> Asp (strongBetaReduce 0 m)
  Lamvec m   -> if t > 0
                   then Lam (strongBetaReduce (t-1) $ Lamvec (addLambda 0 m))
                   else strongBetaReduce 0 (deleteLambda 0 m)
  Appvec i m -> Appvec i (strongBetaReduce 0 m)
  Ann m a -> Ann (strongBetaReduce 0 m) a

-- | eliminates nested Sigma constructions from a given preterm
sigmaElimination :: Preterm -> Preterm
sigmaElimination preterm = case preterm of
  Pi a b     -> case a of
                  Sigma a' b' -> sigmaElimination (Pi a' (Pi b' (subst (shiftIndices b 1 1) (Pair (Var 1) (Var 0)) 0)))
                  _ -> Pi (sigmaElimination a) (sigmaElimination b)
  Lam m      -> Lam (sigmaElimination m)
  App m n    -> App (sigmaElimination m) (sigmaElimination n)
  Not m      -> Not (sigmaElimination m)
  Sigma a b  -> case a of
                  Sigma a' b' -> sigmaElimination (Sigma a' (Sigma b' (subst (shiftIndices b 1 1) (Pair (Var 1) (Var 0)) 0)))
                  _ -> Sigma (sigmaElimination a) (sigmaElimination b)
  Pair m n   -> Pair (sigmaElimination m) (sigmaElimination n)
  Proj s m   -> Proj s (sigmaElimination m)
  Disj a b   -> Disj (sigmaElimination a) (sigmaElimination b)
  Iota s m   -> Iota s (sigmaElimination m)
  Unpack p l m n -> Unpack (sigmaElimination p) (sigmaElimination l) (sigmaElimination m) (sigmaElimination n)
  Succ n     -> Succ (sigmaElimination n)
  Natrec p n e f -> Natrec (sigmaElimination p) (sigmaElimination n) (sigmaElimination e) (sigmaElimination f)
  Eq a m n   -> Eq (sigmaElimination a) (sigmaElimination m) (sigmaElimination n)
  Refl a m   -> Refl (sigmaElimination a) (sigmaElimination m)
  Idpeel p e r -> Idpeel (sigmaElimination p) (sigmaElimination e) (sigmaElimination r)
  Asp m      -> Asp (sigmaElimination m)
  Lamvec m   -> Lamvec (sigmaElimination m)
  Appvec j m -> Appvec j (sigmaElimination m)
  Ann m a    -> Ann (sigmaElimination m) a
  m -> m

-- | adds two preterms (of type `Nat`).
add :: Preterm -> Preterm -> Preterm
add m n = Natrec (Lam Nat) m n (Lam (Lam (Succ (Var 0))))

-- | multiplies two preterms (of type `Nat').
multiply :: Preterm -> Preterm -> Preterm
multiply m n = Natrec (Lam Nat) m Zero (Lam (Lam (add n (Var 0))))

{- Variable Vectors -}

-- | addLambda i preterm: the first subroutine for 'transvec' function,
-- which takes an index and a preterm, transforms the latter in a way that the Var/Appvec with an index j that is equal or greater than i
-- Ex.
-- addLambda 1 (Appvec 0 m) = Appvec 1 (addLambda 1 m)
-- addLambda 0 (Appvec 0 m) = Appvec 0 (App () (Var 1))
addLambda :: Int -> Preterm -> Preterm
addLambda i preterm = case preterm of
  Var j | j > i     -> Var (j+1)
        | j < i     -> Var j
        | otherwise -> Con $ T.concat [" Error in addLambda: var ", T.pack (show j)]
  Pi a b     -> Pi (addLambda i a) (addLambda (i+1) b)
  Lam m      -> Lam (addLambda (i+1) m)
  App m n    -> App (addLambda i m) (addLambda i n)
  Not a      -> Not (addLambda i a)
  Sigma a b  -> Sigma (addLambda i a) (addLambda (i+1) b)
  Pair m n   -> Pair (addLambda i m) (addLambda i n)
  Proj s m   -> Proj s (addLambda i m)
  Disj a b   -> Disj (addLambda i a) (addLambda i b)
  Iota s m   -> Iota s (addLambda i m)
  Unpack p l m n -> Unpack (addLambda i p) (addLambda i l) (addLambda i m) (addLambda i n)
  Succ n     -> Succ (addLambda i n)
  Natrec p n e f -> Natrec (addLambda i p) (addLambda i n) (addLambda i e) (addLambda i f)
  Eq a m n   -> Eq (addLambda i a) (addLambda i m) (addLambda i n)
  Refl m n   -> Refl (addLambda i m) (addLambda i n)
  Idpeel p e r -> Idpeel (addLambda i p) (addLambda i e) (addLambda i r)
  Asp m      -> Asp (addLambda i m)
  Lamvec m   -> Lamvec (addLambda (i+1) m)
  Appvec j m | j > i     -> Appvec (j+1) (addLambda i m)
             | j < i     -> Appvec j (addLambda i m)
             | otherwise -> Appvec j (App (addLambda i m) (Var (j+1)))
  Ann m a    -> Ann (addLambda i m) a
  m -> m -- identity function for 0-ary constructors

-- | deleteLambda i preterm: the second subroutine for 'transvec' function,
-- which takes an index i and a preterm p, transforms the latter in a way that the i-th variable vector within p is deleted.
deleteLambda :: Int -> Preterm -> Preterm 
deleteLambda i preterm = case preterm of
  Var j | j > i     -> Var (j-1)
        | j < i     -> Var j
        | otherwise -> Con $ T.concat ["Error in deleteLambda: var ", T.pack (show j)]
  Pi a b     -> Pi (deleteLambda i a) (deleteLambda (i+1) b)
  Lam m      -> Lam (deleteLambda (i+1) m)
  App m n    -> App (deleteLambda i m) (deleteLambda i n)
  Not a      -> Not (deleteLambda i a)
  Sigma a b  -> Sigma (deleteLambda i a) (deleteLambda (i+1) b)
  Pair m n   -> Pair (deleteLambda i m) (deleteLambda i n)
  Proj s m   -> Proj s (deleteLambda i m)
  Disj a b   -> Disj (deleteLambda i a) (deleteLambda i b)
  Iota s m   -> Iota s (deleteLambda i m)
  Unpack p l m n -> Unpack (deleteLambda i p) (deleteLambda i l) (deleteLambda i m) (deleteLambda i n)
  Succ n     -> Succ (deleteLambda i n)
  Natrec p n e f -> Natrec (deleteLambda i p) (deleteLambda i n) (deleteLambda i e) (deleteLambda i f) 
  Eq a m n   -> Eq (deleteLambda i a) (deleteLambda i m) (deleteLambda i n)
  Refl m n   -> Refl (deleteLambda i m) (deleteLambda i n)
  Idpeel p e r -> Idpeel (deleteLambda i p) (deleteLambda i e) (deleteLambda i r)
  Asp m      -> Asp (deleteLambda i m)
  Lamvec m   -> Lamvec (deleteLambda (i+1) m)
  Appvec j m | j > i     -> Appvec (j-1) (deleteLambda i m)
             | j < i     -> Appvec j (deleteLambda i m)
             | otherwise -> deleteLambda i m
  Ann m a    -> Ann (deleteLambda i m) a
  m -> m -- identity function for 0-ary constructors

-- | replaceLambda i preterm: the third subroutine for 'transvec' function,
-- which takes an index i and a preterm p, transforms the latter in a way that the i-th variable vector within p is replaced by a single variable.
replaceLambda :: Int -> Preterm -> Preterm 
replaceLambda i preterm = deleteLambda i (addLambda i preterm)

{- Conversion between UDTT and DTT -}

-- | from DTT to UDTT
toUDTT :: DTTdB.Preterm -> Preterm
toUDTT preterm = case preterm of
  DTTdB.Var i -> Var i
  DTTdB.Con t -> Con t
  DTTdB.Type -> Type
  DTTdB.Kind -> Kind
  DTTdB.Pi a b -> Pi (toUDTT a) (toUDTT b)
  DTTdB.Not a  -> Not (toUDTT a)
  DTTdB.Lam m  -> Lam (toUDTT m)
  DTTdB.App m n -> App (toUDTT m) (toUDTT n)
  DTTdB.Sigma a b -> Sigma (toUDTT a) (toUDTT b)
  DTTdB.Pair m n  -> Pair (toUDTT m) (toUDTT n)
  DTTdB.Proj sel m  -> Proj (case sel of DTTdB.Fst -> Fst; DTTdB.Snd -> Snd) (toUDTT m)
  DTTdB.Disj a b -> Disj (toUDTT a) (toUDTT b)
  DTTdB.Iota sel m  -> Iota (case sel of DTTdB.Fst -> Fst; DTTdB.Snd -> Snd) (toUDTT m)
  DTTdB.Unpack p l m n -> Unpack (toUDTT p) (toUDTT l) (toUDTT m) (toUDTT n)
  DTTdB.Bot     -> Bot
  DTTdB.Unit    -> Unit
  DTTdB.Top     -> Top
  DTTdB.Entity  -> Entity
  DTTdB.Nat     -> Nat
  DTTdB.Zero    -> Zero
  DTTdB.Succ n  -> Succ (toUDTT n)
  DTTdB.Natrec p e f n -> Natrec (toUDTT p) (toUDTT e) (toUDTT f) (toUDTT n)
  DTTdB.Eq a m n     -> Eq (toUDTT a) (toUDTT m) (toUDTT n)
  DTTdB.Refl a m     -> Refl (toUDTT a) (toUDTT m)
  DTTdB.Idpeel p e r -> Idpeel (toUDTT p) (toUDTT e) (toUDTT r)

-- | from UDTT to DTT
toDTT :: Preterm -> Maybe DTTdB.Preterm
toDTT preterm = case preterm of
  Var i -> return $ DTTdB.Var i
  Con t -> return $ DTTdB.Con t
  Type  -> return DTTdB.Type
  Kind  -> return DTTdB.Kind
  Pi a b -> do
            a' <- toDTT a
            b' <- toDTT b
            return $ DTTdB.Pi a' b'
  Lam m  -> do
            m' <- toDTT m
            return $ DTTdB.Lam m'
  App m n -> do
             m' <- toDTT m
             n' <- toDTT n
             return $ DTTdB.App m' n'
  Not m  -> do
            m' <- toDTT m
            return $ DTTdB.Pi m' DTTdB.Bot
  Sigma a b -> do
               a' <- toDTT a
               b' <- toDTT b
               return $ DTTdB.Sigma a' b'
  Pair m n  -> do
               m' <- toDTT m
               n' <- toDTT n
               return $ DTTdB.Pair m' n'
  Proj sel m  -> do
                 m' <- toDTT m
                 return $ DTTdB.Proj (case sel of Fst -> DTTdB.Fst; Snd -> DTTdB.Snd) m'
  Disj a b -> do
              a' <- toDTT a
              b' <- toDTT b
              return $ DTTdB.Disj a' b'
  Iota sel m -> do
                m' <- toDTT m
                return $ DTTdB.Iota (case sel of Fst -> DTTdB.Fst; Snd -> DTTdB.Snd) m'
  Unpack p l m n -> do
                    p' <- toDTT p
                    l' <- toDTT l
                    m' <- toDTT m
                    n' <- toDTT n
                    return $ DTTdB.Unpack p' l' m' n'
  Bot    -> return DTTdB.Bot
  Unit   -> return DTTdB.Unit
  Top    -> return DTTdB.Top
  Entity -> return DTTdB.Entity
  Nat    -> return DTTdB.Nat
  Zero   -> return DTTdB.Zero
  Succ n  -> do
             n' <- toDTT n
             return $ DTTdB.Succ n'
  Natrec p e f n -> do
                    p' <- toDTT p
                    e' <- toDTT e
                    f' <- toDTT f
                    n' <- toDTT n
                    return $ DTTdB.Natrec p' e' f' n'
  Eq a m n     -> do
                  a' <- toDTT a
                  m' <- toDTT m
                  n' <- toDTT n
                  return $ DTTdB.Eq a' m' n'
  Refl a m     -> do
                  a' <- toDTT a
                  m' <- toDTT m
                  return $ DTTdB.Refl a' m'
  Idpeel p e r -> do
                  p' <- toDTT p
                  e' <- toDTT e
                  r' <- toDTT r
                  return $ DTTdB.Idpeel p' e' r'
  Asp _   -> Nothing
  Lamvec _  -> Nothing
  Appvec _ _ -> Nothing
  Ann _ _ -> Nothing

-- {- Judgment of UDTT in de Bruijn notation -}

-- | A type of an element of a type signature, that is, a list of pairs of a preterm and a type.
-- ex. [entity:type, state:type, event:type, student:entity->type]

-- | The data type for a judgment
data Judgment = Judgment {
  signtr :: DTTdB.Signature  -- ^ A signature
  , contxt :: DTTdB.Context  -- ^ A context \Gamma in \Gamma \vdash M:A
  , trm :: Preterm     -- ^ A term M in \Gamma \vdash M:A
  , typ :: DTTdB.Preterm     -- ^ A type A in \Gamma \vdash M:A
  } deriving (Eq, G.Generic)

embedJudgment :: Judgment -> GeneralTypeQuery DTTdB.Signature DTTdB.Context Preterm DTTdB.Preterm
embedJudgment (Judgment sig cxt trm typ) = GeneralTypeQuery sig cxt (Term trm) (Term typ)

instance Show Judgment where
  show = T.unpack . toText
instance SimpleText Judgment where
  toText = toText . embedJudgment
instance Typeset Judgment where
  toTeX = toTeX . embedJudgment
instance MathML Judgment where
  toMathML = toMathML . embedJudgment
instance Store Judgment

toUDTTJudgment :: DTTdB.Judgment -> Judgment
toUDTTJudgment (DTTdB.Judgment signtr contxt dttTerm typ) = Judgment signtr contxt (toUDTT dttTerm) typ

type TypeCheckQuery = Judgment

embedTypeCheckQuery :: TypeCheckQuery -> GeneralTypeQuery DTTdB.Signature DTTdB.Context Preterm DTTdB.Preterm 
embedTypeCheckQuery = embedJudgment

data TypeInferQuery = TypeInferQuery DTTdB.Signature DTTdB.Context Preterm deriving (Eq)

embedTypeInferQuery :: TypeInferQuery -> GeneralTypeQuery DTTdB.Signature DTTdB.Context Preterm DTTdB.Preterm 
embedTypeInferQuery (TypeInferQuery sig cxt trm) = GeneralTypeQuery sig cxt (Term trm) Question

instance Show TypeInferQuery where
  show = T.unpack . toText
instance SimpleText TypeInferQuery where
  toText = toText . embedTypeInferQuery
instance Typeset TypeInferQuery where
  toTeX = toTeX . embedTypeInferQuery
instance MathML TypeInferQuery where
  toMathML = toMathML . embedTypeInferQuery




