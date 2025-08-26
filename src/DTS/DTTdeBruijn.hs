{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass, TemplateHaskell #-}

{-|
Copyright   : (c) Daisuke Bekki, 2024
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Implementation of Underspecified Dependent Type Theory (Bekki forthcoming).
-}

module DTS.DTTdeBruijn (
  -- * Terms and Types
  Selector(..)
  , Preterm(..)
  , ConName
  -- * General Syntactic Operations
  , subst
  , shiftIndices
  -- * Computations
  , betaReduce
  , strongBetaReduce
  , sigmaElimination
  , add
  , multiply
  -- * Judgment
  , Signature
  , Context
  , Judgment(..)
  , TypeCheckQuery(..)
  , TypeInferQuery(..)
  , ProofSearchQuery(..)
  ) where

import qualified GHC.Generics as G    --base
import qualified Data.Text.Lazy as LazyT --text
import Data.Store (Store(..))         --store
import Data.Store.TH (makeStore)      --store
import Interface.Text                 --lightblue
import Interface.TeX                  --lightblue
import Interface.HTML                 --lightblue
import DTS.GeneralTypeQuery           --lightblue

-- | 'Proj' 'Fst' m is the first projection of m, while 'Proj' 'Snd' m is the second projection of m.
data Selector = Fst | Snd deriving (Eq, Show, G.Generic, Store)

-- | Print a selector as "1" or "2".
instance SimpleText Selector where
  toText Fst = "1"
  toText Snd = "2"

type ConName = LazyT.Text

-- instance Typeset Selector where
--   toTeX = toText
-- 
-- instance MathML Selector where
--   toMathML Fst = "<mn>1</mn>"  -- `Proj` `Fst` m is the first projection of m
--   toMathML Snd = "<mn>2</mn>" -- `Proj` `Snd` m is the second projection of m

-- | Preterms of Dependent Type Theory (DTT).
data Preterm = 
  -- | Basic Preterms
  Var Int                       -- ^ Variables
  | Con LazyT.Text                  -- ^ Constant symbols
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
  | Eq Preterm Preterm Preterm     -- ^ Intensional equality types
  | Refl Preterm Preterm           -- ^ refl
  | Idpeel Preterm Preterm Preterm -- ^ idpeel P E R
  -- | ToDo: add First Universe
  deriving (Eq, G.Generic)

makeStore ''LazyT.Text

instance Store Preterm

instance Show Preterm where
  show = LazyT.unpack . toText

-- | translates a preterm into a simple text notation.
instance SimpleText Preterm where
  toText preterm = case preterm of
    Var i   -> LazyT.pack (show i)
    Con c   -> c
    Type    -> "type"
    Kind    -> "kind"
    Pi a b  -> case b of
                 Bot -> LazyT.concat["¬", toText a]
                 b' -> LazyT.concat["(Π ", toText a, ")", toText b']
    Lam m   -> LazyT.concat["λ.", toText m]
    App m n -> LazyT.concat["(", toText m, " ", toText n, ")"]
    Not m   -> LazyT.concat["¬", toText m]
    Sigma a b  -> LazyT.concat["(Σ ", toText a, ")", toText b]
    Pair m n   -> LazyT.concat["(", toText m, ",", toText n, ")"]
    Proj s m   -> LazyT.concat["π", toText s, "(", toText m, ")"]
    Disj a b -> LazyT.concat[toText a, " + ", toText b] 
    Iota s m -> LazyT.concat["ι", toText s, "(", toText m, ")"]
    Unpack p l m n -> LazyT.concat ["unpack(", toText p, ",", toText l, ",", toText m, ",", toText n, ")"]
    Bot   -> "⊥"
    Unit  -> "()"
    Top   -> "T"
    Entity -> "entity"
    Nat   -> "N"
    Zero  -> "0"
    Succ n -> LazyT.concat ["s", toText n]
    Natrec p n e f -> LazyT.concat ["natrec(", toText p, ",", toText n, ",", toText e, ",", toText f, ")"]
    Eq a m n -> LazyT.concat [toText m, "=[", toText a, "]", toText n]
    Refl a m -> LazyT.concat ["refl", toText a, "(", toText m, ")"]
    Idpeel p e r -> LazyT.concat ["idpeel(", toText p, ",", toText e, ",", toText r, ")"]

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
    e -> App e (betaReduce n)
  Not a  -> Not (betaReduce a)
  Sigma a b -> Sigma (betaReduce a) (betaReduce b)
  Pair m n  -> Pair (betaReduce m) (betaReduce n)
  Proj s m  -> case betaReduce m of
    Pair x y -> case s of
                  Fst -> x
                  Snd -> y
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
                      m -> Natrec (betaReduce p) m (betaReduce e) (betaReduce f) -- Con $ LazyT.concat ["Error in beta-reduction of Natrec: ", toText n]
  Eq a m n -> Eq (betaReduce a) (betaReduce m) (betaReduce n)
  Refl a m -> Refl (betaReduce a) (betaReduce m)
  Idpeel p e r-> case betaReduce e of
                   Refl _ m -> betaReduce $ App r m
                   e' -> Idpeel (betaReduce p) e' (betaReduce r)

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
    e -> App e (strongBetaReduce 0 n)
  Not a  -> Not (strongBetaReduce 0 a)
  Sigma a b -> Sigma (strongBetaReduce 0 a) (strongBetaReduce 0 b)
  Pair m n  -> Pair (strongBetaReduce 0 m) (strongBetaReduce 0 n)
  Proj s m  -> case strongBetaReduce 0 m of
    Pair x y -> case s of
                  Fst -> x
                  Snd -> y
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
                      m -> Natrec (strongBetaReduce 0 p) m (strongBetaReduce 0 e) (strongBetaReduce 0 f) -- Con $ LazyT.concat ["Error in beta-reduction of Natrec: ", toText n]
  Eq a m n -> Eq (strongBetaReduce 0 a) (strongBetaReduce 0 m) (strongBetaReduce 0 n)
  Refl a m -> Refl (strongBetaReduce 0 a) (strongBetaReduce 0 m)
  Idpeel p e r -> case strongBetaReduce 0 e of
                    Refl _ m -> strongBetaReduce 0 $ App r m
                    e' -> Idpeel (strongBetaReduce 0 p) e' (strongBetaReduce 0 r)

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
  m -> m

-- | adds two preterms (of type `Nat`).
add :: Preterm -> Preterm -> Preterm
add m n = Natrec (Lam Nat) m n (Lam (Lam (Succ (Var 0))))

-- | multiplies two preterms (of type `Nat').
multiply :: Preterm -> Preterm -> Preterm
multiply m n = Natrec (Lam Nat) m Zero (Lam (Lam (add n (Var 0))))

{- Judgment of DTT in de Bruijn notation -}

-- | A type of an element of a type signature, that is, a list of pairs of a preterm and a type.
-- ex. [entity:type, state:type, event:type, student:entity->type]
type Signature = [(LazyT.Text, Preterm)]

instance SimpleText Signature where
  toText = (LazyT.intercalate ", ") . (map (\(nm,tm) -> LazyT.concat [nm, ":", toText tm])) . reverse
instance Typeset Signature where
  toTeX = (LazyT.intercalate ",") . (map (\(nm,tm) -> LazyT.concat [nm, ":", toTeX tm])) . reverse
instance MathML Signature where
  toMathML = (LazyT.intercalate "<mo>,<mo>") . (map (\(nm,tm) -> LazyT.concat ["<mrow><mo>", nm, "</mo><mo>:</mo>", toMathML tm, "</mrow>"])) . reverse

-- | A context is a list of preterms
type Context = [Preterm]

instance SimpleText Context where
  toText = (LazyT.intercalate ", ") . (map toText) . reverse
instance Typeset Context where
  toTeX = (LazyT.intercalate ",") . (map toTeX) . reverse
instance MathML Context where
  toMathML = (LazyT.intercalate "<mo>,</mo>") . (map toMathML). reverse

-- | The data type for a judgment
data Judgment = Judgment {
  signtr :: Signature  -- ^ A signature
  , contxt :: Context  -- ^ A context \Gamma in \Gamma \vdash M:A
  , trm :: Preterm     -- ^ A term M in \Gamma \vdash M:A
  , typ :: Preterm     -- ^ A type A in \Gamma \vdash M:A
  } deriving (Eq, G.Generic)

embedJudgment :: Judgment -> GeneralTypeQuery Signature Context Preterm Preterm
embedJudgment (Judgment sig cxt trm typ) = GeneralTypeQuery sig cxt (Term trm) (Term typ)

instance Show Judgment where
  show = LazyT.unpack . toText
instance SimpleText Judgment where
  toText = toText . embedJudgment
instance Typeset Judgment where
  toTeX = toTeX . embedJudgment
instance MathML Judgment where
  toMathML = toMathML . embedJudgment
instance Store Judgment

type TypeCheckQuery = Judgment

embedTypeCheckQuery :: TypeCheckQuery -> GeneralTypeQuery Signature Context Preterm Preterm 
embedTypeCheckQuery = embedJudgment

data TypeInferQuery = TypeInferQuery Signature Context Preterm deriving (Eq)

embedTypeInferQuery :: TypeInferQuery -> GeneralTypeQuery Signature Context Preterm Preterm 
embedTypeInferQuery (TypeInferQuery sig cxt trm) = GeneralTypeQuery sig cxt (Term trm) Question

instance Show TypeInferQuery where
  show = LazyT.unpack . toText
instance SimpleText TypeInferQuery where
  toText = toText . embedTypeInferQuery
instance Typeset TypeInferQuery where
  toTeX = toTeX . embedTypeInferQuery
instance MathML TypeInferQuery where
  toMathML = toMathML . embedTypeInferQuery

data ProofSearchQuery = ProofSearchQuery Signature Context Preterm deriving (Eq)

embedProofSearchQuery :: ProofSearchQuery -> GeneralTypeQuery Signature Context Preterm Preterm
embedProofSearchQuery (ProofSearchQuery sig cxt typ) = GeneralTypeQuery sig cxt Question (Term typ)

instance Show ProofSearchQuery where
  show = LazyT.unpack . toText
instance SimpleText ProofSearchQuery where
  toText = toText . embedProofSearchQuery
instance Typeset ProofSearchQuery where
  toTeX = toTeX . embedProofSearchQuery
instance MathML ProofSearchQuery where
  toMathML = toMathML . embedProofSearchQuery
