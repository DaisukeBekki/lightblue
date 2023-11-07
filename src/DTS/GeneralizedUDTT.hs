{-# LANGUAGE GADTs, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}

{-|
Copyright   : (c) Daisuke Bekki, 2023
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Implementation of Underspecified Dependent Type Theory (Bekki forthcoming).
-}
module DTS.GeneralizedUDTT (
  -- * Terms and Types
  UDTT(..)
  , DTT(..)
  , Selector(..)
  , Preterm(..)
  -- * Conversion
  , toUDTT
  , toDTT
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
  -- * Judgment
  , Signature
  , Context
  , Judgment(..)
  ) where

import qualified Data.Text.Lazy as T      -- text
import Interface.Text
import Interface.TeX
import Interface.HTML

-- | Names of the language
data UDTT = UDTT deriving (Eq, Show)
data DTT = DTT deriving (Eq, Show)

-- | 'Proj' 'Fst' m is the first projection of m, while 'Proj' 'Snd' m is the second projection of m.
data Selector = Fst | Snd deriving (Eq, Show)

-- | Print a selector as "1" or "2".
instance SimpleText Selector where
  toText Fst = "1"
  toText Snd = "2"
instance Typeset Selector where
  toTeX = toText
instance MathML Selector where
  toMathML Fst = "<mn>1</mn>"  -- `Proj` `Fst` m is the first projection of m
  toMathML Snd = "<mn>2</mn>" -- `Proj` `Snd` m is the second projection of m

-- | Preterms of Underspecified Dependent Type Theory (UDTT).
data Preterm a where
  -- | Basic Preterms
  Var :: Int -> Preterm a                       -- ^ Variables
  Con :: T.Text -> Preterm a                    -- ^ Constant symbols
  Type :: Preterm a                             -- ^ The sort \"type\"
  Kind :: Preterm a                             -- ^ The sort \"kind\"
  -- | Pi Types
  Pi :: Preterm a -> Preterm a -> Preterm a     -- ^ Pi types
  Lam :: Preterm a -> Preterm a                 -- ^ Lambda abstractions
  App :: Preterm a -> Preterm a -> Preterm a    -- ^ Function Applications
  -- | Sigma Types
  Sigma :: Preterm a -> Preterm a -> Preterm a  -- ^ Sigma types
  Pair :: Preterm a -> Preterm a -> Preterm a   -- ^ Pairs
  Proj :: Selector -> Preterm a -> Preterm a    -- ^ (First and second) Projections
  -- | UDTT expansions
  Asp :: Int -> Preterm UDTT -> Preterm UDTT -- ^ Underspesified terms
  Lamvec :: Preterm UDTT -> Preterm UDTT        -- ^ Lambda abstractions of a variable vector
  Appvec :: Int -> Preterm UDTT -> Preterm UDTT -- ^ Function applications of a variable vector
  -- | Enumeration Types
  Unit :: Preterm a                             -- ^ The unit term (of type Top)
  Top :: Preterm a                              -- ^ The top type
  Bot :: Preterm a                              -- ^ The bottom type
  -- | Natural Number Types
  Nat :: Preterm a                              -- ^ Natural number type (Nat)
  Zero :: Preterm a                             -- ^ 0 (of type Nat)
  Succ :: Preterm a -> Preterm a                -- ^ The successor function
  Natrec :: Preterm a -> Preterm a -> Preterm a -> Preterm a -- ^ natrec
  -- | Intensional Equality Types
  Eq :: Preterm a -> Preterm a -> Preterm a -> Preterm a -- ^ Intensional equality types
  Refl :: Preterm a -> Preterm a -> Preterm a   -- ^ refl
  Idpeel :: Preterm a -> Preterm a -> Preterm a -- ^ idpeel
  -- | ToDo: add Disjoint Union Types
  -- | ToDo: add First Universe

deriving instance Eq a => Eq (Preterm a)
instance Show (Preterm a) where
  show = T.unpack . toTextDeBruijn

{-
-- | translates a DTS preterm into a simple text notation.
instance SimpleText (Preterm a) where
  toText = toText . initializeIndex . (fromDeBruijn [])

-- | translates a DTS preterm into a tex source code.
instance Typeset (Preterm a) where
  toTeX = toTeX . initializeIndex . (fromDeBruijn [])

-- | translates a DTS preterm into a MathML notation.
instance MathML (Preterm a) where
  toMathML = toMathML . initializeIndex . (fromDeBruijn [])
-}

-- | prints a preterm in text, in the De Bruijn style.
toTextDeBruijn :: Preterm t -> T.Text
toTextDeBruijn preterm = case preterm of
    Var i   -> T.pack (show i)
    Con c   -> c
    Type    -> "type"
    Kind    -> "kind"
    Pi a b  -> case b of
                 Bot -> T.concat["¬", toTextDeBruijn a]
                 b' -> T.concat["(Π ", toTextDeBruijn a, ")", toTextDeBruijn b']
    Lam m   -> T.concat["λ.", toTextDeBruijn m]
    App m n -> T.concat["(", toTextDeBruijn m, " ", toTextDeBruijn n, ")"]
    Sigma a b  -> T.concat["(Σ ", toTextDeBruijn a, ")", toTextDeBruijn b]
    Pair m n   -> T.concat["(", toTextDeBruijn m, ",", toTextDeBruijn n, ")"]
    Proj s m   -> T.concat["π", toText s, "(", toTextDeBruijn m, ")"]
    Lamvec m   -> T.concat ["λ+.", toTextDeBruijn m]
    Appvec i m -> T.concat ["(", toTextDeBruijn m, " ", T.pack (show i), "+)"]
    Unit  -> "()"
    Top   -> "T"
    Bot   -> "⊥"
    Asp i m -> T.concat["@", T.pack (show i), ":", toTextDeBruijn m]
    Nat   -> "N"
    Zero  -> "0"
    Succ n -> T.concat ["s", toTextDeBruijn n]
    Natrec n e f -> T.concat ["natrec(", toTextDeBruijn n, ",", toTextDeBruijn e, ",", toTextDeBruijn f, ")"]
    Eq a m n -> T.concat [toTextDeBruijn m, "=[", toTextDeBruijn a, "]", toTextDeBruijn n]
    Refl a m -> T.concat ["refl", toTextDeBruijn a, "(", toTextDeBruijn m, ")"]
    Idpeel m n -> T.concat ["idpeel(", toTextDeBruijn m, ",", toTextDeBruijn n, ")"]

{- Conversion between UDTT and DTT -}

-- | from DTT to UDTT
toUDTT :: Preterm DTT -> Preterm UDTT
toUDTT preterm = case preterm of
  Var i -> Var i
  Con t -> Con t
  Type -> Type
  Kind -> Kind
  Pi a b -> Pi (toUDTT a) (toUDTT b)
  Lam m  -> Lam (toUDTT m)
  App m n -> App (toUDTT m) (toUDTT n)
  Sigma a b -> Sigma (toUDTT a) (toUDTT b)
  Pair m n  -> Pair (toUDTT m) (toUDTT n)
  Proj sel m  -> Proj sel (toUDTT m)
  Unit    -> Unit
  Top     -> Top
  Bot     -> Bot
  Nat     -> Nat
  Zero    -> Zero
  Succ n  -> Succ (toUDTT n)
  Natrec e f n -> Natrec (toUDTT e) (toUDTT f) (toUDTT n)
  Eq a m n     -> Eq (toUDTT a) (toUDTT m) (toUDTT n)
  Refl a m     -> Refl (toUDTT a) (toUDTT m)
  Idpeel m n   -> Idpeel (toUDTT m) (toUDTT n)

-- | from UDTT to DTT
toDTT :: Preterm UDTT -> Maybe (Preterm DTT)
toDTT preterm = case preterm of
  Var i -> return $ Var i
  Con t -> return $ Con t
  Type  -> return Type
  Kind  -> return Kind
  Pi a b -> do
            a' <- toDTT a
            b' <- toDTT b
            return $ Pi a' b'
  Lam m  -> do
            m' <- toDTT m
            return $ Lam m'
  App m n -> do
             m' <- toDTT m
             n' <- toDTT n
             return $ App m' n'
  Sigma a b -> do
               a' <- toDTT a
               b' <- toDTT b
               return $ Sigma a' b'
  Pair m n  -> do
               m' <- toDTT m
               n' <- toDTT n
               return $ Pair m' n'
  Proj sel m  -> do
                 m' <- toDTT m
                 return $ Proj sel m'
  Asp _ _   -> Nothing
  Lamvec _  -> Nothing
  Appvec _ _ -> Nothing
  Unit   -> return Unit
  Top    -> return Top
  Bot    -> return Bot
  Nat    -> return Nat
  Zero   -> return Zero
  Succ n  -> do
             n' <- toDTT n
             return $ Succ n'
  Natrec e f n -> do
                  e' <- toDTT e
                  f' <- toDTT f
                  n' <- toDTT n
                  return $ Natrec e' f' n'
  Eq a m n     -> do
                  a' <- toDTT a
                  m' <- toDTT m
                  n' <- toDTT n
                  return $ Eq a' m' n'
  Refl a m     -> do
                  a' <- toDTT a
                  m' <- toDTT m
                  return $ Refl a' m'
  Idpeel m n   -> do
                  m' <- toDTT m
                  n' <- toDTT n
                  return $ Idpeel m' n'

{- Syntactic Operations -}

-- | Substitution of the variable i in a preterm M with a preterm L
--   "subst M L i" = M[L/i]
subst :: Preterm a -> Preterm a -> Int -> Preterm a
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
  Sigma a b  -> Sigma (subst a l i) (subst b (shiftIndices l 1 0) (i+1))
  Pair m n   -> Pair (subst m l i) (subst n l i)
  Proj s m   -> Proj s (subst m l i)
  Asp j m    -> Asp j (subst m l i)
  Lamvec m   -> Lamvec (subst m (shiftIndices l 1 0) (i+1))
  Appvec j m -> Appvec j (subst m l i)
  Unit       -> Unit
  Top        -> Top
  Bot        -> Bot
  Nat        -> Nat
  Zero       -> Zero
  Succ n     -> Succ (subst n l i)
  Natrec n e f -> Natrec (subst n l i) (subst e l i) (subst f l i)
  Eq a m n   -> Eq (subst a l i) (subst m l i) (subst n l i)
  Refl a m   -> Refl (subst a l i) (subst m l i)
  Idpeel m n -> Idpeel (subst m l i) (subst n l i)

-- | shiftIndices m d i
-- add d to all the indices that is greater than or equal to i within m (=d-place shift)
shiftIndices :: Preterm a -> Int -> Int -> Preterm a
shiftIndices preterm d i = case preterm of
  Var j      -> if j >= i
                   then Var (j+d)
                   else Var j
  Pi a b     -> Pi (shiftIndices a d i) (shiftIndices b d (i+1))
  Lam m      -> Lam (shiftIndices m d (i+1))
  App m n    -> App (shiftIndices m d i) (shiftIndices n d i)
  Sigma a b  -> Sigma (shiftIndices a d i) (shiftIndices b d (i+1))
  Pair m n   -> Pair (shiftIndices m d i) (shiftIndices n d i)
  Proj s m   -> Proj s (shiftIndices m d i)
  Asp j m    -> Asp j (shiftIndices m d i)
  Lamvec m   -> Lamvec (shiftIndices m d (i+1))
  Appvec j m -> if j >= i
                   then Appvec (j+d) (shiftIndices m d i)
                   else Appvec j (shiftIndices m d i)
  Succ n     -> Succ (shiftIndices n d i)
  Natrec n e f -> Natrec (shiftIndices n d i) (shiftIndices e d i) (shiftIndices f d i)
  Eq a m n   -> Eq (shiftIndices a d i) (shiftIndices m d i) (shiftIndices n d i)
  Refl a m   -> Refl (shiftIndices a d i) (shiftIndices m d i)
  Idpeel m n -> Idpeel (shiftIndices m d i) (shiftIndices n d i)

{- Computations -}

-- | Beta reduction
betaReduce :: Preterm a -> Preterm a
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

-- | strong Beta reduction
strongBetaReduce :: Int -> Preterm a -> Preterm a
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
  Sigma a b -> Sigma (strongBetaReduce 0 a) (strongBetaReduce 0 b)
  Pair m n  -> Pair (strongBetaReduce 0 m) (strongBetaReduce 0 n)
  Proj s m  -> case strongBetaReduce 0 m of
    Pair x y -> case s of
                  Fst -> x
                  Snd -> y
    e -> Proj s e
  Lamvec m   -> if t > 0
                   then Lam (strongBetaReduce (t-1) $ Lamvec (addLambda 0 m))
                   else strongBetaReduce 0 (deleteLambda 0 m)
  Appvec i m -> Appvec i (strongBetaReduce 0 m)
  Unit -> Unit
  Top  -> Top
  Bot  -> Bot
  Asp i m -> Asp i (strongBetaReduce 0 m)
  Nat  -> Nat
  Zero -> Zero
  Succ n -> Succ (strongBetaReduce 0 n)
  Natrec n e f -> case strongBetaReduce 0 n of
                    Zero -> strongBetaReduce 0 e
                    Succ m -> strongBetaReduce 0 $ (App (App f m) (Natrec m e f))
                    m -> Natrec m (strongBetaReduce 0 e) (strongBetaReduce 0 f) -- Con $ T.concat ["Error in beta-reduction of Natrec: ", toText n]
  Eq a m n -> Eq (strongBetaReduce 0 a) (strongBetaReduce 0 m) (strongBetaReduce 0 n)
  Refl a m -> Refl (strongBetaReduce 0 a) (strongBetaReduce 0 m)
  Idpeel m n -> case strongBetaReduce 0 m of
                  Refl _ m' -> strongBetaReduce 0 (App n m')
                  m' -> Idpeel m' (strongBetaReduce 0 n)

-- | eliminates nested Sigma constructions from a given preterm
sigmaElimination :: Preterm a -> Preterm a
sigmaElimination preterm = case preterm of
  Pi a b     -> case a of
                  Sigma a' b' -> sigmaElimination (Pi a' (Pi b' (subst (shiftIndices b 1 1) (Pair (Var 1) (Var 0)) 0)))
                  _ -> Pi (sigmaElimination a) (sigmaElimination b)
  Lam m      -> Lam (sigmaElimination m)
  App m n    -> App (sigmaElimination m) (sigmaElimination n)
  Sigma a b  -> case a of
                  Sigma a' b' -> sigmaElimination (Sigma a' (Sigma b' (subst (shiftIndices b 1 1) (Pair (Var 1) (Var 0)) 0)))
                  _ -> Sigma (sigmaElimination a) (sigmaElimination b)
  Pair m n   -> Pair (sigmaElimination m) (sigmaElimination n)
  Proj s m   -> Proj s (sigmaElimination m)
  Asp j m    -> Asp j (sigmaElimination m)
  Lamvec m   -> Lamvec (sigmaElimination m)
  Appvec j m -> Appvec j (sigmaElimination m)
  Succ n     -> Succ (sigmaElimination n)
  Natrec n e f -> Natrec (sigmaElimination n) (sigmaElimination e) (sigmaElimination f)
  Eq a m n   -> Eq (sigmaElimination a) (sigmaElimination m) (sigmaElimination n)
  Refl a m   -> Refl (sigmaElimination a) (sigmaElimination m)
  Idpeel m n -> Idpeel (sigmaElimination m) (sigmaElimination n)
  --m -> m

-- | adds two preterms (of type `Nat`).
add :: Preterm a -> Preterm a -> Preterm a
add m n = Natrec m n (Lam (Lam (Succ (Var 0))))

-- | multiplies two preterms (of type `Nat').
multiply :: Preterm a -> Preterm a -> Preterm a
multiply m n = Natrec m Zero (Lam (Lam (add n (Var 0))))

{- Variable Vectors -}

-- | addLambda i preterm: the first subroutine for 'transvec' function,
-- which takes an index and a preterm, transforms the latter in a way that the Var/Appvec with an index j that is equal or greater than i
-- Ex.
-- addLambda 1 (Appvec 0 m) = Appvec 1 (addLambda 1 m)
-- addLambda 0 (Appvec 0 m) = Appvec 0 (App () (Var 1))
addLambda :: Int -> Preterm UDTT -> Preterm UDTT
addLambda i preterm = case preterm of
  Var j | j > i     -> Var (j+1)
        | j < i     -> Var j
        | otherwise -> Con $ T.concat [" Error in addLambda: var ", T.pack (show j)]
  Pi a b     -> Pi (addLambda i a) (addLambda (i+1) b)
  Lam m      -> Lam (addLambda (i+1) m)
  App m n    -> App (addLambda i m) (addLambda i n)
  Sigma a b  -> Sigma (addLambda i a) (addLambda (i+1) b)
  Pair m n   -> Pair (addLambda i m) (addLambda i n)
  Proj s m   -> Proj s (addLambda i m)
  Asp j m    -> Asp j (addLambda i m)
  Lamvec m   -> Lamvec (addLambda (i+1) m)
  Appvec j m | j > i     -> Appvec (j+1) (addLambda i m)
             | j < i     -> Appvec j (addLambda i m)
             | otherwise -> Appvec j (App (addLambda i m) (Var (j+1)))
  m -> m

-- | deleteLambda i preterm: the second subroutine for 'transvec' function,
-- which takes an index i and a preterm p, transforms the latter in a way that the i-th variable vector within p is deleted.
deleteLambda :: Int -> Preterm UDTT -> Preterm UDTT
deleteLambda i preterm = case preterm of
  Var j | j > i     -> Var (j-1)
        | j < i     -> Var j
        | otherwise -> Con $ T.concat ["Error in deleteLambda: var ", T.pack (show j)]
  Pi a b     -> Pi (deleteLambda i a) (deleteLambda (i+1) b)
  Lam m      -> Lam (deleteLambda (i+1) m)
  App m n    -> App (deleteLambda i m) (deleteLambda i n)
  Sigma a b  -> Sigma (deleteLambda i a) (deleteLambda (i+1) b)
  Pair m n   -> Pair (deleteLambda i m) (deleteLambda i n)
  Proj s m   -> Proj s (deleteLambda i m)
  Asp j m    -> Asp j (deleteLambda i m)
  Lamvec m   -> Lamvec (deleteLambda (i+1) m)
  Appvec j m | j > i     -> Appvec (j-1) (deleteLambda i m)
             | j < i     -> Appvec j (deleteLambda i m)
             | otherwise -> deleteLambda i m
  m -> m

-- | replaceLambda i preterm: the third subroutine for 'transvec' function,
-- which takes an index i and a preterm p, transforms the latter in a way that the i-th variable vector within p is replaced by a single variable.
replaceLambda :: Int -> Preterm UDTT -> Preterm UDTT
replaceLambda i preterm = deleteLambda i (addLambda i preterm)

{- Judgment of UDTT in de Bruijn notation -}

-- | A type of an element of a type signature, that is, a list of pairs of a preterm and a type.
-- ex. [entity:type, state:type, event:type, student:entity->type]
type Signature = [(T.Text, Preterm DTT)]

-- | A context is a list of preterms
type Context a = [Preterm a]

{-
instance SimpleText (Context a) where
  toText = toText . fromDeBruijnContext

instance Typeset (Context a) where
  toTeX = toTeX . fromDeBruijnContext

instance MathML (Context a) where
  toMathML = toMathML . fromDeBruijnContext
-}

-- | The data type for a judgment
data Judgment a = Judgment {
  sig :: Signature         -- ^ A signature
  , context :: Context DTT -- ^ A context \Gamma in \Gamma \vdash M:A
  , term :: Preterm a      -- ^ A term M in \Gamma \vdash M:A
  , typ :: Preterm DTT     -- ^ A type A in \Gamma \vdash M:A
  } deriving (Eq)

{-
instance SimpleText (Judgment a) where
  toText = toText . fromDeBruijnJudgment

instance Typeset (Judgment a) where
  toTeX = toTeX . fromDeBruijnJudgment

instance MathML (Judgment a) where
  toMathML = toMathML . fromDeBruijnJudgment
-}
