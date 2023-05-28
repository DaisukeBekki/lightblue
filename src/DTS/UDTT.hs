{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

{-|
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Implementation of Underspecified Dependent Type Theory (Bekki forthcoming).
-}
module DTS.UDTT (
  -- * Types
  Preterm(..),
  Selector(..),
  Signature,
  --printSignature,
  toTextDeBruijn,
  -- * Syntactic Operations
  subst,
  shiftIndices,
  addLambda,
  deleteLambda,
  replaceLambda,
  -- * Computations
  betaReduce,
  strongBetaReduce,
  sigmaElimination,
  add,
  multiply,
  -- * De Bruijn notation from/to Variable-name notation
  Indexed(..),
  initializeIndex,
  fromDeBruijn,
  toDeBruijn,
  -- * Judgment
  Context,
  toVerticalMathML,
  printVerticalMathML,
  Judgment(..),
  fromDeBruijnContext,
  fromDeBruijnJudgment,
  fromDeBruijnSRlist,
  printProofSearchQuery
  ) where

import qualified Data.Text.Lazy as T      -- text
import qualified Data.List as L           -- base
import qualified Control.Applicative as M -- base
import qualified Control.Monad as M       -- base
import qualified DTS.UDTTwithName as VN
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

-- | Preterms of Underspecified Dependent Type Theory (UDTT).
data Preterm =
  Var Int                 -- ^ Variables
  | Con T.Text            -- ^ Constant symbols
  | Type                  -- ^ The sort \"type\"
  | Kind                  -- ^ The sort \"kind\"
  | Pi Preterm Preterm    -- ^ Dependent function types (or Pi types)
  | Not Preterm           -- ^ Negations
  | Lam Preterm           -- ^ Lambda abstractions
  | App Preterm Preterm   -- ^ Function Applications
  | Sigma Preterm Preterm -- ^ Dependent product types (or Sigma types)
  | Pair Preterm Preterm  -- ^ Pairs
  | Proj Selector Preterm -- ^ (First and second) Projections
  | Asp Int Preterm       -- ^ Underspesified terms
  | Lamvec Preterm        -- ^ Lambda abstractions of a variable vector
  | Appvec Int Preterm    -- ^ Function applications of a variable vector
  | Unit                  -- ^ The unit term (of type Top)
  | Top                   -- ^ The top type
  | Bot                   -- ^ The bottom type
  | Nat                   -- ^ Natural number type (Nat)
  | Zero                  -- ^ 0 (of type Nat)
  | Succ Preterm          -- ^ The successor function
  | Natrec Preterm Preterm Preterm -- ^ natrec
  | Eq Preterm Preterm Preterm     -- ^ Intensional equality types
  | Refl Preterm Preterm           -- ^ refl
  | Idpeel Preterm Preterm         -- ^ idpeel
  -- DRel Int T.Text Preterm Preterm  -- ^ Discourse relations
  deriving (Eq)

instance Show Preterm where
  --show = T.unpack . toTextDeBruijn
  show = T.unpack . toText

-- | translates a DTS preterm into a simple text notation.
instance SimpleText Preterm where
  toText = toText . initializeIndex . (fromDeBruijn [])

-- | translates a DTS preterm into a tex source code.
instance Typeset Preterm where
  toTeX = toTeX . initializeIndex . (fromDeBruijn [])

-- | translates a DTS preterm into a MathML notation.
instance MathML Preterm where
  toMathML = toMathML . initializeIndex . (fromDeBruijn [])

-- | prints a preterm in text, in the De Bruijn style.
toTextDeBruijn :: Preterm -> T.Text
toTextDeBruijn preterm = case preterm of
    Var i   -> T.pack (show i)
    Con c   -> c
    Type    -> "type"
    Kind    -> "kind"
    Pi a b  -> T.concat["(Π ", toTextDeBruijn a, ")", toTextDeBruijn b]
    Not m   -> T.concat["¬", toTextDeBruijn m]
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
    --DRel i t m n -> T.concat ["DRel", T.pack (show i), "[", t, "](", toTextDeBruijn m, ",", toTextDeBruijn n, ")"]

-- | A type of an element of a type signature, that is, a list of pairs of a preterm and a type.
-- ex. [entity:type, state:type, event:type, student:entity->type]
type Signature = [(T.Text,Preterm)]

instance SimpleText Signature where
  toText sigs = T.concat ["[", (T.intercalate ", " $ map (\(cname,ty) -> T.concat $ [toText (Con cname), ":", toText ty]) sigs), "]"]

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
  Not m  -> Not (subst m l i)
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
  --DRel j t m n -> DRel j t (subst m l i) (subst n l i)

-- | shiftIndices m d i
-- add d to all the indices that is greater than or equal to i within m (=d-place shift)
shiftIndices :: Preterm -> Int -> Int -> Preterm
shiftIndices preterm d i = case preterm of
  Var j      -> if j >= i
                   then Var (j+d)
                   else Var j
  Pi a b     -> Pi (shiftIndices a d i) (shiftIndices b d (i+1))
  Not m      -> Not (shiftIndices m d i)
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
  --DRel j t m n -> DRel j t (shiftIndices m d i) (shiftIndices n d i)
  m -> m

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
  --DRel i t m n -> DRel i t (betaReduce m) (betaReduce n)

-- | strong Beta reduction
strongBetaReduce :: Int -> Preterm -> Preterm
strongBetaReduce t preterm = case preterm of
  Var i  -> Var i
  Con c  -> Con c
  Type   -> Type
  Kind   -> Kind
  Pi a b -> Pi (strongBetaReduce 0 a) (strongBetaReduce 0 b)
  Not a  -> Not (strongBetaReduce 0 a)
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
  --DRel i text m n -> DRel i text (strongBetaReduce 0 m) (strongBetaReduce 0 n)

-- | eliminates nested Sigma constructions from a given preterm
sigmaElimination :: Preterm -> Preterm
sigmaElimination preterm = case preterm of
  Pi a b     -> case a of
                  Sigma a' b' -> sigmaElimination (Pi a' (Pi b' (subst (shiftIndices b 1 1) (Pair (Var 1) (Var 0)) 0)))
                  _ -> Pi (sigmaElimination a) (sigmaElimination b)
  Not m      -> Not (sigmaElimination m)
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
  --DRel j t m n -> DRel j t (sigmaElimination m) (sigmaElimination n)
  m -> m

-- | adds two preterms (of type `Nat`).
add :: Preterm -> Preterm -> Preterm
add m n = Natrec m n (Lam (Lam (Succ (Var 0))))

-- | multiplies two preterms (of type `Nat').
multiply :: Preterm -> Preterm -> Preterm
multiply m n = Natrec m Zero (Lam (Lam (add n (Var 0))))

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
  Not a      -> Not (addLambda i a)
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
  --DRel j t m n -> DRel j t (addLambda i m) (addLambda i n)
  m -> m

-- | deleteLambda i preterm: the second subroutine for 'transvec' function,
-- which takes an index i and a preterm p, transforms the latter in a way that the i-th variable vector within p is deleted.
deleteLambda :: Int -> Preterm -> Preterm
deleteLambda i preterm = case preterm of
  Var j | j > i     -> Var (j-1)
        | j < i     -> Var j
        | otherwise -> Con $ T.concat ["Error in deleteLambda: var ", T.pack (show j)]
  Pi a b     -> Pi (deleteLambda i a) (deleteLambda (i+1) b)
  Not a      -> Not (deleteLambda i a)
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
  --DRel j t m n -> DRel j t (deleteLambda i m) (deleteLambda i n)
  m -> m

-- | replaceLambda i preterm: the third subroutine for 'transvec' function,
-- which takes an index i and a preterm p, transforms the latter in a way that the i-th variable vector within p is replaced by a single variable.
replaceLambda :: Int -> Preterm -> Preterm
replaceLambda i preterm = deleteLambda i (addLambda i preterm)

{-
  case preterm of
  Pi a b     -> Pi (replaceLambda i a) (replaceLambda (i+1) b)
  Not a      -> Not (replaceLambda i a)
  Lam m      -> Lam (replaceLambda (i+1) m)
  App m n    -> App (replaceLambda i m) (replaceLambda i n)
  Sigma a b  -> Sigma (replaceLambda i a) (replaceLambda (i+1) b)
  Pair m n   -> Pair (replaceLambda i m) (replaceLambda i n)
  Proj s m   -> Proj s (replaceLambda i m)
  Asp j m    -> Asp j (replaceLambda i m)
  Lamvec m   -> Lamvec (replaceLambda (i+1) m)
  Appvec j m | i == j    -> App (replaceLambda i m) (Var j)
             | otherwise -> Appvec j (replaceLambda i m)
  DRel j t m n -> DRel j t (replaceLambda i m) (replaceLambda i n)
  m -> m
-}

{- Initializing or Re-indexing of vars, @s and DRels -}

-- | Indexed monad controls indices to be attached to preterms.  Arguments correspond to:
--   u for variables for propositions
-- | x for variables for entities
-- | e for variables for eventualities
-- | indices for asp-operators
-- | indices for DReL operators
newtype Indexed a = Indexed { indexing :: Int -> Int -> Int -> Int -> Int -> Int -> (a,Int,Int,Int,Int,Int,Int) }

instance Monad Indexed where
  return m = Indexed (\s u x e a d -> (m,s,u,x,e,a,d))
  (Indexed m) >>= f = Indexed (\s u x e a d -> let (m',s',u',x',e',a',d') = m s u x e a d;
                                                   (Indexed n) = f m';
                                               in
                                               n s' u' x' e' a' d')

instance Functor Indexed where
  fmap = M.liftM

instance M.Applicative Indexed where
  pure = return
  (<*>) = M.ap

-- | A sequential number for variable names (i.e. x_1, x_2, ...) in a context
sIndex :: Indexed Int
sIndex = Indexed (\s u x e a d -> (s,s+1,u,x,e,a,d))

uIndex :: Indexed Int
uIndex = Indexed (\s u x e a d -> (u,s,u+1,x,e,a,d))

xIndex :: Indexed Int
xIndex = Indexed (\s u x e a d -> (x,s,u,x+1,e,a,d))

eIndex :: Indexed Int
eIndex = Indexed (\s u x e a d -> (e,s,u,x,e+1,a,d))

aspIndex :: Indexed Int
aspIndex = Indexed (\s u x e a d -> (a,s,u,x,e,a+1,d))

dRelIndex :: Indexed Int
dRelIndex = Indexed (\s u x e a d -> (d,s,u,x,e,a,d+1))

-- | re-assigns sequential indices to all asperands that appear in a given preterm.
initializeIndex :: Indexed a -> a
initializeIndex (Indexed m) = let (m',_,_,_,_,_,_) = m 0 0 0 0 0 0 in m'

-- | translates a preterm in de Bruijn notation into a preterm with variable name.
fromDeBruijn :: [VN.VarName] -- ^ A context (= a list of variable names)
                -> Preterm   -- ^ A preterm in de Bruijn notation
                -> Indexed VN.Preterm -- ^ A preterm with variable names
fromDeBruijn vnames preterm = case preterm of
  Var j -> if j < length vnames
              then return $ VN.Var (vnames!!j)
              else return $ VN.Con $ T.concat ["error: var ",T.pack (show j), " in ", T.pack (show vnames)]
  Con cname -> return $ VN.Con cname
  Type -> return VN.Type
  Kind -> return VN.Kind
  Pi a b -> do
    vname <- variableNameFor a
    a' <- fromDeBruijn vnames a
    b' <- fromDeBruijn (vname:vnames) b
    return $ VN.Pi vname a' b'
  Not a   -> do
    a' <- fromDeBruijn vnames a
    return $ VN.Not a'
  Lam m   -> do
    i <- xIndex
    let vname = case m of
                  Sigma _ _ -> VN.VarName 'x' i
                  Pi _ _    -> VN.VarName 'x' i
                  _         -> VN.VarName 'x' i
    m' <- fromDeBruijn (vname:vnames) m
    return $ VN.Lam vname m'
  App m n -> do
    m' <- fromDeBruijn vnames m
    n' <- fromDeBruijn vnames n
    return $ VN.App m' n'
  Sigma a b -> do
    vname <- variableNameFor a
    a' <- fromDeBruijn vnames a
    b' <- fromDeBruijn (vname:vnames) b
    return $ VN.Sigma vname a' b'
  Pair m n  -> do
    m' <- fromDeBruijn vnames m
    n' <- fromDeBruijn vnames n
    return $ VN.Pair m' n'
  Proj s m  -> do
    m' <- fromDeBruijn vnames m
    return $ case s of
               Fst -> VN.Proj VN.Fst m'
               Snd -> VN.Proj VN.Snd m'
  Lamvec m  -> do
    i <- xIndex
    let vname = VN.VarName 'x' i
    m' <- fromDeBruijn (vname:vnames) m
    return $ VN.Lamvec vname m'
  Appvec j m -> do
    let vname = vnames!!j
    m' <- fromDeBruijn vnames m
    return $ VN.Appvec vname m'
  Unit    -> return VN.Unit
  Top     -> return VN.Top
  Bot     -> return VN.Bot
  Asp _ m -> do
    j' <- aspIndex
    m' <- fromDeBruijn vnames m
    return $ VN.Asp j' m'
  Nat    -> return VN.Nat
  Zero   -> return VN.Zero
  Succ n -> do
    n' <- fromDeBruijn vnames n
    return $ VN.Succ n'
  Natrec n e f -> do
    n' <- fromDeBruijn vnames n
    e' <- fromDeBruijn vnames e
    f' <- fromDeBruijn vnames f
    return $ VN.Natrec n' e' f'
  Eq a m n -> do
    a' <- fromDeBruijn vnames a
    m' <- fromDeBruijn vnames m
    n' <- fromDeBruijn vnames n
    return $ VN.Eq a' m' n'
  Refl a m -> do
    a' <- fromDeBruijn vnames a
    m' <- fromDeBruijn vnames m
    return $ VN.Refl a' m'
  Idpeel m n -> do
    m' <- fromDeBruijn vnames m
    n' <- fromDeBruijn vnames n
    return $ VN.Idpeel m' n'
  -- DRel _ t m n -> do
  --   j' <- dRelIndex
  --   m' <- fromDeBruijn vnames m
  --   n' <- fromDeBruijn vnames n
  --   return $ VN.DRel j' t m' n'
  -- App (App (Con (T.concat ["DRel",T.pack $ show j',"[",t,"]"])) m') n'

variableNameFor :: Preterm -> Indexed VN.VarName
variableNameFor preterm =
  case preterm of
    Con cname | cname == "entity" -> do i <- xIndex; return $ VN.VarName 'x' i
              | cname == "evt"    -> do i <- eIndex; return $ VN.VarName 'e' i
              -- cname == "state"  -> VN.VarName 's' i
    Eq _ _ _ -> do i <- xIndex; return $ VN.VarName 's' i
    Nat      -> do i <- xIndex; return $ VN.VarName 'k' i
    _        -> do i <- uIndex; return $ VN.VarName 'u' i

-- | translates a preterm with variable name into a preterm in de Bruijn notation.
toDeBruijn :: [VN.VarName]  -- ^ A context (= a list of variable names)
              -> VN.Preterm -- ^ A preterm with variable names
              -> Preterm    -- ^ A preterm in de Bruijn notation
toDeBruijn vnames preterm = case preterm of
  VN.Var vname -> case L.elemIndex vname vnames of
                    Just i -> Var i
                    Nothing -> Con "Error: vname not found in toDeBruijn Var"
  VN.Con cname -> Con cname
  VN.Type -> Type
  VN.Kind -> Kind
  VN.Pi vname a b -> Pi (toDeBruijn (vname:vnames) a) (toDeBruijn (vname:vnames) b)
  VN.Not a -> Not (toDeBruijn vnames a)
  VN.Lam vname m -> Lam (toDeBruijn (vname:vnames) m)
  VN.App m n -> App (toDeBruijn vnames m) (toDeBruijn vnames n)
  VN.Sigma vname a b -> Sigma (toDeBruijn (vname:vnames) a) (toDeBruijn (vname:vnames) b)
  VN.Pair m n -> Pair (toDeBruijn vnames m) (toDeBruijn vnames n)
  VN.Proj s m -> case s of
                   VN.Fst -> Proj Fst (toDeBruijn vnames m)
                   VN.Snd -> Proj Snd (toDeBruijn vnames m)
  VN.Lamvec vname m -> Lamvec (toDeBruijn (vname:vnames) m)
  VN.Appvec vname m -> case L.elemIndex vname vnames of
                        Just i -> Appvec i (toDeBruijn vnames m)
                        Nothing -> Con "Error: vname not found in toDeBruijn Appvec"
  VN.Unit -> Unit
  VN.Top -> Top
  VN.Bot -> Bot
  VN.Asp i m -> Asp i (toDeBruijn vnames m)
  VN.Nat -> Nat
  VN.Zero -> Zero
  VN.Succ n -> Succ (toDeBruijn vnames n)
  VN.Natrec n e f -> Natrec (toDeBruijn vnames n) (toDeBruijn vnames e) (toDeBruijn vnames f)
  VN.Eq a m n -> Eq (toDeBruijn vnames a) (toDeBruijn vnames m) (toDeBruijn vnames n)
  VN.Refl a m -> Refl (toDeBruijn vnames a) (toDeBruijn vnames m)
  VN.Idpeel m n -> Idpeel (toDeBruijn vnames m) (toDeBruijn vnames n)
  --VN.DRel j t m n -> DRel j t (toDeBruijn vnames m) (toDeBruijn vnames n)

{- Judgment of UDTT in de Bruijn notation -}

-- | A context is a list of preterms
type Context = [Preterm]

instance SimpleText Context where
  toText = toText . fromDeBruijnContext

instance Typeset Context where
  toTeX = toTeX . fromDeBruijnContext

instance MathML Context where
  toMathML = toMathML . fromDeBruijnContext

-- | translates a context in de Bruijn notation (i.e. [DTS.DependentTypes.Preterm])
-- into one with variable names
-- (i.e. [(DTS.DTSwithVarName.VarName, DTS.DTSwithVarName.Preterm)]).
fromDeBruijnContext :: Context -> VN.Context
fromDeBruijnContext = snd . initializeIndex . fromDeBruijnContextLoop

-- | the internal function of the fromDeBruijnContext function
fromDeBruijnContextLoop :: Context -> Indexed ([VN.VarName], VN.Context)
fromDeBruijnContextLoop [] = return ([],[])
fromDeBruijnContextLoop (x:xs) = do
  (varnames,ixs) <- fromDeBruijnContextLoop xs
  i <- sIndex
  let varname = VN.VarName 's' i
  ix <- fromDeBruijn varnames x
  return $ (varname:varnames,(varname,ix):ixs)

-- | prints a context in vertical manner.
toVerticalMathML :: Context -> T.Text
toVerticalMathML = VN.toVerticalMathML . fromDeBruijnContext

-- | A list of semantic representations (the first element is for the first sentence)
fromDeBruijnSRlist :: [Preterm] -> [(VN.VarName,VN.Preterm)]
fromDeBruijnSRlist = initializeIndex . (fromDeBruijnSRlistLoop [])

-- | the internal function of the fromDeBruijnSRlist function
fromDeBruijnSRlistLoop :: [VN.VarName] -> [Preterm] -> Indexed ([(VN.VarName,VN.Preterm)])
fromDeBruijnSRlistLoop _ [] = return []
fromDeBruijnSRlistLoop varnames (x:xs) = do
  i <- sIndex
  let varname = VN.VarName 's' i
  ix <- fromDeBruijn varnames x
  ixs <- fromDeBruijnSRlistLoop (varname:varnames) xs
  return ((varname,ix):ixs)

-- | prints an SR list in vertical manner.
printVerticalMathML :: [Preterm] -> IO()
printVerticalMathML = VN.printVerticalMathML . fromDeBruijnSRlist

-- | The data type for a judgment
data Judgment = Judgment {
  context :: Context, -- ^ A context \Gamma in \Gamma \vdash M:A
  term :: Preterm,    -- ^ A term M in \Gamma \vdash M:A
  typ :: Preterm      -- ^ A type A in \Gamma \vdash M:A
  } deriving (Eq)

instance SimpleText Judgment where
  toText = toText . fromDeBruijnJudgment

instance Typeset Judgment where
  toTeX = toTeX . fromDeBruijnJudgment

instance MathML Judgment where
  toMathML = toMathML . fromDeBruijnJudgment

-- | translates a judgment in de Bruijn notation into one with variable names
fromDeBruijnJudgment :: Judgment -> VN.Judgment
fromDeBruijnJudgment judgment =
  let (vcontext', vterm', vtyp')
        = initializeIndex $ do
                            (varnames,vcontext) <- fromDeBruijnContextLoop $ context judgment
                            vterm <- fromDeBruijn varnames (term judgment)
                            vtyp <- fromDeBruijn varnames (typ judgment)
                            return (vcontext, vterm, vtyp)
  in VN.Judgment { VN.context = vcontext',
                   VN.term = vterm',
                   VN.typ = vtyp' }

-- | prints a proof search query in MathML
printProofSearchQuery :: Context -> Preterm -> T.Text
printProofSearchQuery cont ty =
  let (vcontext', vtyp')
        = initializeIndex $ do
                            (varnames,vcontext) <- fromDeBruijnContextLoop cont
                            vtyp <- fromDeBruijn varnames ty
                            return (vcontext, vtyp)
  in T.concat ["<mrow>", toMathML vcontext', "<mo>&vdash;</mo><mo>?</mo><mo>:</mo>", toMathML vtyp', "</mrow>"]
