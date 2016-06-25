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
  -- * Syntactic Operations
  subst,
  shiftIndices,
  addLambda,
  deleteLambda,
  replaceLambda,
  -- * Computations
  betaReduce,
  strongBetaReduce,
  add,
  multiply,
  -- * De Bruijn notation <-> Variable-name notation
  fromDeBruijn,
  toDeBruijn,
  Indexed(..),
  sentenceIndex,
  initializeIndex
  ) where

import qualified Data.Text.Lazy as T      -- text
import qualified Data.List as L           -- base
import qualified Control.Applicative as M -- base
import qualified Control.Monad as M       -- base
import qualified DTS.DTSwithVarName as VN
import Interface.Text
import Interface.TeX

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
  Asp Int Preterm |       -- ^ Underspesified terms
  Lamvec Preterm |        -- ^ Lambda abstractions of a variable vector
  Appvec Int Preterm |    -- ^ Function applications of a variable vector
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
  show = T.unpack . toTextDeBruijn

-- | translates a DTS preterm into a simple text notation.
instance SimpleText Preterm where
  toText = toText . initializeIndex . fromDeBruijn

-- | translates a DTS preterm into a tex source code.
instance Typeset Preterm where
  toTeX = toTeX . initializeIndex . fromDeBruijn

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
    DRel i t m n -> T.concat ["DRel", T.pack (show i), "[", t, "](", toTextDeBruijn m, ",", toTextDeBruijn n, ")"]

-- | A type of an element of a type signature, that is, a list of pairs of a preterm and a type.
-- ex. [entity:type, state:type, event:type, student:entity->type]
type Signature = (T.Text,Preterm)

instance SimpleText Signature where
  toText (cname,typ) = T.concat $ [toText (Con cname), ":", toText typ]

-- | prints a signature in text.
printSignatures :: [Signature] -> T.Text
printSignatures sigs = T.concat ["[", (T.intercalate ", " $ map toText sigs), "]"]

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
  DRel j t m n -> DRel j t (subst m l i) (subst n l i)

-- | shiftIndices m d i
-- add d to all the indices that is greater than or equal to i within m (=d-place shift)
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
  DRel j t m n -> DRel j t (shiftIndices m d i) (shiftIndices n d i)
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
  DRel i t m n -> DRel i t (betaReduce m) (betaReduce n)

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
  DRel i text m n -> DRel i text (strongBetaReduce 0 m) (strongBetaReduce 0 n)

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
  DRel j t m n -> DRel j t (addLambda i m) (addLambda i n)
  m -> m

-- | deleteLambda i preterm: the second subroutine for 'transvec' function.
-- this function takes an index and a preterm, transforms the latter in a way that the Var/Appvec with an index j 
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
  DRel j t m n -> DRel j t (deleteLambda i m) (deleteLambda i n)
  m -> m

replaceLambda :: Int -> Preterm -> Preterm
replaceLambda i preterm = case preterm of
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

{- Initializing or Re-indexing of vars, @s and DRels -}

newtype Indexed a = Indexed { indexing :: Int -> Int -> Int -> Int -> (a,Int,Int,Int,Int) }

instance Monad Indexed where
  return m = Indexed (\s x a d -> (m,s,x,a,d))
  (Indexed m) >>= f = Indexed (\s x a d -> let (m',s',x',a',d') = m s x a d;
                                               (Indexed n) = f m' in
                                           n s' x' a' d')

instance Functor Indexed where
  fmap = M.liftM

instance M.Applicative Indexed where
  pure = return
  (<*>) = M.ap

sentenceIndex :: Indexed Int
sentenceIndex = Indexed (\s x a d -> (s,s+1,x,a,d))

varIndex :: Indexed Int
varIndex = Indexed (\s x a d -> (x,s,x+1,a,d))

aspIndex :: Indexed Int
aspIndex = Indexed (\s x a d -> (a,s,x,a+1,d))

dRelIndex :: Indexed Int
dRelIndex = Indexed (\s x a d -> (d,s,x,a,d+1))

-- | re-assigns sequential indices to all asperands that appear in a given preterm.
initializeIndex :: Indexed a -> a
initializeIndex (Indexed m) = let (m',_,_,_,_) = m 1 1 1 1 in m'

fromDeBruijn :: Preterm -> Indexed VN.Preterm
fromDeBruijn = fromDeBruijn2 []

fromDeBruijn2 :: [VN.VName] -> Preterm -> Indexed VN.Preterm
fromDeBruijn2 vnames preterm = case preterm of
  Var j -> if j < length vnames
              then return $ VN.Var (vnames!!j)
              else return $ VN.Con $ T.concat ["error: var ",T.pack (show j), " in ", T.pack (show vnames)]
  Con cname -> return $ VN.Con cname
  Type -> return VN.Type
  Kind -> return VN.Kind
  Pi a b -> do
    i <- varIndex
    let vname = case a of
                  Con cname | cname == "entity" -> ('x',i)
                            | cname == "event"  -> ('e',i)
                            | cname == "state"  -> ('s',i)
                  Type -> ('p',i)
                  Kind -> ('p',i)
                  App _ _   -> ('u',i)
                  Sigma _ _ -> ('u',i)
                  Pi _ _    -> ('u',i)
                  Not _     -> ('u',i)
                  Appvec _ _ -> ('u',i)
                  Eq _ _ _ -> ('s',i)
                  Nat -> ('k',i)
                  _ -> ('x',i)
    a' <- fromDeBruijn2 vnames a
    b' <- fromDeBruijn2 (vname:vnames) b
    return $ VN.Pi vname a' b'
  Not a   -> do
    a' <- fromDeBruijn2 vnames a
    return $ VN.Not a'
  Lam m   -> do
    i <- varIndex
    let vname = case m of
                  Sigma _ _ -> ('x',i)
                  Pi _ _    -> ('x',i)
                  _         -> ('x',i)
    m' <- fromDeBruijn2 (vname:vnames) m
    return $ VN.Lam vname m'
  App m n -> do
    m' <- fromDeBruijn2 vnames m
    n' <- fromDeBruijn2 vnames n
    return $ VN.App m' n'
  Sigma a b -> do
    i <- varIndex
    let vname = case a of
                  Con cname | cname == "entity" -> ('x',i)
                                | cname == "event"  -> ('e',i)
                                | cname == "state"  -> ('s',i)
                  Type -> ('p',i)
                  Kind -> ('p',i)
                  App _ _   -> ('u',i)
                  Sigma _ _ -> ('u',i)
                  Pi _ _    -> ('u',i)
                  Not _     -> ('u',i)
                  Appvec _ _ -> ('u',i)
                  Eq _ _ _ -> ('s',i)
                  Nat -> ('k',i)
                  _ -> ('x',i)
    a' <- fromDeBruijn2 vnames a
    b' <- fromDeBruijn2 (vname:vnames) b
    return $ VN.Sigma vname a' b'
  Pair m n  -> do
    m' <- fromDeBruijn2 vnames m
    n' <- fromDeBruijn2 vnames n
    return $ VN.Pair m' n'
  Proj s m  -> do
    m' <- fromDeBruijn2 vnames m
    return $ case s of
               Fst -> VN.Proj VN.Fst m'
               Snd -> VN.Proj VN.Snd m'
  Lamvec m  -> do
    i <- varIndex
    let vname = ('x',i)
    m' <- fromDeBruijn2 (vname:vnames) m
    return $ VN.Lamvec vname m'
  Appvec j m -> do
    let vname = vnames!!j
    m' <- fromDeBruijn2 vnames m
    return $ VN.Appvec vname m'
  Unit    -> return VN.Unit
  Top     -> return VN.Top
  Bot     -> return VN.Bot
  Asp _ m -> do
    j' <- aspIndex
    m' <- fromDeBruijn2 vnames m
    return $ VN.Asp j' m'
  Nat    -> return VN.Nat
  Zero   -> return VN.Zero
  Succ n -> do
    n' <- fromDeBruijn2 vnames n
    return $ VN.Succ n'
  Natrec n e f -> do
    n' <- fromDeBruijn2 vnames n
    e' <- fromDeBruijn2 vnames e
    f' <- fromDeBruijn2 vnames f
    return $ VN.Natrec n' e' f'
  Eq a m n -> do
    a' <- fromDeBruijn2 vnames a
    m' <- fromDeBruijn2 vnames m
    n' <- fromDeBruijn2 vnames n
    return $ VN.Eq a' m' n'
  Refl a m -> do
    a' <- fromDeBruijn2 vnames a
    m' <- fromDeBruijn2 vnames m
    return $ VN.Refl a' m'
  Idpeel m n -> do
    m' <- fromDeBruijn2 vnames m
    n' <- fromDeBruijn2 vnames n
    return $ VN.Idpeel m' n'
  DRel _ t m n -> do
    j' <- dRelIndex
    m' <- fromDeBruijn2 vnames m
    n' <- fromDeBruijn2 vnames n
    return $ VN.DRel j' t m' n'
    -- App (App (Con (T.concat ["DRel",T.pack $ show j',"[",t,"]"])) m') n'

toDeBruijn :: VN.Preterm -> Preterm
toDeBruijn = toDeBruijn2 []

toDeBruijn2 :: [VN.VName] -> VN.Preterm -> Preterm
toDeBruijn2 vnames preterm = case preterm of
  VN.Var vname -> case L.elemIndex vname vnames of
                    Just i -> Var i
                    Nothing -> Type
  VN.Con cname -> Con cname
  VN.Type -> Type
  VN.Kind -> Kind
  VN.Pi vname a b -> Pi (toDeBruijn2 (vname:vnames) a) (toDeBruijn2 (vname:vnames) b)
  VN.Not a -> Not (toDeBruijn2 vnames a)
  VN.Lam vname m -> Lam (toDeBruijn2 (vname:vnames) m)
  VN.App m n -> App (toDeBruijn2 vnames m) (toDeBruijn2 vnames n)
  VN.Sigma vname a b -> Sigma (toDeBruijn2 (vname:vnames) a) (toDeBruijn2 (vname:vnames) b)
  VN.Pair m n -> Pair (toDeBruijn2 vnames m) (toDeBruijn2 vnames n)
  _ -> Type
