{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Dependent Types (with de Bruijn index)
module DependentTypes (
  Preterm(..),
  Selector(..),
  --
  Typeset(..),
  toTeXWithVN,
  --
  SimpleText(..),
  toTextWithVN,
  --
  subst,
  betaReduce,
  --
  addLambda,
  deleteLambda
  ) where

import qualified Data.Text.Lazy as T

class Typeset a where
  toTeX :: a -> T.Text

data Selector = Fst | Snd
  deriving (Eq, Show)

instance Typeset Selector where
  toTeX Fst = "1"
  toTeX Snd = "2"

data Preterm =
  Var Int |
  Con T.Text |
  Type |
  Kind |
  Pi Preterm Preterm |
  Not Preterm |
  Lam Preterm |
  App Preterm Preterm |
  Sigma Preterm Preterm |
  Pair Preterm Preterm |
  Proj Selector Preterm |
  Unit |
  Top |
  Bot |
  Asp Int Preterm |
  Lamvec Preterm |
  Appvec Int Preterm
    deriving (Eq, Show)

instance Typeset Preterm where
  toTeX preterm = toTeXWithVN [] preterm
{-  
  toTeX preterm = case preterm of
    Var i -> T.pack (show i)
    Con c -> T.concat["\\pred{", c, "}"]
    Type  -> "\\type{type}"
    Kind  -> "\\type{kind}"
    Pi a b -> T.concat["\\dPi[", (T.pack ""), "]{", (toTeX a), "}{", (toTeX b), "}"]
    Not m  -> T.concat["\\neg ", toTeXEmbedded m]
    Lam m  -> T.concat["\\LAM[", (T.pack ""), "]", (toTeX m)]
    App m n -> case n of
                 (Var _) -> T.concat ["\\APP{", (toTeXEmbedded m), "}{(", toTeX n, ")}"]
                 (Con _) -> T.concat ["\\APP{", (toTeXEmbedded m), "}{(", toTeX n, ")}"]
                 (Asp _ _) -> T.concat ["\\APP{", (toTeXEmbedded m), "}{\\left(", toTeX n ,"\\right)}"]
                 _ -> T.concat["\\APP{", toTeXEmbedded m, "}{", toTeXEmbedded n, "}"]
    Lamvec m   -> T.concat ["\\vec{\\lambda}", toTeX m]
    Appvec i m -> T.concat ["\\APP{", (toTeXEmbedded m), "}{\\vec{", T.pack (show i), "}}"]
    Sigma a b -> T.concat["\\dSigma[", (T.pack ""), "]{", toTeX a, "}{", toTeX b, "}"]
    Pair m n  -> T.concat["\\left(", toTeX m, ",", toTeX n, "\\right)"]
    Proj s m  -> T.concat["\\pi_", toTeX s, "\\left(", toTeX m, "\\right)"] 
    Unit      -> "()"
    Top       -> "\\top"
    Bot       -> "\\bot"
    Asp i m   -> T.concat["@_{", T.pack (show i), "}:", (toTeX m)]
    
toTeXEmbedded :: Preterm -> T.Text  
toTeXEmbedded preterm = case preterm of
  Lam m -> T.concat["\\left(\\LAM[]", toTeX m, "\\right)"]
  App m n -> T.concat["\\left(\\APP{", toTeXEmbedded m, "}{", toTeXEmbedded n, "}\\right)"]
  Lamvec m -> T.concat ["\\left(\\vec{\\lambda}.", toTeX m, "\\right)"]
  Appvec i m -> T.concat ["\\left(\\APP{", toTeXEmbedded m, "}{\\vec{x}_", T.pack (show i), "}\\right)"]
  m          -> toTeX m
-}

toTeXWithVN :: [T.Text] -> Preterm -> T.Text
toTeXWithVN varlist preterm = toTeXWithVNLoop varlist preterm 0
-- | toTeXWithVNLoop context preterm index
--
toTeXWithVNLoop :: [T.Text] -> Preterm -> Int -> T.Text
toTeXWithVNLoop vlist preterm i = case preterm of
    Var j -> if j < (length vlist)
             then vlist!!j
             else T.concat ["error:", T.pack (show j), " in ", T.pack (show vlist)]
    Con c -> T.concat["\\pred{", c, "}"]
    Type  -> "\\type{type}"
    Kind  -> "\\type{kind}"
    Pi a b -> let varname = T.concat ["x_{", T.pack (show i), "}"] in 
              T.concat["\\dPi[", varname , "]{", (toTeXWithVNLoop vlist a (i+1)), "}{", (toTeXWithVNLoop (varname:vlist) b (i+1)), "}"]
    Not a  -> T.concat["\\neg ", toTeXWithVNEmbedded vlist a i]
    Lam m  -> let varname = T.concat ["x_{", T.pack (show i), "}"] in
              T.concat["\\LAM[", varname, "]", (toTeXWithVNLoop (varname:vlist) m (i+1))]
    App m n -> case n of
                 (Var _) -> T.concat ["\\APP{", (toTeXWithVNEmbedded vlist m i), "}{(", toTeXWithVNLoop vlist n i, ")}"]
                 (Con _) -> T.concat ["\\APP{", (toTeXWithVNEmbedded vlist m i), "}{(", toTeXWithVNLoop vlist n i, ")}"]
                 (Proj _ _) -> T.concat ["\\APP{", (toTeXWithVNEmbedded vlist m i), "}{\\left(", toTeXWithVNLoop vlist n i, "\\right)}"]
                 (Asp _ _) -> T.concat ["\\APP{", (toTeXWithVNEmbedded vlist m i), "}{\\left(", toTeXWithVNLoop vlist n i,"\\right)}"]
                 _ -> T.concat["\\APP{", toTeXWithVNEmbedded vlist m i, "}{", toTeXWithVNEmbedded vlist n i, "}"]
    Sigma a b -> let varname = T.concat ["x_{", T.pack (show i), "}"] in 
                 T.concat["\\dSigma[", varname, "]{", toTeXWithVNLoop vlist a (i+1), "}{", toTeXWithVNLoop (varname:vlist) b (i+1), "}"]
    Pair m n  -> T.concat["\\left(", toTeXWithVNLoop vlist m i, ",", toTeXWithVNLoop vlist n i, "\\right)"]
    Proj s m  -> T.concat["\\pi_", toTeX s, "\\left(", toTeXWithVNLoop vlist m i, "\\right)"] 
    Lamvec m   -> let varname = T.concat ["x_{", T.pack (show i), "}"] in
                  T.concat ["\\lambda\\vec{", varname, "}.", toTeXWithVNLoop (varname:vlist) m (i+1)]
    Appvec j m -> if j < (length vlist)
                  then T.concat ["\\APP{", (toTeXWithVNEmbedded vlist m i), "}{\\vec{", vlist!!j, "}}"]
                  else T.concat ["\\APP{", (toTeXWithVNEmbedded vlist m i), "}{\\vec{error:", T.pack (show j), " in ", T.pack (show vlist), "}}"]
    Unit      -> "()"
    Top       -> "\\top"
    Bot       -> "\\bot"
    Asp j m   -> T.concat["@_{", T.pack (show j), "}:", (toTeXWithVNLoop vlist m i)]

toTeXWithVNEmbedded :: [T.Text] -> Preterm -> Int -> T.Text  
toTeXWithVNEmbedded vlist preterm i = case preterm of
  Lam m -> let varname = T.concat ["x_{", T.pack (show i), "}"] in
           T.concat["\\left(\\LAM[", varname, "]", toTeXWithVNLoop (varname:vlist) m (i+1), "\\right)"]
  App m n -> T.concat["\\left(\\APP{", toTeXWithVNEmbedded vlist m i, "}{", toTeXWithVNEmbedded vlist n i, "}\\right)"]
  Lamvec m -> let varname = T.concat ["x_{", T.pack (show i), "}"] in
              T.concat ["\\left(\\lambda\\vec{", varname, "}.", toTeXWithVNLoop (varname:vlist) m (i+1), "\\right)"]
--  Appvec j m -> T.concat ["\\left(\\APP{", toTeXWithVNEmbedded vlist m i, "}{\\vec{x}_", T.pack (show j), "}\\right)"]
  m          -> toTeXWithVNLoop vlist m i

class SimpleText a where
  toText :: a -> T.Text

-- | convert a term to a simple notation text
--
instance SimpleText Preterm where
  toText preterm = case preterm of
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
    Proj s m   -> T.concat["π", toTeX s, "(", toText m, ")"] 
    Lamvec m   -> T.concat ["λ+.", toText m]
    Appvec i m -> T.concat ["(", toText m, " ", T.pack (show i), "+)"]
    Unit  -> "()"
    Top   -> "T"
    Bot   -> "⊥"
    Asp i m -> T.concat["@", T.pack (show i), ":", toText m]

-- | "toText" with variable names: convert a term to a non-de-Bruijn notation
--
toTextWithVN :: [T.Text] -> Preterm -> T.Text
toTextWithVN varlist term = 
  toTextWithVNLoop varlist term 0

toTextWithVNLoop :: [T.Text] -> Preterm -> Int -> T.Text
toTextWithVNLoop vlist preterm i = case preterm of
  Var j -> if j < (length vlist)
           then vlist!!j
           else  T.concat ["error",T.pack (show j), " in ", T.pack (show vlist)]
  Con c -> c
  Type  -> "type"
  Kind  -> "kind"
  Pi a b  -> let varname = T.concat ["x", T.pack (show i)] in 
             T.concat ["(", varname, ":", toTextWithVNLoop vlist a (i+1), ")→ ", toTextWithVNLoop (varname:vlist) b (i+1)]
  Not a   -> T.concat["¬", toTextWithVNLoop vlist a i]        
  Lam m   -> let varname = T.concat ["x", T.pack (show i)] in
             T.concat ["λ", varname, ".", toTextWithVNLoop (varname:vlist) m (i+1)]
  App m n -> T.concat ["(", toTextWithVNLoop vlist m i, "\\ ", toTextWithVNLoop vlist n i, ")"]
  Sigma a b -> let varname = T.concat ["x", T.pack (show i)] in 
               T.concat ["(", varname, ":", toTextWithVNLoop vlist a (i+1), ")× ", toTextWithVNLoop (varname:vlist) b (i+1)]
  Pair m n  -> T.concat ["(", toTextWithVNLoop vlist m i, ",", toTextWithVNLoop vlist n i, ")"]    
  Proj s m  -> T.concat ["π", toTeX s, "(", toTextWithVNLoop vlist m i, ")"] 
  Lamvec m  -> let varname = T.concat ["x", T.pack (show i)] in
               T.concat ["λ+", varname, ".", toTextWithVNLoop (varname:vlist) m (i+1)]
  Appvec j m -> T.concat ["(", toTextWithVNLoop vlist m i, "x<", T.pack (show j), ">)"]
  Unit       -> "()"  
  Top        -> "T"
  Bot        -> "⊥"
  Asp j m    -> T.concat["@", T.pack (show j), ":", toTextWithVNLoop vlist m i]

-- | Substitution of the variable i in a preterm M with a preterm L
--   subst M L i = M[L/i]
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

-- | shiftIndices m d i 
--   mの中のi以上のindexに+d (d-place shift)
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
  t -> t

-- | Beta Reduction
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
    --Lamvec v -> betaReduce (shiftIndices (subst (addLambda 0 v) (shiftIndices n 1 0) 0) (-1) 0)
    e -> App e (betaReduce n)
  Sigma a b -> Sigma (betaReduce a) (betaReduce b)
  Pair m n  -> Pair (betaReduce m) (betaReduce n)
  Proj s m  -> case betaReduce m of 
    Pair x y -> case s of 
                  Fst -> x
                  Snd -> y
    e -> (Proj s e)
  Lamvec m   -> Lamvec (betaReduce m)
  Appvec i m -> Appvec i (betaReduce m) 
  Unit -> Unit
  Top  -> Top
  Bot  -> Bot
  Asp i m -> Asp i (betaReduce m)

-- | addLambda i preterm:  
--   preterm中のindex iについて
addLambda :: Int -> Preterm -> Preterm
addLambda i preterm = case preterm of
  Var j      -> if j > i 
                then Var (j+1)
                else if j < i
                     then Var j   
                     else Con $ T.concat [" Error: var ", T.pack (show j)]
  Pi a b     -> Pi (addLambda i a) (addLambda (i+1) b)
  Not a      -> Not (addLambda (i+1) a)
  Lam m      -> Lam (addLambda (i+1) m)
  App m n    -> App (addLambda i m) (addLambda i n)
  Sigma a b  -> Sigma (addLambda i a) (addLambda (i+1) b)
  Pair m n   -> Pair (addLambda i m) (addLambda i n)
  Proj s m   -> Proj s (addLambda i m)
  Lamvec m   -> Lamvec (addLambda (i+1) m)
  Appvec j m -> if j > i 
                then Appvec (j+1) (addLambda i m)
                else if j < i
                     then Appvec j (addLambda i m)
                     else Appvec j (App (addLambda (i+1) m) (Var (i+1)))
  Asp j m    -> Asp j (addLambda i m)
  t -> t

deleteLambda :: Int -> Preterm -> Preterm
deleteLambda i preterm = case preterm of
  Var j      -> if j > i 
                then Var (j-1)
                else if j < i
                     then Var j   
                     else Con $ T.concat ["Error: var ", T.pack (show j)]
  Pi a b     -> Pi (deleteLambda i a) (deleteLambda (i+1) b)
  Not a      -> Not (deleteLambda (i+1) a)
  Lam m      -> Lam (deleteLambda (i+1) m)
  App m n    -> App (deleteLambda i m) (deleteLambda i n)
  Sigma a b  -> Sigma (deleteLambda i a) (deleteLambda (i+1) b)
  Pair m n   -> Pair (deleteLambda i m) (deleteLambda i n)
  Proj s m   -> Proj s (deleteLambda i m)
  Lamvec m   -> Lamvec (deleteLambda (i+1) m)
  Appvec j m -> if j > i 
                then Appvec (j-1) (deleteLambda i m)
                else if j < i
                     then Appvec j (deleteLambda i m)
                     else deleteLambda i m
  Asp j m    -> Asp j (deleteLambda i m)
  t -> t
