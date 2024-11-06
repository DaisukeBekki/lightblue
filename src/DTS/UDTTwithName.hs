{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}

{-|
Module      : DTS.UDTTwithName
Copyright   : (c) Daisuke Bekki, 2024
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Underspecified Dependent Type Theory (with variable names).
-}

module DTS.UDTTwithName (
  -- * Terms and Types
  VarName(..)
  , Selector(..)
  , Preterm(..)
  , toText'
  -- * Conversion btw. UDTT and DTT
  , toUDTT
  , toDTT
  -- * Conversion btw. De Bruijn notation and Variable-name notation
  , fromDeBruijn
  , toDeBruijn
  --, fromDeBruijnLoop -- exportしない方向に
  -- * Judgment
  -- , Signature
  -- , Context
  -- , toVerticalMathML    -- 再考
  -- , printVerticalMathML -- 再考
  -- , fromDeBruijnSignature
  -- , fromDeBruijnContext
  -- , fromDeBruijnContextLoop -- exportしない方向に
  , Judgment(..)
  , fromDeBruijnJudgment
  , TypeCheckQuery
  , fromDeBruijnTypeCheckQuery
  , TypeInferQuery
  , fromDeBruijnTypeInferQuery
  ) where

import qualified GHC.Generics        as G --base
import qualified System.Environment as E -- base
import qualified Data.List as L           --base
import qualified Data.Text.Lazy as T      -- text
--import qualified Data.Text.Lazy.IO as T -- text
import Data.Store (Store(..))             --store
import Interface.Text
import Interface.TeX
import Interface.HTML
import qualified DTS.UDTTdeBruijn as UDTTdB --lightblue
import qualified DTS.DTTwithName as DTTwN   --lightblue
import DTS.Index                            --lightblue
import DTS.GeneralTypeQuery                 --lightblue

-- | A variable name consists of Char (e.g. 'x') and Int (e.g. 1), which is displayed as $x_{1}$ in TeX and $x1$ in Text.
data VarName = VarName Char Int deriving (Eq,Show)

toUDTTvarName :: DTTwN.VarName -> VarName
toUDTTvarName (DTTwN.VarName c i) = VarName c i

instance SimpleText VarName where
  toText (VarName v i) = T.cons v $ T.pack (show i)

instance Typeset VarName where
  toTeX (VarName v i) = T.cons v $ T.concat ["_{", T.pack (show i), "}"]

instance MathML VarName where
  toMathML (VarName v i) = T.concat ["<msub><mi>", T.singleton v, "</mi><mn><mstyle fontsize=8pt>", T.pack (show i), "</mstyle></mn></msub>"]

-- | 'Proj' 'Fst' m is the first projection of m, while 'Proj' 'Snd' m is the second projection of m.
data Selector = Fst | Snd
  deriving (Eq, Show)

-- | translates a selector into either 1 or 2.
instance SimpleText Selector where
  toText Fst = "1"  -- `Proj` `Fst` m is the first projection of m
  toText Snd = "2" -- `Proj` `Snd` m is the second projection of m

instance Typeset Selector where
  toTeX = toText

instance MathML Selector where
  toMathML Fst = "<mn>1</mn>"  -- `Proj` `Fst` m is the first projection of m
  toMathML Snd = "<mn>2</mn>" -- `Proj` `Snd` m is the second projection of m

-- | Preterms of Underspecified Dependent Type Theory (UDTT).
data Preterm = 
  -- | Basic Preterms
  Var VarName                             -- ^ Variables
  | Con T.Text                            -- ^ Constant symbols
  | Type                                  -- ^ The sort \"type\"
  | Kind                                  -- ^ The sort \"kind\"
  -- | Pi types
  | Pi VarName Preterm Preterm            -- ^ Pi types
  | Lam VarName Preterm                   -- ^ Lambda abstraction
  | App Preterm Preterm                   -- ^ Function Application
  | Not Preterm                           -- ^ Negation
  -- | Sigma types
  | Sigma VarName Preterm Preterm         -- ^ Dependent product type (or Sigma type)
  | Pair Preterm Preterm                  -- ^ Pair
  | Proj Selector Preterm                 -- ^ (First and second) Projections
  -- | Disjoint Union Types
  | Disj Preterm Preterm                  -- ^ Disjoint Union types
  | Iota Selector Preterm                 -- ^ (FIrst and second) Injections
  | Unpack Preterm Preterm Preterm Preterm  -- ^ Unpack P L M N
  -- | Enumeration Types
  | Bot                                   -- ^ The bottom type
  | Unit                                  -- ^ The unit term (of type Top)
  | Top                                   -- ^ The top type
  | Entity                                -- ^ The entity type
  -- | Natural Number Types
  | Nat                                   -- ^ Natural number type (Nat)
  | Zero                                  -- ^ 0 (of type Nat)
  | Succ Preterm                          -- ^ The successor function
  | Natrec Preterm Preterm Preterm        -- ^ natrec
  -- | Intensional Equality Types
  | Eq Preterm Preterm Preterm            -- ^ Intensional equality types
  | Refl Preterm Preterm                  -- ^ refl
  | Idpeel Preterm Preterm                -- ^ idpeel
  -- | UDTT extensions
  | Asp Preterm                           -- ^ The underspesified term
  | Lamvec VarName Preterm                -- ^ Variable-length lambda abstraction
  | Appvec VarName Preterm                -- ^ Variable-length function application
  -- | ToDo: add First Universe
  deriving (Eq, G.Generic)

{- Printing of Preterms -}

instance Show Preterm where
  show = T.unpack . toText

instance SimpleText Preterm where
  toText preterm = toText' True preterm

-- | flag=True : f(y)(x) is printed as f(x,y)
-- | flag=False : f(y)(x) is printed as it is
toText' :: Bool -> Preterm -> T.Text
toText' flag preterm = case preterm of
    Var vname -> toText vname
    Con cname -> cname
    Type -> "type"
    Kind -> "kind"
    Pi vname a b -> case b of
                      Bot -> T.concat["¬", toText' flag a]
                      b' -> T.concat ["(", toText vname, ":", toText' flag a, ")→ ", toText' flag b']
    Lam vname m -> T.concat ["λ", toText vname, ".", toText' flag m]
    App (App (Con cname) y) x ->
      if flag
        then T.concat [cname, "(", toText' flag x, ",", toText' flag y, ")"]
        else T.concat [cname, "(", toText' flag y, ")(", toText' flag x, ")"]
    App (App (App (Con cname) z) y) x ->
      if flag
        then T.concat [cname, "(", toText' flag x, ",", toText' flag y, ",",toText' flag z, ")"]
        else T.concat [cname, "(", toText' flag z, ")(", toText' flag y, ")(",toText' flag x, ")"]
    App (App (App (App (Con cname) u) z) y) x ->
      if flag
        then T.concat [cname, "(", toText' flag x, ",", toText' flag y, ",",toText' flag z, ",", toText' flag u, ")"]
        else T.concat [cname, "(", toText' flag u, ")(", toText' flag z, ")(", toText' flag y, ")(", toText' flag x, ")"]
    App m n -> T.concat [toText' flag m, "(", toText' flag n, ")"]
    Sigma vname a b -> case b of
                         Top -> T.concat ["(", toText' flag a, ")"]
                         _   -> T.concat ["(", toText vname, ":", toText' flag a, ")✕", toText' flag b]
    Pair m n  -> T.concat ["(", toText' flag m, ",", toText' flag n, ")"]
    Proj s m  -> T.concat ["π", toText s, "(", toText' flag m, ")"]
    Disj a b -> T.concat [toText' flag a, " + ", toText' flag b]
    Iota s m -> T.concat ["ι", toText s, "(", toText' flag m, ")"]
    Unpack p l m n -> T.concat ["unpack(", toText' flag p, ",", toText' flag l, ",", toText' flag m, ",", toText' flag n, ")"]
    Bot        -> "⊥"
    Unit       -> "()"
    Top        -> "T"
    Entity     -> "entity"
    Nat    -> "N"
    Zero   -> "0"
    Succ n -> T.concat ["s", toText' flag n]
    Natrec n e f -> T.concat ["natrec(", toText' flag n, ",", toText' flag e, ",", toText' flag f, ")"]
    Eq a m n -> T.concat [toText' flag m, "=[", toText' flag a, "]", toText' flag n]
    Refl a m -> T.concat ["refl", toText' flag a, "(", toText' flag m, ")"]
    Idpeel m n -> T.concat ["idpeel(", toText' flag m, ",", toText' flag n, ")"]
    Asp m    -> T.concat ["@", toText' flag m]
    Lamvec vname m  -> T.concat ["λ", toText vname, "+.", toText' flag m]
    Appvec vname m -> T.concat ["(", toText' flag m, " ", toText vname, "+)"]
    _ -> "Error: The definition of DTS.UDTTwithname.toText' is not exhaustive."

-- | Each `Preterm` is translated by the `toTeX` method into a representation \"with variable names\" in a TeX source code.
instance Typeset Preterm where
  toTeX preterm = case preterm of
    Var vname -> toTeX vname
    Con c -> T.concat["\\pred{", T.replace "~" "\\~{}" c, "}"]
    Type  -> "\\type{type}"
    Kind  -> "\\type{kind}"
    Pi vname a b -> T.concat["\\dPi[", toTeX vname , "]{", toTeX a, "}{", toTeX b, "}"]
    Not a  -> T.concat["\\neg ", toTeXEmbedded a]
    Lam vname m  -> T.concat["\\LAM[", toTeX vname, "]", toTeX m]
    (App (App (Con c) y) x) -> T.concat ["\\APP{", toTeXEmbedded (Con c), "}{\\left(", toTeX x, ",", toTeX y, "\\right)}" ]
    (App (App (App (Con c) z) y) x) -> T.concat ["\\APP{", toTeXEmbedded (Con c), "}{\\left(", toTeX x, ",", toTeX y, ",", toTeX z, "\\right)}" ]
    (App (App (App (App (Con c) u) z) y) x) -> T.concat ["\\APP{", toTeXEmbedded (Con c), "}{\\left(", toTeX x, ",", toTeX y, ",", toTeX z, ",", toTeX u, "\\right)}" ]
    App m n -> case n of
                 (Var _) -> T.concat ["\\APP{", toTeXEmbedded m, "}{(", toTeX n, ")}"]
                 (Con _) -> T.concat ["\\APP{", toTeXEmbedded m, "}{(", toTeX n, ")}"]
                 (Proj _ _) -> T.concat ["\\APP{", toTeXEmbedded m, "}{\\left(", toTeX n, "\\right)}"]
                 (Asp _) -> T.concat ["\\APP{", toTeXEmbedded m, "}{\\left(", toTeX n,"\\right)}"]
                 _ -> T.concat["\\APP{", toTeXEmbedded m, "}{", toTeXEmbedded n, "}"]
    Sigma vname a b -> case b of
                         Top -> toTeX a
                         _   -> T.concat ["\\dSigma[", toTeX vname, "]{", toTeX a, "}{", toTeX b, "}"]
    Pair m n  -> T.concat ["\\left(", toTeX m, ",", toTeX n, "\\right)"]
    Proj s m  -> T.concat ["\\pi_{", toTeX s, "}\\left(", toTeX m, "\\right)"]
    Disj a b -> T.concat [toTeX a, " + ", toTeX b]
    Iota s m -> T.concat ["\\iota_{", toTeX s, "}\\left(", toTeX m, "\\right)"]
    Unpack p l m n -> T.concat ["\\type{unpack}\\left(", toTeX p, ",", toTeX l, ",", toTeX m, ",", toTeX n, "\\right)"]
    Unit      -> "()"
    Top       -> "\\top"
    Bot       -> "\\bot"
    Entity    -> "\\type{entity}"
    Nat       -> "\\Set{N}"
    Zero      -> "0"
    Succ n    -> T.concat ["\\type{s}", toTeX n]
    Natrec n e f -> T.concat ["\\type{natrec}\\left(", toTeX n, ",", toTeX e, ",", toTeX f, "\\right)"]
    Eq a m n  -> T.concat [toTeX m, "=_{", toTeX a,"}", toTeX n]
    Refl a m  -> T.concat ["\\type{refl}_{", toTeX a, "}\\left(", toTeX m,"\\right)"]
    Idpeel m n -> T.concat ["\\type{idpeel}\\left(", toTeX m, ",", toTeX n, "\\right)"]
    Asp m     -> T.concat ["@", toTeX m]
    Lamvec vname m   -> T.concat ["\\lambda\\vec{", toTeX vname, "}.", toTeX m]
    Appvec vname m -> T.concat ["\\APP{", toTeXEmbedded m, "}{\\vec{", toTeX vname, "}}"]
    _ -> "Error: The definition of DTS.UDTTwithname.toTeX is not exhaustive."

toTeXEmbedded :: Preterm -> T.Text
toTeXEmbedded preterm = case preterm of
  Lam vname m -> T.concat["\\left(\\LAM[", toTeX vname, "]", toTeX m, "\\right)"]
  Lamvec vname m -> T.concat ["\\left(\\lambda\\vec{", toTeX vname, "}.", toTeX m, "\\right)"]
  m          -> toTeX m

instance MathML Preterm where
  toMathML preterm = case preterm of
    Var vname -> toMathML vname
    Con cname -> T.concat ["<mtext>", cname, "</mtext>"]
    Type -> "<mi>type</mi>"
    Kind -> "<mi>kind</mi>"
    Pi vname a b -> T.concat ["<mrow><mo>(</mo>", toMathML vname, "<mo>:</mo>", toMathML a, "<mo>&rarr;</mo>", toMathML b, "</mrow>"]
    Not a -> T.concat["<mrow><mi>&not;</mi>", toMathML a, "</mrow>"]
    Lam vname m -> T.concat ["<mrow><mi>&lambda;</mi>", toMathML vname, "<mpadded lspace='-0.2em' width='-0.2em'><mo>.</mo></mpadded>", toMathML m, "</mrow>"]
    App (App (Con cname) y) x ->
      T.concat ["<mrow><mtext>", cname, "</mtext><mo>(</mo>", toMathML x, "<mo>,</mo>", toMathML y,"<mo>)</mo></mrow>"]
    App (App (App (Con cname) z) y) x ->
      T.concat ["<mrow><mtext>", cname, "</mtext><mo>(</mo>", toMathML x, "<mo>,</mo>", toMathML y, "<mo>,</mo>", toMathML z,"<mo>)</mo></mrow>"]
    App (App (App (App (Con cname) u) z) y) x ->
      T.concat ["<mrow><mtext>", cname, "</mtext><mo>(</mo>", toMathML x, "<mo>,</mo>", toMathML y, "<mo>,</mo>", toMathML z, "<mo>,</mo>", toMathML u, "<mo>)</mo></mrow>"]
    App m n -> T.concat ["<mrow>", toMathML m, "<mo>(</mo>", toMathML n, "<mo>)</mo></mrow>"]
    Sigma vname a b -> case b of
                         Top -> toMathML a
                         _   -> T.concat ["<mrow><mo>[</mo><mtable columnalign='left'><mtr><mtd>", toMathML vname, "<mo>:</mo>", toMathML a, "</mtd></mtr><mtr><mtd><mpadded height='-0.5em'>", toMathML b, "</mpadded></mtd></mtr></mtable><mo>]</mo></mrow>"]
    Pair m n  -> T.concat ["<mrow><mo>(</mo>", toMathML m, "<mo>,</mo>", toMathML n, "<mo>)</mo></mrow>"]
    Proj s m  -> T.concat ["<mrow><msub><mi>&pi;</mi>", toMathML s, "</msub><mo>(</mo>", toMathML m, "<mo>)</mo></mrow>"]
    Disj a b  -> T.concat ["<mrow>", toMathML a, "<mo>+</mo>", toMathML b, "</mrow>"]
    Iota s m  -> T.concat ["<mrow><msub><mi>&iota;</mi>", toMathML s, "</msub><mo>(</mo>", toMathML m, "<mo>)</mo></mrow>"]
    Unpack p l m n -> T.concat ["<mrow><msubsup><mi>unpack</mi><mn>", toMathML l, "</mn><mn>", toMathML p, "</mn></msubsup><mo>(</mo>", toMathML m, "<mo>,</mo>", toMathML n,"<mo>)</mo></mrow>"]
    Unit       -> "<mi>()</mi>"
    Top        -> "<mi>&top;</mi>"
    Bot        -> "<mi>&bot;</mi>"
    Entity     -> "<mi>entity</mi>"
    Nat    -> "<mi>N</mi>"
    Zero   -> "<mi>0</mi>"
    Succ n -> T.concat ["<mrow><mi>s</mi>", toMathML n, "</mrow>"]
    Natrec n e f -> T.concat ["<mrow><mi>natrec</mi><mo>(</mo>", toMathML n, toMathML e, toMathML f, "<mo>)</mo></mrow>"]
    Eq a m n -> T.concat ["<mrow>", toMathML m, "<msub><mo>=</mo>", toMathML a, "</msub>", toMathML n, "</mrow>"]
    Refl a m -> T.concat ["<mrow><mi>refl</mi>", toMathML a, "<mo>(</mo>", toMathML m, "<mo>)</mo></mrow>"]
    Idpeel m n -> T.concat ["<mrow><mi>idpeel</mi><mo>(</mo>", toMathML m, toMathML n, "<mo>)</mo></mrow>"]
    Asp m      -> T.concat["<mrow><mo>@</mo>", toMathML m, "</mrow>"]
    Lamvec vname m  -> T.concat ["<mrow><mi>&lambda;</mi><mover>", toMathML vname, "<mo>&rarr;</mo></mover><mo>.</mo>", toMathML m, "</mrow>"]
    Appvec vname m -> T.concat ["<mrow>", toMathML m, "<mo> </mo><mover>", toMathML vname, "<mo>&rarr;</mo></mover></mrow>"]
    _ -> "Error: The definition of DTS.UDTTwithname.toMathML is not exhaustive."
  
{- Conversion between UDTT and DTT -}

-- | from DTT to UDTT
toUDTT :: DTTwN.Preterm -> Preterm
toUDTT preterm = case preterm of
  DTTwN.Var (DTTwN.VarName c i) -> Var (VarName c i)
  DTTwN.Con t -> Con t
  DTTwN.Type -> Type
  DTTwN.Kind -> Kind
  DTTwN.Pi (DTTwN.VarName c i) a b -> Pi (VarName c i) (toUDTT a) (toUDTT b)
  DTTwN.Not a  -> Not (toUDTT a)
  DTTwN.Lam (DTTwN.VarName c i) m  -> Lam (VarName c i) (toUDTT m)
  DTTwN.App m n -> App (toUDTT m) (toUDTT n)
  DTTwN.Sigma (DTTwN.VarName c i) a b -> Sigma (VarName c i) (toUDTT a) (toUDTT b)
  DTTwN.Pair m n  -> Pair (toUDTT m) (toUDTT n)
  DTTwN.Proj sel m  -> Proj (case sel of
                              DTTwN.Fst -> Fst
                              DTTwN.Snd -> Snd
                              ) (toUDTT m)
  DTTwN.Disj a b -> Disj (toUDTT a) (toUDTT b)
  DTTwN.Iota sel m -> Iota (case sel of 
                             DTTwN.Fst -> Fst
                             DTTwN.Snd -> Snd
                             ) (toUDTT m)
  DTTwN.Unpack p l m n -> Unpack (toUDTT p) (toUDTT l) (toUDTT m) (toUDTT n)
  DTTwN.Bot     -> Bot
  DTTwN.Unit    -> Unit
  DTTwN.Top     -> Top
  DTTwN.Entity  -> Entity
  DTTwN.Nat     -> Nat
  DTTwN.Zero    -> Zero
  DTTwN.Succ n  -> Succ (toUDTT n)
  DTTwN.Natrec e f n -> Natrec (toUDTT e) (toUDTT f) (toUDTT n)
  DTTwN.Eq a m n     -> Eq (toUDTT a) (toUDTT m) (toUDTT n)
  DTTwN.Refl a m     -> Refl (toUDTT a) (toUDTT m)
  DTTwN.Idpeel m n   -> Idpeel (toUDTT m) (toUDTT n)

-- | from UDTT to DTT
toDTT :: Preterm -> Maybe DTTwN.Preterm
toDTT preterm = case preterm of
  Var (VarName c i) -> return $ DTTwN.Var (DTTwN.VarName c i)
  Con t -> return $ DTTwN.Con t
  Type  -> return DTTwN.Type
  Kind  -> return DTTwN.Kind
  Pi (VarName c i) a b -> do
            a' <- toDTT a
            b' <- toDTT b
            return $ DTTwN.Pi (DTTwN.VarName c i) a' b'
  Lam (VarName c i) m  -> do
            m' <- toDTT m
            return $ DTTwN.Lam (DTTwN.VarName c i) m'
  App m n -> do
             m' <- toDTT m
             n' <- toDTT n
             return $ DTTwN.App m' n'
  Not m  -> do
            m' <- toDTT m
            return $ DTTwN.Pi (DTTwN.VarName '_' 0) m' DTTwN.Bot
  Sigma (VarName c i) a b -> do
               a' <- toDTT a
               b' <- toDTT b
               return $ DTTwN.Sigma (DTTwN.VarName c i) a' b'
  Pair m n  -> do
               m' <- toDTT m
               n' <- toDTT n
               return $ DTTwN.Pair m' n'
  Proj sel m  -> do
                 m' <- toDTT m
                 return $ DTTwN.Proj (case sel of Fst -> DTTwN.Fst; Snd -> DTTwN.Snd) m'
  Disj a b -> do
              a' <- toDTT a
              b' <- toDTT b
              return $ DTTwN.Disj a' b'
  Iota sel m -> do 
                m' <- toDTT m
                return $ DTTwN.Iota (case sel of Fst -> DTTwN.Fst; Snd -> DTTwN.Snd) m'
  Unpack p l m n -> do
                    p' <- toDTT p
                    l' <- toDTT l
                    m' <- toDTT m
                    n' <- toDTT n
                    return $ DTTwN.Unpack p' l' m' n'
  Bot    -> return DTTwN.Bot
  Unit   -> return DTTwN.Unit
  Top    -> return DTTwN.Top
  Entity -> return DTTwN.Entity
  Nat    -> return DTTwN.Nat
  Zero   -> return DTTwN.Zero
  Succ n  -> do
             n' <- toDTT n
             return $ DTTwN.Succ n'
  Natrec e f n -> do
                  e' <- toDTT e
                  f' <- toDTT f
                  n' <- toDTT n
                  return $ DTTwN.Natrec e' f' n'
  Eq a m n     -> do
                  a' <- toDTT a
                  m' <- toDTT m
                  n' <- toDTT n
                  return $ DTTwN.Eq a' m' n'
  Refl a m     -> do
                  a' <- toDTT a
                  m' <- toDTT m
                  return $ DTTwN.Refl a' m'
  Idpeel m n   -> do
                  m' <- toDTT m
                  n' <- toDTT n
                  return $ DTTwN.Idpeel m' n'
  Asp _   -> Nothing
  Lamvec _ _ -> Nothing
  Appvec _ _ -> Nothing

-- | Conversion btw. de Bruijn notation and a variable name notation.
fromDeBruijn :: [VarName] -> UDTTdB.Preterm -> Preterm
fromDeBruijn ctx = initializeIndex . (fromDeBruijnLoop ctx)

fromDeBruijnLoop :: [VarName] -- ^ A context (= a list of variable names)
                    -> UDTTdB.Preterm  -- ^ A preterm in de Bruijn notation
                    -> Indexed Preterm -- ^ A preterm with variable names
fromDeBruijnLoop vnames preterm = case preterm of
  UDTTdB.Var j -> if j < length vnames
                      then return $ Var (vnames!!j)
                      else return $ Con $ T.concat ["fromDeBruijn error: var ",T.pack (show j), " in ", T.pack (show vnames)]
  UDTTdB.Con cname -> return $ Con cname
  UDTTdB.Type -> return Type
  UDTTdB.Kind -> return Kind
  UDTTdB.Pi a b -> do
    vname <- variableNameFor a
    a' <- fromDeBruijnLoop vnames a
    b' <- fromDeBruijnLoop (vname:vnames) b
    return $ Pi vname a' b'
  UDTTdB.Lam m   -> do
    i <- xIndex
    let vname = case m of
                  UDTTdB.Sigma _ _ -> VarName 'x' i
                  UDTTdB.Pi _ _    -> VarName 'x' i
                  _         -> VarName 'x' i
    m' <- fromDeBruijnLoop (vname:vnames) m
    return $ Lam vname m'
  UDTTdB.App m n -> do
    m' <- fromDeBruijnLoop vnames m
    n' <- fromDeBruijnLoop vnames n
    return $ App m' n'
  UDTTdB.Not a   -> do
    a' <- fromDeBruijnLoop vnames a
    return $ Not a'
  UDTTdB.Sigma a b -> do
    vname <- variableNameFor a
    a' <- fromDeBruijnLoop vnames a
    b' <- fromDeBruijnLoop (vname:vnames) b
    return $ Sigma vname a' b'
  UDTTdB.Pair m n  -> do
    m' <- fromDeBruijnLoop vnames m
    n' <- fromDeBruijnLoop vnames n
    return $ Pair m' n'
  UDTTdB.Proj s m  -> do
    m' <- fromDeBruijnLoop vnames m
    return $ Proj (case s of UDTTdB.Fst -> Fst; UDTTdB.Snd -> Snd) m'
  UDTTdB.Disj a b -> do
    a' <- fromDeBruijnLoop vnames a
    b' <- fromDeBruijnLoop vnames b
    return $ Disj a' b'
  UDTTdB.Iota s m -> do
    m' <- fromDeBruijnLoop vnames m
    return $ Iota (case s of UDTTdB.Fst -> Fst; UDTTdB.Snd -> Snd) m'
  UDTTdB.Unpack p l m n -> do
    p' <- fromDeBruijnLoop vnames p
    l' <- fromDeBruijnLoop vnames l
    m' <- fromDeBruijnLoop vnames m
    n' <- fromDeBruijnLoop vnames n
    return $ Unpack p' l' m' n'
  UDTTdB.Bot     -> return Bot
  UDTTdB.Unit    -> return Unit
  UDTTdB.Top     -> return Top
  UDTTdB.Entity  -> return Entity
  UDTTdB.Nat    -> return Nat
  UDTTdB.Zero   -> return Zero
  UDTTdB.Succ n -> do
    n' <- fromDeBruijnLoop vnames n
    return $ Succ n'
  UDTTdB.Natrec n e f -> do
    n' <- fromDeBruijnLoop vnames n
    e' <- fromDeBruijnLoop vnames e
    f' <- fromDeBruijnLoop vnames f
    return $ Natrec n' e' f'
  UDTTdB.Eq a m n -> do
    a' <- fromDeBruijnLoop vnames a
    m' <- fromDeBruijnLoop vnames m
    n' <- fromDeBruijnLoop vnames n
    return $ Eq a' m' n'
  UDTTdB.Refl a m -> do
    a' <- fromDeBruijnLoop vnames a
    m' <- fromDeBruijnLoop vnames m
    return $ Refl a' m'
  UDTTdB.Idpeel m n -> do
    m' <- fromDeBruijnLoop vnames m
    n' <- fromDeBruijnLoop vnames n
    return $ Idpeel m' n'
  UDTTdB.Asp m -> do
    m' <- fromDeBruijnLoop vnames m
    return $ Asp m'
  UDTTdB.Lamvec m  -> do
    i <- xIndex
    let vname = VarName 'x' i
    m' <- fromDeBruijnLoop (vname:vnames) m
    return $ Lamvec vname m'
  UDTTdB.Appvec j m -> do
    let vname = vnames!!j
    m' <- fromDeBruijnLoop vnames m
    return $ Appvec vname m'

variableNameFor :: UDTTdB.Preterm -> Indexed VarName
variableNameFor preterm =
  case preterm of
    UDTTdB.Con cname | cname == "entity" -> do i <- xIndex; return $ VarName 'x' i
              | cname == "evt"    -> do i <- eIndex; return $ VarName 'e' i
              -- cname == "state"  -> VN.VN.VarName 's' i
    UDTTdB.Eq _ _ _ -> do i <- xIndex; return $ VarName 's' i
    UDTTdB.Nat      -> do i <- xIndex; return $ VarName 'k' i
    _        -> do i <- uIndex; return $ VarName 'u' i

-- | translates a preterm with variable name into a preterm in de Bruijn notation.
toDeBruijn :: [VarName]  -- ^ A context (= a list of variable names)
              -> Preterm -- ^ A preterm with variable names
              -> UDTTdB.Preterm   -- ^ A preterm in de Bruijn notation
toDeBruijn vnames preterm = case preterm of
  Var vname -> case L.elemIndex vname vnames of
                    Just i -> UDTTdB.Var i
                    Nothing -> UDTTdB.Con "Error: vname not found in toDeBruijn Var"
  Con cname -> UDTTdB.Con cname
  Type -> UDTTdB.Type
  Kind -> UDTTdB.Kind
  Pi vname a b -> UDTTdB.Pi (toDeBruijn vnames a) (toDeBruijn (vname:vnames) b)
  Lam vname m -> UDTTdB.Lam (toDeBruijn (vname:vnames) m)
  App m n -> UDTTdB.App (toDeBruijn vnames m) (toDeBruijn vnames n)
  Not a -> UDTTdB.Not (toDeBruijn vnames a)
  Sigma vname a b -> UDTTdB.Sigma (toDeBruijn vnames a) (toDeBruijn (vname:vnames) b)
  Pair m n -> UDTTdB.Pair (toDeBruijn vnames m) (toDeBruijn vnames n)
  Proj s m -> UDTTdB.Proj (case s of Fst -> UDTTdB.Fst; Snd -> UDTTdB.Snd) (toDeBruijn vnames m)
  Disj a b -> UDTTdB.Disj (toDeBruijn vnames a) (toDeBruijn vnames b)
  Iota s m -> UDTTdB.Iota (case s of Fst -> UDTTdB.Fst; Snd -> UDTTdB.Snd) (toDeBruijn vnames m)
  Unpack p l m n -> UDTTdB.Unpack (toDeBruijn vnames p) (toDeBruijn vnames l) (toDeBruijn vnames m) (toDeBruijn vnames n) 
  Bot -> UDTTdB.Bot
  Unit -> UDTTdB.Unit
  Top -> UDTTdB.Top
  Entity -> UDTTdB.Entity
  Nat -> UDTTdB.Nat
  Zero -> UDTTdB.Zero
  Succ n -> UDTTdB.Succ (toDeBruijn vnames n)
  Natrec n e f -> UDTTdB.Natrec (toDeBruijn vnames n) (toDeBruijn vnames e) (toDeBruijn vnames f)
  Eq a m n -> UDTTdB.Eq (toDeBruijn vnames a) (toDeBruijn vnames m) (toDeBruijn vnames n)
  Refl a m -> UDTTdB.Refl (toDeBruijn vnames a) (toDeBruijn vnames m)
  Idpeel m n -> UDTTdB.Idpeel (toDeBruijn vnames m) (toDeBruijn vnames n)
  Asp m -> UDTTdB.Asp (toDeBruijn vnames m)
  Lamvec vname m -> UDTTdB.Lamvec (toDeBruijn (vname:vnames) m)
  Appvec vname m -> case L.elemIndex vname vnames of
                        Just i -> UDTTdB.Appvec i (toDeBruijn vnames m)
                        Nothing -> UDTTdB.Con "Error: vname not found in toDeBruijn Appvec"

-- | The data type for a judgment
data Judgment = Judgment {
  signtr :: DTTwN.Signature  -- ^ A signature
  , contxt :: DTTwN.Context  -- ^ A context \Gamma in \Gamma \vdash M:A
  , trm :: Preterm     -- ^ A term M in \Gamma \vdash M:A
  , typ :: DTTwN.Preterm     -- ^ A type A in \Gamma \vdash M:A
  } deriving (Eq)

fromDeBruijnJudgment :: UDTTdB.Judgment -> Judgment
fromDeBruijnJudgment judgment = 
  initializeIndex $ do
    let vsig = DTTwN.fromDeBruijnSignature $ UDTTdB.signtr judgment
    vcontext <- (fmap reverse) $ DTTwN.fromDeBruijnContextLoop [] $ reverse $ UDTTdB.contxt judgment
    let varnames = fst $ unzip vcontext
    vterm <- fromDeBruijnLoop (map toUDTTvarName varnames) $ UDTTdB.trm judgment
    vtype <- DTTwN.fromDeBruijnLoop varnames $ UDTTdB.typ judgment 
    return $ Judgment {
      signtr = vsig
      , contxt = vcontext
      , trm = vterm
      , typ = vtype
      }

embedJudgment :: Judgment -> GeneralTypeQuery DTTwN.Signature DTTwN.Context Preterm DTTwN.Preterm
embedJudgment (Judgment sig cxt trm typ) = GeneralTypeQuery sig cxt (Term trm) (Term typ)

instance Show Judgment where
  show = T.unpack . toText
instance SimpleText Judgment where
  toText = toText . embedJudgment
instance Typeset Judgment where
  toTeX = toTeX . embedJudgment
instance MathML Judgment where
  toMathML = toMathML . embedJudgment

type TypeCheckQuery = Judgment

fromDeBruijnTypeCheckQuery :: UDTTdB.TypeCheckQuery -> TypeCheckQuery
fromDeBruijnTypeCheckQuery = fromDeBruijnJudgment

data TypeInferQuery = TypeInferQuery DTTwN.Signature DTTwN.Context Preterm deriving (Eq)

fromDeBruijnTypeInferQuery :: UDTTdB.TypeInferQuery -> TypeInferQuery
fromDeBruijnTypeInferQuery (UDTTdB.TypeInferQuery signtr contxt trm) = 
  initializeIndex $ do
    let vsig = DTTwN.fromDeBruijnSignature signtr
    vcontext <- (fmap reverse) $ DTTwN.fromDeBruijnContextLoop [] $ reverse contxt
    let varnames = fst $ unzip vcontext
    vterm <- fromDeBruijnLoop (map toUDTTvarName varnames) trm
    return $ TypeInferQuery vsig vcontext vterm 

embedTypeInferQuery :: TypeInferQuery -> GeneralTypeQuery DTTwN.Signature DTTwN.Context Preterm DTTwN.Preterm
embedTypeInferQuery (TypeInferQuery sig cxt trm) = GeneralTypeQuery sig cxt (Term trm) Question

instance Show TypeInferQuery where
  show = T.unpack . toText
instance SimpleText TypeInferQuery where
  toText = toText . embedTypeInferQuery
instance Typeset TypeInferQuery where
  toTeX = toTeX . embedTypeInferQuery
instance MathML TypeInferQuery where
  toMathML = toMathML . embedTypeInferQuery


-- {- Judgment -}
-- type Signature = [(T.Text, Preterm)]

-- instance SimpleText Signature where
--   toText = toText . fromDeBruijnSignature

-- instance Typeset Signature where
--   toTeX = toTeX . fromDeBruijnSignature

-- instance MathML Signature where
--   toMathML = toMathML . fromDeBruijnSignature

-- -- | A context is a list of pairs of a variable and a preterm.
-- type Context = [(VarName, Preterm DTT)]

-- -- instance SimpleText Context where
-- --   toText = toText . fromDeBruijnContext

-- -- instance Typeset Context where
-- --   toTeX = toTeX . fromDeBruijnContext

-- -- instance MathML Context where
-- --   toMathML = toMathML . fromDeBruijnContext

-- instance SimpleText Context where
--   toText = (T.intercalate ", ") . (map (\(nm,tm) -> T.concat [toText nm, ":", toText tm])) . reverse

-- instance Typeset Context where
--   toTeX = (T.intercalate ",") . (map (\(nm,tm) -> T.concat [toTeX nm, ":", toTeX tm])) . reverse

-- instance MathML Context where
--   toMathML cont = T.concat $ ["<mfenced separators=',' open='' close=''>"] ++ (map (\(nm,tm) -> T.concat ["<mrow>", toMathML nm, "<mo>:</mo>", toMathML tm, "</mrow>"]) $ reverse cont) ++ ["</mfenced>"]

-- -- | prints a context vertically.
-- toVerticalMathML :: Context -> T.Text
-- toVerticalMathML cont = T.concat $ ["<mtable columnalign='left'>"] ++ (map (\(nm,tm) -> T.concat ["<mtr><mtd>", toMathML nm, "<mo>:</mo>", toMathML tm, "</mtd></mtr>"]) $ reverse cont) ++ ["</mtable>"]

-- -- | prints an SR list vertically.
-- printVerticalMathML :: [(VarName, Preterm)] -> IO()
-- printVerticalMathML cont = do
--   T.putStr "<mtable columnalign='left'>"
--   mapM_ (\(nm,tm) -> mapM_ T.putStr ["<mtr><mtd>",
--                                      toMathML nm,
--                                      "<mo>:</mo>",
--                                      toMathML tm,
--                                      "</mtd></mtr>"
--                                      ]) cont
--   T.putStrLn "</mtable>"

-- -- | The data type for a judgment
-- data Judgment = Judgment {
--   signtr :: Signature, 
--   context :: Context,  -- ^ A context \Gamma in \Gamma \vdash M:A
--   term :: Preterm,     -- ^ A term M in \Gamma \vdash M:A
--   typ :: Preterm -- ^ A type A in \Gamma \vdash M:A
--   } deriving (Eq)

-- instance SimpleText Judgment where
--   toText j = T.concat [toText $ context j, " |- ", toText $ term j, ":", toText $ typ j]

-- instance Typeset Judgment a where
--   toTeX j = T.concat [toTeX $ context j, "{\\vdash}", toTeX $ term j, "{:}", toTeX $ typ j]

-- instance MathML Judgment a where
--   toMathML j = T.concat ["<mrow>", toMathML $ context j, "<mo>&vdash;</mo>", toMathML $ term j, "<mo>:</mo>", toMathML $ typ j, "</mrow>"]

-- fromDeBruijnSignature :: Signature -> VN.Signature
-- fromDeBruijnSignature = map (\(cname, ty) -> (cname, fromDeBruijn [] ty))

-- -- | As list of semantic representations (the first element is for the first sentence)
-- fromDeBruijnContext :: [Preterm a] -> [(VN.VarName, VN.Preterm a)]
-- fromDeBruijnContext = reverse . initializeIndex . (fromDeBruijnContextLoop []) . reverse

-- -- | the internal function of the fromDeBruijnSRlist function
-- fromDeBruijnContextLoop :: [VN.VarName] -> [Preterm a] -> Indexed ([(VN.VarName, VN.Preterm a)])
-- fromDeBruijnContextLoop _ [] = return []
-- fromDeBruijnContextLoop varnames (x:xs) = do
--   i <- sIndex
--   let varname = VN.VarName 's' i
--   ix <- fromDeBruijnLoop varnames x
--   ixs <- fromDeBruijnContextLoop (varname:varnames) xs
--   return $ (varname,ix):ixs

-- -- | translates a judgment in de Bruijn notation into one with variable names
-- fromDeBruijnJudgment :: Judgment a -> VN.Judgment a
-- fromDeBruijnJudgment judgment =
--   let (vcontext', vterm', vtyp')
--         = initializeIndex $ do
--                             vcontext <- fromDeBruijnContextLoop [] $ reverse $ context judgment
--                             let varnames = fst $ unzip vcontext
--                             vterm <- fromDeBruijnLoop varnames (term judgment)
--                             vtyp <- fromDeBruijnLoop varnames (typ judgment)
--                             return (reverse vcontext, vterm, vtyp)
--   in VN.Judgment { VN.signtr = fromDeBruijnSignature $ signtr judgment
--                  , VN.context = vcontext'
--                  , VN.term = vterm'
--                  , VN.typ = vtyp'
--                  }
