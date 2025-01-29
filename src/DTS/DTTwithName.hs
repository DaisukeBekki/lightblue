{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}

{-|
Module      : DTS.UDTTwithName
Copyright   : (c) Daisuke Bekki, 2024
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Dependent Type Theory (with variable names).
-}

module DTS.DTTwithName (
  -- * Terms and Types
  VarName(..)
  , Selector(..)
  , Preterm(..)
  , toText'
  -- * Conversion btw. De Bruijn notation and Variable-name notation
  , fromDeBruijn
  , fromDeBruijnLoop
  , toDeBruijn
  -- * Judgment
  , Signature
  , Context
  , Judgment(..)
  -- , toVerticalMathML    -- 再考
  -- , printVerticalMathML -- 再考
  , fromDeBruijnSignature
  , fromDeBruijnContext
  , fromDeBruijnContextLoop
  , fromDeBruijnJudgment
  , TypeCheckQuery(..)
  , fromDeBruijnTypeCheckQuery
  , TypeInferQuery(..)
  , fromDeBruijnTypeInferQuery
  , ProofSearchQuery(..)
  , fromDeBruijnProofSearchQuery 
  ) where

import qualified GHC.Generics        as G --base
import qualified Data.List as L           --base
import qualified Data.Text.Lazy as T    -- text
import qualified Data.Text.Lazy.IO as T -- text
import Data.Store (Store(..))           --store
import Interface.Text
import Interface.TeX
import Interface.HTML
import qualified DTS.DTTdeBruijn as DTTdB --lightblue
import DTS.Index                          --lightblue
import DTS.GeneralTypeQuery               --lightblue

-- | A variable name consists of Char (e.g. 'x') and Int (e.g. 1), which is displayed as $x_{1}$ in TeX and $x1$ in Text.
data VarName = VarName Char Int deriving (Eq,Show)

instance SimpleText VarName where
  toText (VarName v i) = T.cons v $ T.pack (show i)

instance Typeset VarName where
  toTeX (VarName v i) = T.cons v $ T.concat ["_{", T.pack (show i), "}"]

instance MathML VarName where
  toMathML (VarName v i) = T.concat ["<msub><mi>", T.singleton v, "</mi><mn>", T.pack (show i), "</mn></msub>"]

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
  | Natrec Preterm Preterm Preterm Preterm -- ^ natrec
  -- | Intensional Equality Types
  | Eq Preterm Preterm Preterm            -- ^ Intensional equality types
  | Refl Preterm Preterm                  -- ^ refl
  | Idpeel Preterm Preterm Preterm        -- ^ idpeel
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
    Natrec p n e f -> T.concat ["natrec(", toText' flag p, ",", toText' flag n, ",", toText' flag e, ",", toText' flag f, ")"]
    Eq a m n -> T.concat [toText' flag m, "=[", toText' flag a, "]", toText' flag n]
    Refl a m -> T.concat ["refl", toText' flag a, "(", toText' flag m, ")"]
    Idpeel p e r -> T.concat ["idpeel(", toText' flag p, ",", toText' flag e, ",", toText' flag r, ")"]
    _ -> "Error: The definition of DTS.DTTwithname.toText' is not exhaustive."

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
                 _ -> T.concat["\\APP{", toTeXEmbedded m, "}{", toTeXEmbedded n, "}"]
    Sigma vname a b -> case b of
                         Top -> toTeX a
                         _   -> T.concat["\\dSigma[", toTeX vname, "]{", toTeX a, "}{", toTeX b, "}"]
    Pair m n  -> T.concat["\\left(", toTeX m, ",", toTeX n, "\\right)"]
    Proj s m  -> T.concat["\\pi_", toTeX s, "\\left(", toTeX m, "\\right)"]
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
    Natrec p n e f -> T.concat ["\\type{natrec}^{", toTeX p, "}_{", toTeX n, "}\\left(", toTeX e, ",", toTeX f, "\\right)"]
    Eq a m n  -> T.concat [toTeX m, "=_{", toTeX a,"}", toTeX n]
    Refl a m  -> T.concat ["\\type{refl}_{", toTeX a, "}\\left(", toTeX m,"\\right)"]
    Idpeel p e r -> T.concat ["\\type{idpeel}^{", toTeX p, "}_{", toTeX e, "}\\left(", toTeX r, "\\right)"]
    _ -> "Error: The definition of DTS.DTTwithname.toTeX is not exhaustive."

toTeXEmbedded :: Preterm -> T.Text
toTeXEmbedded preterm = case preterm of
  Lam vname m -> T.concat["\\left(\\LAM[", toTeX vname, "]", toTeX m, "\\right)"]
  m          -> toTeX m

instance MathML Preterm where
  toMathML preterm = case preterm of
    Var vname -> toMathML vname
    Con cname -> T.concat ["<mtext>", cname, "</mtext>"]
    Type -> "<mi>type</mi>"
    Kind -> "<mi>kind</mi>"
    Pi vname a b -> T.concat ["<mrow><mo>(</mo>", toMathML vname, "<mo>:</mo>", toMathML a, "<mo>)</mo><mo>&rarr;</mo>", toMathML b, "</mrow>"]
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
    Natrec p n e f -> T.concat ["<mrow><msubsup><mi>natrec</mi><mn>", toMathML p, "</mn><mn>", toMathML n, "</mn><mn></mn></msubsup><mo>(</mo>", toMathML e, toMathML f, "<mo>)</mo></mrow>"]
    Eq a m n -> T.concat ["<mrow>", toMathML m, "<msub><mo>=</mo>", toMathML a, "</msub>", toMathML n, "</mrow>"]
    Refl a m -> T.concat ["<mrow><mi>refl</mi>", toMathML a, "<mo>(</mo>", toMathML m, "<mo>)</mo></mrow>"]
    Idpeel p e r -> T.concat ["<mrow><msubsup><mi>idpeel</mi><mn>", toMathML p, "</mn><mn>", toMathML e, "</mn></msubsup><mo>(</mo>", toMathML r, "<mo>)</mo></mrow>"]
    _ -> "Error: The definition of DTS.DTTwithname.toMathML is not exhaustive."

-- | Conversion btw. de Bruijn notation and a variable name notation.
fromDeBruijn :: [VarName] -> DTTdB.Preterm -> Preterm
fromDeBruijn varNames = initializeIndex . (fromDeBruijnLoop varNames)

fromDeBruijnLoop :: [VarName] -- ^ A context (= a list of variable names)
                    -> DTTdB.Preterm  -- ^ A preterm in de Bruijn notation
                    -> Indexed Preterm -- ^ A preterm with variable names
fromDeBruijnLoop vnames preterm = case preterm of
  DTTdB.Var j -> if j < length vnames
                      then return $ Var (vnames!!j)
                      else return $ Con $ T.concat ["Error in fromDeBruijn: a variable ",T.pack (show j), " not found in ", T.pack (show vnames)]
  DTTdB.Con cname -> return $ Con cname
  DTTdB.Type -> return Type
  DTTdB.Kind -> return Kind
  DTTdB.Pi a b -> do
    vname <- variableNameFor a
    a' <- fromDeBruijnLoop vnames a
    b' <- fromDeBruijnLoop (vname:vnames) b
    return $ Pi vname a' b'
  DTTdB.Lam m   -> do
    i <- xIndex
    let vname = case m of
                  DTTdB.Sigma _ _ -> VarName 'x' i
                  DTTdB.Pi _ _    -> VarName 'x' i
                  _         -> VarName 'x' i
    m' <- fromDeBruijnLoop (vname:vnames) m
    return $ Lam vname m'
  DTTdB.App m n -> do
    m' <- fromDeBruijnLoop vnames m
    n' <- fromDeBruijnLoop vnames n
    return $ App m' n'
  DTTdB.Not a   -> do
    a' <- fromDeBruijnLoop vnames a
    return $ Not a'
  DTTdB.Sigma a b -> do
    vname <- variableNameFor a
    a' <- fromDeBruijnLoop vnames a
    b' <- fromDeBruijnLoop (vname:vnames) b
    return $ Sigma vname a' b'
  DTTdB.Pair m n  -> do
    m' <- fromDeBruijnLoop vnames m
    n' <- fromDeBruijnLoop vnames n
    return $ Pair m' n'
  DTTdB.Proj s m  -> do
    m' <- fromDeBruijnLoop vnames m
    return $ Proj (case s of DTTdB.Fst -> Fst; DTTdB.Snd -> Snd) m'
  DTTdB.Disj a b -> do
    a' <- fromDeBruijnLoop vnames a
    b' <- fromDeBruijnLoop vnames b
    return $ Disj a' b'
  DTTdB.Iota s m -> do
    m' <- fromDeBruijnLoop vnames m
    return $ Iota (case s of DTTdB.Fst -> Fst; DTTdB.Snd -> Snd) m'
  DTTdB.Unpack p l m n -> do
    p' <- fromDeBruijnLoop vnames p
    l' <- fromDeBruijnLoop vnames l
    m' <- fromDeBruijnLoop vnames m
    n' <- fromDeBruijnLoop vnames n
    return $ Unpack p' l' m' n'
  DTTdB.Bot     -> return Bot
  DTTdB.Unit    -> return Unit
  DTTdB.Top     -> return Top
  DTTdB.Entity  -> return Entity
  DTTdB.Nat    -> return Nat
  DTTdB.Zero   -> return Zero
  DTTdB.Succ n -> do
    n' <- fromDeBruijnLoop vnames n
    return $ Succ n'
  DTTdB.Natrec p n e f -> do
    p' <- fromDeBruijnLoop vnames p
    n' <- fromDeBruijnLoop vnames n
    e' <- fromDeBruijnLoop vnames e
    f' <- fromDeBruijnLoop vnames f
    return $ Natrec p' n' e' f'
  DTTdB.Eq a m n -> do
    a' <- fromDeBruijnLoop vnames a
    m' <- fromDeBruijnLoop vnames m
    n' <- fromDeBruijnLoop vnames n
    return $ Eq a' m' n'
  DTTdB.Refl a m -> do
    a' <- fromDeBruijnLoop vnames a
    m' <- fromDeBruijnLoop vnames m
    return $ Refl a' m'
  DTTdB.Idpeel p e r -> do
    p' <- fromDeBruijnLoop vnames p
    e' <- fromDeBruijnLoop vnames e
    r' <- fromDeBruijnLoop vnames r
    return $ Idpeel p' e' r'

variableNameFor :: DTTdB.Preterm -> Indexed VarName
variableNameFor preterm =
  case preterm of
    DTTdB.Entity -> do i <- xIndex; return $ VarName 'x' i
    -- DTTdB.Con cname | cname == "entity" -> do i <- xIndex; return $ VarName 'x' i
    --                 | cname == "evt"    -> do i <- eIndex; return $ VarName 'e' i
    --           -- cname == "state"  -> VN.VN.VarName 's' i
    DTTdB.Eq _ _ _ -> do i <- xIndex; return $ VarName 's' i
    DTTdB.Nat      -> do i <- xIndex; return $ VarName 'k' i
    _        -> do i <- uIndex; return $ VarName 'u' i

-- | translates a preterm with variable name into a preterm in de Bruijn notation.
toDeBruijn :: [VarName]  -- ^ A context (= a list of variable names)
              -> Preterm -- ^ A preterm with variable names
              -> DTTdB.Preterm   -- ^ A preterm in de Bruijn notation
toDeBruijn vnames preterm = case preterm of
  Var vname -> case L.elemIndex vname vnames of
                    Just i -> DTTdB.Var i
                    Nothing -> DTTdB.Con $ T.concat ["Error in toDeBruijn: ", toText vname, " not found in ", T.pack (show vnames)]
  Con cname -> DTTdB.Con cname
  Type -> DTTdB.Type
  Kind -> DTTdB.Kind
  Pi vname a b -> DTTdB.Pi (toDeBruijn vnames a) (toDeBruijn (vname:vnames) b)
  Lam vname m -> DTTdB.Lam (toDeBruijn (vname:vnames) m)
  App m n -> DTTdB.App (toDeBruijn vnames m) (toDeBruijn vnames n)
  Not a -> DTTdB.Not (toDeBruijn vnames a)
  Sigma vname a b -> DTTdB.Sigma (toDeBruijn vnames a) (toDeBruijn (vname:vnames) b)
  Pair m n -> DTTdB.Pair (toDeBruijn vnames m) (toDeBruijn vnames n)
  Proj s m -> DTTdB.Proj (case s of Fst -> DTTdB.Fst; Snd -> DTTdB.Snd) (toDeBruijn vnames m)
  Disj a b -> DTTdB.Disj (toDeBruijn vnames a) (toDeBruijn vnames b)
  Iota s m -> DTTdB.Iota (case s of Fst -> DTTdB.Fst; Snd -> DTTdB.Snd) (toDeBruijn vnames m)
  Unpack p l m n -> DTTdB.Unpack (toDeBruijn vnames p) (toDeBruijn vnames l) (toDeBruijn vnames m) (toDeBruijn vnames n) 
  Bot -> DTTdB.Bot
  Unit -> DTTdB.Unit
  Top -> DTTdB.Top
  Entity -> DTTdB.Entity
  Nat -> DTTdB.Nat
  Zero -> DTTdB.Zero
  Succ n -> DTTdB.Succ (toDeBruijn vnames n)
  Natrec p n e f -> DTTdB.Natrec (toDeBruijn vnames p) (toDeBruijn vnames n) (toDeBruijn vnames e) (toDeBruijn vnames f)
  Eq a m n -> DTTdB.Eq (toDeBruijn vnames a) (toDeBruijn vnames m) (toDeBruijn vnames n)
  Refl a m -> DTTdB.Refl (toDeBruijn vnames a) (toDeBruijn vnames m)
  Idpeel p e r -> DTTdB.Idpeel (toDeBruijn vnames p) (toDeBruijn vnames e) (toDeBruijn vnames r)

{- Judgment -}
type Signature = [(T.Text, Preterm)]

fromDeBruijnSignature :: DTTdB.Signature -> Signature
fromDeBruijnSignature = map (\(constantSymbol, typ') -> (constantSymbol, fromDeBruijn [] typ'))

instance SimpleText Signature where
  toText = (T.intercalate ", ") . (map (\(nm,tm) -> T.concat [nm, ":", toText tm])) . reverse
instance Typeset Signature where
  toTeX = (T.intercalate ",") . (map (\(nm,tm) -> T.concat [nm, ":", toTeX tm])) . reverse
instance MathML Signature where
  toMathML = (T.intercalate "<mo>,<mo>") . (map (\(nm,tm) -> T.concat ["<mrow><mo>", nm, "</mo><mo>:</mo>", toMathML tm, "</mrow>"])) . reverse

-- | A context is a list of pairs of a variable and a preterm.
type Context = [(VarName, Preterm)]

-- | As list of semantic representations (the first element is for the first sentence)
fromDeBruijnContext :: DTTdB.Context -> Context
fromDeBruijnContext = reverse . initializeIndex . (fromDeBruijnContextLoop []) . reverse

-- | the internal function of the fromDeBruijnSRlist function
fromDeBruijnContextLoop :: [VarName] -> DTTdB.Context -> Indexed Context
fromDeBruijnContextLoop _ [] = return []
fromDeBruijnContextLoop varnames (x:xs) = do
  i <- sIndex
  let varname = VarName 's' i
  ix <- fromDeBruijnLoop varnames x
  ixs <- fromDeBruijnContextLoop (varname:varnames) xs
  return $ (varname,ix):ixs

instance SimpleText Context where
  toText = (T.intercalate ", ") . (map (\(nm,tm) -> T.concat [toText nm, ":", toText tm])) . reverse
instance Typeset Context where
  toTeX = (T.intercalate ",") . (map (\(nm,tm) -> T.concat [toTeX nm, ":", toTeX tm])) . reverse
instance MathML Context where
  toMathML = (T.intercalate "<mo>,</mo>") . (map (\(nm,tm) -> T.concat ["<mrow>", toMathML nm, "<mo>:</mo>", toMathML tm, "</mrow>"])) . reverse

-- | The data type for a DTT judgment
data Judgment = Judgment {
  signtr :: Signature  -- ^ A signature 
  , contxt :: Context  -- ^ A context \Gamma in \Gamma \vdash M:A
  , trm :: Preterm     -- ^ A term M in \Gamma \vdash M:A
  , typ :: Preterm     -- ^ A type A in \Gamma \vdash M:A
  } deriving (Eq)

-- | translates a judgment in de Bruijn notation into one with variable names
fromDeBruijnJudgment :: DTTdB.Judgment -> Judgment
fromDeBruijnJudgment judgment = 
  initializeIndex $ do
    let vsig = fromDeBruijnSignature $ DTTdB.signtr judgment
    vcontext <- (fmap reverse) $ fromDeBruijnContextLoop [] $ reverse $ DTTdB.contxt judgment
    let varnames = fst $ unzip vcontext
    vterm <- fromDeBruijnLoop varnames $ DTTdB.trm judgment
    vtype <- fromDeBruijnLoop varnames $ DTTdB.typ judgment 
    return $ Judgment {
      signtr = vsig
      , contxt = vcontext
      , trm = vterm
      , typ = vtype
      }

embedJudgment :: Judgment -> GeneralTypeQuery Signature Context Preterm Preterm
embedJudgment (Judgment sig cxt trm typ) = GeneralTypeQuery sig cxt (Term trm) (Term typ)

instance Show Judgment where
  show = T.unpack . toText
instance SimpleText Judgment where
  toText = toText . embedJudgment
instance Typeset Judgment where
  toTeX = toTeX . embedJudgment
instance MathML Judgment where
  toMathML = toMathML . embedJudgment

-- -- | prints a context vertically.
-- toVerticalMathML :: Context -> T.Text
-- toVerticalMathML cont = T.concat $ ["<mtable columnalign='left'>"] ++ (map (\(nm,tm) -> T.concat ["<mtr><mtd>", toMathML nm, "<mo>:</mo>", toMathML tm, "</mtd></mtr>"]) $ reverse cont) ++ ["</mtable>"]

-- -- | prints an SR list vertically.
-- printVerticalMathML :: Context -> IO ()
-- printVerticalMathML cont = do
--   T.putStr "<mtable columnalign='left'>"
--   mapM_ (\(nm,tm) -> mapM_ T.putStr ["<mtr><mtd>",
--                                      toMathML nm,
--                                      "<mo>:</mo>",
--                                      toMathML tm,
--                                      "</mtd></mtr>"
--                                      ]) cont
--   T.putStrLn "</mtable>"

type TypeCheckQuery = Judgment

fromDeBruijnTypeCheckQuery :: DTTdB.TypeCheckQuery -> TypeCheckQuery
fromDeBruijnTypeCheckQuery = fromDeBruijnJudgment

data TypeInferQuery = TypeInferQuery Signature Context Preterm deriving (Eq)

fromDeBruijnTypeInferQuery :: DTTdB.TypeInferQuery -> TypeInferQuery 
fromDeBruijnTypeInferQuery (DTTdB.TypeInferQuery sgntr contxt trm) = 
  initializeIndex $ do
    let vsig = fromDeBruijnSignature sgntr
    vcontext <- (fmap reverse) $ fromDeBruijnContextLoop [] $ reverse contxt 
    let varnames = fst $ unzip vcontext
    vterm <- fromDeBruijnLoop varnames trm 
    return $ TypeInferQuery vsig vcontext vterm 

embedTypeInferQuery :: TypeInferQuery -> GeneralTypeQuery Signature Context Preterm Preterm
embedTypeInferQuery (TypeInferQuery sig cxt trm) = GeneralTypeQuery sig cxt (Term trm) Question

instance Show TypeInferQuery where
  show = T.unpack . toText
instance SimpleText TypeInferQuery where
  toText = toText . embedTypeInferQuery
instance Typeset TypeInferQuery where
  toTeX = toTeX . embedTypeInferQuery
instance MathML TypeInferQuery where
  toMathML = toMathML . embedTypeInferQuery

data ProofSearchQuery = ProofSearchQuery Signature Context Preterm deriving (Eq)

fromDeBruijnProofSearchQuery :: DTTdB.ProofSearchQuery -> ProofSearchQuery
fromDeBruijnProofSearchQuery (DTTdB.ProofSearchQuery sgn contxt typ) = 
  initializeIndex $ do
    let vsig = fromDeBruijnSignature sgn
    vcontext <- (fmap reverse) $ fromDeBruijnContextLoop [] $ reverse contxt
    let varnames = fst $ unzip vcontext
    vtype <- fromDeBruijnLoop varnames typ
    return $ ProofSearchQuery vsig vcontext vtype

embedProofSearchQuery :: ProofSearchQuery -> GeneralTypeQuery Signature Context Preterm Preterm
embedProofSearchQuery (ProofSearchQuery sig cxt typ) = GeneralTypeQuery sig cxt Question (Term typ)

instance Show ProofSearchQuery where
  show = T.unpack . toText
instance SimpleText ProofSearchQuery where
  toText = toText . embedProofSearchQuery
instance Typeset ProofSearchQuery where
  toTeX = toTeX . embedProofSearchQuery
instance MathML ProofSearchQuery where
  toMathML = toMathML . embedProofSearchQuery
