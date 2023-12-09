{-# LANGUAGE GADTs, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}

{-|
Module      : DTS.UDTTvarName
Copyright   : (c) Daisuke Bekki, 2023
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Underspecified Dependent Type Theory (with variable names).
-}

module DTS.UDTTvarName (
  -- * Terms and Types
  VarName(..)
  , Selector(..)
  , Preterm(..)
  , toText'
  -- * Judgment
  , Signature
  , Context
  , Judgment(..)
  , toVerticalMathML
  , printVerticalMathML
  ) where

import qualified Data.Text.Lazy as T    -- text
import qualified Data.Text.Lazy.IO as T -- text
import Interface.Text                   --lightblue
import Interface.TeX                    --lightblue
import Interface.HTML                   --lightblue
import DTS.Labels (UDTT,DTT)            --lightblue

-- | A variable name consists of Char (e.g. 'x') and Int (e.g. 1), which is displayed as $x_{1}$ in TeX and $x1$ in Text.
data VarName = VarName Char Int deriving (Eq,Show)

instance SimpleText VarName where
  toText (VarName v i) = T.cons v $ T.pack (show i)
instance Typeset VarName where
  toTeX (VarName v i) = T.cons v $ T.concat ["_{", T.pack (show i), "}"]
instance MathML VarName where
  toMathML (VarName v i) = T.concat ["<mstyle mathcolor='purple'><msub><mi>", T.singleton v, "</mi><mn>", T.pack (show i), "</mn></msub></mstyle>"]

-- | 'Proj' 'Fst' m is the first projection of m, while 'Proj' 'Snd' m is the second projection of m.
data Selector = Fst | Snd
  deriving (Eq, Show)

-- | print a selector as "1" or "2".
instance SimpleText Selector where
  toText Fst = "1"  -- `Proj` `Fst` m is the first projection of m
  toText Snd = "2" -- `Proj` `Snd` m is the second projection of m
instance Typeset Selector where
  toTeX = toText
instance MathML Selector where
  toMathML Fst = "<mn>1</mn>"  -- `Proj` `Fst` m is the first projection of m
  toMathML Snd = "<mn>2</mn>" -- `Proj` `Snd` m is the second projection of m

-- | Preterms of Underspecified Dependent Type Theory (UDTT) with variable names
data Preterm a where
  -- | Basic Preterms
  Var :: VarName -> Preterm a       -- ^ Variables
  Con :: T.Text -> Preterm a        -- ^ Constant symbols
  Type :: Preterm a                 -- ^ The sort \"type\"
  Kind :: Preterm a                 -- ^ The sort \"kind\"
  -- | Pi Types
  Pi :: VarName -> Preterm a -> Preterm a -> Preterm a -- ^ Pi types
  Lam :: VarName -> Preterm a -> Preterm a             -- ^ Lambda abstractions
  App :: Preterm a -> Preterm a -> Preterm a           -- ^ Function Applications
  Not :: Preterm a -> Preterm a                        -- ^ Negations
  -- | Sigma Types
  Sigma :: VarName -> Preterm a -> Preterm a -> Preterm a -- ^ Sigma types
  Pair :: Preterm a -> Preterm a -> Preterm a             -- ^ Pair
  Proj :: Selector -> Preterm a -> Preterm a -- ^ (First and second) Projections
  -- | UDTT expansions
  Asp :: Int -> Preterm UDTT -> Preterm UDTT -- ^ Underspesified types
  Lamvec :: VarName -> Preterm UDTT -> Preterm UDTT   -- ^ Variable-length lambda abstraction
  Appvec :: VarName -> Preterm UDTT -> Preterm UDTT   -- ^ Variable-length function application
  -- | Disjoint Union Types
  Disj :: Preterm a -> Preterm a -> Preterm a   -- ^ Disjoint Union types
  Iota :: Selector -> Preterm a -> Preterm a    -- ^ (FIrst and second) Injections
  Unpack :: Preterm a -> Preterm a -> Preterm a -> Preterm a -> Preterm a -- ^ Unpack P L M N
  -- | Enumeration Types
  Bot :: Preterm a                 -- ^ The bottom type
  Unit :: Preterm a                -- ^ The unit term (of type Top)
  Top :: Preterm a                 -- ^ The top type
  Entity :: Preterm a              -- ^ The entity type
  -- | Natural Number Types
  Nat :: Preterm a                 -- ^ The natural number type (Nat)
  Zero :: Preterm a                -- ^ 0 (of type Nat)
  Succ :: Preterm a -> Preterm a   -- ^ The successor function
  Natrec :: Preterm a -> Preterm a -> Preterm a -> Preterm a -- ^ natrec
  -- | Intensional Equality Types
  Eq :: Preterm a -> Preterm a -> Preterm a -> Preterm a -- ^ Intensional equality type
  Refl :: Preterm a -> Preterm a -> Preterm a            -- ^ refl
  Idpeel :: Preterm a -> Preterm a -> Preterm a          -- ^ idpeel
  -- | ToDo: add Disjoint Union Types
  -- | ToDo: add First Universe

deriving instance Eq a => Eq (Preterm a)

{- Printing of Preterms -}

instance Show (Preterm a) where
  show = T.unpack . toText

instance SimpleText (Preterm a) where
  toText preterm = toText' True preterm

-- | flag=True : f(y)(x) is printed as f(x,y)
-- | flag=False : f(y)(x) is printed as it is
toText' :: Bool -> (Preterm a) -> T.Text
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
                         _   -> T.concat ["(", toText vname, ":", toText' flag a, ")× ", toText' flag b]
    Pair m n  -> T.concat ["(", toText' flag m, ",", toText' flag n, ")"]
    Proj s m  -> T.concat ["π", toText s, "(", toText' flag m, ")"]
    Lamvec vname m  -> T.concat ["λ", toText vname, "+.", toText' flag m]
    Appvec vname m -> T.concat ["(", toText' flag m, " ", toText vname, "+)"]
    Bot        -> "⊥"
    Unit       -> "()"
    Top        -> "T"
    Entity     -> "entity"
    Asp j m    -> T.concat["@", T.pack (show j), ":", toText' flag m]
    Nat    -> "N"
    Zero   -> "0"
    Succ n -> T.concat ["s", toText' flag n]
    Natrec n e f -> T.concat ["natrec(", toText' flag n, ",", toText' flag e, ",", toText' flag f, ")"]
    Eq a m n -> T.concat [toText' flag m, "=[", toText' flag a, "]", toText' flag n]
    Refl a m -> T.concat ["refl", toText' flag a, "(", toText' flag m, ")"]
    Idpeel m n -> T.concat ["idpeel(", toText' flag m, ",", toText' flag n, ")"]

-- | Each `Preterm` is translated by the `toTeX` method into a representation \"with variable names\" in a TeX source code.
instance Typeset (Preterm a) where
  toTeX preterm = case preterm of
    Var vname -> toTeX vname
    Con c -> T.concat["\\pred{", T.replace "~" "\\~{}" c, "}"]
    Type  -> "\\type{type}"
    Kind  -> "\\type{kind}"
    Pi vname a b -> case b of
                      Bot -> T.concat["\\neg ", toTeXEmbedded a]
                      b' -> T.concat["\\dPi[", toTeX vname , "]{", toTeX a, "}{", toTeX b, "}"]
    Lam vname m  -> T.concat["\\LAM[", toTeX vname, "]", toTeX m]
    (App (App (Con c) y) x) -> T.concat ["\\APP{", toTeXEmbedded (Con c), "}{\\left(", toTeX x, ",", toTeX y, "\\right)}" ]
    (App (App (App (Con c) z) y) x) -> T.concat ["\\APP{", toTeXEmbedded (Con c), "}{\\left(", toTeX x, ",", toTeX y, ",", toTeX z, "\\right)}" ]
    (App (App (App (App (Con c) u) z) y) x) -> T.concat ["\\APP{", toTeXEmbedded (Con c), "}{\\left(", toTeX x, ",", toTeX y, ",", toTeX z, ",", toTeX u, "\\right)}" ]
    App m n -> case n of
                 (Var _) -> T.concat ["\\APP{", toTeXEmbedded m, "}{(", toTeX n, ")}"]
                 (Con _) -> T.concat ["\\APP{", toTeXEmbedded m, "}{(", toTeX n, ")}"]
                 (Proj _ _) -> T.concat ["\\APP{", toTeXEmbedded m, "}{\\left(", toTeX n, "\\right)}"]
                 (Asp _ _) -> T.concat ["\\APP{", toTeXEmbedded m, "}{\\left(", toTeX n,"\\right)}"]
                 _ -> T.concat["\\APP{", toTeXEmbedded m, "}{", toTeXEmbedded n, "}"]
    Sigma vname a b -> case b of
                         Top -> toTeX a
                         _   -> T.concat["\\dSigma[", toTeX vname, "]{", toTeX a, "}{", toTeX b, "}"]
    Pair m n  -> T.concat["\\left(", toTeX m, ",", toTeX n, "\\right)"]
    Proj s m  -> T.concat["\\pi_", toTeX s, "\\left(", toTeX m, "\\right)"]
    Lamvec vname m   -> T.concat ["\\lambda\\vec{", toTeX vname, "}.", toTeX m]
    Appvec vname m -> T.concat ["\\APP{", toTeXEmbedded m, "}{\\vec{", toTeX vname, "}}"]
    Bot       -> "\\bot"
    Unit      -> "()"
    Top       -> "\\top"
    Entity    -> "\\Entity"
    Asp j m   -> T.concat ["@_{", T.pack (show j), "}:", toTeX m]
    Nat       -> "\\Set{N}"
    Zero      -> "0"
    Succ n    -> T.concat ["\\type{s}", toTeX n]
    Natrec n e f -> T.concat ["\\type{natrec}\\left(", toTeX n, ",", toTeX e, ",", toTeX f, "\\right)"]
    Eq a m n  -> T.concat [toTeX m, "=_{", toTeX a,"}", toTeX n]
    Refl a m  -> T.concat ["\\type{refl}_{", toTeX a, "}\\left(", toTeX m,"\\right)"]
    Idpeel m n -> T.concat ["\\type{idpeel}\\left(", toTeX m, ",", toTeX n, "\\right)"]

toTeXEmbedded :: (Preterm a) -> T.Text
toTeXEmbedded preterm = case preterm of
  Lam vname m -> T.concat["\\left(\\LAM[", toTeX vname, "]", toTeX m, "\\right)"]
  Lamvec vname m -> T.concat ["\\left(\\lambda\\vec{", toTeX vname, "}.", toTeX m, "\\right)"]
  m          -> toTeX m

instance MathML (Preterm a) where
  toMathML preterm = case preterm of
    Var vname -> toMathML vname
    Con cname -> T.concat ["<mstyle font-family='bold'><mtext>", cname, "</mtext></mstyle>"]
    Type -> "<mi>type</mi>"
    Kind -> "<mi>kind</mi>"
    Pi vname a b -> case b of
      Bot -> T.concat["<mrow><mi>&not;</mi>", toMathML a, "</mrow>"]
      b' -> T.concat ["<mrow><mo>(</mo>", toMathML vname, "<mo>:</mo>", toMathML a, "<mo>)</mo><mo>&rarr;</mo>", toMathML b, "</mrow>"]
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
    Lamvec vname m  -> T.concat ["<mrow><mi>&lambda;</mi><mover>", toMathML vname, "<mo>&rarr;</mo></mover><mo>.</mo>", toMathML m, "</mrow>"]
    Appvec vname m -> T.concat ["<mrow>", toMathML m, "<mover>", toMathML vname, "<mo>&rarr;</mo></mover></mrow>"]
    Bot        -> "<mi>&bot;</mi>"
    Unit       -> "<mi>()</mi>"
    Top        -> "<mi>&top;</mi>"
    Entity     -> "<mi>entity</mi>"
    Asp j m    -> T.concat["<mrow><msub><mo>@</mo><mn>", T.pack (show j), "</mn></msub><mo>::</mo>", toMathML m, "</mrow>"]
    Nat    -> "<mi>N</mi>"
    Zero   -> "<mi>0</mi>"
    Succ n -> T.concat ["<mrow><mi>s</mi>", toMathML n, "</mrow>"]
    Natrec n e f -> T.concat ["<mrow><mi>natrec</mi><mo>(</mo>", toMathML n, "<mo>,</mo>", toMathML e, "<mo>,</mo>", toMathML f, "<mo>)</mo></mrow>"]
    Eq a m n -> T.concat ["<mrow>", toMathML m, "<msub><mo>=</mo>", toMathML a, "</msub>", toMathML n, "</mrow>"]
    Refl a m -> T.concat ["<mrow><mi>refl</mi>", toMathML a, "<mo>(</mo>", toMathML m, "<mo>)</mo></mrow>"]
    Idpeel m n -> T.concat ["<mrow><mi>idpeel</mi><mo>(</mo>", toMathML m, "<mo>,</mo>", toMathML n, "<mo>)</mo></mrow>"]

{- Judgment -}

type Signature = [(T.Text, Preterm DTT)]

instance SimpleText Signature where
  toText sigs = T.concat ["[", (T.intercalate ", " $ map (\(cname,ty) -> T.concat $ [toText $ Con cname, ":", toText ty]) sigs), "]"]
instance Typeset Signature where
  toTeX _ = T.empty
instance MathML Signature where
  toMathML _ = T.empty

-- | A context is a list of pairs of a variable and a preterm.
type Context = [(VarName, Preterm DTT)]

instance SimpleText Context where
  toText = (T.intercalate ", ") . (map (\(nm,tm) -> T.concat [toText nm, ":", toText tm])) . reverse
instance Typeset Context where
  toTeX = (T.intercalate ",") . (map (\(nm,tm) -> T.concat [toTeX nm, ":", toTeX tm])) . reverse
instance MathML Context where
  toMathML = (T.intercalate "<mo>,</mo>") . (map (\(nm,tm) -> T.concat ["<mrow>", toMathML nm, "<mo>:</mo><mstyle mathcolor='blue' mathbackground='white'>", toMathML tm, "</mstyle></mrow>"])) . reverse 

-- | The data type for a judgment
data Judgment a = Judgment {
  sig :: Signature         -- ^ A signature
  , context :: Context  -- ^ A context \Gamma in \Gamma \vdash M:A
  , term :: Preterm a      -- ^ A term M in \Gamma \vdash M:A
  , typ :: Preterm DTT     -- ^ A type A in \Gamma \vdash M:A
  } deriving (Eq)

instance SimpleText (Judgment a) where
  toText j = T.concat [toText $ context j, " |- ", toText $ term j, ":", toText $ typ j]
instance Typeset (Judgment a) where
  toTeX j = T.concat [toTeX $ context j, "{\\vdash}", toTeX $ term j, "{:}", toTeX $ typ j]
instance MathML (Judgment a) where
  toMathML j = T.concat ["<mrow>", toMathML $ context j, "<mo>&vdash;</mo>", toMathML $ term j, "<mo>:</mo><mstyle mathcolor='blue' mathbackground='white'>", toMathML $ typ j, "</mstyle></mrow>"]

-- | prints a context vertically.
toVerticalMathML :: Context -> T.Text
toVerticalMathML cont = T.concat $ ["<mtable columnalign='left'>"] ++ (map (\(nm,tm) -> T.concat ["<mtr><mtd>", toMathML nm, "<mo>:</mo>", toMathML tm, "</mtd></mtr>"]) $ reverse cont) ++ ["</mtable>"]

-- | prints an SR list vertically.
printVerticalMathML :: [(VarName, Preterm a)] -> IO()
printVerticalMathML cont = do
  T.putStr "<mtable columnalign='left'>"
  mapM_ (\(nm,tm) -> mapM_ T.putStr ["<mtr><mtd>",
                                     toMathML nm,
                                     "<mo>:</mo>",
                                     toMathML tm,
                                     "</mtd></mtr>"
                                     ]) cont
  T.putStrLn "</mtable>"

-- | prints a context in vertical manner.
--toVerticalMathML :: Context -> T.Text
--toVerticalMathML = toVerticalMathML . fromDeBruijnContext

-- | prints an SR list in vertical manner.
--printVerticalMathML :: [Preterm] -> IO()
--printVerticalMathML = printVerticalMathML . fromDeBruijnSRlist

{-
-- | prints a proof search query in MathML
printProofSearchQuery :: Context DTT -> Preterm a -> T.Text
printProofSearchQuery cont ty =
  let (vcontext', vtyp')
        = initializeIndex $ do
                            (varnames,vcontext) <- fromDeBruijnContextLoop cont
                            vtyp <- fromDeBruijnLoop varnames ty
                            return (vcontext, vtyp)
  in T.concat ["<mrow>", toMathML vcontext', "<mo>&vdash;</mo><mo>?</mo><mo>:</mo>", toMathML vtyp', "</mrow>"]
-}
