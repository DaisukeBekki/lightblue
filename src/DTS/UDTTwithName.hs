{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{-|
Module      : DTS.UDTTwithName
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Underspecified Dependent Type Theory (with variable names).
-}

module DTS.UDTTwithName (
  VarName(..),
  Selector(..),
  Preterm(..),
  Context,
  toVerticalMathML,
  printVerticalMathML,
  Judgment(..),
  ) where

import qualified Data.Text.Lazy as T    -- text
import qualified Data.Text.Lazy.IO as T -- text
import Interface.Text
import Interface.TeX
import Interface.HTML

-- | A variable name consists of Char (e.g. 'x') and Int (e.g. 1), which is displayed as $x_{1}$ in TeX and $x1$ in Text.
data VarName = VarName Char Int deriving (Eq,Show)

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
  Var VarName |                    -- ^ Variable
  Con T.Text |                     -- ^ Constant symbol
  Type |                           -- ^ The sort \"type\"
  Kind |                           -- ^ The sort \"kind\"
  Pi VarName Preterm Preterm |     -- ^ Dependent function type (or Pi type)
  Not Preterm |                    -- ^ Negation
  Lam VarName Preterm |            -- ^ Lambda abstraction
  App Preterm Preterm |            -- ^ Function Application
  Sigma VarName Preterm Preterm |  -- ^ Dependent product type (or Sigma type)
  Pair Preterm Preterm |           -- ^ Pair
  Proj Selector Preterm |          -- ^ (First and second) Projections
  Lamvec VarName Preterm |         -- ^ Variable-length lambda abstraction
  Appvec VarName Preterm |         -- ^ Variable-length function application
  Unit |                           -- ^ The unit term (of type Top)
  Top |                            -- ^ The top type
  Bot |                            -- ^ The bottom type
  Asp Int Preterm |                -- ^ The asperand term (or Underspesified term)
  Nat |                            -- ^ The natural number type (Nat)
  Zero |                           -- ^ 0 (of type Nat)
  Succ Preterm |                   -- ^ The successor function
  Natrec Preterm Preterm Preterm | -- ^ natrec
  Eq Preterm Preterm Preterm |     -- ^ Intensional equality type
  Refl Preterm Preterm |           -- ^ refl
  Idpeel Preterm Preterm           -- ^ idpeel
  --DRel Int T.Text Preterm Preterm  -- ^ Discourse relations
  deriving (Eq)

{- Printing of Preterms -}

instance Show Preterm where
  show = T.unpack . toText

instance SimpleText Preterm where
  toText preterm = case preterm of
    Var vname -> toText vname
    Con cname -> cname
    Type -> "type"
    Kind -> "kind"
    Pi vname a b -> T.concat ["(", toText vname, ":", toText a, ")→ ", toText b]
    Not a -> T.concat["¬", toText a]
    Lam vname m -> T.concat ["λ", toText vname, ".", toText m]
    App (App (Con cname) y) x ->
      T.concat [cname, "(", toText x, ",", toText y,")"]
    App (App (App (Con cname) z) y) x ->
      T.concat [cname, "(", toText x, ",", toText y,",",toText z,")"]
    App (App (App (App (Con cname) u) z) y) x ->
      T.concat [cname, "(", toText x, ",", toText y,",",toText z,",", toText u, ")"]
    App m n -> T.concat [toText m, "(", toText n, ")"]
    Sigma vname a b -> case b of
                         Top -> T.concat ["(", toText a, ")"]
                         _   -> T.concat ["(", toText vname, ":", toText a, ")× ", toText b]
    Pair m n  -> T.concat ["(", toText m, ",", toText n, ")"]
    Proj s m  -> T.concat ["π", toText s, "(", toText m, ")"]
    Lamvec vname m  -> T.concat ["λ", toText vname, "+.", toText m]
    Appvec vname m -> T.concat ["(", toText m, " ", toText vname, "+)"]
    Unit       -> "()"
    Top        -> "T"
    Bot        -> "⊥"
    Asp j m    -> T.concat["@", T.pack (show j), ":", toText m]
    Nat    -> "N"
    Zero   -> "0"
    Succ n -> T.concat ["s", toText n]
    Natrec n e f -> T.concat ["natrec(", toText n, ",", toText e, ",", toText f, ")"]
    Eq a m n -> T.concat [toText m, "=[", toText a, "]", toText n]
    Refl a m -> T.concat ["refl", toText a, "(", toText m, ")"]
    Idpeel m n -> T.concat ["idpeel(", toText m, ",", toText n, ")"]
    --DRel i t u v -> T.concat ["DRel", T.pack (show i), "[", t, "](", toText u, ",", toText v, ")"]

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
                 (Asp _ _) -> T.concat ["\\APP{", toTeXEmbedded m, "}{\\left(", toTeX n,"\\right)}"]
                 _ -> T.concat["\\APP{", toTeXEmbedded m, "}{", toTeXEmbedded n, "}"]
    Sigma vname a b -> case b of
                         Top -> toTeX a
                         _   -> T.concat["\\dSigma[", toTeX vname, "]{", toTeX a, "}{", toTeX b, "}"]
    Pair m n  -> T.concat["\\left(", toTeX m, ",", toTeX n, "\\right)"]
    Proj s m  -> T.concat["\\pi_", toTeX s, "\\left(", toTeX m, "\\right)"]
    Lamvec vname m   -> T.concat ["\\lambda\\vec{", toTeX vname, "}.", toTeX m]
    Appvec vname m -> T.concat ["\\APP{", toTeXEmbedded m, "}{\\vec{", toTeX vname, "}}"]
    Unit      -> "()"
    Top       -> "\\top"
    Bot       -> "\\bot"
    Asp j m   -> T.concat ["@_{", T.pack (show j), "}:", toTeX m]
    Nat       -> "\\Set{N}"
    Zero      -> "0"
    Succ n    -> T.concat ["\\type{s}", toTeX n]
    Natrec n e f -> T.concat ["\\type{natrec}\\left(", toTeX n, ",", toTeX e, ",", toTeX f, "\\right)"]
    Eq a m n  -> T.concat [toTeX m, "=_{", toTeX a,"}", toTeX n]
    Refl a m  -> T.concat ["\\type{refl}_{", toTeX a, "}\\left(", toTeX m,"\\right)"]
    Idpeel m n -> T.concat ["\\type{idpeel}\\left(", toTeX m, ",", toTeX n, "\\right)"]
    --DRel j t m n -> T.concat ["\\pred{DRel}_{", T.pack (show j), "}[", t, "]\\left(", toTeX m, ",", toTeX n, "\\right)"]

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
    Pair m n  -> T.concat ["<mrow><mo>(</mo>", toMathML m, toMathML n, "<mo>)</mo></mrow>"]
    Proj s m  -> T.concat ["<mrow><msub><mi>&pi;</mi>", toMathML s, "</msub><mo>(</mo>", toMathML m, "<mo>)</mo></mrow>"]
    Lamvec vname m  -> T.concat ["<mrow><mi>&lambda;</mi><mover>", toMathML vname, "<mo>&rarr;</mo></mover><mo>.</mo>", toMathML m, "</mrow>"]
    Appvec vname m -> T.concat ["<mrow><mo>(</mo>", toMathML m, "<mover>", toMathML vname, "<mo>&rarr;</mo></mover><mo>)</mo></mrow>"]
    Unit       -> "<mi>()</mi>"
    Top        -> "<mi>&top;</mi>"
    Bot        -> "<mi>&bot;</mi>"
    Asp j m    -> T.concat["<mrow><mo>@</mo>", toMathML m, "</mrow>"]
    Nat    -> "<mi>N</mi>"
    Zero   -> "<mi>0</mi>"
    Succ n -> T.concat ["<mrow><mi>s</mi>", toMathML n, "</mrow>"]
    Natrec n e f -> T.concat ["<mrow><mi>natrec</mi><mo>(</mo>", toMathML n, toMathML e, toMathML f, "<mo>)</mo></mrow>"]
    Eq a m n -> T.concat ["<mrow>", toMathML m, "<msub><mo>=</mo>", toMathML a, "</msub>", toMathML n, "</mrow>"]
    Refl a m -> T.concat ["<mrow><mi>refl</mi>", toMathML a, "<mo>(</mo>", toMathML m, "<mo>)</mo></mrow>"]
    Idpeel m n -> T.concat ["<mrow><mi>idpeel</mi><mo>(</mo>", toMathML m, toMathML n, "<mo>)</mo></mrow>"]
    --DRel i t u v -> T.concat ["<mrow><msub><mi>DRel</mi><mn>", T.pack (show i), "</mn></msub><mtext>", t, "</mtext><mfenced>", toMathML u, toMathML v, "</mfenced></mrow>"]

{- Judgment -}

-- | A context is a list of pairs of a variable and a preterm.
type Context = [(VarName,Preterm)]

instance SimpleText Context where
  toText = (T.intercalate ", ") . (map (\(nm,tm) -> T.concat [toText nm, ":", toText tm])) . reverse

instance Typeset Context where
  toTeX = (T.intercalate ",") . (map (\(nm,tm) -> T.concat [toTeX nm, ":", toTeX tm])) . reverse

instance MathML Context where
  toMathML = (T.intercalate ",") . (map (\(nm,tm) -> T.concat [toMathML nm, "<mo>:</mo>", toMathML tm])) . reverse

-- | prints a context vertically.
toVerticalMathML :: Context -> T.Text
toVerticalMathML cont = T.concat $ ["<mtable columnalign='left'>"] ++ (map (\(nm,tm) -> T.concat ["<mtr><mtd>", toMathML nm, "<mo>:</mo>", toMathML tm, "</mtd></mtr>"]) $ reverse cont) ++ ["</mtable>"]

-- | prints an SR list vertically.
printVerticalMathML :: [(VarName,Preterm)] -> IO()
printVerticalMathML cont = do
  T.putStr "<mtable columnalign='left'>"
  mapM_ (\(nm,tm) -> mapM_ T.putStr ["<mtr><mtd>",
                                     toMathML nm,
                                     "<mo>:</mo>",
                                     toMathML tm,
                                     "</mtd></mtr>"
                                     ]) cont
  T.putStrLn "</mtable>"

-- | The data type for a judgment
data Judgment = Judgment {
  context :: Context, -- ^ A context \Gamma in \Gamma \vdash M:A
  term :: Preterm,    -- ^ A term M in \Gamma \vdash M:A
  typ :: Preterm      -- ^ A type A in \Gamma \vdash M:A
  } deriving (Eq)

instance SimpleText Judgment where
  toText j = T.concat [toText $ context j, " |- ", toText $ term j, ":", toText $ typ j]

instance Typeset Judgment where
  toTeX j = T.concat [toTeX $ context j, "{\\vdash}", toTeX $ term j, "{:}", toTeX $ typ j]

instance MathML Judgment where
  toMathML j = T.concat ["<mrow>", toMathML $ context j, "<mo>&vdash;</mo>", toMathML $ term j, "<mo>:</mo>", toMathML $ typ j, "</mrow>"]
