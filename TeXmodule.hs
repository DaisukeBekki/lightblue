{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : TeX interface
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}
module TeXmodule (
  Typeset(..),
  printNNodesInTeX,
  printNodesInTeX
  ) where

import qualified Data.Text.Lazy as T
import qualified Data.Maybe as Maybe
import qualified System.IO as S        --base
import DependentTypes
import CombinatoryCategorialGrammar as CCG

-- | `Typeset` is a class of types whose terms can be translated into a TeX source (in Data.Text.Lazy). 
class Typeset a where
  -- | `toTeX` method translates a Typeset term into a TeX source (in Data.Text.Lazy).
  toTeX :: a -> T.Text

-- | `Fst` is translated into \"1\", and `Snd` is into \"2\".
instance Typeset Selector where
  toTeX Fst = "1"
  toTeX Snd = "2"

-- | Each `Preterm` is translated by the `toTeX` method into a representation \"with variable names\" in a TeX source code.
instance Typeset Preterm where
  toTeX = toTeXWithVN [] 0

toTeXWithVN :: [T.Text] -> Int -> Preterm -> T.Text
toTeXWithVN vlist i preterm = case preterm of
    Var j -> if j < (length vlist)
             then vlist!!j
             else T.concat ["error:", T.pack (show j), " in ", T.pack (show vlist)]
    Con c -> T.concat["\\pred{", T.replace "~" "\\~{}" c, "}"]
    Type  -> "\\type{type}"
    Kind  -> "\\type{kind}"
    Pi a b -> let varname = case a of
                              Con "entity" -> T.concat ["x_{", T.pack (show i), "}"] 
                              Con "event" -> T.concat ["e_{", T.pack (show i), "}"] 
                              Con "state" -> T.concat ["s_{", T.pack (show i), "}"] 
                              App _ _ -> T.concat ["u_{", T.pack (show i), "}"] 
                              Sigma _ _ -> T.concat ["u_{", T.pack (show i), "}"] 
                              Pi _ _ -> T.concat ["u_{", T.pack (show i), "}"] 
                              Not _  -> T.concat ["u_{", T.pack (show i), "}"] 
                              Appvec _ _ -> T.concat ["u_{", T.pack (show i), "}"] 
                              Type -> T.concat ["p_{", T.pack (show i), "}"] 
                              Kind -> T.concat ["p_{", T.pack (show i), "}"] 
                              Eq _ _ _ -> T.concat ["s_{", T.pack (show i), "}"] 
                              Nat -> T.concat ["k_{", T.pack (show i), "}"] 
                              _ -> T.concat ["x_{", T.pack (show i), "}"] in
              T.concat["\\dPi[", varname , "]{", (toTeXWithVN vlist (i+1) a), "}{", (toTeXWithVN (varname:vlist) (i+1) b), "}"]
    Not a  -> T.concat["\\neg ", toTeXWithVNEmbedded vlist i a]
    Lam m  -> let varname = case m of
                              Sigma _ _ -> T.concat ["x_{", T.pack (show i), "}"] 
                              Pi _ _ -> T.concat ["x_{", T.pack (show i), "}"] 
                              _ -> T.concat ["x_{", T.pack (show i), "}"] in
              T.concat["\\LAM[", varname, "]", (toTeXWithVN (varname:vlist) (i+1) m)]
    (App (App (Con c) y) x) -> T.concat ["\\APP{", toTeXWithVNEmbedded vlist i (Con c), "}{\\left(", toTeXWithVN vlist i x, ",", toTeXWithVN vlist i y, "\\right)}" ]
    (App (App (App (Con c) z) y) x) -> T.concat ["\\APP{", toTeXWithVNEmbedded vlist i (Con c), "}{\\left(", toTeXWithVN vlist i x, ",", toTeXWithVN vlist i y, ",", toTeXWithVN vlist i z, "\\right)}" ]
    (App (App (App (App (Con c) u) z) y) x) -> T.concat ["\\APP{", toTeXWithVNEmbedded vlist i (Con c), "}{\\left(", toTeXWithVN vlist i x, ",", toTeXWithVN vlist i y, ",", toTeXWithVN vlist i z, ",", toTeXWithVN vlist i u, "\\right)}" ]
    App m n -> case n of
                 (Var _) -> T.concat ["\\APP{", (toTeXWithVNEmbedded vlist i m), "}{(", toTeXWithVN vlist i n, ")}"]
                 (Con _) -> T.concat ["\\APP{", (toTeXWithVNEmbedded vlist i m), "}{(", toTeXWithVN vlist i n, ")}"]
                 (Proj _ _) -> T.concat ["\\APP{", (toTeXWithVNEmbedded vlist i m), "}{\\left(", toTeXWithVN vlist i n, "\\right)}"]
                 (Asp _ _) -> T.concat ["\\APP{", (toTeXWithVNEmbedded vlist i m), "}{\\left(", toTeXWithVN vlist i n,"\\right)}"]
                 _ -> T.concat["\\APP{", toTeXWithVNEmbedded vlist i m, "}{", toTeXWithVNEmbedded vlist i n, "}"]
    Sigma a b -> let varname = case a of
                                 Con "entity" -> T.concat ["x_{", T.pack (show i), "}"] 
                                 Con "event" -> T.concat ["e_{", T.pack (show i), "}"] 
                                 Con "state" -> T.concat ["s_{", T.pack (show i), "}"] 
                                 App _ _ -> T.concat ["u_{", T.pack (show i), "}"] 
                                 Sigma _ _ -> T.concat ["u_{", T.pack (show i), "}"] 
                                 Pi _ _ -> T.concat ["u_{", T.pack (show i), "}"] 
                                 Not _  -> T.concat ["u_{", T.pack (show i), "}"] 
                                 Appvec _ _ -> T.concat ["u_{", T.pack (show i), "}"] 
                                 Type -> T.concat ["p_{", T.pack (show i), "}"] 
                                 Kind -> T.concat ["p_{", T.pack (show i), "}"] 
                                 Eq _ _ _ -> T.concat ["s_{", T.pack (show i), "}"] 
                                 Nat -> T.concat ["k_{", T.pack (show i), "}"] 
                                 _ -> T.concat ["x_{", T.pack (show i), "}"] in
                 case b of
                   Top -> toTeXWithVN vlist (i+1) a
                   _ -> T.concat["\\dSigma[", varname, "]{", toTeXWithVN vlist (i+1) a, "}{", toTeXWithVN (varname:vlist) (i+1) b, "}"]
    Pair m n  -> T.concat["\\left(", toTeXWithVN vlist i m, ",", toTeXWithVN vlist i n, "\\right)"]
    Proj s m  -> T.concat["\\pi_", toTeX s, "\\left(", toTeXWithVN vlist i m, "\\right)"] 
    Lamvec m   -> let varname = T.concat ["x_{", T.pack (show i), "}"] in
                  T.concat ["\\lambda\\vec{", varname, "}.", toTeXWithVN (varname:vlist) (i+1) m]
    Appvec j m -> if j < (length vlist)
                  then T.concat ["\\APP{", (toTeXWithVNEmbedded vlist i m), "}{\\vec{", vlist!!j, "}}"]
                  else T.concat ["\\APP{", (toTeXWithVNEmbedded vlist i m), "}{\\vec{error:", T.pack (show j), " in ", T.pack (show vlist), "}}"]
    Unit      -> "()"
    Top       -> "\\top"
    Bot       -> "\\bot"
    Asp j m   -> T.concat ["@_{", T.pack (show j), "}:", (toTeXWithVN vlist i m)]
    Nat       -> "\\Set{N}"
    Zero      -> "0"
    Succ n    -> T.concat ["\\type{s}", (toTeXWithVN vlist i n)]
    Natrec n e f -> T.concat ["\\type{natrec}\\left(", (toTeXWithVN vlist i n), ",", (toTeXWithVN vlist i e), ",", (toTeXWithVN vlist i f), "\\right)"]
    Eq a m n  -> T.concat [(toTeXWithVN vlist i m),"=_{",(toTeXWithVN vlist i a),"}",(toTeXWithVN vlist i n)]
    Refl a m  -> T.concat ["\\type{refl}_{",(toTeXWithVN vlist i a),"}\\left(",(toTeXWithVN vlist i m),"\\right)"]
    Idpeel m n -> T.concat ["\\type{idpeel}\\left(", (toTeXWithVN vlist i m), ",", (toTeXWithVN vlist i n), "\\right)"]

toTeXWithVNEmbedded :: [T.Text] -> Int -> Preterm -> T.Text  
toTeXWithVNEmbedded vlist i preterm = case preterm of
  Lam m -> let varname = T.concat ["x_{", T.pack (show i), "}"] in
           T.concat["\\left(\\LAM[", varname, "]", toTeXWithVN (varname:vlist) (i+1) m, "\\right)"]
  --App m n -> T.concat["\\left(\\APP{", toTeXWithVNEmbedded vlist i m, "}{", toTeXWithVNEmbedded vlist i n, "}\\right)"]
  Lamvec m -> let varname = T.concat ["x_{", T.pack (show i), "}"] in
              T.concat ["\\left(\\lambda\\vec{", varname, "}.", toTeXWithVN (varname:vlist) (i+1) m, "\\right)"]
--  Appvec j m -> T.concat ["\\left(\\APP{", toTeXWithVNEmbedded vlist i m, "}{\\vec{x}_", T.pack (show j), "}\\right)"]
  m          -> toTeXWithVN vlist i m

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

instance Typeset Node where
  toTeX node@(Node _ _ _ _ _ _ _) = 
    case daughters node of 
      [] -> T.concat ["\\vvlex[", (source node), "]{", (pf node), "}{", toTeX (cat node), "}{", toTeX (sem node), "}"] --, "\\ensuremath{", (source node), "}"]
      _ -> T.concat ["\\nd[", toTeX (rs node), "]{\\vvcat{", toTeX (cat node), "}{", toTeX (sem node), "}}{", daughtersTeX, "}"] --, "\\ensuremath{", (source node), "}"]
    where daughtersTeX =
            case daughters node of
              [l,c,r] -> T.concat [toTeX l, "&", toTeX c, "&", toTeX r]
              [l,r]   -> T.concat [toTeX l, "&", toTeX r]
              [c]     -> T.concat [toTeX c]
              x  -> T.pack $ "(Error: daughter nodes are ill-formed: " ++ show x ++ ")"

instance Typeset Cat where
  toTeX category = case category of
    S x1 x2 x3 -> T.concat [
                    "S\\f{",
                    printF x1,",",
                    printF x2,",",
                    pmFeatures2TeX x3,
                    "}"
                    ]
    NP cas      -> T.concat ["\\np\\f{",(printF cas),"}"]
    Sbar x      -> T.concat ["\\bar{S}\\f{",(printF x),"}"]
    N           -> "N"
    CONJ        -> "\\conj"
    LPAREN      -> "LPAREN"
    RPAREN      -> "RPAREN"
    SL x y      -> T.concat [toTeX x, "/", toTeX' y]
    BS x y      -> T.concat [toTeX x, "{\\bs}", toTeX' y]
    T True i u  -> T.concat ["\\vT^{\\sq{",(T.pack $ show i),"}}_{",toTeX u,"}"]
    T False i u -> T.concat [toTeX' u, "^{\\sq{",(T.pack $ show i),"}}"]

toTeX' :: Cat -> T.Text
toTeX' c = if isBaseCategory c 
           then toTeX c
           else T.concat ["(", toTeX c, ")"]

pmFeature2TeX :: Bool -> T.Text -> PMFeature -> Maybe T.Text
pmFeature2TeX _ label pmf = case (label,pmf) of
    (l,P)     -> Just $ T.concat ["{+}", l]
    (_,M)     -> Nothing -- if shared then Just $ T.concat ["{-}", l] else Nothing
    (l,PM)    -> Just $ T.concat ["{\\pm}", l]
    (l,F i f) -> do
                 x <- pmFeature2TeX True l f
                 return $ T.concat [x,":\\sq{",T.pack (show i),"}"]

pmFeatures2TeX :: [PMFeature] -> T.Text
pmFeatures2TeX pmfs = T.intercalate "{,}" $ Maybe.catMaybes $ pmFeatures2TeXLoop ["t","p","n","N","T"] pmfs

pmFeatures2TeXLoop :: [T.Text] -> [PMFeature] -> [Maybe T.Text]
pmFeatures2TeXLoop labels pmfs = case (labels,pmfs) of
  ([],[])         -> []
  ((l:ls),(p:ps)) -> (pmFeature2TeX False l p):(pmFeatures2TeXLoop ls ps)
  _ -> [Just $ T.concat ["Error: mismatch in ", T.pack (show labels), " and ", T.pack (show pmfs)]]

instance Typeset RuleSymbol where
  toTeX rulesymbol = case rulesymbol of 
    LEX -> "lex"
    EC  -> "ec"
    FFA -> ">"
    BFA -> "<"
    FFC1 -> ">B"
    BFC1 -> "<B"
    FFC2 -> ">B^2"
    BFC2 -> "<B^2"
    FFC3 -> ">B^3"
    BFC3 -> "<B^3"
    FFCx1 -> ">B_{\\times}"
    FFCx2 -> ">B_{\\times}^2"
    FFSx  -> ">S_{\\times}"
    COORD -> "\\langle\\Phi\\rangle"
    PAREN -> "PAREN"
    -- CNP -> "CNP"

-- | prints n-nodes from given list of CCG nodes (=a parsing result) as a TeX source code.
printNNodesInTeX :: S.Handle -> Int -> [CCG.Node] -> IO()
printNNodesInTeX handle n nodes = mapM_ (\node -> S.hPutStrLn handle $ "\\noindent\\kern-2em\\scalebox{.2}{" ++ T.unpack (toTeX node) ++ "\\\\}\\par\\medskip") $ take n nodes

-- | prints CCG nodes (=a parsing result) as a TeX source code.
printNodesInTeX :: S.Handle -> [CCG.Node] -> IO()
printNodesInTeX handle = mapM_ (\node -> S.hPutStrLn handle $ "\\noindent\\kern-2em\\scalebox{.2}{" ++ T.unpack (toTeX node) ++ "\\\\}\\par\\medskip")

