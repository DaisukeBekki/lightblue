{-# LANGUAGE GADTs, FlexibleInstances #-}

{-|
Module      : Parser.CCG
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Syntactic categories, syntactic features and combinatory rules of Combinatory Categorial Grammar.
-}
module Parser.CCG (
  -- * Types
  Node(..),
  RuleSymbol(..),
  Cat(..),
  Feature(..),
  FeatureValue(..),
  -- * Text and TeX interface
  showScore,
  -- * Tests
  isBaseCategory,
  --isBunsetsu,
  -- * Combinatory Rules
  unaryRules,
  binaryRules,
  coordinationRule,
  parenthesisRule,
  -- * for debugging
  unifyCategory,
  unifyWithHead,
  -- * Partial Parsing
  wrapNode,
  conjoinNodes
  -- drel,
  ) where

import Prelude hiding (id)
import qualified Data.Text.Lazy as T --text
import qualified Data.List as L      --base
import qualified Data.Maybe as Maybe --base
import Data.Fixed                    --base
import Data.Ratio                    --base
import qualified DTS.DTTdeBruijn as DTTdB      --lightblue
import DTS.UDTTdeBruijn as DTS hiding (sig)--lightblue
import qualified DTS.DTTwithName as DTTwN  --lightblue
import qualified DTS.UDTTwithName as VN  --lightblue
import qualified Interface.Tree as Tree --lightblue
import Interface.Text
import Interface.TeX
import Interface.HTML

type UDTTpreterm = DTS.Preterm
type Signature = DTTdB.Signature

-- | A node in CCG derivation tree.
data Node = Node {
  rs :: RuleSymbol,    -- ^ The name of the rule
  pf :: T.Text,        -- ^ The phonetic form
  cat :: Cat,          -- ^ The syntactic category (in CCG)
  sem :: UDTTpreterm,      -- ^ The semantic representation (in DTS)
  sig :: Signature,    -- ^ Signature
  daughters :: [Node], -- ^ The daughter nodes
  score :: Rational,   -- ^ The score (between 0.00 to 1.00, larger the better)
  source :: T.Text    -- ^ The source of the lexical entry
  } deriving (Eq)

instance Show Node where
  show = T.unpack . toText

-- | A node with a larger score is less than the one with a smaller score, thus larger comes earlier in [Node]
instance Ord Node where
  (Node {score=i}) `compare` (Node {score=j})
    | i < j  = GT
    | i == j = EQ
    | i > j  = LT
  (Node _ _ _ _ _ _ _ _) `compare` (Node _ _ _ _ _ _ _ _) = EQ

{- Printing Nodes -}

-- | shows the score of the given node
showScore :: Node -> T.Text
showScore node = T.pack (show ((fromRational $ score node)::Fixed E2))

instance SimpleText Node where
  toText n -- @(Node _ _ _ _ sig' _ _ _) 
    = T.concat [toTextLoop "" n, "Sig. ", toText $ DTTwN.fromDeBruijnSignature $ sig n, "\n"]
    where toTextLoop indent node =
            case daughters node of 
              [] -> T.concat [T.pack indent, toText (rs node), " ", pf node, " ", toText (cat node), " ", toText $ VN.fromDeBruijn [] $ sem node, " ", source node, " [", showScore node, "]\n"]
              dtrs -> T.concat $ [T.pack indent, toText (rs node), " ", toText (cat node), " ", toText $ VN.fromDeBruijn [] $ sem node, " [", showScore node, "]\n"] ++ (map (\d -> toTextLoop (indent++"  ") d) dtrs)

instance Typeset Node where
  toTeX node = -- @(Node _ _ _ _ _ _ _ _) =
    case daughters node of 
      [] -> T.concat ["\\vvlex[", (source node), "]{", (pf node), "}{", toTeX (cat node), "}{", toTeX $ VN.fromDeBruijn [] $ sem node, "}"] --, "\\ensuremath{", (source node), "}"]
      dtrs -> T.concat ["\\nd[", toTeX (rs node), "]{\\vvcat{", toTeX (cat node), "}{", toTeX $ VN.fromDeBruijn [] $ sem node, "}}{", T.intercalate "&" $ map toTeX dtrs, "}"] --, "\\ensuremath{", (source node), "}"]

instance MathML Node where
  toMathML node = -- @(Node _ _ _ _ _ _ _ _) =
    case daughters node of 
      [] -> T.concat ["<mrow><mfrac linethickness='2px'><mtext fontsize='1.0' color='Black'>", pf node, "</mtext><mfrac linethickness='0px'><mstyle color='Red'>", toMathML $ cat node, "</mstyle><mstyle color='Black'>", toMathML $ VN.fromDeBruijn [] $ betaReduce $ sem node, "</mstyle></mfrac></mfrac><mtext fontsize='0.8' color='Black'>", source node, "</mtext></mrow>"] 
      dtrs -> T.concat ["<mrow><mfrac linethickness='2px'><mrow>", T.concat $ map toMathML dtrs, "</mrow><mfrac linethickness='0px'><mstyle color='Red'>", toMathML $ cat node, "</mstyle><mstyle color='Black'>", toMathML $ VN.fromDeBruijn [] $ betaReduce $ sem node, "</mstyle></mfrac></mfrac><mtext fontsize='0.8' color='Black'>", toMathML $ rs node, "</mtext></mrow>"] 

-- | ゆくゆくは、Catはtype classとして再定義し、
-- | merge : Cat -> Cat -> Cat??のようにclass関数を用意してパーザはそれのみ使う
-- | Featureは言語別にモジュールを分けて定義できるようにする

-- | Syntactic categories of 
data Cat =
  S [Feature]        -- ^ S
  | NP [Feature]     -- ^ NP
  | N                -- ^ N
  | Sbar [Feature]   -- ^ S bar
  | CONJ             -- ^ CON
  | LPAREN           -- ^ A category for left parentheses
  | RPAREN           -- ^ A category for right parentheses
  | PUNCT            -- ^ A category for a punctuation
  | PERIOD           -- ^ A category for a period
  | SL Cat Cat       -- ^ X/Y
  | BS Cat Cat       -- ^ X\\Y
  | T Bool Int Cat   -- ^ Category variables, where Int is an index, Cat is a restriction for its head. 

-- | checks if given two categories can be coordinated.
instance Eq Cat where
  SL x1 x2 == SL y1 y2 = (x1 == y1) && (x2 == y2)
  BS x1 x2 == BS y1 y2 = (x1 == y1) && (x2 == y2)
  T f1 _ x == T f2 _ y = (f1 == f2) && (x == y)
  S (_:(f1:_))  == S (_:(f2:_))  = case unifyFeature [] f1 f2 of
                                     Just _ -> True
                                     Nothing -> False
  NP f1 == NP f2 = unifiable f1 f2
  N == N = True
  Sbar f1 == Sbar f2 = unifiable f1 f2
  CONJ == CONJ = True
  LPAREN == LPAREN = True
  RPAREN == RPAREN = True
  PUNCT == PUNCT = True
  PERIOD == PERIOD = True
  _ == _ = False

-- | `toText` method is invoked.
instance Show Cat where
  show = T.unpack . toText

instance SimpleText Cat where
  toText category = case category of
    SL x y      -> T.concat [toText x, "/", toText' y]
    BS x y      -> T.concat [toText x, "\\", toText' y]
--    T True i c     -> T.concat ["T[",toText c,"]<", (T.pack $ show i),">"]
    T True i _     -> T.concat ["T", T.pack $ show i]
    T False i c     -> T.concat [toText c, "<", (T.pack $ show i), ">"]
    S (pos:(conj:pmf)) -> 
              T.concat [
                       "S[",
                       toText pos,"][",
                       toText conj,"][",
                       toText pmf,
                       "]"
                       ]
    S []        -> "S"
    NP [cas]    -> T.concat ["NP[", toText cas, "]"]
    NP []       -> "NP"
    Sbar [sf]   -> T.concat ["Sbar[", toText sf, "]"]
    N           -> "N"
    CONJ        -> "CONJ"
    LPAREN      -> "LPAREN"
    RPAREN      -> "RPAREN"
    PUNCT       -> "PUNCT"
    PERIOD      -> "PERIOD"
    _ -> "Error in Simpletext Cat"
    where -- A bracketed version of `toText'` function
    toText' c = if isBaseCategory c
                  then toText c
                  else T.concat ["(", toText c, ")"]

instance Typeset Cat where
  toTeX category = case category of
    SL x y      -> T.concat [toTeX x, "/", toTeX' y]
    BS x y      -> T.concat [toTeX x, "{\\bs}", toTeX' y]
    T True i u  -> T.concat ["\\vT^{\\sq{",(T.pack $ show i),"}}_{",toTeX u,"}"]
    T False i u -> T.concat [toTeX' u, "^{\\sq{",(T.pack $ show i),"}}"]
    S (pos:(conj:pm)) -> 
                   T.concat [
                     "S\\f{",
                     toTeX pos,",",
                     toTeX conj,",",
                     toTeX pm,
                     "}"
                     ]
    NP [cas]    -> T.concat ["\\np\\f{", toText cas, "}"]
    Sbar [sf]   -> T.concat ["\\bar{S}\\f{", toText sf, "}"]
    N           -> "N"
    CONJ        -> "\\conj"
    LPAREN      -> "LPAREN"
    RPAREN      -> "RPAREN"
    PUNCT       -> "PUNCT"
    PERIOD      -> "PERIOD"
    _ -> "Error: toTeX Cat"
    where toTeX' c = if isBaseCategory c 
                     then toTeX c
                     else T.concat ["(", toTeX c, ")"]

instance MathML Cat where
  toMathML category = case category of
    SL x y      -> T.concat ["<mrow>", toMathML x, "<mo>/</mo>", toMathML' y, "</mrow>"]
    BS x y      -> T.concat ["<mrow>", toMathML x, "<mo>\\</mo>", toMathML' y, "</mrow>"]
    T True i _  -> T.concat ["<msub><mi>T</mi><mn>",(T.pack $ show i),"</mn></msub>"]
    T False i u -> T.concat ["<msub>", toMathML' u, "<mn>",(T.pack $ "<"++ show i ++">"),"</mn></msub>"]
    S (pos:(conj:pm)) -> 
                   T.concat [
                     "<msub><mi>S</mi><mstyle color='Purple'><mtable columnalign='left'><mtr><mtd>",
                     toMathML pos,"</mtd></mtr><mtr><mtd><mpadded height='-0.5em'>",
                     toMathML conj,
                     let x = toMathML pm in 
                       if T.null x
                       then T.empty
                       else T.append "</mtd></mtr><mtr><mtd><mpadded height='-0.5em'>" x,
                     "</mpadded></mtd></mtr></mtable></mstyle></msub>"
                     ]
    NP [cas]    -> T.concat ["<msub><mi>NP</mi><mtext color='Purple'>", toMathML cas, "</mtext></msub>"]
    Sbar [sf]   -> T.concat ["<msub><menclose notation='top'><mi>S</mi></menclose><mtext color='Purple'>", toMathML sf, "</mtext></msub>"]
    N           -> "<mi>N</mi>"
    CONJ        -> "<mi>CONJ</mi>"
    LPAREN      -> "<mi>LPAREN</mi>"
    RPAREN      -> "<mi>RPAREN</mi>"
    PUNCT       -> "<mi>PUNCT</mi>"
    PERIOD      -> "<mi>PERIOD</mi>"
    _           -> "<mtext>Error: toMathML Cat</mtext>"
    -- newcat = T.replace ">" "&gt;" $ T.replace "<" "&lt;" $ toText (cat node);
    where toMathML' c = if isBaseCategory c 
                        then toMathML c
                        else T.concat ["<mrow><mo>(</mo>", toMathML c, "<mo>)</mo></mrow>"]

-- | Syntactic features of 
data Feature = 
  F [FeatureValue]        -- ^ Syntactic feature
  | SF Int [FeatureValue] -- ^ Shared syntactic feature (with an index)
  deriving (Eq, Show)

-- | Values of syntactic features of Japanese 
data FeatureValue =
  V5k | V5s | V5t | V5n | V5m | V5r | V5w | V5g | V5z | V5b |
  V5IKU | V5YUK | V5ARU | V5NAS | V5TOW |
  V1 | VK | VS | VSN | VZ | VURU |
  Aauo | Ai | ANAS | ATII | ABES |
  Nda | Nna | Nno | Ntar | Nni | Nemp | Nto |
  Exp | -- Error |
  Stem | UStem | NStem |
  Neg | Cont | Term | Attr | Hyp | Imper | Pre | NTerm | 
  NegL | TeForm | NiForm |
  EuphT | EuphD |
  ModU | ModD | ModS | ModM |
  VoR | VoS | VoE |
  P | M |
  Nc | Ga | O | Ni | To | Niyotte | No |
  ToCL | YooniCL |
  Decl 
  deriving (Eq)

instance Show FeatureValue where
  show V5k = "v:5:k"
  show V5s = "v:5:s"
  show V5t = "v:5:t"
  show V5n = "v:5:n"
  show V5m = "v:5:m"
  show V5r = "v:5:r"
  show V5w = "v:5:w"
  show V5g = "v:5:g"
  show V5z = "v:5:z"
  show V5b = "v:5:b"
  show V5IKU = "v:5:IKU"
  show V5YUK = "v:5:YUK"
  show V5ARU = "v:5:ARU"
  show V5NAS = "v:5:NAS"
  show V5TOW = "v:5:TOW"
  show V1 = "v:1"
  show VK = "v:K"
  show VS = "v:S"
  show VSN = "v:SN"
  show VZ = "v:Z"
  show VURU = "v:URU"
  show Aauo = "a:i:auo"
  show Ai = "a:i:i"
  show ANAS = "a:i:NAS"
  show ATII = "a:i:TII"
  show ABES = "a:BES"
  show Nda = "n:da"
  show Nna = "n:na"
  show Nno = "n:no"
  show Nni = "n:ni"
  show Nemp = "n:\\emp"
  show Ntar = "n:tar"
  show Nto  = "n:to"
  show Exp = "exp"
  -- show Error = "error"
  --
  show Stem = "stem"
  show UStem = "ustem"
  show NStem = "nstem"
  show Neg = "neg"
  show Cont = "cont"
  show Term = "term"
  show Attr = "attr"
  show Hyp = "hyp"
  show Imper = "imp"
  show Pre = "pre"
  show NTerm = "nterm"
  show NegL = "neg+l"
  show TeForm = "te"
  show NiForm = "ni"
  show EuphT = "euph:t"
  show EuphD = "euph:d"
  show ModU = "mod:u"
  show ModD = "mod:d"
  show ModS = "mod:s"
  show ModM = "mod:m"
  show VoR = "vo:r"
  show VoS = "vo:s"
  show VoE = "vo:e"
  -- 
  show ToCL = "to"
  show YooniCL = "yooni"
  show Decl = "decl"
  --
  show P = "+"
  show M = "-"
  --
  show Nc = "nc"
  show Ga = "ga"
  show O = "o"
  show Ni = "ni"
  show To = "to"
  show Niyotte = "niyotte"
  show No = "no"

-- | checks if two lists of features are unifiable.
unifiable :: [Feature] -> [Feature] -> Bool
unifiable f1 f2 = case unifyFeatures [] f1 f2 of
                    Just _ -> True
                    Nothing -> False

{- Text interface -}

instance SimpleText Feature where
  toText (SF i f) = T.concat [toText f, "<", T.pack (show i), ">"]
  toText (F f) = toText f

-- | prints a value of a syntactic feature.
instance SimpleText [FeatureValue] where
  toText [] = T.empty
  toText [pos] = T.pack $ show pos
  toText [pos1,pos2] = T.pack $ (show pos1) ++ "|" ++ (show pos2)
  --toText (pos1:(pos2:_)) = T.pack $ (show pos1) ++ "|" ++ (show pos2) ++ "|+"
  toText (pos1:(pos2:(pos3:_))) = T.pack $ (show pos1) ++ "|" ++ (show pos2) ++ "|" ++ (show pos3) ++ "|+"

-- | prints a list of syntactic features each of which ranges over {P,M,PM}.
instance SimpleText [Feature] where
  toText pmfs = T.intercalate "," $ Maybe.catMaybes $ printPMFsLoop ["t","p","n","N","T"] pmfs

printPMFsLoop :: [T.Text] -> [Feature] -> [Maybe T.Text]
printPMFsLoop labels pmfs = case (labels,pmfs) of
  ([],[])         -> []
  ((l:ls),(p:ps)) -> (printPMF l p):(printPMFsLoop ls ps)
  _ -> [Just $ T.concat ["Error: mismatch in ", T.pack (show labels), " and ", T.pack (show pmfs)]]

-- | prints a syntactic feature that ranges over {P,M,PM}.
printPMF :: T.Text -> Feature -> Maybe T.Text
printPMF label pmf = case (label,pmf) of
    (l,F [P])   -> Just $ T.concat ["+", l]
    (_,F [M])   -> Nothing -- if shared then Just $ T.concat ["-", l] else Nothing
    (l,F [P,M]) -> Just $ T.concat ["±", l]
    (l,F [M,P]) -> Just $ T.concat ["±", l]
    (l,SF i f)  -> do
                   x <- printPMF l (F f)
                   return $ T.concat [x,"<",T.pack (show i),">"]
    _ -> return $ T.concat ["Error: printPMF", T.pack $ show pmf]

{- TeX Interface -}

instance Typeset Feature where
  toTeX (SF i ft) = T.concat [toTeX ft, ":\\sq{", T.pack (show i), "}"]
  toTeX (F ft)    = toTeX ft

instance Typeset [FeatureValue] where
  toTeX [] = T.empty
  toTeX [pos] = T.pack $ show pos
  toTeX [pos1,pos2] = T.pack $ (show pos1) ++ "|" ++ (show pos2)
  toTeX (pos1:(pos2:_)) = T.pack $ (show pos1) ++ "|" ++ (show pos2) ++ "|+"

pmf2TeX :: Bool -> T.Text -> Feature -> Maybe T.Text
pmf2TeX _ label pmf = case (label,pmf) of
    (l,F [P])       -> Just $ T.concat ["{+}", l]
    (_,F [M])       -> Nothing -- if shared then Just $ T.concat ["{-}", l] else Nothing
    (l,F [P,M]) -> Just $ T.concat ["{\\pm}", l]
    (l,F [M,P]) -> Just $ T.concat ["{\\pm}", l]
    (l,SF i f) -> do
                 x <- pmf2TeX True l (F f)
                 return $ T.concat [x,":\\sq{",T.pack (show i),"}"]
    _ -> return $ T.pack "Error: pmf2TeX"

instance Typeset [Feature] where
  toTeX pmfs = T.intercalate "{,}" $ Maybe.catMaybes $ pmfs2TeXLoop ["t","p","n","N","T"] pmfs

pmfs2TeXLoop :: [T.Text] -> [Feature] -> [Maybe T.Text]
pmfs2TeXLoop labels pmfs = case (labels,pmfs) of
  ([],[])         -> []
  ((l:ls),(p:ps)) -> (pmf2TeX False l p):(pmfs2TeXLoop ls ps)
  _ -> [Just $ T.concat ["Error: mismatch in ", T.pack (show labels), " and ", T.pack (show pmfs)]]

{- MathML Interface -}

instance MathML Feature where 
  toMathML (SF i f) = T.concat ["<mtext>", toTeX f, "</mtext><mo>:</mo><menclose notation='box'><mn>", T.pack (show i), "</mn></menclose>"]
  toMathML (F f) = T.concat ["<mtext>", toTeX f, "</mtext>"]

pmf2MathML :: T.Text -> Feature -> Maybe T.Text
pmf2MathML label pmf = case (label,pmf) of
  (l,F [P])   -> Just $ T.concat ["+", l]
  (_,F [M])   -> Nothing -- if shared then Just $ T.concat ["{-}", l] else Nothing
  (l,F [P,M]) -> Just $ T.concat ["&plusmn;", l]
  (l,F [M,P]) -> Just $ T.concat ["&plusmn;", l]
  (l,SF i f)  -> do
                 x <- pmf2MathML l (F f)
                 return $ T.concat [x, ":<menclose notation='box'><mn>", T.pack $ show i, "</mn></menclose>"]
  _ -> return $ T.pack "Error: pmf2MathML"

instance MathML [Feature] where
  toMathML pmfs = T.intercalate "," $ Maybe.catMaybes $ pmfs2MathMLLoop ["t","p","n","N","T"] pmfs

pmfs2MathMLLoop :: [T.Text] -> [Feature] -> [Maybe T.Text]
pmfs2MathMLLoop labels pmfs = case (labels,pmfs) of
  ([],[])         -> []
  ((l:ls),(p:ps)) -> (pmf2MathML l p):(pmfs2MathMLLoop ls ps)
  _ -> [Just $ T.concat ["Error: mismatch in ", T.pack (show labels), " and ", T.pack (show pmfs)]]

{- Tests for syntactic categories -}

-- | A test to check if a given category is a base category (i.e. not a functional category nor a category variable).
isBaseCategory :: Cat -> Bool
isBaseCategory c = case c of
  S _  -> True
  NP _ -> True
  T False _ c2 -> isBaseCategory c2
  T True _ _ -> True
  N -> True 
  Sbar _ -> True
  CONJ -> True
  LPAREN -> True
  RPAREN -> True
  _ -> False

isArgumentCategory :: Cat -> Bool
isArgumentCategory c = case c of
  NP _ | isNoncaseNP c -> False
       | otherwise -> True
  Sbar _ -> True
  _ -> False

-- | A test to check if a given category is T\NPnc.
isTNoncaseNP :: Cat -> Bool
isTNoncaseNP c = case c of
  (T _ _ _) `BS` x -> isNoncaseNP x
  _ -> False

isNoncaseNP :: Cat -> Bool
isNoncaseNP c = case c of
  NP (F v:_)    -> Nc `elem` v
  NP (SF _ v:_) -> Nc `elem` v
  _ -> False

endsWithT :: Cat -> Bool
endsWithT c = case c of
  SL x _ -> endsWithT x
  T _ _ _ -> True
  _ -> False

isNStem :: Cat -> Bool
isNStem c = case c of
  BS x _ -> isNStem x
  S (_:(f:_)) -> case unifyFeature [] f (F[NStem]) of
                   Just _ -> True
                   Nothing -> False
  _ -> False

-- | The name of the CCG rule to derive the node.
data RuleSymbol = 
  LEX    -- ^ A lexical item
  | EC   -- ^ An empty category
  | FFA  -- ^ Forward function application rule.
  | BFA  -- ^ Backward function application rule
  | FFC1 -- ^ Forward function composition rule 1
  | BFC1 -- ^ Backward function composition rule 1
  | FFC2 -- ^ Forward function composition rule 2
  | BFC2 -- ^ Backward function composition rule 2
  | FFC3 -- ^ Forward function composition rule 3
  | BFC3 -- ^ Backward function composition rule 3
  | FFCx1 -- ^ Forward function crossed composition rule 1
  | FFCx2 -- ^ Forward function crossed composition rule 2
  | FFSx  -- ^ Forward function crossed substitution rule
  | FFSx2 -- ^ Forward function crossed substitution rule 2
  | COORD -- ^ Coordination rule
  | PAREN -- ^ Parenthesis rule
  | PNCT  -- ^ Punctuation rule
  | PRD   -- ^ Period rule
  | WRAP  -- ^ Wrap rule
  | DC    -- ^ Dynamic conjunction rule
  -- | DREL  -- ^ Discourse Relation rule
  deriving (Eq, Show)

-- | The simple-text representation of the rule symbols.
instance SimpleText RuleSymbol where
  toText rulesymbol = case rulesymbol of 
    LEX -> "LEX"
    EC  -> "EC"
    FFA -> ">"
    BFA -> "<"
    FFC1 -> ">B"
    BFC1 -> "<B"
    FFC2 -> ">B2"
    BFC2 -> "<B2"
    FFC3 -> ">B3"
    BFC3 -> "<B3"
    FFCx1 -> ">Bx"
    FFCx2 -> ">Bx2"
    FFSx  -> ">Sx"
    FFSx2 -> ">Sx2"
    COORD -> "<Φ>"
    PAREN -> "PAREN"
    PNCT   -> "PUNCT"
    PRD   -> "PERIOD"
    WRAP  -> "WRAP"
    DC    -> "DC"
    -- DREL  -> "DREL"
    -- CNP -> "CNP"

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
    FFSx2 -> ">S_{\\times}^2"
    COORD -> "\\langle\\Phi\\rangle"
    PAREN -> "PAREN"
    PNCT  -> "PUNCT"
    PRD   -> "PERIOD"
    WRAP  -> "WRAP"
    DC    -> "DC"
    -- DREL  -> "DREL"
    -- CNP -> "CNP"

instance MathML RuleSymbol where
  toMathML rulesymbol = T.replace ">" "&gt;" $ T.replace "<" "&lt;" $ toText rulesymbol

{- Classes of Combinatory Rules -}

-- | The function to apply all the unaryRules to a CCG node.
unaryRules :: Node -> [Node] -> [Node]
unaryRules _ prevlist = prevlist

-- | The function to apply all the binary rules to a given pair of CCG nodes.
binaryRules :: Node -> Node -> [Node] -> [Node]
binaryRules lnode rnode = 
  periodRule lnode rnode
  . punctRule lnode rnode
  . forwardFunctionCrossedSubstitution2Rule lnode rnode
  . forwardFunctionCrossedSubstitutionRule lnode rnode
  . forwardFunctionCrossedComposition2Rule lnode rnode
  . forwardFunctionCrossedComposition1Rule lnode rnode
  . backwardFunctionComposition3Rule lnode rnode
  . backwardFunctionComposition2Rule lnode rnode
  . forwardFunctionComposition2Rule lnode rnode
  . backwardFunctionComposition1Rule lnode rnode
  . forwardFunctionComposition1Rule lnode rnode
  . backwardFunctionApplicationRule lnode rnode
  . forwardFunctionApplicationRule lnode rnode

{- Test -}

--plus :: [FeatureValue]
--plus = [P]

{-
minus :: [FeatureValue]
minus = [M]

pm :: [FeatureValue]
pm = [P,M]

verb :: [FeatureValue]
verb = [V5k, V5s, V5t, V5n, V5m, V5r, V5w, V5g, V5z, V5b, V5IKU, V5YUK, V5ARU, V5NAS, V5TOW, V1, VK, VS, VSN, VZ, VURU]

adjective :: [FeatureValue]
adjective = [Aauo, Ai, ANAS, ATII, ABES]

nomPred :: [FeatureValue]
nomPred = [Nda, Nna, Nno, Nni, Nemp, Ntar]

anyPos :: [FeatureValue]
anyPos = verb ++ adjective ++ nomPred ++ [Exp]

nonStem :: [FeatureValue]
nonStem = [Neg, Cont, Term, Attr, Hyp, Imper, Pre, ModU, ModS, VoR, VoS, VoE, NegL, TeForm]

anySExStem :: Cat
anySExStem = S [F anyPos, F nonStem, SF 1 pm, SF 2 pm, SF 3 pm, F minus, F minus]

test :: IO()
test = do
  let x = T True 1 anySExStem ;
      y1 = T True 1 anySExStem `BS` NP [F [Nc]];
      y2 = (T True 1 (S [F anyPos, F nonStem, SF 1 [P,M], SF 2 [P,M], SF 3 [P,M], F[M], F[M]]) `BS` T True 1 (S [F anyPos, F nonStem, SF 1 [P,M], SF 2 [P,M], SF 3 [P,M], F[M], F[M]])) `BS` NP [F [Nc]]
  T.putStrLn "Functional application: x/y1 y2"
  T.putStr "x: "    
  T.putStrLn $ toText x
  T.putStr "y1: "
  T.putStrLn $ toText y1
  T.putStr "y2: "
  T.putStrLn $ toText y2
  let inc = maximumIndexC y2
  T.putStr "maximumIndexC y2: "
  print $ maximumIndexC y2
  T.putStr "increment y1 by inc: "    
  print (incrementIndexC y1 inc)
  let Just uc@(_,csub,fsub) = unifyCategory [] [] y2 (incrementIndexC y1 inc)
  T.putStr "unifyCategory y2 (incr. y1 inc): "
  print uc
  T.putStr "csub: "
  print csub
  T.putStr "fsub: "
  print fsub
  let newcat = simulSubstituteCV csub fsub (incrementIndexC x inc)
  T.putStr "newcat: "
  T.putStrLn $ toText newcat
-}

-- | Forward function application rule.
forwardFunctionApplicationRule :: Node -> Node -> [Node] -> [Node]
forwardFunctionApplicationRule lnode@(Node {rs=r, cat=SL x y1, sem=f}) rnode@(Node {cat=y2, sem=a}) prevlist =
  -- [>] x/y1  y2  ==>  x
  if r == FFC1 || r == FFC2 || r == FFC3 -- Non-normal forms
  then prevlist
  else
    case y1 of
      T True _ _ -> prevlist -- Ad-hoc rule
      _ -> let inc = maximumIndexC y2 in
           case unifyCategory [] [] [] y2 (incrementIndexC y1 inc) of
             Nothing -> prevlist -- Unification failure
             Just (_,csub,fsub) ->
               let newcat = simulSubstituteCV csub fsub (incrementIndexC x inc) in
                 Node {
                   rs = FFA,
                   pf = pf(lnode) `T.append` pf(rnode),
                   cat = newcat,
                   sem = betaReduce $ transvec newcat $ betaReduce $ App f a,
                   daughters = [lnode,rnode],
                   score = score(lnode)*score(rnode),
                   source = "", --T.concat $ map (\(i,c)-> T.concat [T.pack (show i)," \\mapsto ",toTeX c,", "]) sub
                   sig = sig(lnode) ++ sig(rnode)
                   }:prevlist
forwardFunctionApplicationRule _ _ prevlist = prevlist

-- | Backward function application rule.
backwardFunctionApplicationRule :: Node -> Node -> [Node] -> [Node]
backwardFunctionApplicationRule lnode@(Node {cat=y1, sem=a}) rnode@(Node {rs=r, cat=(BS x y2), sem=f}) prevlist =
  -- [<] y1  x\y2  ==> x
  if r == BFC1 || r == BFC2 || r == BFC3 -- Non-normal forms
  then prevlist
  else     
    let inc = maximumIndexC y1 in
    case unifyCategory [] [] [] y1 (incrementIndexC y2 inc) of
      Nothing -> prevlist -- Unification failure
      Just (_,csub,fsub) -> let newcat = simulSubstituteCV csub fsub (incrementIndexC x inc) in
                      Node {
                        rs = BFA,
                        pf = pf(lnode) `T.append` pf(rnode),
                        cat = newcat,
                        sem = betaReduce $ transvec newcat $ betaReduce $ App f a,
                        daughters = [lnode,rnode],
                        score = score(lnode)*score(rnode),
                        source = "", -- pf(lnode) `T.append` pf(rnode)
                        sig = sig(lnode) ++ sig(rnode)
                        }:prevlist
backwardFunctionApplicationRule _ _ prevlist = prevlist

-- | Forward function composition rule.
forwardFunctionComposition1Rule :: Node -> Node -> [Node] -> [Node]
forwardFunctionComposition1Rule lnode@(Node {rs=r,cat=SL x y1, sem=f}) rnode@(Node {cat=SL y2 z, sem=g}) prevlist =
  -- [>B] x/y1  y2/z  ==> x/z
  if r == FFC1 || r == FFC2 || r == FFC3 || (isTNoncaseNP y1) -- Non-normal forms (+ Ad-hoc rule 1)
  then prevlist
  else  
    let inc = maximumIndexC (cat rnode) in
    case unifyCategory [] [] [] y2 (incrementIndexC y1 inc) of
      Nothing -> prevlist -- Unification failure
      Just (_,csub,fsub) -> 
        let z' = simulSubstituteCV csub fsub z in
        if numberOfArguments z' > 3  -- Ad-hoc rule 2
        then prevlist
        else let newcat = (simulSubstituteCV csub fsub (incrementIndexC x inc)) `SL` z' in
             Node {
               rs = FFC1,
               pf = pf(lnode) `T.append` pf(rnode),
               cat = newcat,
               sem = betaReduce $ transvec newcat $ betaReduce $ (Lam (App f (App g (Var 0)))),
               daughters = [lnode,rnode],
               score = score(lnode)*score(rnode),
               source = "",
               sig = sig(lnode) ++ sig(rnode)
               }:prevlist
forwardFunctionComposition1Rule _ _ prevlist = prevlist

-- | Backward function composition rule.
backwardFunctionComposition1Rule :: Node -> Node -> [Node] -> [Node]
backwardFunctionComposition1Rule lnode@(Node {cat=BS y1 z, sem=g}) rnode@(Node {rs=r,cat=(BS x y2), sem=f}) prevlist =
  -- [<B] y1\z:g  x\y2:f  ==> x\z
  if r == BFC1 || r == BFC2 || r == BFC3 -- Non-normal forms
  then prevlist
  else
    let inc = maximumIndexC (cat lnode) in
    case unifyCategory [] [] [] y1 (incrementIndexC y2 inc) of
      Nothing -> prevlist -- Unification failure
      Just (_,csub,fsub) -> let newcat = simulSubstituteCV csub fsub ((incrementIndexC x inc) `BS` z) in
                      Node {
                        rs = BFC1,
                        pf = pf(lnode) `T.append` pf(rnode),
                        cat = newcat,
                        sem = betaReduce $ transvec newcat $ betaReduce $ Lam (App f (App g (Var 0))),
                        daughters = [lnode,rnode],
                        score = score(lnode)*score(rnode),
                        source = "",
                        sig = sig(lnode) ++ sig(rnode)
                        }:prevlist
backwardFunctionComposition1Rule _ _ prevlist = prevlist

-- | Forward function composition rule 2.
forwardFunctionComposition2Rule :: Node -> Node -> [Node] -> [Node]
forwardFunctionComposition2Rule lnode@(Node {rs=r,cat=(x `SL` y1), sem=f}) rnode@(Node {cat=(y2 `SL` z1) `SL` z2, sem=g}) prevlist =
  -- [>B2] x/y1:f  y2/z1/z2:g  ==> x/z1/z2
  if r == FFC1 || r == FFC2 || r == FFC3 || (isTNoncaseNP y1) -- Non-normal forms
  then prevlist
  else     
    let inc = maximumIndexC (cat rnode) in
    case unifyCategory [] [] [] (incrementIndexC y1 inc) y2 of
      Nothing -> prevlist -- Unification failure
      Just (_,csub,fsub) -> 
        let z1' = simulSubstituteCV csub fsub z1 in
        if numberOfArguments z1' > 2  -- Ad-hoc rule 2
        then prevlist
        else let newcat = simulSubstituteCV csub fsub (((incrementIndexC x inc) `SL` z1') `SL` z2) in
                      Node {
                        rs = FFC2,
                        pf = pf(lnode) `T.append` pf(rnode),
                        cat = newcat,
                        sem = betaReduce $ transvec newcat $ betaReduce $ Lam (Lam (App f (App (App g (Var 1)) (Var 0)))),
                        daughters = [lnode,rnode],
                        score = score(lnode)*score(rnode),
                        source = "",
                        sig = sig(lnode) ++ sig(rnode)
                        }:prevlist
forwardFunctionComposition2Rule _ _ prevlist = prevlist

-- | Backward function composition rule 2.
backwardFunctionComposition2Rule :: Node -> Node -> [Node] -> [Node]
backwardFunctionComposition2Rule lnode@(Node {cat=(y1 `BS` z1) `BS` z2, sem=g}) rnode@(Node {rs=r,cat=(x `BS` y2), sem=f}) prevlist =
  -- [<B2] y1\z1\z2  x\y2  ==> x\z1\z2
  if r == BFC1 || r ==BFC2 || r == BFC3 -- Non-normal forms
  then prevlist
  else
    let inc = maximumIndexC (cat lnode) in
    case unifyCategory [] [] [] (incrementIndexC y2 inc) y1 of
      Nothing -> prevlist -- Unification failure
      Just (_,csub,fsub) -> let newcat = simulSubstituteCV csub fsub (((incrementIndexC x inc) `BS` z1) `BS` z2) in
                      Node {
                        rs = BFC2,
                        pf = pf(lnode) `T.append` pf(rnode),
                        cat = newcat,
                        sem = betaReduce $ transvec newcat $ betaReduce $ Lam (Lam (App f (App (App g (Var 1)) (Var 0)))),
                        daughters = [lnode,rnode],
                        score = score(lnode)*score(rnode),
                        source = "",
                        sig = sig(lnode) ++ sig(rnode)
                        }:prevlist
backwardFunctionComposition2Rule _ _ prevlist = prevlist

-- | Backward function composition rule 3.
backwardFunctionComposition3Rule :: Node -> Node -> [Node] -> [Node]
backwardFunctionComposition3Rule lnode@(Node {cat=((y1 `BS` z1) `BS` z2) `BS` z3, sem=g}) rnode@(Node {rs=r,cat=(x `BS` y2), sem=f}) prevlist =
  -- [<B3] y1\z1\z2\z3  x\y2  ==> x\z1\z2\z3
  if r == BFC1 || r ==BFC2 || r == BFC3 -- Non-normal forms
  then prevlist
  else  
    let inc = maximumIndexC (cat lnode) in
    case unifyCategory [] [] [] (incrementIndexC y2 inc) y1 of
      Nothing -> prevlist -- Unification failure
      Just (_,csub,fsub) -> let newcat = simulSubstituteCV csub fsub ((((incrementIndexC x inc) `BS` z1) `BS` z2) `BS` z3) in
                      Node {
                        rs = BFC3,
                        pf = pf(lnode) `T.append` pf(rnode),
                        cat = newcat,
                        sem = betaReduce $ transvec newcat $ betaReduce $ Lam (Lam (Lam (App f (App (App (App g (Var 2)) (Var 1)) (Var 0))))),
                        daughters = [lnode,rnode],
                        score = score(lnode)*score(rnode),
                        source = "",
                        sig = sig(lnode) ++ sig(rnode)
                        }:prevlist
backwardFunctionComposition3Rule _ _ prevlist = prevlist

-- | Forward function crossed composition rule.
forwardFunctionCrossedComposition1Rule :: Node -> Node -> [Node] -> [Node]
forwardFunctionCrossedComposition1Rule lnode@(Node {rs=r,cat=SL x y1, sem=f}) rnode@(Node {cat=BS y2 z, sem=g}) prevlist =
  -- [>Bx] x/y1  y2\z  ==> x\z
  if r == FFC1 || r == FFC2 || r == FFC3 || (isTNoncaseNP y1) || not (isArgumentCategory z) -- Non-normal forms (+ Add-hoc rule 1)
  then prevlist
  else 
    let inc = maximumIndexC (cat rnode) in
    case unifyCategory [] [] [] y2 (incrementIndexC y1 inc) of
      Nothing -> prevlist -- Unification failure
      Just (_,csub,fsub) -> 
        let z' = simulSubstituteCV csub fsub z in
        --if numberOfArguments z' > 3  -- Ad-hoc rule 2
        --then prevlist
        --else 
        let newcat = (simulSubstituteCV csub fsub (incrementIndexC x inc)) `BS` z' in
          Node {
            rs = FFCx1,
            pf = pf(lnode) `T.append` pf(rnode),
            cat = newcat,
            sem = betaReduce $ transvec newcat $ betaReduce $ (Lam (App f (App g (Var 0)))),
            daughters = [lnode,rnode],
            score = score(lnode)*score(rnode)*(100 % 100), -- degrade the score when this rule is used.
            source = "",
            sig = sig(lnode) ++ sig(rnode)
            }:prevlist
forwardFunctionCrossedComposition1Rule _ _ prevlist = prevlist

-- | Forward function crossed composition rule 2.
forwardFunctionCrossedComposition2Rule :: Node -> Node -> [Node] -> [Node]
forwardFunctionCrossedComposition2Rule lnode@(Node {rs=r,cat=(x `SL` y1), sem=f}) rnode@(Node {cat=(y2 `BS` z1) `BS` z2, sem=g}) prevlist =
  -- [>Bx2] x/y1:f  y2\z1\z2:g  ==> x\z1\z2
  if r == FFC1 || r == FFC2 || r == FFC3 || r == EC || (isTNoncaseNP y1) || not (isArgumentCategory z2) || not (isArgumentCategory z1) -- Non-normal forms + Ad-hoc rule
  then prevlist
  else
    let inc = maximumIndexC (cat rnode) in
    case unifyCategory [] [] [] (incrementIndexC y1 inc) y2 of
      Nothing -> prevlist -- Unification failure
      Just (_,csub,fsub) ->
        let z1' = simulSubstituteCV csub fsub z1 in
        if numberOfArguments z1' > 2  -- Ad-hoc rule 2
        then prevlist
        else let newcat = simulSubstituteCV csub fsub (((incrementIndexC x inc) `BS` z1') `BS` z2) in
                      Node {
                        rs = FFCx2,
                        pf = pf(lnode) `T.append` pf(rnode),
                        cat = newcat,
                        sem = betaReduce $ transvec newcat $ betaReduce $ Lam (Lam (App f (App (App g (Var 1)) (Var 0)))),
                        daughters = [lnode,rnode],
                        score = score(lnode)*score(rnode)*(100 % 100), -- degrade the score more when this rule is used.
                        source = "",
                        sig = sig(lnode) ++ sig(rnode)
                        }:prevlist
forwardFunctionCrossedComposition2Rule _ _ prevlist = prevlist

-- | Forward functional crossed substitution rule
forwardFunctionCrossedSubstitutionRule :: Node -> Node -> [Node] -> [Node]
forwardFunctionCrossedSubstitutionRule lnode@(Node {rs=_,cat=((x `SL` y1) `BS` z1), sem=f}) rnode@(Node {cat=(y2 `BS` z2), sem=g}) prevlist =
  -- [>Sx] x/y1\z:f  y2\z:g  ==> x\z: \x.(fx)(gx)
  if not (isArgumentCategory z1) || not (isArgumentCategory z2) -- isNoncaseNP z1 -- to block CM + be-ident -- r == FFC1 || r == FFC2 || r == FFC3 || (isTNoncaseNP y1) || not (isArgumentCategory z2) || not (isArgumentCategory z1) -- Non-normal forms + Ad-hoc rule
  then prevlist
  else
    let inc = maximumIndexC (cat rnode) in
    case unifyCategory [] [] [] (incrementIndexC z1 inc) z2 of
      Nothing -> prevlist -- Unification failure
      Just (z,csub1,fsub1) ->
        case unifyCategory csub1 fsub1 [] (incrementIndexC y1 inc) y2 of
          Nothing -> prevlist -- Unification failure
          Just (_,csub2,fsub2) ->
            let newcat = simulSubstituteCV csub2 fsub2 ((incrementIndexC x inc) `BS` z) in
                      Node {
                        rs = FFSx,
                        pf = pf(lnode) `T.append` pf(rnode),
                        cat = newcat,
                        sem = betaReduce $ transvec newcat $ betaReduce $ Lam (App (App f (Var 0)) (App g (Var 0))),
                        daughters = [lnode,rnode],
                        score = score(lnode)*score(rnode)*(100 % 100),
                        source = "",
                        sig = sig(lnode) ++ sig(rnode)
                        }:prevlist
forwardFunctionCrossedSubstitutionRule _ _ prevlist = prevlist

-- | Forward functional crossed substitution rule Lv.2
forwardFunctionCrossedSubstitution2Rule :: Node -> Node -> [Node] -> [Node]
forwardFunctionCrossedSubstitution2Rule lnode@(Node {rs=_,cat=(((x `SL` y1) `BS` z1) `BS` w1), sem=f}) rnode@(Node {cat=((y2 `BS` z2) `BS` w2), sem=g}) prevlist =
  -- [>Sx] x/y1\z:f  y2\z:g  ==> x\z: \z.(fz)(gz)
  -- [>Sx2] x/y1\z1\w1:f  y2\z2\w2:g  ==> x\z\w: \w.\z.(fwz)(gwz)
  if not (isArgumentCategory z1) || not (isArgumentCategory z2) -- isNoncaseNP z1 -- to block CM + be-ident -- r == FFC1 || r == FFC2 || r == FFC3 || (isTNoncaseNP y1) || not (isArgumentCategory z2) || not (isArgumentCategory z1) -- Non-normal forms + Ad-hoc rule
  then prevlist
  else
    let inc = maximumIndexC (cat rnode) in
    case unifyCategory [] [] [] (incrementIndexC w1 inc) w2 of
      Nothing -> prevlist -- Unification failure
      Just (w,csub1,fsub1) ->
        case unifyCategory csub1 fsub1 [] (incrementIndexC z1 inc) z2 of
          Nothing -> prevlist -- Unification failure
          Just (z,csub2,fsub2) ->
            case unifyCategory csub2 fsub2 [] (incrementIndexC y1 inc) y2 of
              Nothing -> prevlist -- Unification failure
              Just (_,csub3,fsub3) ->
                let newcat = simulSubstituteCV csub3 fsub3 (((incrementIndexC x inc) `BS` z) `BS` w) in
                      Node {
                        rs = FFSx2,
                        pf = pf(lnode) `T.append` pf(rnode),
                        cat = newcat,
                        sem = betaReduce $ transvec newcat $ betaReduce $ Lam (Lam (App (App (App f (Var 1)) (Var 0)) (App (App g (Var 1)) (Var 0)))),
                        daughters = [lnode,rnode],
                        score = score(lnode)*score(rnode)*(100 % 100),
                        source = "",
                        sig = sig(lnode) ++ sig(rnode)
                        }:prevlist
forwardFunctionCrossedSubstitution2Rule _ _ prevlist = prevlist

-- | Punct rule.
punctRule :: Node -> Node -> [Node] -> [Node]
punctRule lnode@(Node {rs=r, cat=x, sem=f}) rnode@(Node {cat=PUNCT}) prevlist =
  if isBunsetsu x 
    then 
      Node {
        rs = PNCT,
        pf = pf(lnode) `T.append` pf(rnode),
        cat = x,
        sem = f,
        daughters = [lnode,rnode],
        score = score(lnode),
        source = "", 
        sig = sig lnode
        }:prevlist
    else prevlist
punctRule _ _ prevlist = prevlist

-- | A test to check if a given category is the one that can appear on the left adjacent of a punctuation.
isBunsetsu :: Cat -> Bool
isBunsetsu c = case c of
  SL x _ -> isBunsetsu x
  BS x _ -> isBunsetsu x
  LPAREN -> False
  S (_:(f:_)) -> let katsuyo = case f of 
                                 F feat -> feat
                                 SF _ feat -> feat in
                 if null $ L.intersect katsuyo [Cont,Term,Attr,Hyp,Imper,Pre,NTerm,NStem,TeForm,NiForm]
                    then False
                    else True
  N -> False
  _ -> True

-- | Period rule.
periodRule :: Node -> Node -> [Node] -> [Node]
periodRule lnode@(Node {rs=r, cat=(S s1), sem=f}) rnode@(Node {cat=PERIOD}) prevlist =
  Node {
    rs = PRD,
    pf = pf(lnode) `T.append` pf(rnode),
    cat = S s1,
    sem = f,
    daughters = [lnode,rnode],
    score = score(lnode),
    source = "", 
    sig = sig lnode
    }:prevlist
periodRule _ _ prevlist = prevlist

-- | Coordination rule.
coordinationRule :: Node -> Node -> Node -> [Node] -> [Node]
coordinationRule lnode@(Node {rs=r, cat=x1, sem=s1}) cnode@(Node {cat=CONJ, sem=conj}) rnode@(Node {cat=x2, sem=s2}) prevlist =
  -- [<Phi>] x1:f1  CONJ  x2:f2  ==>  x:\lambda\vec{x} (conj f1\vec{x}) f2\vec{x}
  if r == COORD
  then prevlist
  else
    if (endsWithT x2 || isNStem x2) && x1 == x2
    --if (endsWithT x2 || isNStem x2 || x2 == N) && x1 == x2
       then Node {
              rs = COORD,
              pf = T.concat [pf(lnode),pf(cnode),pf(rnode)],
              cat = x2,
              sem = betaReduce $ transvec x2 $ betaReduce $ Lamvec (App (App conj (Appvec 0 s1)) (Appvec 0 s2)),
              --sem = betaReduce $ transvec x2 $ betaReduce $ Lamvec (Lam (App (App conj (App (Appvec 1 s1) (Var 0))) (App (Appvec 1 s2) (Var 0)))),
              daughters = [lnode,cnode,rnode],
              score = score(lnode)*score(rnode),
              source = "",
              sig = sig(lnode) ++ sig(rnode)
              }:prevlist
       else prevlist
coordinationRule _ _ _ prevlist = prevlist

-- | Parenthesis rule.
parenthesisRule :: Node -> Node -> Node -> [Node] -> [Node]
parenthesisRule lnode@(Node {cat=LPAREN}) cnode rnode@(Node {cat=RPAREN}) prevlist =
  Node {
    rs = PAREN,
    pf = T.concat [pf(lnode),pf(cnode),pf(rnode)],
    cat = cat(cnode),
    sem = sem(cnode),
    daughters = [lnode,cnode,rnode],
    score = score(cnode),
    source = "",
    sig = sig cnode
    }:prevlist
parenthesisRule _ _ _ prevlist = prevlist

{- Variable-length Lambda Calculus -}

-- | Lamvec, Appvec: 
-- "transvec" function transforms the first argument (of type UDTTpreterm)
-- into the one without 
transvec :: Cat -> UDTTpreterm -> UDTTpreterm
transvec c preterm = case c of
  SL x _ -> case preterm of 
              Lam m    -> Lam (transvec x m)
              Lamvec m -> Lam (transvec x (Lamvec (addLambda 0 m)))
              m        -> m -- Var, Con, App, Proj, Asp, Appvec
                     -- Error: Type, Kind, Pi, Not, Sigma, Pair, Unit, Top, bot
  BS x _ -> case preterm of 
              Lam m    -> Lam (transvec x m)
              Lamvec m -> Lam (transvec x (Lamvec (addLambda 0 m)))
              m        -> m -- Var, Con, App, Proj, Asp, Appvec
                     -- Error: Type, Kind, Pi, Not, Sigma, Pair, Unit, Top, bot
  NP _   -> case preterm of
              Lamvec m -> deleteLambda 0 m
              m        -> m
  S _ -> case preterm of
               Lam (Lamvec m) -> Lam (deleteLambda 0 m)
               Lamvec (Lam m) -> deleteLambda 0 (Lam m)
               Lamvec m -> Lam (replaceLambda 0 m)
               m        -> m
  Sbar _ -> case preterm of
               Lam (Lamvec m) -> Lam (deleteLambda 0 m)
               Lamvec (Lam m) -> deleteLambda 0 (Lam m)
               Lamvec m -> Lam (replaceLambda 0 m)
               m        -> m
  N -> case preterm of
              Lam (Lam (Lamvec m)) -> Lam (Lam (deleteLambda 0 m))
              Lam (Lamvec (Lam m)) -> Lam (deleteLambda 0 (Lam m))
              Lamvec (Lam (Lam m)) -> deleteLambda 0 (Lam (Lam m))
              Lamvec (Lam m) -> Lam (replaceLambda 0 (Lam m))
              Lam (Lamvec m) -> Lam (Lam (replaceLambda 0 m))
              Lamvec m -> Lam (Lam (replaceLambda 0 (addLambda 0 m)))
              m        -> m
  _ -> preterm

{- Implementation of CCG Unification -}

-- | returns the number of arguments of a given syntactic category.  
-- For a category variable, `numberOfArguments` simply returns 0.
numberOfArguments :: Cat -> Int
numberOfArguments c = case c of
  SL c1 _ -> 1 + numberOfArguments c1
  BS c1 _ -> 1 + numberOfArguments c1
  _ -> 0

-- | returns a maximum index of category variables contained in a given category.
--
-- >>> maximumIndex T(1)/T(3) == 3
maximumIndexC :: Cat -> Int
maximumIndexC c = case c of
  T _ i c2 -> max i (maximumIndexC c2) 
  SL c1 c2 -> max (maximumIndexC c1) (maximumIndexC c2)
  BS c1 c2 -> max (maximumIndexC c1) (maximumIndexC c2)
  S f -> maximumIndexF f
  NP f -> maximumIndexF f
  Sbar f -> maximumIndexF f
  _ -> 0

maximumIndexF :: [Feature] -> Int
maximumIndexF fs = case fs of
  [] -> 0
  ((SF i _):fs2) -> max i (maximumIndexF fs2)
  (_:fs2) -> maximumIndexF fs2

-- | returns 
incrementIndexC :: Cat -> Int -> Cat
incrementIndexC c i = case c of
  T f j u -> T f (i+j) (incrementIndexC u i)
  SL c1 c2 -> SL (incrementIndexC c1 i) (incrementIndexC c2 i)
  BS c1 c2 -> BS (incrementIndexC c1 i) (incrementIndexC c2 i)
  S f -> S (incrementIndexF f i)
  Sbar f -> Sbar (incrementIndexF f i)
  NP f -> NP (incrementIndexF f i)
  cc -> cc

incrementIndexF :: [Feature] -> Int -> [Feature]
incrementIndexF fs i = case fs of
  [] -> []
  ((SF j f2):fs2) -> (SF (i+j) f2):(incrementIndexF fs2 i)
  (fh:ft) -> fh:(incrementIndexF ft i)

-- | Data for category/feature unification
-- csub :: SubstData Cat
-- fsub :: SubstData [FeatureValue]
data SubstData a = SubstLink Int | SubstVal a deriving (Show, Eq)
type Assignment a = [(Int,SubstData a)]

-- | takes a key 'i' and a value 'v', an assignment function, and returns
-- its 'i'-variant (that maps 'i' to 'v').
alter :: (Ord a, Eq a) => a -> b -> [(a,b)] -> [(a,b)]
alter i v mp = (i,v):(filter (\(j,_) -> i /= j) mp)

-- | takes an assignment, an integer 'i' (as a key) and a value 'v',
-- returns a pair of 'i' and its value if it exists,
-- or a pair of 'i' and 'v' if it does not exists or an illegal link is found in the assignment.
fetchValue :: Assignment a -> Int -> a -> (Int, a)
fetchValue sub i v =
  case L.lookup i sub of
    Just (SubstLink j) | j < i -> fetchValue sub j v
    Just (SubstVal v') -> (i,v')
    _ -> (i,v)

-- | substituteCateogoryVariable 
--
-- >>> T1 [1->X/Y] ==> X/Y
simulSubstituteCV :: Assignment Cat -> Assignment [FeatureValue] -> Cat -> Cat
simulSubstituteCV csub fsub c = case c of
    T _ i _ -> snd $ fetchValue csub i c
    SL ca cb -> SL (simulSubstituteCV csub fsub ca) (simulSubstituteCV csub fsub cb)
    BS ca cb -> BS (simulSubstituteCV csub fsub ca) (simulSubstituteCV csub fsub cb)
    S f -> S (simulSubstituteFV fsub f)
    Sbar f -> Sbar (simulSubstituteFV fsub f)
    NP f -> NP (simulSubstituteFV fsub f)
    _ -> c

-- | unifies two syntactic categories (`Cat`) and returns a unified syntactic category, under a given category assignment and a given feature assignment.
unifyCategory :: Assignment Cat               -- ^ A category assignment function
                 -> Assignment [FeatureValue] -- ^ A feature assignment function
                 -> [Int] -- ^ A list of banned indices (unification to which is banned, preventing cyclic unification)
                 -> Cat   -- ^ A first argument
                 -> Cat   -- ^ A second argument
                 -> Maybe (Cat, Assignment Cat, Assignment [FeatureValue])
unifyCategory csub fsub banned c1 c2 =
  let c1' = case c1 of
              T _ i _ -> snd $ fetchValue csub i c1
              _ -> c1 in
  let c2' = case c2 of
              T _ j _ -> snd $ fetchValue csub j c2
              _ -> c2 in
  unifyCategory2 csub fsub banned c1' c2'

unifyCategory2 :: Assignment Cat -> Assignment [FeatureValue] -> [Int] -> Cat -> Cat -> Maybe (Cat, Assignment Cat, Assignment [FeatureValue])
unifyCategory2 csub fsub banned c1 c2 = case (c1,c2) of
  (T f1 i u1, T f2 j u2) ->
    if i `elem` banned || j `elem` banned
    then Nothing
    else
      if i == j
         then Just (c1,csub,fsub)
         else do
              let ijmax = max i j; ijmin = min i j
              (u3,csub2,fsub2) <- case (f1,f2) of
                                    (True,True) -> unifyCategory2 csub fsub (ijmin:banned) u1 u2
                                    (True,False) -> unifyWithHead csub fsub (ijmin:banned) u1 u2
                                    (False,True) -> unifyWithHead csub fsub (ijmin:banned) u2 u1
                                    (False,False) -> unifyCategory2 csub fsub (ijmin:banned) u1 u2
              let result = T (f1 && f2) ijmin u3
              Just (result, alter ijmin (SubstVal result) (alter ijmax (SubstLink ijmin) csub2), fsub2)
  (T f i u, c) -> if i `elem` banned
                     then Nothing
                     else
                       do
                       (c3,csub2,fsub2) <- case f of
                                             True -> unifyWithHead csub fsub (i:banned) u c
                                             False -> unifyCategory csub fsub (i:banned) u c
                       Just (c3, alter i (SubstVal c3) csub2, fsub2)
  (c, T f i u) -> if i `elem` banned
                     then Nothing
                     else
                       do
                       (c3,csub2,fsub2) <- case f of
                                             True -> unifyWithHead csub fsub (i:banned) u c
                                             False -> unifyCategory csub fsub (i:banned) u c
                       Just (c3, alter i (SubstVal c3) csub2, fsub2)
  (NP f1, NP f2) -> do
                    (f3,fsub2) <- unifyFeatures fsub f1 f2
                    return ((NP f3), csub, fsub2)
  (S f1, S f2) -> do
                  (f3,fsub2) <- unifyFeatures fsub f1 f2
                  return ((S f3), csub, fsub2)
  (Sbar f1, Sbar f2) -> do
                        (f3,fsub2) <- unifyFeatures fsub f1 f2
                        return ((Sbar f3), csub, fsub2)
  (SL c3 c4, SL c5 c6) -> do
                          (c7,csub2,fsub2) <- unifyCategory csub fsub banned c4 c6
                          (c8,csub3,fsub3) <- unifyCategory csub2 fsub2 banned c3 c5
                          return (SL c8 c7,csub3,fsub3)
  (BS c3 c4, BS c5 c6) -> do
                          (c7,csub2,fsub2) <- unifyCategory csub fsub banned c4 c6
                          (c8,csub3,fsub3) <- unifyCategory csub2 fsub2 banned c3 c5
                          return (BS c8 c7, csub3, fsub3)
  (N, N)           -> Just (N, csub, fsub)
  (CONJ, CONJ)     -> Just (CONJ, csub, fsub)
  (LPAREN, LPAREN) -> Just (LPAREN, csub, fsub)
  (RPAREN, RPAREN) -> Just (RPAREN, csub, fsub)
  _ -> Nothing

-- | unifies a cyntactic category `c1` (in `T True i c1`) with the head of `c2`, under a given feature assignment.
unifyWithHead :: Assignment Cat 
                 -> Assignment [FeatureValue] -- ^ A feature assignment function.
                 -> [Int] -- ^ A list of indices
                 -> Cat   -- ^ A first argument
                 -> Cat   -- ^ A second argument
                 -> Maybe (Cat, Assignment Cat, Assignment [FeatureValue])
unifyWithHead csub fsub banned c1 c2 = case c2 of
  SL x y -> do
            (x',csub2,fsub2) <- unifyWithHead csub fsub banned c1 x
            return $ (SL x' y, csub2, fsub2)
  BS x y -> do
            (x',csub2,fsub2) <- unifyWithHead csub fsub banned c1 x
            return $ (BS x' y, csub2, fsub2)
  T f i u -> if i `elem` banned
                then Nothing
                else
                   do
                   (x',csub2,fsub2) <- unifyCategory csub fsub (i:banned) c1 u
                   return $ (T f i x', alter i (SubstVal $ T f i x') csub2, fsub2)
  x -> unifyCategory csub fsub banned c1 x

-- | substituteFeatureVariable
--
-- >>> F 1 f [1->PM] ==> f[PM/1]
substituteFV :: Assignment [FeatureValue] -> Feature -> Feature
substituteFV fsub f1 = case f1 of
  SF i v -> let (j,v') = fetchValue fsub i v in SF j v'
  _ -> f1

simulSubstituteFV :: Assignment [FeatureValue] -> [Feature] -> [Feature]
simulSubstituteFV fsub = map (substituteFV fsub)

-- | unifyFeature
unifyFeature :: Assignment [FeatureValue] -> Feature -> Feature -> Maybe (Feature, Assignment [FeatureValue])
unifyFeature fsub f1 f2 = case (f1,f2) of
  (SF i v1, SF j v2) -> if i == j
                           then
                             let (i',v1') = fetchValue fsub i v1;
                                 v3 = L.intersect v1' v2 in
                             if null v3
                                then Nothing
                                else Just (SF i' v3, (alter i' (SubstVal v3) fsub))
                           else
                             let (i',v1') = fetchValue fsub i v1;
                                 (j',v2') = fetchValue fsub j v2;
                                 v3 = L.intersect v1' v2' in
                             if null v3
                                then Nothing
                                else
                                  let ijmax = max i' j'; ijmin = min i' j' in
                                  Just (SF ijmin v3, (alter ijmax (SubstLink ijmin) (alter ijmin (SubstVal v3) fsub)))
  (SF i v1, F v2) -> let (i',v1') = fetchValue fsub i v1;
                         v3 = L.intersect v1' v2 in
                     if null v3
                        then Nothing
                        else Just (SF i' v3, (alter i' (SubstVal v3) fsub))
  (F v1, SF j v2) -> let (j',v2') = fetchValue fsub j v2;
                         v3 = L.intersect v1 v2' in
                     if null v3
                        then Nothing
                        else Just (SF j' v3, (alter j' (SubstVal v3) fsub))
  (F v1, F v2) -> let v3 = L.intersect v1 v2 in
                  if null v3
                     then Nothing
                     else Just (F v3, fsub)

-- |
unifyFeatures :: Assignment [FeatureValue] -> [Feature] -> [Feature] -> Maybe ([Feature], Assignment [FeatureValue])
unifyFeatures fsub f1 f2 = case (f1,f2) of
  ([],[]) -> Just ([],fsub)
  ((f1h:f1t),(f2h:f2t)) -> do
                           (f3h,fsub2) <- unifyFeature fsub f1h f2h
                           (f3t,fsub3) <- unifyFeatures fsub2 f1t f2t
                           return ((f3h:f3t), fsub3)
  _ -> Nothing

{- Functions for partial parsing -}

category2type :: Cat -> UDTTpreterm
category2type ct = case ct of
  SL x y -> Pi (category2type y) (category2type x)
  BS x y -> Pi (category2type y) (category2type x)
  S _ -> Pi (Pi Entity Type) Type
  NP _ -> Entity
  N   -> Pi Entity (Pi (Pi Entity Type) Type)
  Sbar _ -> Pi (Pi Entity Type) Type
  T _ _ c -> category2type c
  _ -> Unit

terminator :: UDTTpreterm
terminator = Ann (Lam Top) (DTTdB.Pi DTTdB.Entity DTTdB.Type)

preterm2prop :: Cat -> UDTTpreterm -> UDTTpreterm
preterm2prop ct preterm = case ct of
  SL x y -> Sigma (category2type y) (preterm2prop x (App (shiftIndices preterm 1 0) (Var 0)))
  BS x y -> Sigma (category2type y) (preterm2prop x (App (shiftIndices preterm 1 0) (Var 0)))
  S _ -> App preterm terminator
  NP _ -> Sigma (Pi Entity Type) (App (shiftIndices preterm 1 0) (Var 0))
  N -> Sigma Entity (App (App (shiftIndices preterm 1 0) (Var 0)) terminator)
  Sbar _ -> App preterm terminator
  T _ _ c -> preterm2prop c $ transvec c preterm
  _ -> Top

-- | receives a node and returns an Sbar node, whose SR is obtained by existentially quantifying all the missing arguments of the SR of a given node.
wrapNode :: Node -> Node
wrapNode node =
  Node {
    rs = WRAP,
    pf = pf node,
    cat = Sbar [F[Decl]],
    sem = strongBetaReduce 0 $ preterm2prop (cat node) (sem node),
          --takes a CCG node and returns a semantic reprentation of type Type, obtained by existentially quantifying empty slots.
    daughters = [node],
    score = (score node)*0.9,
    source = "",
    sig = sig node
    }

-- | receives a pair of Sbar nodes, and returns a node whose SR is their conjunction.  Note that this function does not check if given nodes are of Sbar.
conjoinNodes :: Node -> Node -> Node
conjoinNodes lnode rnode =
  Node {
    rs = DC,
    pf = pf(lnode) `T.append` pf(rnode),
    cat = Sbar [F[Decl]],
    sem = Sigma (sem lnode) (sem rnode),
    daughters = [lnode,rnode],
    score = score(lnode)*score(rnode),
    source = "",
    sig = sig(lnode) ++ sig(rnode)
    }

{-
-- | receives an Sbar node, and returns an Sbar node with DRel between a given node and some (anaphorically determined) preceding node.
drel :: Node -> Node
drel node =
  Node {
    rs = DREL,
    pf = pf node,
    cat = Sbar [F[Decl]],
    sem = Sigma (sem node) (DRel 0 "S" (Proj Snd $ Asp 1 (Sigma Type (Var 0))) (Var 0)),
    daughters = [node],
    score = (score node)*0.9,
    source = "",
    sig = sig node
    }
-}

