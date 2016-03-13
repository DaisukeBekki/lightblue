{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Combinatory Categorial Grammar
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Syntactic categories, syntactic features and combinatory rules of CCG.
-}
module CombinatoryCategorialGrammar (
  -- * Types
  Node(..),
  RuleSymbol(..),
  Cat(..),
  -- * Syntactic Features
  PosFeature(..),
  ConjFeature(..),
  PMFeature(..),
  CaseFeature(..),
  SbarFeature(..),
  -- * Classes
  Feature(..),
  SimpleText(..),
  -- * Tests
  isBaseCategory,
  --isCONJ,
  -- * Combinatory Rules
  unaryRules,
  binaryRules,
  -- trinaryRules  
  coordinationRule,
  parenthesisRule,
  -- * Macros for lexical items
  lexicalitem,
  -- * Macros for CCG syntactic features
  defS,
  verb,
  adjective,
  nomPred,
  nonStem,
  anySExStem,
  anyPos,
  -- * Templates for DTS representations
  id,
  verbSR,
  predSR,
  properNameSR,
  commonNounSR,
  intensionalVerb,
  modal,
  mannerAdverb,
  eventModifier,
  negOperator,
  argumentCM,
  adjunctCM
  ) where

import Prelude hiding (id)
import qualified Data.Text.Lazy as T --text
import qualified Data.List as L      --base
import qualified Data.Maybe as Maybe --base
import Data.Fixed                    --base
import Data.Ratio                    --base
import Control.Monad                 --base
--import Text.Printf -- for 'printf'
import DependentTypes

data Node = Node {
  rs :: RuleSymbol,    -- ^ The name of the rule
  pf :: T.Text,        -- ^ The phonetic form
  cat :: Cat,          -- ^ The syntactic category (in CCG)
  sem :: Preterm,      -- ^ The semantic representation (in DTS)
  daughters :: [Node], -- ^ The daughter nodes
  score :: Rational,   -- ^ The score (between 0.00 to 1.00, larger the better)
  memo :: T.Text       -- ^ The source of the lexical entry
  } deriving (Eq, Show)

instance Ord Node where
  (Node {score=i}) `compare` (Node {score=j})
    | i < j  = GT
    | i == j = EQ
    | i > j  = LT
  (Node _ _ _ _ _ _ _) `compare` (Node _ _ _ _ _ _ _) = EQ

instance SimpleText Node where
  toText n@(Node _ _ _ _ _ _ _) = toTextLoop "" n
    where toTextLoop indent node =
            case daughters node of 
              [] -> T.concat [(T.pack indent), toText (rs node), " ", pf node, " ", toText (cat node), " ", toTextWithVN [] (sem node), " ", memo node, " [", T.pack (show ((fromRational $ score node)::Fixed E2)), "]\n"]
              dtrs -> T.concat $ [(T.pack indent), toText (rs node), " ", toText (cat node), " ", toTextWithVN [] (sem node), " [", T.pack (show ((fromRational $ score node)::Fixed E2)), "]\n"] ++ (map (\d -> toTextLoop (indent++"  ") d) dtrs)

data Cat = 
  S [PosFeature] [ConjFeature] PMFeatures -- ^ S
  | NP [CaseFeature]   -- ^ NP
  | N                  -- ^ N
  | Sbar [SbarFeature] -- ^ S bar
  | CONJ               -- ^ CON
  | LPAREN             -- ^ A category for left parentheses
  | RPAREN             -- ^ A category for right parentheses
  | SL Cat Cat         -- ^ X/Y
  | BS Cat Cat         -- ^ X\\Y
  | T Bool Int Cat     -- ^ Category variables, where Int is an index, Cat is a restriction for its head. 

instance Eq Cat where
  S x1 x2 x3  == S y1 y2 y3 = (L.intersect x1 y1 /= [])
                              && (L.intersect x2 y2) /=[]
                              && (case unifyFeatures [] x3 y3 of
                                     Just _ -> True
                                     Nothing -> False)
  NP x == NP y = L.intersect x y /= []
  N == N = True
  Sbar x == Sbar y = L.intersect x y /= []
  CONJ == CONJ = True
  LPAREN == LPAREN = True
  RPAREN == RPAREN = True
  SL x1 x2 == SL y1 y2 = (x1 == y1) && (x2 == y2)
  BS x1 x2 == BS y1 y2 = (x1 == y1) && (x2 == y2)
  T f1 i x == T f2 j y = (f1 == f2) && (i == j) && (x == y)
  _ == _ = False

-- | `toText` method is invoked.
instance Show Cat where
  show = T.unpack . toText

data PosFeature = V5k | V5s | V5t | V5n | V5m | V5r | V5w | V5g | V5z | V5b |
              V5IKU | V5YUK | V5ARU | V5NAS | V5TOW |
              V1 | VK | VS | VZ | VURU |
              Aauo | Ai | ANAS | ATII | ABES |
              Nda | Nna | Nno | Ntar | Nni | Nemp | Nto |
              Exp | Error
              deriving (Eq)

data ConjFeature = Stem | UStem | Neg | Cont | Term | Attr | Hyp | Imper | Pre |
               NegL | TeForm | NiForm |
               EuphT | EuphD |
               ModU | ModD | ModS |
               VoR | VoS | VoE 
               deriving (Eq)

data PMFeature = P | M | PM | F Int PMFeature deriving (Eq, Show)

type PMFeatures = [PMFeature]

data CaseFeature = Nc | Ga | O | Ni | To | Niyotte | No deriving (Eq)

data SbarFeature = ToCL | YooniCL deriving (Eq)

instance Show PosFeature where
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
  show Error = "error"

instance Show ConjFeature where
  show Stem = "stem"
  show UStem = "ustem"
  show Neg = "neg"
  show Cont = "cont"
  show Term = "term"
  show Attr = "attr"
  show Hyp = "hyp"
  show Imper = "imp"
  show Pre = "pre"
  show NegL = "neg+l"
  show EuphT = "euph:t"
  show EuphD = "euph:d"
  show ModU = "mod:u"
  show ModD = "mod:d"
  show ModS = "mod:s"
  show VoR = "vo:r"
  show VoS = "vo:s"
  show VoE = "vo:e"
  show TeForm = "te"
  show NiForm = "ni"

instance Show SbarFeature where
  show ToCL = "to"
  show YooniCL = "yooni"

instance Show CaseFeature where
  show Nc = "nc"
  show Ga = "ga"
  show O = "o"
  show Ni = "ni"
  show To = "to"
  show Niyotte = "niyotte"
  show No = "no"

class (Show a) => Feature a where
  printF :: [a] -> T.Text
  printF [] = T.empty
  printF [pos] = T.pack $ show pos
  printF [pos1,pos2] = T.pack $ (show pos1) ++ "|" ++ (show pos2)
  printF (pos1:(pos2:_)) = T.pack $ (show pos1) ++ "|" ++ (show pos2) ++ "|+"

instance Feature PosFeature
instance Feature ConjFeature
instance Feature CaseFeature
instance Feature SbarFeature

instance SimpleText Cat where
  toText category = case category of
    S pos conj pm -> T.concat [
                       "S[",
                       printF pos,
                       "][",
                       printF conj,
                       "][",
                       pmFeatures2Text pm,
                       "]"
                       ]
    NP cas      -> T.concat ["NP[", printF cas, "]"]
    Sbar x      -> T.concat ["Sbar[", printF x, "]"]
    N           -> "N"
    CONJ        -> "CONJ"
    LPAREN      -> "LPAREN"
    RPAREN      -> "RPAREN"
    SL x y      -> T.concat [toText x, "/", toText' y]
    BS x y      -> T.concat [toText x, "\\", toText' y]
    T True i _     -> T.concat ["T", (T.pack $ show i)]
    T False i c     -> T.concat [toText c, "[", (T.pack $ show i), "]"]
    where -- A bracketed version of `toText'` function
    toText' c = if isBaseCategory c
                  then toText c
                  else T.concat ["(", toText c, ")"]

pmFeature2Text :: Bool -> T.Text -> PMFeature -> Maybe T.Text
pmFeature2Text _ label pmf = case (label,pmf) of
    (l,P)     -> Just $ T.concat ["+", l]
    (_,M)     -> Nothing -- if shared then Just $ T.concat ["-", l] else Nothing
    (l,PM)    -> Just $ T.concat ["±", l]
    (l,F i f) -> do
                 x <- pmFeature2Text True l f
                 return $ T.concat [x,"<",T.pack (show i),">"]

pmFeatures2Text :: [PMFeature] -> T.Text
pmFeatures2Text pmfs = T.intercalate "," $ Maybe.catMaybes $ pmFeatures2TextLoop ["t","p","n","N","T"] pmfs

pmFeatures2TextLoop :: [T.Text] -> [PMFeature] -> [Maybe T.Text]
pmFeatures2TextLoop labels pmfs = case (labels,pmfs) of
  ([],[])         -> []
  ((l:ls),(p:ps)) -> (pmFeature2Text False l p):(pmFeatures2TextLoop ls ps)
  _ -> [Just $ T.concat ["Error: mismatch in ", T.pack (show labels), " and ", T.pack (show pmfs)]]

-- | A test to check if a given category is a base category (i.e. not a functional category nor a category variable).
isBaseCategory :: Cat -> Bool
isBaseCategory c = case c of
  S _ _ _ -> True
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
  NP f -> case f of
            [Nc] -> False
            _ -> True
  -- N -> False
  Sbar _ -> True
  _ -> False

--isCONJ :: Cat -> Bool
--isCONJ c = c == CONJ

-- | A test to check if a given category is T\NPnc.
isNoncaseNP :: Cat -> Bool
isNoncaseNP c = case c of
  (T _ _ _) `BS` (NP cas) -> if cas == [Nc] then True else False
  _ -> False

-- | The name of the CCG rule to derive the node.
data RuleSymbol = 
  LEX    -- ^ A lexical item
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
  | COORD -- ^ Coordination rule
  | PAREN -- ^ Parenthesis rule
  deriving (Eq, Show)

-- | The simple-text representation of the rule symbols.
instance SimpleText RuleSymbol where
  toText rulesymbol = case rulesymbol of 
    LEX -> "LEX"
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
    COORD -> "<Phi>"
    PAREN -> "PAREN"
    -- CNP -> "CNP"

{- Classes of Combinatory Rules -}

-- | The function to apply all the unaryRules to a CCG node.
unaryRules :: Node -> [Node] -> [Node]
unaryRules _ prevlist = prevlist
--unaryRules = sseriesRule

{-
sseriesRule :: Node -> [Node] -> [Node]
sseriesRule node@(Node {rs=LEX, cat=((S [VS] [Stem] `BS` NP [Ga]) `BS` NP [O]), sem=f}) prevlist =
  Node {
    rs = LEX,
    pf = pf(node),
    cat = (((T 1 anySExStem `SL` (T 1 anySExStem `BS` NP [Nc])) `BS` NP [No]) `BS` NP [No]),
    sem = (Lam (Lam (Lam (Lamvec (Sigma (App (App f (Var 3)) (Var 2)) (Appvec 1 (App (Var 2) (Var 0)))))))),
    daughters = [node],
    score = score(node),
    memo = ""
    }: prevlist
sseriesRule _ prevlist = prevlist
-}

-- | The function to apply all the binary rules to a given pair of CCG nodes.
binaryRules :: Node -> Node -> [Node] -> [Node]
binaryRules lnode rnode = 
  --compoundNPRule lnode rnode
    forwardFunctionCrossedComposition2Rule lnode rnode
  . forwardFunctionCrossedComposition1Rule lnode rnode
  . backwardFunctionComposition3Rule lnode rnode
  . backwardFunctionComposition2Rule lnode rnode
  . forwardFunctionComposition2Rule lnode rnode
  . backwardFunctionComposition1Rule lnode rnode
  . forwardFunctionComposition1Rule lnode rnode
  . backwardFunctionApplicationRule lnode rnode
  . forwardFunctionApplicationRule lnode rnode

{-
-- | The function to apply all the trinary rules to a given triple of CCG nodes.
trinaryRules :: Node -> Node -> Node -> [Node] -> [Node]
trinaryRules lnode cnode rnode =
  parenthesisRule lnode cnode rnode
  . coordinationRule lnode cnode rnode
-}

{- Combinatory Rules -}

{-
x :: Cat
x = T True 1 (S anyPos anyConj [M,M,M,M,M])
y1 :: Cat
y1 =  T True 1 (S anyPos anyConj [M,M,M,M,M]) `BS` NP [Nc]
y2 :: Cat
y2 = (T False 1 (S anyPos anyConj [(F 1 PM),(F 2 PM),(F 3 PM),M,M]) `SL` T False 1 (S anyPos anyConj [(F 1 PM),(F 2 PM),(F 3 PM),M,M])) `BS` NP [Nc]
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
           case unifyCategory [] [] y2 (incrementIndexC y1 inc) of
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
                   memo = "" --T.concat $ map (\(i,c)-> T.concat [T.pack (show i)," \\mapsto ",toTeX c,", "]) sub
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
    case unifyCategory [] [] y1 (incrementIndexC y2 inc) of
      Nothing -> prevlist -- Unification failure
      Just (_,csub,fsub) -> let newcat = simulSubstituteCV csub fsub (incrementIndexC x inc) in
                      Node {
                        rs = BFA,
                        pf = pf(lnode) `T.append` pf(rnode),
                        cat = newcat,
                        sem = betaReduce $ transvec newcat $ betaReduce $ App f a,
                        daughters = [lnode,rnode],
                        score = score(lnode)*score(rnode),
                        memo = "" -- pf(lnode) `T.append` pf(rnode)
                        }:prevlist
backwardFunctionApplicationRule _ _ prevlist = prevlist

-- | Forward function composition rule.
forwardFunctionComposition1Rule :: Node -> Node -> [Node] -> [Node]
forwardFunctionComposition1Rule lnode@(Node {rs=r,cat=SL x y1, sem=f}) rnode@(Node {cat=SL y2 z, sem=g}) prevlist =
  -- [>B] x/y1  y2/z  ==> x/z
  if r == FFC1 || r == FFC2 || r == FFC3 || (isNoncaseNP y1) -- Non-normal forms (+ Ad-hoc rule 1)
  then prevlist
  else  
    let inc = maximumIndexC (cat rnode) in
    case unifyCategory [] [] y2 (incrementIndexC y1 inc) of
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
               memo = ""
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
    case unifyCategory [] [] y1 (incrementIndexC y2 inc) of
      Nothing -> prevlist -- Unification failure
      Just (_,csub,fsub) -> let newcat = simulSubstituteCV csub fsub ((incrementIndexC x inc) `BS` z) in
                      Node {
                        rs = BFC1,
                        pf = pf(lnode) `T.append` pf(rnode),
                        cat = newcat,
                        sem = betaReduce $ transvec newcat $ betaReduce $ Lam (App f (App g (Var 0))),
                        daughters = [lnode,rnode],
                        score = score(lnode)*score(rnode),
                        memo = ""
                        }:prevlist
backwardFunctionComposition1Rule _ _ prevlist = prevlist

-- | Forward function composition rule 2.
forwardFunctionComposition2Rule :: Node -> Node -> [Node] -> [Node]
forwardFunctionComposition2Rule lnode@(Node {rs=r,cat=(x `SL` y1), sem=f}) rnode@(Node {cat=(y2 `SL` z1) `SL` z2, sem=g}) prevlist =
  -- [>B2] x/y1:f  y2/z1/z2:g  ==> x/z1/z2
  if r == FFC1 || r == FFC2 || r == FFC3 || (isNoncaseNP y1) -- Non-normal forms
  then prevlist
  else     
    let inc = maximumIndexC (cat rnode) in
    case unifyCategory [] [] (incrementIndexC y1 inc) y2 of
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
                        memo = ""
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
    case unifyCategory [] [] (incrementIndexC y2 inc) y1 of
      Nothing -> prevlist -- Unification failure
      Just (_,csub,fsub) -> let newcat = simulSubstituteCV csub fsub (((incrementIndexC x inc) `BS` z1) `BS` z2) in
                      Node {
                        rs = BFC2,
                        pf = pf(lnode) `T.append` pf(rnode),
                        cat = newcat,
                        sem = betaReduce $ transvec newcat $ betaReduce $ Lam (Lam (App f (App (App g (Var 1)) (Var 0)))),
                        daughters = [lnode,rnode],
                        score = score(lnode)*score(rnode),
                        memo = ""
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
    case unifyCategory [] [] (incrementIndexC y2 inc) y1 of
      Nothing -> prevlist -- Unification failure
      Just (_,csub,fsub) -> let newcat = simulSubstituteCV csub fsub ((((incrementIndexC x inc) `BS` z1) `BS` z2) `BS` z3) in
                      Node {
                        rs = BFC3,
                        pf = pf(lnode) `T.append` pf(rnode),
                        cat = newcat,
                        sem = betaReduce $ transvec newcat $ betaReduce $ Lam (Lam (Lam (App f (App (App (App g (Var 2)) (Var 1)) (Var 0))))),
                        daughters = [lnode,rnode],
                        score = score(lnode)*score(rnode),
                        memo = ""
                        }:prevlist
backwardFunctionComposition3Rule _ _ prevlist = prevlist

-- | Forward function crossed composition rule.
forwardFunctionCrossedComposition1Rule :: Node -> Node -> [Node] -> [Node]
forwardFunctionCrossedComposition1Rule lnode@(Node {rs=r,cat=SL x y1, sem=f}) rnode@(Node {cat=BS y2 z, sem=g}) prevlist =
  -- [>Bx] x/y1  y2\z  ==> x\z
  if r == FFC1 || r == FFC2 || r == FFC3 || (isNoncaseNP y1) || not (isArgumentCategory z) -- Non-normal forms (+ Add-hoc rule 1)
  then prevlist
  else 
    let inc = maximumIndexC (cat rnode) in
    case unifyCategory [] [] y2 (incrementIndexC y1 inc) of
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
            memo = ""
            }:prevlist
forwardFunctionCrossedComposition1Rule _ _ prevlist = prevlist

-- | Forward function crossed composition rule 2.
forwardFunctionCrossedComposition2Rule :: Node -> Node -> [Node] -> [Node]
forwardFunctionCrossedComposition2Rule lnode@(Node {rs=r,cat=(x `SL` y1), sem=f}) rnode@(Node {cat=(y2 `BS` z1) `BS` z2, sem=g}) prevlist =
  -- [>Bx2] x/y1:f  y2\z1\z2:g  ==> x\z1\z2
  if r == FFC1 || r == FFC2 || r == FFC3 || (isNoncaseNP y1) || not (isArgumentCategory z2) || not (isArgumentCategory z1) -- Non-normal forms + Ad-hoc rule
  then prevlist
  else
    let inc = maximumIndexC (cat rnode) in
    case unifyCategory [] [] (incrementIndexC y1 inc) y2 of
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
                        score = score(lnode)*score(rnode)*(99 % 100), -- degrade the score more when this rule is used.
                        memo = ""
                        }:prevlist
forwardFunctionCrossedComposition2Rule _ _ prevlist = prevlist

-- | Coordination rule.
coordinationRule :: Node -> Node -> Node -> [Node] -> [Node]
coordinationRule lnode@(Node {cat=x1, sem=s1}) cnode@(Node {cat=c, sem=conj}) rnode@(Node {cat=x2, sem=s2}) prevlist =
  -- [<Phi>] x1:f1  CONJ  x2:f2  ==>  x:\lambda\vec{x} (conj f1\vec{x}) f2\vec{x}
  case (x1,c,x2) of
    ((T True i (S _ a2 a3) `SL` (T True j (S _ b2 b3) `BS` NP [Nc])),CONJ,(T True _ (S c1 c2 c3) `SL` (T True _ (S d1 d2 d3) `BS` NP [Nc]))) -> 
      let e2 = L.intersect a2 c2; f2 = L.intersect b2 d2 in
      -- let inc = max (maximumIndexC x1) (maximumIndexC x2) in
      if e2==[] || f2==[]
      then prevlist
      else 
        case do; (e3,fsub) <- unifyFeatures [] a3 c3; (f3,_) <- unifyFeatures fsub b3 d3; return (e3,f3) of
          Nothing -> prevlist
          Just (e3,f3) -> let newcat = (T True i (S c1 e2 e3) `SL` (T True j (S d1 f2 f3) `BS` NP [Nc])) in
            Node {
              rs = COORD,
              pf = T.concat [pf(lnode),pf(cnode),pf(rnode)],
              cat = newcat,
              sem = betaReduce $ transvec newcat $ betaReduce $ Lamvec (App (App conj (Appvec 0 s1)) (Appvec 0 s2)),
              daughters = [lnode,cnode,rnode],
              score = score(lnode)*score(rnode),
              memo = ""
              }:prevlist
    _ -> prevlist

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
    memo = ""
    }:prevlist
parenthesisRule _ _ _ prevlist = prevlist

{-
if c /= CONJ   
  then prevlist   
  else     
    let inc = maximumIndexC x1 in
    case unifyCategory [] [] x1 (incrementIndexC x2 inc) of
      Nothing -> prevlist
      Just (x3,csub,fsub) -> case unifyWithHead (S anyPos nonStem) x3 of
                         Nothing -> prevlist   
                         _ -> let inc2 = maximumIndexC (incrementIndexC x2 inc) in
                              Node {
                                rs = COORD,
                                pf = T.concat [pf(lnode),pf(cnode),pf(rnode)],
                                cat = simulSubstituteCV csub fsub x3,
                                sem = DependentTypes.id, 
                                      --betaReduce $ transvec (Lamvec (inc2+1) (App (App conj (Appvec (inc2+1) f1)) (Appvec (inc2+1) (incrementVec f2 inc)))) sub sub,
                                daughters = [lnode,cnode,rnode],
                                score = score(lnode)*score(rnode),
                                memo = ""
                                }:prevlist
coordinationRule _ _ _ prevlist = prevlist
-}

{-
test :: IO()
test = do
  let t1 = (SL (T 1 anyS) (BS (T 1 anyS) (NP [Nc])))
  let t2 = (SL (T 1 anyS) (BS (T 1 anyS) (NP [Nc])))
  let inc = maximumIndexC t1
  case unifyCategory [] [] t1 (incrementIndexC t2 inc) of
    Nothing -> putStrLn "nothing"
    Just (t3,sub) -> putStrLn $ T.unpack $ toTeX $ betaReduce $ transvec (Lamvec 1 (App (App (Lam "p" (Lam "q" (Sigma "u" (Var "p") (Var "q")))) (Appvec 1 (Lam "p" (App (Var "p") (Con "t1"))))) (Appvec 1 (Lam "p" (App (Var "p") (Con "t2")))))) sub sub
-}

{-
compoundNPRule :: Node -> Node -> [Node] -> [Node]
compoundNPRule lnode@(Node {rs=r, cat=x}) rnode@(Node {cat=y}) prevlist =
  -- (N or NP)  (N or NP)  ==>  (N or NP)
  if r /= LEX -- Non-normal forms
  then prevlist
  else case (x,y) of  
       (N, N) -> compoundNPnode (Lam "x" (App (Con $ T.concat [pf(lnode),pf(rnode)]) (Var "x")))
       (N, NP [Nc]) -> compoundNPnode (Con $ T.concat [pf(lnode),pf(rnode)])
       (NP [Nc],N) -> compoundNPnode (Lam "x" (App (Con $ T.concat [pf(lnode),pf(rnode)]) (Var "x")))
       (NP [Nc], NP [Nc]) -> compoundNPnode (Con $ T.concat [pf(lnode),pf(rnode)])
       _ -> prevlist  
  where compoundNPnode sr = Node {
                        rs = CNP,
                        pf = pf(lnode) `T.append` pf(rnode),
                        cat = y,
                        sem = sr,
                        daughters = [lnode,rnode],
                        score = score(lnode)*score(rnode)*(9 % 10),
                        memo = "" 
                        }:prevlist
-}

{- Implementation of CCG Unification -}

-- | `numberOfArguments` returns the number of arguments of a given syntactic category.  
-- For a category variable, `numberOfArguments` simply returns 0.
numberOfArguments :: Cat -> Int
numberOfArguments c = case c of
  SL c1 _ -> 1 + numberOfArguments c1
  BS c1 _ -> 1 + numberOfArguments c1
  _ -> 0

-- | `maximumIndexC` returns a maximum index of category variables contained in a given category.
-- >>> maximumIndex T(1)/T(3) == 3
maximumIndexC :: Cat -> (Int,Int)
maximumIndexC c = case c of
  T _ i c2 -> let (a,b) = maximumIndexC c2 in 
             (max a i, b)
  SL c1 c2 -> let (x,y) = maximumIndexC c1; (u,v) = maximumIndexC c2 in 
              (max x u, max y v)
  BS c1 c2 -> let (x,y) = maximumIndexC c1; (u,v) = maximumIndexC c2 in 
              (max x u, max y v)
  S _ _ f -> (0, maximumIndexF f)
  _ -> (0,0)

maximumIndexF :: [PMFeature] -> Int
maximumIndexF fs = case fs of
  [] -> 0
  ((F i _):fs2) -> max i (maximumIndexF fs2)
  (_:fs2) -> maximumIndexF fs2

-- | `incrementIndex` returns 
incrementIndexC :: Cat -> (Int,Int) -> Cat
incrementIndexC c i = case c of
  T f j u -> T f ((fst i)+j) u
  SL c1 c2 -> SL (incrementIndexC c1 i) (incrementIndexC c2 i)
  BS c1 c2 -> BS (incrementIndexC c1 i) (incrementIndexC c2 i)
  S x1 x2 x3 -> S x1 x2 (incrementIndexF x3 (snd i))
  cc -> cc

incrementIndexF :: [PMFeature] -> Int -> [PMFeature]
incrementIndexF fs i = case fs of
  [] -> []
  ((F j f2):fs2) -> (F (i+j) f2):(incrementIndexF fs2 i)
  (fh:ft) -> fh:(incrementIndexF ft i)

-- | substituteCateogoryVariable T1 [1->X/Y] ==> X/Y
simulSubstituteCV :: [(Int,Cat)] -> [(Int,PMFeature)] -> Cat -> Cat
simulSubstituteCV csub fsub c = case c of
    T f j c3 -> case L.lookup j csub of
                  Just c4 -> simulSubstituteCV csub fsub c4
                  Nothing -> T f j (simulSubstituteCV csub fsub c3)
    SL ca cb -> SL (simulSubstituteCV csub fsub ca) (simulSubstituteCV csub fsub cb)
    BS ca cb -> BS (simulSubstituteCV csub fsub ca) (simulSubstituteCV csub fsub cb)
    S x1 x2 x3 -> S x1 x2 (simulSubstituteFV x3 fsub)
    _ -> c

-- | substituteFeatureVariable F 1 f [1->PM] ==> f[PM/1]
substituteFV :: [PMFeature] -> (Int,PMFeature) -> [PMFeature]
substituteFV f1 (i,f2) = case f1 of
  [] -> []
  ((F j _):ft)
    | i == j -> f2:(substituteFV ft (i,f2))
  (fh:ft) -> fh:(substituteFV ft (i,f2)) -- No substitution

simulSubstituteFV :: [PMFeature] -> [(Int,PMFeature)] -> [PMFeature]
simulSubstituteFV = L.foldl' substituteFV
-- foldl' 
-- ([PMFeature] -> (Int,PMFeature) -> [PMFeature])
-- -> [PMFeature]
-- -> [(Int,PMFeature)]
-- -> [PMFeature]

unifyCategory :: [(Int,Cat)] -> [(Int,PMFeature)] -> Cat -> Cat -> Maybe (Cat, [(Int,Cat)], [(Int,PMFeature)])
unifyCategory csub fsub c1 c2 = case (c1,c2) of
  (T f1 i c3, T f2 j c4) -> do
                            (c5,csub2,fsub2) <- unifyCategory csub fsub c3 c4
                            return $ case () of
                                       _ | i <= j    -> (T (f1 && f2) j c5, (i,(T (f1 && f2) j c5)):csub2, fsub2)
                                         | otherwise -> (T (f1 && f2) i c5, (j,(T (f1 && f2) i c5)):csub2, fsub2)
  (T f i c3, c4) -> do --- c4 is not T
                    (c5,csub2,fsub2) <- if f == True 
                                    then unifyWithHead csub fsub c3 c4
                                    else unifyCategory csub fsub c3 c4
                    return (c5,(i,c5):csub2,fsub2)
  (NP x, NP y) -> let z = L.intersect x y in
                  if z == []
                  then Nothing
                  else Just (NP z, csub, fsub)
  (S x1 x2 x3, S y1 y2 y3) -> do
                              let z1 = L.intersect x1 y1
                              guard (z1 /= [])
                              let z2 = L.intersect x2 y2
                              guard (z2 /= [])
                              (z3,fsub2) <- unifyFeatures fsub x3 y3
                              return ((S z1 z2 z3), csub, fsub2)
  (SL c3 c4, SL c5 c6) -> do
                          (c7,csub2,fsub2) <- unifyCategory csub fsub c4 c6
                          (c8,csub3,fsub3) <- unifyCategory csub2 fsub2 c3 c5
                          return (SL c8 c7,csub3,fsub3)
  (BS c3 c4, BS c5 c6) -> do
                          (c7,csub2,fsub2) <- unifyCategory csub fsub c4 c6
                          (c8,csub3,fsub3) <- unifyCategory csub2 fsub2 c3 c5
                          return (BS c8 c7, csub3, fsub3)
  (N, N)           -> Just (N, csub, fsub)
  (CONJ, CONJ)     -> Just (CONJ, csub, fsub)
  (Sbar x, Sbar y) -> let z = L.intersect x y in
                      if z == []
                         then Nothing
                         else Just (Sbar z, csub, fsub)
  (LPAREN, LPAREN) -> Just (LPAREN, csub, fsub)
  (RPAREN, RPAREN) -> Just (RPAREN, csub, fsub)
  (_, T _ _ _) -> unifyCategory csub fsub c2 c1
  _ -> Nothing

-- | Unify c1 with the head of c2
unifyWithHead :: [(Int,Cat)] -> [(Int,PMFeature)] -> Cat -> Cat -> Maybe (Cat, [(Int,Cat)], [(Int,PMFeature)])
unifyWithHead csub fsub c1 c2 = case c2 of
  SL x y -> do
            (x',csub2,fsub2) <- unifyWithHead csub fsub c1 x
            return $ (SL x' y, csub2, fsub2)
  BS x y -> do
            (x',csub2,fsub2) <- unifyWithHead csub fsub c1 x
            return $ (BS x' y, csub2, fsub2)
  T f i u -> do
           (x',csub2,fsub2) <- unifyCategory csub fsub c1 u
           return $ (T f i x', csub2, fsub2)
  bc -> do
        (x',csub2,fsub2) <- unifyCategory csub fsub c1 bc
        return (x', csub2, fsub2)

-- | unifyFeature
unifyFeature :: [(Int,PMFeature)] -> PMFeature -> PMFeature -> Maybe (PMFeature, [(Int,PMFeature)])
unifyFeature sub f1 f2 = case (f1,f2) of
--  (F i f3, F j f4) -> do
--                      (f5,sub2) <- unifyFeature sub f3 f4
--                      return $ case () of
--                                 _ | i <= j    -> (F j f5, (i, F i f5):((j, F j f5):sub2))
--                                   | otherwise -> (F i f5, (i, F i f5):((j, F j f5):sub2))
  (F i f3, f4) -> do
                  (f5,sub2) <- unifyFeature sub f3 f4
                  return (F i f5, (i,F i f5):sub2)
  (P,P)       -> Just (P, sub)
  (P,M)       -> Nothing
  (P,PM)      -> Just (P, sub)
  (M,P)       -> Nothing
  (M,M)       -> Just (M, sub)
  (M,PM)      -> Just (M, sub)
  (PM,P)      -> Just (P, sub)
  (PM,M)      -> Just (M, sub)
  (PM,PM)     -> Just (PM, sub)
  (_, F _ _)  -> unifyFeature sub f2 f1

unifyFeatures :: [(Int,PMFeature)] -> [PMFeature] -> [PMFeature] -> Maybe ([PMFeature], [(Int,PMFeature)])
unifyFeatures sub f1 f2 = case (f1,f2) of
  ([],[]) -> Just ([],sub)
  ((f1h:f1t),(f2h:f2t)) -> do
                           (f3h,sub2) <- unifyFeature sub f1h f2h
                           (f3t,sub3t) <- unifyFeatures sub2 f1t f2t
                           return ((f3h:f3t), sub3t)
  _ -> Nothing

-- | Lamvec, Appvec: 
-- "transvec" function transforms the first argument (of type Preterm)
-- into the one without 
transvec :: Cat -> Preterm -> Preterm
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
  S _ _ _ -> case preterm of
               Lam (Lamvec m) -> Lam (deleteLambda 0 m)
               Lamvec (Lam m) -> deleteLambda 0 (Lam m)
               Lamvec m -> Lam (deleteLambda 0 (addLambda 0 m))
               m        -> m
  N -> case preterm of
              Lam (Lamvec m) -> Lam (deleteLambda 0 m)
              Lamvec (Lam m) -> deleteLambda 0 (Lam m)
              Lamvec m -> Lam (deleteLambda 0 (addLambda 0 m))
              m        -> m
  _ -> preterm

{- Some Macros for defining lexical items -}

lexicalitem :: T.Text -> T.Text -> Integer -> Cat -> Preterm -> Node
lexicalitem word source r c s = Node {rs=LEX, pf=word, cat=c, sem=s, daughters=[], score=(r % 100), memo=source}

{- Some Marcos for CCG categories/features -}

-- | Category S with the default feature setting
defS :: [PosFeature] -> [ConjFeature] -> Cat
defS p c = S p c [M,M,M,M,M]

verb :: [PosFeature]
verb = [V5k, V5s, V5t, V5n, V5m, V5r, V5w, V5g, V5z, V5b, V5IKU, V5YUK, V5ARU, V5NAS, V5TOW, V1, VK, VS, VZ, VURU]

adjective :: [PosFeature]
adjective = [Aauo, Ai, ANAS, ATII, ABES]

nomPred :: [PosFeature]
nomPred = [Nda, Nna, Nno, Nni, Nemp, Ntar]

anyPos :: [PosFeature]
anyPos = verb ++ adjective ++ nomPred ++ [Exp]

nonStem :: [ConjFeature]
nonStem = [Neg, Cont, Term, Attr, Hyp, Imper, Pre, ModU, ModS, VoR, VoS, VoE, NegL, TeForm]

anySExStem :: Cat
anySExStem = S anyPos nonStem [F 1 PM,F 2 PM,F 3 PM,M,M]

--anyConj :: [ConjFeature]
--anyConj = [Stem, UStem, Neg, Cont, Term, Attr, Hyp, Imper, Pre, EuphT, EuphD, ModU, ModS, VoR, VoS, VoE, TeForm, NiForm, Yooni]

--anyCase :: [CaseFeature] 
--anyCase = [Nc, Ga, O, Ni, To, Niyotte, No]

{- Templates for Semantic Representation -}
-- | \x.x
id :: Preterm
id = Lam (Var 0)

-- | verbSR i op
-- i==1 -> S\NP:             \x.\c.(e:event)Xop(e,x)X(ce)
-- i==2 -> S\NP\NP:       \y.\x.\c.(e:event)X(op(e,x,y)X(ce)
-- i==3 -> S\NP\NP\NP: \z.\y.\x.\c.(e:event)X(op(e,x,y,z)X(ce)
-- i==4 -> error
verbSR :: Int -> T.Text -> Preterm
verbSR i op | i == 1 = (Lam (Lam (Sigma (Con "event") (Sigma (App (App (Con op) (Var 2)) (Var 0)) (App (Var 2) (Var 1))))))
            | i == 2 = (Lam (Lam (Lam (Sigma (Con "event") (Sigma (App (App (App (Con op) (Var 3)) (Var 2)) (Var 0)) (App (Var 2) (Var 1)))))))
            | i == 3 = (Lam (Lam (Lam (Lam (Sigma (Con "event") (Sigma (App (App (App (App (Con op) (Var 4)) (Var 3)) (Var 2)) (Var 0)) (App (Var 2) (Var 1))))))))
            | otherwise = Con $ T.concat ["verbSR: verb ",op," of ", T.pack (show i), " arguments"]

-- | S\NP: \x.\c.(s:state)Xop(s,x)X(ce)
predSR :: T.Text -> Preterm
predSR op = (Lam (Lam (Sigma (Con "state") (Sigma (App (App (Con op) (Var 2)) (Var 0)) (App (Var 2) (Var 1))))))

-- | NP: 
properNameSR :: T.Text -> Preterm
properNameSR op = (Lam (App (Var 0) (Con op)))

-- | N: 
commonNounSR :: T.Text -> Preterm
commonNounSR op = (Lam (Lam (Sigma (Con "state") (Sigma (App (App (Con op) (Var 2)) (Var 0)) (App (Var 2) (Var 1))))))

-- | S\S, S/S: \p.\c.op (pc)
modal :: T.Text -> Preterm 
modal op = (Lam (Lam (App (Con op) (App (Var 1) (Var 0)))))

-- | S\NP\(S\NP):    \p.\x.\c.op(x,\z.(pz)c)
--   S\NP\NP\(S\NP): \p.\y.\x.\c.op(x,\z.((py)z)c)
intensionalVerb :: Int -> T.Text -> Preterm 
intensionalVerb i op | i == 1 = (Lam (Lam (Lam (App (App (Con op) (Lam (App (App (Var 3) (Var 0)) (Var 1)))) (Var 1)))))
                     | i == 2 = (Lam (Lam (Lam (Lam (App (App (Con op) (Lam (App (App (App (Var 4) (Var 3)) (Var 0)) (Var 1)))) (Var 2))))))
                     | otherwise = Con $ T.concat ["intensionalVerb: verb ",op," of ", T.pack (show i), " arguments"]

-- | T/T: \p.\v.\c.pv(\e.(op e) X ce)
mannerAdverb :: T.Text -> Preterm 
mannerAdverb op = (Lam (Lamvec (Lam (App (Appvec 1 (Var 2)) (Lam (Sigma (App (Con op) (Var 0)) (App (Var 3) (Var 0))))))))

-- | S\S: \p.\c.p(\e.(op e) X ce)
eventModifier :: T.Text -> Preterm 
eventModifier op = (Lam (Lam (App (Var 1) (Lam (Sigma (App (Con op) (Var 0)) (App (Var 2) (Var 1)))))))

-- | S\S: \p.\c.not (pc)
negOperator :: Preterm -- 
negOperator = (Lam (Lam (Not (App (Var 1) (Var 0)))))

-- | T/(T\NP[cm])\NP[nc]: \x.\p.px
argumentCM :: Preterm 
argumentCM = (Lam (Lam (App (Var 0) (Var 1))))

-- | T/T\NP[nc]: \x.\p.\v.\c.p (\e.op(e,x) X ce)
adjunctCM :: T.Text -> Preterm 
adjunctCM c = (Lam (Lam (Lam (App (Var 1) (Lam (Sigma (App (App (Con c) (Var 3)) (Var 0)) (App (Var 2) (Var 1))))))))

{-
data PartialCategory = PS | PNP | PN | PC | PT | PArrow PartialCategory PartialCategory
  deriving (Eq, Show)

category2PC :: Cat -> PartialCategory
category2PC c = case c of
  S _ _  -> PS
  NP _   -> PNP
  N      -> PN
  CONJ   -> PC
  LPAREN -> PC
  RPAREN -> PC
  SL x y -> PArrow (category2PC y) (category2PC x)
  BS x y -> PArrow (category2PC y) (category2PC x)
  T _ _  -> PT
-}

{-
-- | category2VN
-- |   convert a category to a corresponding variable name        
-- |
category2VN :: Cat -> T.Text
category2VN c = case c of
  NP _ -> "x"
  N -> "n"  
  S _ _ -> "p"
  CONJ -> "o"
  SL _ _ -> "p"
  BS _ _ -> "p"  
  T _ _ -> "p"  
-}

