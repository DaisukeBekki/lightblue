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
  Feature(..),
  FeatureValue(..),
  -- * Classes
  SimpleText(..),
  printF,
  printPMFs,
  -- * Tests
  isBaseCategory,
  --isCONJ,
  -- * Combinatory Rules
  unaryRules,
  binaryRules,
  -- trinaryRules  
  --coordinationRule,
  parenthesisRule,
  -- Test code
  test
  ) where

import Prelude hiding (id)
import qualified Data.Text.Lazy as T --text
import qualified Data.Text.Lazy.IO as T --for test code only
import qualified Data.List as L      --base
import qualified Data.Maybe as Maybe --base
import Data.Fixed                    --base
import Data.Ratio                    --base
--import Control.Monad                 --base
--import Text.Printf -- for 'printf'
import DependentTypes

data Node = Node {
  rs :: RuleSymbol,    -- ^ The name of the rule
  pf :: T.Text,        -- ^ The phonetic form
  cat :: Cat,          -- ^ The syntactic category (in CCG)
  sem :: Preterm,      -- ^ The semantic representation (in DTS)
  sig :: [Signature],   -- ^ Signature
  daughters :: [Node], -- ^ The daughter nodes
  score :: Rational,   -- ^ The score (between 0.00 to 1.00, larger the better)
  source :: T.Text    -- ^ The source of the lexical entry
  } deriving (Eq, Show)

instance Ord Node where
  (Node {score=i}) `compare` (Node {score=j})
    | i < j  = GT
    | i == j = EQ
    | i > j  = LT
  (Node _ _ _ _ _ _ _ _) `compare` (Node _ _ _ _ _ _ _ _) = EQ

instance SimpleText Node where
  toText n@(Node _ _ _ _ sig' _ _ _) = T.concat [toTextLoop "" n, "Sig. ", printSignatures sig', "\n"]
    where toTextLoop indent node =
            case daughters node of 
              [] -> T.concat [(T.pack indent), toText (rs node), " ", pf node, " ", toText (cat node), " ", toTextWithVN [] (sem node), " ", source node, " [", T.pack (show ((fromRational $ score node)::Fixed E2)), "]\n"]
              dtrs -> T.concat $ [(T.pack indent), toText (rs node), " ", toText (cat node), " ", toTextWithVN [] (sem node), " [", T.pack (show ((fromRational $ score node)::Fixed E2)), "]\n"] ++ (map (\d -> toTextLoop (indent++"  ") d) dtrs)

data Cat =
  S [Feature]        -- ^ S
  | NP [Feature]     -- ^ NP
  | N                -- ^ N
  | Sbar [Feature]   -- ^ S bar
  | CONJ             -- ^ CON
  | LPAREN           -- ^ A category for left parentheses
  | RPAREN           -- ^ A category for right parentheses
  | SL Cat Cat       -- ^ X/Y
  | BS Cat Cat       -- ^ X\\Y
  | T Bool Int Cat   -- ^ Category variables, where Int is an index, Cat is a restriction for its head. 

instance Eq Cat where
  SL x1 x2 == SL y1 y2 = (x1 == y1) && (x2 == y2)
  BS x1 x2 == BS y1 y2 = (x1 == y1) && (x2 == y2)
  T f1 i x == T f2 j y = (f1 == f2) && (i == j) && (x == y)
  S f1  == S f2  = unifiable f1 f2
  NP f1 == NP f2 = unifiable f1 f2
  N == N = True
  Sbar f1 == Sbar f2 = unifiable f1 f2
  CONJ == CONJ = True
  LPAREN == LPAREN = True
  RPAREN == RPAREN = True
  _ == _ = False

-- | checks if two lists of features are unifiable.
unifiable :: [Feature] -> [Feature] -> Bool
unifiable f1 f2 = case unifyFeatures [] f1 f2 of
                    Just _ -> True
                    Nothing -> False

-- | `toText` method is invoked.
instance Show Cat where
  show = T.unpack . toText

data Feature = 
  F [FeatureValue]        -- ^ Syntactic feature
  | SF Int [FeatureValue] -- ^ Shared syntactic feature (with an index)
  deriving (Eq, Show)

data FeatureValue =
               V5k | V5s | V5t | V5n | V5m | V5r | V5w | V5g | V5z | V5b |
               V5IKU | V5YUK | V5ARU | V5NAS | V5TOW |
               V1 | VK | VS | VSN | VZ | VURU |
               Aauo | Ai | ANAS | ATII | ABES |
               Nda | Nna | Nno | Ntar | Nni | Nemp | Nto |
               Exp | Error |
               Stem | UStem | Neg | Cont | Term | Attr | Hyp | Imper | Pre |
               NegL | TeForm | NiForm |
               EuphT | EuphD |
               ModU | ModD | ModS |
               VoR | VoS | VoE |
               P | M |
               Nc | Ga | O | Ni | To | Niyotte | No |
               ToCL | YooniCL
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
  show Error = "error"
  --
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
  show TeForm = "te"
  show NiForm = "ni"
  show EuphT = "euph:t"
  show EuphD = "euph:d"
  show ModU = "mod:u"
  show ModD = "mod:d"
  show ModS = "mod:s"
  show VoR = "vo:r"
  show VoS = "vo:s"
  show VoE = "vo:e"
  -- 
  show ToCL = "to"
  show YooniCL = "yooni"
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

instance SimpleText Cat where
  toText category = case category of
    SL x y      -> T.concat [toText x, "/", toText' y]
    BS x y      -> T.concat [toText x, "\\", toText' y]
    T True i c     -> T.concat ["T[", toText c, "]<", (T.pack $ show i),">"]
    T False i c     -> T.concat [toText c, "<", (T.pack $ show i), ">"]
    S (pos:(conj:pmf)) -> 
              T.concat [
                       "S[",
                       printF pos,
                       "][",
                       printF conj,
                       "][",
                       printPMFs pmf,
                       "]"
                       ]
    NP [cas]    -> T.concat ["NP[", printF cas, "]"]
    N           -> "N"
    Sbar [sf]   -> T.concat ["Sbar[", printF sf, "]"]
    CONJ        -> "CONJ"
    LPAREN      -> "LPAREN"
    RPAREN      -> "RPAREN"
    _ -> "Error in Simpletext Cat"
    where -- A bracketed version of `toText'` function
    toText' c = if isBaseCategory c
                  then toText c
                  else T.concat ["(", toText c, ")"]

printF :: Feature -> T.Text
printF (SF i f) = T.concat [printFVal f, "<", T.pack (show i), ">"]
printF (F f) = printFVal f

printFVal :: [FeatureValue] -> T.Text
printFVal [] = T.empty
printFVal [pos] = T.pack $ show pos
printFVal [pos1,pos2] = T.pack $ (show pos1) ++ "|" ++ (show pos2)
printFVal (pos1:(pos2:_)) = T.pack $ (show pos1) ++ "|" ++ (show pos2) ++ "|+"

printPMF :: Bool -> T.Text -> Feature -> Maybe T.Text
printPMF _ label pmf = case (label,pmf) of
    (l,F [P])       -> Just $ T.concat ["+", l]
    (_,F [M])      -> Nothing -- if shared then Just $ T.concat ["-", l] else Nothing
    (l,F [P,M]) -> Just $ T.concat ["±", l]
    (l,F [M,P]) -> Just $ T.concat ["±", l]
    (l,SF i f) -> do
                  x <- printPMF True l (F f)
                  return $ T.concat [x,"<",T.pack (show i),">"]
    _ -> return $ T.concat ["Error: printPMF", T.pack $ show pmf]

printPMFs :: [Feature] -> T.Text
printPMFs pmfs = T.intercalate "," $ Maybe.catMaybes $ printPMFsLoop ["t","p","n","N","T"] pmfs

printPMFsLoop :: [T.Text] -> [Feature] -> [Maybe T.Text]
printPMFsLoop labels pmfs = case (labels,pmfs) of
  ([],[])         -> []
  ((l:ls),(p:ps)) -> (printPMF False l p):(printPMFsLoop ls ps)
  _ -> [Just $ T.concat ["Error: mismatch in ", T.pack (show labels), " and ", T.pack (show pmfs)]]

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
  NP f -> case f of
            (F [Nc]:_) -> False
            (SF _ [Nc]:_) -> False
            _ -> True
  -- N -> False
  Sbar _ -> True
  _ -> False

--isCONJ :: Cat -> Bool
--isCONJ c = c == CONJ

-- | A test to check if a given category is T\NPnc.
isNoncaseNP :: Cat -> Bool
isNoncaseNP c = case c of
  (T _ _ _) `BS` (NP cas) -> 
    case cas of
      (F v:_)    -> Nc `elem` v
      (SF _ v:_) -> Nc `elem` v
      _ -> False
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
  | COORD -- ^ Coordination rule
  | PAREN -- ^ Parenthesis rule
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
    COORD -> "<Phi>"
    PAREN -> "PAREN"
    -- CNP -> "CNP"

{- Classes of Combinatory Rules -}

-- | The function to apply all the unaryRules to a CCG node.
unaryRules :: Node -> [Node] -> [Node]
unaryRules _ prevlist = prevlist
--unaryRules = sseriesRule

-- | The function to apply all the binary rules to a given pair of CCG nodes.
binaryRules :: Node -> Node -> [Node] -> [Node]
binaryRules lnode rnode = 
  --compoundNPRule lnode rnode
  forwardFunctionCrossedSubstitutionRule lnode rnode
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
  T.putStr "y1: "
  T.putStrLn $ toText y1
  T.putStr "y2: "
  T.putStrLn $ toText y2
  let inc = maximumIndexC y2
  T.putStr "maximumIndexC y2: "
  print $ maximumIndexC y2
  T.putStr "incr.y1.inc: "    
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
                        source = "", -- pf(lnode) `T.append` pf(rnode)
                        sig = sig(lnode) ++ sig(rnode)
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
                        source = "",
                        sig = sig(lnode) ++ sig(rnode)
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
                        source = "",
                        sig = sig(lnode) ++ sig(rnode)
                        }:prevlist
backwardFunctionComposition3Rule _ _ prevlist = prevlist

-- | Forward function crossed composition rule.
forwardFunctionCrossedComposition1Rule :: Node -> Node -> [Node] -> [Node]
forwardFunctionCrossedComposition1Rule lnode@(Node {rs=r,cat=SL x y1, sem=f}) rnode@(Node {cat=BS y2 z, sem=g}) prevlist =
  -- [>Bx] x/y1  y2\z  ==> x\z
  if r == FFC1 || r == FFC2 || r == FFC3 || r == EC || (isNoncaseNP y1) || not (isArgumentCategory z) -- Non-normal forms (+ Add-hoc rule 1)
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
            source = "",
            sig = sig(lnode) ++ sig(rnode)
            }:prevlist
forwardFunctionCrossedComposition1Rule _ _ prevlist = prevlist

-- | Forward function crossed composition rule 2.
forwardFunctionCrossedComposition2Rule :: Node -> Node -> [Node] -> [Node]
forwardFunctionCrossedComposition2Rule lnode@(Node {rs=r,cat=(x `SL` y1), sem=f}) rnode@(Node {cat=(y2 `BS` z1) `BS` z2, sem=g}) prevlist =
  -- [>Bx2] x/y1:f  y2\z1\z2:g  ==> x\z1\z2
  if r == FFC1 || r == FFC2 || r == FFC3 || r == EC || (isNoncaseNP y1) || not (isArgumentCategory z2) || not (isArgumentCategory z1) -- Non-normal forms + Ad-hoc rule
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
                        source = "",
                        sig = sig(lnode) ++ sig(rnode)
                        }:prevlist
forwardFunctionCrossedComposition2Rule _ _ prevlist = prevlist

-- | Forward functional crossed substitution rule
forwardFunctionCrossedSubstitutionRule :: Node -> Node -> [Node] -> [Node]
forwardFunctionCrossedSubstitutionRule lnode@(Node {rs=_,cat=((x `SL` y1) `BS` z1), sem=f}) rnode@(Node {cat=(y2 `BS` z2), sem=g}) prevlist =
  -- [>Sx] x/y1\z:f  y2\z:g  ==> x\z: \x.(fx)(gx)
  if False -- r == FFC1 || r == FFC2 || r == FFC3 || (isNoncaseNP y1) || not (isArgumentCategory z2) || not (isArgumentCategory z1) -- Non-normal forms + Ad-hoc rule
  then prevlist
  else
    let inc = maximumIndexC (cat rnode) in
    case unifyCategory [] [] (incrementIndexC z1 inc) z2 of
      Nothing -> prevlist -- Unification failure
      Just (z,csub1,fsub1) ->
        case unifyCategory csub1 fsub1 (incrementIndexC y1 inc) y2 of
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

{-
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
              source = "",
              sig = sig(lnode) ++ sig(rnode)
              }:prevlist
    _ -> prevlist
-}

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
    sig = sig(lnode) ++ sig(rnode)
    }:prevlist
parenthesisRule _ _ _ prevlist = prevlist

{- Variable-length Lambda Calculus -}

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
  S _ -> case preterm of
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

-- | substituteCateogoryVariable 
--
-- >>> T1 [1->X/Y] ==> X/Y
simulSubstituteCV :: [(Int,Cat)] -> [(Int,Feature)] -> Cat -> Cat
simulSubstituteCV csub fsub c = case c of
    T f j c3 -> case L.lookup j csub of
                  Just c4 -> simulSubstituteCV csub fsub c4
                  Nothing -> T f j (simulSubstituteCV csub fsub c3)
    SL ca cb -> SL (simulSubstituteCV csub fsub ca) (simulSubstituteCV csub fsub cb)
    BS ca cb -> BS (simulSubstituteCV csub fsub ca) (simulSubstituteCV csub fsub cb)
    S f -> S (simulSubstituteFV fsub f)
    Sbar f -> Sbar (simulSubstituteFV fsub f)
    NP f -> NP (simulSubstituteFV fsub f)
    _ -> c

unifyCategory :: [(Int,Cat)] -> [(Int,Feature)] -> Cat -> Cat -> Maybe (Cat, [(Int,Cat)], [(Int,Feature)])
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
                          (c7,csub2,fsub2) <- unifyCategory csub fsub c4 c6
                          (c8,csub3,fsub3) <- unifyCategory csub2 fsub2 c3 c5
                          return (SL c8 c7,csub3,fsub3)
  (BS c3 c4, BS c5 c6) -> do
                          (c7,csub2,fsub2) <- unifyCategory csub fsub c4 c6
                          (c8,csub3,fsub3) <- unifyCategory csub2 fsub2 c3 c5
                          return (BS c8 c7, csub3, fsub3)
  (N, N)           -> Just (N, csub, fsub)
  (CONJ, CONJ)     -> Just (CONJ, csub, fsub)
  (LPAREN, LPAREN) -> Just (LPAREN, csub, fsub)
  (RPAREN, RPAREN) -> Just (RPAREN, csub, fsub)
  (_, T _ _ _) -> unifyCategory csub fsub c2 c1
  _ -> Nothing

-- | Unify c1 with the head of c2
unifyWithHead :: [(Int,Cat)] -> [(Int,Feature)] -> Cat -> Cat -> Maybe (Cat, [(Int,Cat)], [(Int,Feature)])
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

getFeatureValue :: [(Int,Feature)] -> Int -> [FeatureValue] -> (Int, [FeatureValue])
getFeatureValue fsub i v =
  case L.lookup i fsub of
    Just (SF j v') | j <= i -> getFeatureValue fsub j v'
    Just (F v') -> (i,v')
    _ -> (i,v)

-- | substituteFeatureVariable
--
-- >>> F 1 f [1->PM] ==> f[PM/1]
substituteFV :: [(Int,Feature)] -> Feature -> Feature
substituteFV fsub f1 = case f1 of
  SF i v -> let (j,v') = getFeatureValue fsub i v in 
            SF j v'
  _ -> f1

simulSubstituteFV :: [(Int,Feature)] -> [Feature] -> [Feature]
simulSubstituteFV fsub = map (substituteFV fsub)

alter :: (Ord a, Eq a) => a -> b -> [(a,b)] -> [(a,b)]
alter i v mp = (i,v):(filter (\(j,_) -> i /= j) mp)

-- | unifyFeature
unifyFeature :: [(Int,Feature)] -> Feature -> Feature -> Maybe (Feature, [(Int,Feature)])
unifyFeature fsub f1 f2 = case (f1,f2) of
  (SF i v1, SF j v2) -> if i == j
                           then
                             let (i',v1') = getFeatureValue fsub i v1;
                                 v3 = L.intersect v1' v2 in
                             if v3 == []
                                then Nothing
                                else 
                                  let sf = SF i v3 in
                                  Just (sf, (alter i' (F v3) fsub))
                           else
                             let (i',v1') = getFeatureValue fsub i v1;
                                 (j',v2') = getFeatureValue fsub j v2;
                                 v3 = L.intersect v1' v2' in
                             if v3 == []
                                then Nothing
                                else
                                  let sf = SF (min i' j') v3 in
                                  Just (sf, (alter (max i' j') sf (alter (min i j) (F v3) fsub)))
  (SF i v1, F v2) -> let (i',v1') = getFeatureValue fsub i v1;
                         v3 = L.intersect v1' v2 in
                     if v3 == []
                        then Nothing
                        else Just (SF i' v3, (alter i' (F v3) fsub))
  (F v1, SF j v2) -> let (j',v2') = getFeatureValue fsub j v2;
                         v3 = L.intersect v1 v2' in
                     if v3 == []
                        then Nothing
                        else Just (SF j' v3, (alter j' (F v3) fsub))
  (F v1, F v2) -> let v3 = L.intersect v1 v2 in
                  if v3 == []
                     then Nothing
                     else Just (F v3, fsub)

-- |
unifyFeatures :: [(Int,Feature)] -> [Feature] -> [Feature] -> Maybe ([Feature], [(Int,Feature)])
unifyFeatures fsub f1 f2 = case (f1,f2) of
  ([],[]) -> Just ([],fsub)
  ((f1h:f1t),(f2h:f2t)) -> do
                           (f3h,fsub2) <- unifyFeature fsub f1h f2h
                           (f3t,fsub3) <- unifyFeatures fsub2 f1t f2t
                           return ((f3h:f3t), fsub3)
  _ -> Nothing

