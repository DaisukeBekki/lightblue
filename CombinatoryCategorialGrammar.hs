{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module CombinatoryCategorialGrammar (
  Node(..),
  Typeset(..),
  SimpleText(..),
  RuleSymbol(..),
  Cat(..),
  CatPos(..),
  CatConj(..),
  CatCase(..),
  --isCONJ,
  unaryRules,
  binaryRules,
  -- trinaryRules  
  coordinationRule,
  parenthesisRule
  ) where

import qualified Data.Text.Lazy as T
import qualified Data.List as L
import DependentTypes
import Data.Fixed
--import Text.Printf -- for 'printf'

data Node = Node {
  rs :: RuleSymbol,
  pf :: T.Text,
  cat :: Cat,
  sem :: Preterm,
  daughters :: [Node],
  score :: Rational,
  memo :: T.Text
  } deriving (Eq, Show)

instance Ord Node where
  (Node {score=i}) `compare` (Node {score=j})
    | i < j  = GT
    | i == j = EQ
    | i > j  = LT
  (Node _ _ _ _ _ _ _) `compare` (Node _ _ _ _ _ _ _) = EQ

instance Typeset Node where
  toTeX node@(Node _ _ _ _ _ _ _) = 
    case daughters node of 
      [] -> T.concat ["\\vvlex[", (memo node), "]{", (pf node), "}{", toTeX (cat node), "}{", toTeXWithVN [] (sem node), "}"] --, "\\ensuremath{", (memo node), "}"]
      _ -> T.concat ["\\nd[", toTeX (rs node), "]{\\vvcat{", toTeX (cat node), "}{", toTeXWithVN [] (sem node), "}}{", daughtersTeX, "}"] --, "\\ensuremath{", (memo node), "}"]
    where daughtersTeX =
            case daughters node of
              [l,c,r] -> T.concat [toTeX l, "&", toTeX c, "&", toTeX r]
              [l,r]   -> T.concat [toTeX l, "&", toTeX r]
              [c]     -> T.concat [toTeX c]
              x  -> T.pack $ "(Error: daughter nodes are ill-formed: " ++ show x ++ ")"

instance SimpleText Node where
  toText n@(Node _ _ _ _ _ _ _) = toTextLoop "" n
    where toTextLoop indent node =
            case daughters node of 
              [] -> T.concat [(T.pack indent), toText (rs node), " ", pf node, " ", toText (cat node), " ", toTextWithVN [] (sem node), " ", memo node, " [", T.pack (show ((fromRational $ score node)::Fixed E2)), "]\n"]
              dtrs -> T.concat $ [(T.pack indent), toText (rs node), " ", toText (cat node), " ", toTextWithVN [] (sem node), " [", T.pack (show ((fromRational $ score node)::Fixed E2)), "]\n"] ++ (map (\d -> toTextLoop (indent++"  ") d) dtrs)

data Cat = 
  S [CatPos] [CatConj] | NP [CatCase] | N | CONJ | LPAREN | RPAREN |
  SL Cat Cat | BS Cat Cat |
  T Int Cat

instance Eq Cat where
  S x1 x2 == S y1 y2 = (L.intersect x1 y1 /= []) && (L.intersect x2 y2) /=[]
  NP x == NP y = L.intersect x y /= []
  N == N = True
  CONJ == CONJ = True
  LPAREN == LPAREN = True
  RPAREN == RPAREN = True
  SL x1 x2 == SL y1 y2 = (x1 == y1) && (x2 == y2)
  BS x1 x2 == BS y1 y2 = (x1 == y1) && (x2 == y2)
  T x _ == T y _ = x == y
  _ == _ = False

instance Show Cat where
  show = T.unpack . toText

data CatPos = V5k | V5s | V5t | V5n | V5m | V5r | V5w | V5g | V5z | V5b |
              V5IKU | V5YUK | V5ARU | V5NAS | V5TOW |
              V1 | VK | VS | VZ | VURU |
              Aauo | Ai | ANAS | ATII | ABES |
              Nda | Nna | Nno | Nni | Nemp | Ntar |
              Exp | Error
              deriving (Eq)
data CatConj = Stem | UStem | Neg | Cont | Term | Attr | Hyp | Imper | Pre | NegL | TeForm | NiForm | EuphT | EuphD | ModU | ModD | ModS | VoR | VoS | VoE | Yooni
               deriving (Eq)
data CatCase = Nc | Ga | O | Ni | To | Niyotte | No
               deriving (Eq)

instance Show CatPos where
  --show AnyPos = ""
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
  show Exp = "exp"
  show Error = "error"

instance Show CatConj where
  --show AnyConj = "" 
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
  show Yooni = "yooni"

instance Show CatCase where
  --show AnyCase = ""
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

instance Feature CatPos
instance Feature CatConj
instance Feature CatCase

instance Typeset Cat where
  toTeX category = case category of
    S pos cnj   -> T.concat ["S\\f{",(printF pos),",",(printF cnj),"}"]
    NP cas      -> T.concat ["\\np\\f{",(printF cas),"}"]
    N           -> "N"
    CONJ        -> "\\conj"
    LPAREN      -> "LPAREN"
    RPAREN      -> "RPAREN"
    SL x y      -> T.concat [toTeX x, "/", toTeX' y]
    BS x y      -> T.concat [toTeX x, "{\\bs}", toTeX' y]
    T i u       -> T.concat ["\\vT_{",(T.pack $ show i),":",toTeX u,"}"]

instance SimpleText Cat where
  toText category = case category of
    S pos conj  -> T.concat ["S[", printF pos, "][", printF conj, "]"]
    NP cas      -> T.concat ["NP[", printF cas, "]"]
    N           -> "N"
    CONJ        -> "CONJ"
    LPAREN      -> "LPAREN"
    RPAREN      -> "RPAREN"
    SL x y      -> T.concat [toText x, "/", toText' y]
    BS x y      -> T.concat [toText x, "\\", toText' y]
    T i _       -> T.concat ["T", (T.pack $ show i)]

toTeX' :: Cat -> T.Text
toTeX' c = if isBaseCategory c 
           then toTeX c
           else T.concat ["(", toTeX c, ")"]

toText' :: Cat -> T.Text
toText' c = if isBaseCategory c
            then toText c   
            else T.concat ["(", toText c, ")"]     

isBaseCategory :: Cat -> Bool
isBaseCategory c = case c of
  S _ _ -> True
  NP _ -> True
  N -> True 
  CONJ -> True
  LPAREN -> True
  RPAREN -> True
  T _ _ -> True
  _ -> False

--isCONJ :: Cat -> Bool
--isCONJ c = c == CONJ

isNoncaseNP :: Cat -> Bool -- Check if T\NPnc
isNoncaseNP c = case c of
  (T _ _) `BS` (NP cas) -> if cas == [Nc] then True else False
  _ -> False

data RuleSymbol = LEX | FFA | BFA | FFC1 | BFC1 | FFC2 | BFC2 | FFC3 | BFC3 | FFCx1 | COORD | PAREN deriving (Eq, Show)

instance Typeset RuleSymbol where
  toTeX rulesymbol = case rulesymbol of 
    LEX -> "lex"
    FFA -> ">"
    BFA -> "<"
    FFC1 -> ">B"
    BFC1 -> "<B"
    FFC2 -> ">B^2"
    BFC2 -> "<B^2"
    FFC3 -> ">B^3"
    BFC3 -> "<B^3"
    FFCx1 -> ">Bx"
    COORD -> "\\langle\\Phi\\rangle"
    PAREN -> "PAREN"
    -- CNP -> "CNP"

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
    COORD -> "<Phi>"
    PAREN -> "PAREN"
    -- CNP -> "CNP"

-- | Classes of Combinatory Rules
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

binaryRules :: Node -> Node -> [Node] -> [Node]
binaryRules lnode rnode = 
  --compoundNPRule lnode rnode
  forwardFunctionCrossedComposition1Rule lnode rnode
  . backwardFunctionComposition3Rule lnode rnode
  . backwardFunctionComposition2Rule lnode rnode
  . forwardFunctionComposition2Rule lnode rnode
  . backwardFunctionComposition1Rule lnode rnode
  . forwardFunctionComposition1Rule lnode rnode
  . backwardFunctionApplicationRule lnode rnode
  . forwardFunctionApplicationRule lnode rnode
          
{-
trinaryRules :: Node -> Node -> Node -> [Node] -> [Node]
trinaryRules lnode cnode rnode =
  parenthesisRule lnode cnode rnode
  . coordinationRule lnode cnode rnode
-}

-- | Combinatory Rules
forwardFunctionApplicationRule :: Node -> Node -> [Node] -> [Node]
forwardFunctionApplicationRule lnode@(Node {rs=r, cat=SL x y1, sem=f}) rnode@(Node {cat=y2, sem=a}) prevlist =
  -- x/y1  y2  ==>  x
  if r == FFC1 || r == FFC2 || r == FFC3 -- Non-normal forms
  then prevlist
  else 
    let inc = maximumIndex y2 in
    case unifyCategory y2 (incrementIndex y1 inc) [] of
      Nothing -> prevlist -- Unification failure
      Just (_,sub) -> let newcat = simulSubstituteCV (incrementIndex x inc) sub in
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

backwardFunctionApplicationRule :: Node -> Node -> [Node] -> [Node]
backwardFunctionApplicationRule lnode@(Node {cat=y1, sem=a}) rnode@(Node {rs=r, cat=(BS x y2), sem=f}) prevlist =
  -- y1  x\y2  ==> x
  if r == BFC1 || r == BFC2 || r == BFC3 -- Non-normal forms
  then prevlist
  else     
    let inc = maximumIndex y1 in
    case unifyCategory y1 (incrementIndex y2 inc) [] of
      Nothing -> prevlist -- Unification failure
      Just (_,sub) -> let newcat = simulSubstituteCV (incrementIndex x inc) sub in
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

forwardFunctionComposition1Rule :: Node -> Node -> [Node] -> [Node]
forwardFunctionComposition1Rule lnode@(Node {rs=r,cat=SL x y1, sem=f}) rnode@(Node {cat=SL y2 z, sem=g}) prevlist =
  -- x/y1  y2/z  ==> x/z
  if r == FFC1 || r == FFC2 || r == FFC3 || (isNoncaseNP y1) -- Non-normal forms (+ Add-hoc rule 1)
  then prevlist
  else  
    let inc = maximumIndex (cat rnode) in
    case unifyCategory y2 (incrementIndex y1 inc) [] of          
      Nothing -> prevlist -- Unification failure
      Just (_,sub) -> 
        let z' = simulSubstituteCV z sub in
        if numberOfArguments z' > 3  -- Ad-hoc rule 2
        then prevlist   
        else let newcat = (simulSubstituteCV (incrementIndex x inc) sub) `SL` z' in
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

backwardFunctionComposition1Rule :: Node -> Node -> [Node] -> [Node]
backwardFunctionComposition1Rule lnode@(Node {cat=BS y1 z, sem=g}) rnode@(Node {rs=r,cat=(BS x y2), sem=f}) prevlist =
  -- y1\z:g  x\y2:f  ==> x\z
  if r == BFC1 || r == BFC2 || r == BFC3 -- Non-normal forms
  then prevlist
  else     
    let inc = maximumIndex (cat lnode) in
    case unifyCategory y1 (incrementIndex y2 inc) [] of
      Nothing -> prevlist -- Unification failure
      Just (_,sub) -> let newcat = simulSubstituteCV ((incrementIndex x inc) `BS` z) sub in
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

forwardFunctionComposition2Rule :: Node -> Node -> [Node] -> [Node]
forwardFunctionComposition2Rule lnode@(Node {rs=r,cat=(x `SL` y1), sem=f}) rnode@(Node {cat=(y2 `SL` z1) `SL` z2, sem=g}) prevlist =
  -- x/y1:f  y2/z1/z2:g  ==> x/z1/z2
  if r == FFC1 || r == FFC2 || r == FFC3 || (isNoncaseNP y1) -- Non-normal forms
  then prevlist
  else     
    let inc = maximumIndex (cat rnode) in
    case unifyCategory (incrementIndex y1 inc) y2 [] of
      Nothing -> prevlist -- Unification failure
      Just (_,sub) -> 
        let z1' = simulSubstituteCV z1 sub in
        if numberOfArguments z1' > 2  -- Ad-hoc rule 2
        then prevlist   
        else let newcat = simulSubstituteCV (((incrementIndex x inc) `SL` z1') `SL` z2) sub in
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

backwardFunctionComposition2Rule :: Node -> Node -> [Node] -> [Node]
backwardFunctionComposition2Rule lnode@(Node {cat=(y1 `BS` z1) `BS` z2, sem=g}) rnode@(Node {rs=r,cat=(x `BS` y2), sem=f}) prevlist =
  -- y1\z1\z2  x\y2  ==> x\z1\z2
  if r == BFC1 || r ==BFC2 || r == BFC3 -- Non-normal forms
  then prevlist
  else  
    let inc = maximumIndex (cat lnode) in
    case unifyCategory (incrementIndex y2 inc) y1 [] of
      Nothing -> prevlist -- Unification failure
      Just (_,sub) -> let newcat = simulSubstituteCV (((incrementIndex x inc) `BS` z1) `BS` z2) sub in
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

backwardFunctionComposition3Rule :: Node -> Node -> [Node] -> [Node]
backwardFunctionComposition3Rule lnode@(Node {cat=((y1 `BS` z1) `BS` z2) `BS` z3, sem=g}) rnode@(Node {rs=r,cat=(x `BS` y2), sem=f}) prevlist =
  -- y1\z1\z2\z3  x\y2  ==> x\z1\z2\z3
  if r == BFC1 || r ==BFC2 || r == BFC3 -- Non-normal forms
  then prevlist
  else  
    let inc = maximumIndex (cat lnode) in
    case unifyCategory (incrementIndex y2 inc) y1 [] of
      Nothing -> prevlist -- Unification failure
      Just (_,sub) -> let newcat = simulSubstituteCV ((((incrementIndex x inc) `BS` z1) `BS` z2) `BS` z3) sub in
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

forwardFunctionCrossedComposition1Rule :: Node -> Node -> [Node] -> [Node]
forwardFunctionCrossedComposition1Rule lnode@(Node {rs=r,cat=SL x y1, sem=f}) rnode@(Node {cat=BS y2 z, sem=g}) prevlist =
  -- x/y1  y2\z  ==> x\z
  if r == FFC1 || r == FFC2 || r == FFC3 || (isNoncaseNP y1) -- Non-normal forms (+ Add-hoc rule 1)
  then prevlist
  else  
    let inc = maximumIndex (cat rnode) in
    case unifyCategory y2 (incrementIndex y1 inc) [] of          
      Nothing -> prevlist -- Unification failure
      Just (_,sub) -> 
        let z' = simulSubstituteCV z sub in
        --if numberOfArguments z' > 3  -- Ad-hoc rule 2
        --then prevlist   
        --else 
        let newcat = (simulSubstituteCV (incrementIndex x inc) sub) `BS` z' in
             Node {
               rs = FFCx1,
               pf = pf(lnode) `T.append` pf(rnode),
               cat = newcat,
               sem = betaReduce $ transvec newcat $ betaReduce $ (Lam (App f (App g (Var 0)))),
               daughters = [lnode,rnode],
               score = score(lnode)*score(rnode),
               memo = ""
               }:prevlist
forwardFunctionCrossedComposition1Rule _ _ prevlist = prevlist

coordinationRule :: Node -> Node -> Node -> [Node] -> [Node]
coordinationRule lnode@(Node {cat=x1, sem=f1}) cnode@(Node {cat=c, sem=conj}) rnode@(Node {cat=x2, sem=f2}) prevlist =
  -- x1:f1  CONJ  x2:f2  ==>  x:\lambda\vec{x} (conj f1\vec{x}) f2\vec{x}
  case (x1,c,x2) of
    ((T i (S _ conj1) `SL` (T j (S _ conj2) `BS` NP [Nc])),CONJ,(T _ (S p1 conj3) `SL` (T _ (S p2 conj4) `BS` NP [Nc]))) -> 
      let conj5 = L.intersect conj1 conj3 in
      let conj6 = L.intersect conj2 conj4 in
      -- let inc = max (maximumIndex x1) (maximumIndex x2) in
      if conj5 == [] || conj6 == []
      then prevlist   
      else let newcat = (T i (S p1 conj5) `SL` (T j (S p2 conj6) `BS` NP [Nc])) in
      Node {
        rs = COORD,
        pf = T.concat [pf(lnode),pf(cnode),pf(rnode)],
        cat = newcat,
        sem = betaReduce $ transvec newcat $ betaReduce $ Lamvec (App (App conj (Appvec 0 f1)) (Appvec 0 f2)),
        daughters = [lnode,cnode,rnode],
        score = score(lnode)*score(rnode),
        memo = ""
        }:prevlist
    _ -> prevlist

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
    let inc = maximumIndex x1 in
    case unifyCategory x1 (incrementIndex x2 inc) [] of
      Nothing -> prevlist
      Just (x3,sub) -> case unifyWithHead (S anyPos nonStem) x3 of
                         Nothing -> prevlist   
                         _ -> let inc2 = maximumIndex (incrementIndex x2 inc) in
                              Node {
                                rs = COORD,
                                pf = T.concat [pf(lnode),pf(cnode),pf(rnode)],
                                cat = simulSubstituteCV x3 sub,
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
  let inc = maximumIndex t1
  case unifyCategory t1 (incrementIndex t2 inc) [] of
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

-- | 
numberOfArguments :: Cat -> Int
numberOfArguments c = case c of
  SL c1 _ -> 1 + numberOfArguments c1
  BS c1 _ -> 1 + numberOfArguments c1
  _ -> 0

-- | Implementation of CCG Unification:
-- |
-- | Usage: maximumIndex T(1)/T(3) = 3
maximumIndex :: Cat -> Int
maximumIndex c = case c of
  T i _ -> i
  SL c1 c2 -> max (maximumIndex c1) (maximumIndex c2)
  BS c1 c2 -> max (maximumIndex c1) (maximumIndex c2)
  _ -> 0
  
incrementIndex :: Cat -> Int -> Cat
incrementIndex c i = case c of
  T j u -> T (i+j) u
  SL c1 c2 -> SL (incrementIndex c1 i) (incrementIndex c2 i)
  BS c1 c2 -> BS (incrementIndex c1 i) (incrementIndex c2 i)
  cc@_ -> cc

-- | substituteCateogoryVariable T1 [1->X/Y] ==> X/Y
substituteCV :: Cat -> (Int,Cat) -> Cat
substituteCV c1 (i,c2) = case c1 of
  T j _ -> if i==j then c2 else c1
  SL ca cb -> SL (substituteCV ca (i,c2)) (substituteCV cb (i,c2))
  BS ca cb -> BS (substituteCV ca (i,c2)) (substituteCV cb (i,c2))
  _ -> c1

simulSubstituteCV :: Cat -> [(Int,Cat)] -> Cat
simulSubstituteCV c = L.foldl' substituteCV c

unifyCategory :: Cat -> Cat -> [(Int,Cat)] -> Maybe (Cat, [(Int,Cat)])
unifyCategory c1 c2 sub = case (c1,c2) of
  (NP x, NP y) -> let z = L.intersect x y in
                  if z == []
                  then Nothing
                  else Just (NP z, sub)
  (S x1 x2, S y1 y2) -> let x3 = L.intersect x1 y1 in
                        if x3 == []
                        then Nothing
                        else let y3 = L.intersect x2 y2 in
                             if y3 == []
                             then Nothing
                             else Just ((S x3 y3), sub)
  (N, N) -> Just (N, sub)
  (SL c3 c4, SL c5 c6) -> do
                          (c7,sub2) <- unifyCategory c4 c6 sub
                          (c8,sub3) <- unifyCategory c3 c5 sub2
                          return (SL c8 c7, sub3)
  (BS c3 c4, BS c5 c6) -> do
                          (c7,sub2) <- unifyCategory c4 c6 sub
                          (c8,sub3) <- unifyCategory c3 c5 sub2
                          return (BS c8 c7, sub3)
  (T i u, T j v) -> do
                    (w, _) <- unifyCategory u v []
                    return $ if i == j 
                             then (T i w, sub) 
                             else if i < j 
                                  then (T j w, (i,(T j w)):sub) 
                                  else (T i w, (j,(T i w)):sub)
  (T i u, c) -> do
                w <- unifyWithHead u c 
                return (w, (i,w):sub)
  (_, T _ _) -> unifyCategory c2 c1 sub
  (CONJ, CONJ) -> Just (CONJ, sub)
  (LPAREN, LPAREN) -> Just (LPAREN, sub)
  (RPAREN, RPAREN) -> Just (RPAREN, sub)
  _ -> Nothing

-- | Unify c1 with the head of c2
unifyWithHead :: Cat -> Cat -> Maybe Cat
unifyWithHead c1 c2 = case c2 of
  SL x y -> do 
            x' <- unifyWithHead c1 x
            return $ SL x' y
  BS x y -> do
            x' <- unifyWithHead c1 x
            return $ BS x' y
  T i u -> do
           (x',_) <- unifyCategory c1 u []
           return $ T i x'
  bc -> do
        (x',_) <- unifyCategory c1 bc []
        return x'

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
  S _ _ -> case preterm of
              Lamvec m -> deleteLambda 0 m
              m        -> m
  NP _ -> case preterm of
              Lamvec m -> deleteLambda 0 m
              m        -> m
  _ -> preterm

-- | addLambda i preterm: the first subroutine for 'transvec' function.
-- this function takes an index and a preterm, transforms the latter in a way that the Var/Appvec with an index j that is equal or greater than i 
-- Ex.
-- addLambda 1 (Appvec 0 m) = Appvec 1 (addLambda 1 m)
-- addLambda 0 (Appvec 0 m) = Appvec 0 (App () (Var 1))
addLambda :: Int -> Preterm -> Preterm
addLambda i preterm = case preterm of
  Var j      -> case () of
                  _ | j > i     -> Var (j+1)
                    | j < i     -> Var j
                    | otherwise -> Con $ T.concat [" Error: var ", T.pack (show j)]
  Pi a b     -> Pi (addLambda i a) (addLambda (i+1) b)
  Not a      -> Not (addLambda (i+1) a)
  Lam m      -> Lam (addLambda (i+1) m)
  App m n    -> App (addLambda i m) (addLambda i n)
  Sigma a b  -> Sigma (addLambda i a) (addLambda (i+1) b)
  Pair m n   -> Pair (addLambda i m) (addLambda i n)
  Proj s m   -> Proj s (addLambda i m)
  Lamvec m   -> Lamvec (addLambda (i+1) m)
  Appvec j m -> case () of
                  _ | j > i      -> Appvec (j+1) (addLambda i m)
                    | j < i      -> Appvec j (addLambda i m)
                    | otherwise  -> Appvec j (App (addLambda i m) (Var (j+1)))
  Asp j m    -> Asp j (addLambda i m)
  t -> t

-- | deleteLambda i preterm: the second subroutine for 'transvec' function.
-- this function takes an index and a preterm, transforms the latter in a way that the Var/Appvec with an index j 
deleteLambda :: Int -> Preterm -> Preterm
deleteLambda i preterm = case preterm of
  Var j      -> case () of
                  _ | j > i     -> Var (j-1)
                    | j < i     -> Var j
                    | otherwise -> Con $ T.concat ["Error: var ", T.pack (show j)]
  Pi a b     -> Pi (deleteLambda i a) (deleteLambda (i+1) b)
  Not a      -> Not (deleteLambda (i+1) a)
  Lam m      -> Lam (deleteLambda (i+1) m)
  App m n    -> App (deleteLambda i m) (deleteLambda i n)
  Sigma a b  -> Sigma (deleteLambda i a) (deleteLambda (i+1) b)
  Pair m n   -> Pair (deleteLambda i m) (deleteLambda i n)
  Proj s m   -> Proj s (deleteLambda i m)
  Lamvec m   -> Lamvec (deleteLambda (i+1) m)
  Appvec j m -> case () of
                  _ | j > i     -> Appvec (j-1) (deleteLambda i m)
                    | j < i     -> Appvec j (deleteLambda i m)
                    | otherwise -> deleteLambda i m
  Asp j m    -> Asp j (deleteLambda i m)
  t -> t


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

