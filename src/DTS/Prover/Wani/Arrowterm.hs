{-# LANGUAGE  TypeSynonymInstances, FlexibleInstances  #-}

module DTS.Prover.Wani.Arrowterm
(
  Arrowterm(..),
  AJudgment(..),
  aVar,
  aCon,
  aType,
  dt2Arrow,
  arrow2DT,
  arrowNotat,
  a2dtJudgment,
  dt2aJudgment,
  Arrowrule(..),
  aTreeTojTree',
  jTreeToaTree',
  fromDT2A,
  SUEnv,
  AEnv,
  SAEnv,
  Context,
  contextLen,
  sameCon,
  sameTerm,
  envfromAJudgment,
  termfromAJudgment,
  typefromAJudgment,
  shiftIndices,
  arrowSubst,
  ArrowSelector(..),
  downSide',
  reduce,
  subst,
  changeDownSide',
  genFreeCon,
  isFreeCon,
  canBeSame,
  canBeSame',
  addLam,
  addLam',
  rmLam,
  addApp,
  thereIsVar,
  varsInaTerm,
  boundUpLim,
  betaReduce
) where

import qualified DTS.DTTdeBruijn as DdB   -- DTT
import qualified Data.Text.Lazy as T      -- text
import qualified Data.List as L           -- base
import Interface.Text ( SimpleText(..) )
import qualified Data.Bifunctor
import qualified Data.List.Split as S
import qualified Debug.Trace as D
import qualified Interface.Tree as UDT
import qualified DTS.QueryTypes as QT
import qualified Data.Maybe as M

data ArrowSelector = ArrowFst | ArrowSnd deriving (Eq, Show)
-- | Arrowterm
data Arrowterm =
  Conclusion DdB.Preterm -- ^ 末尾の部分
  | ArrowSigma' AEnv Arrowterm 
  | ArrowApp Arrowterm Arrowterm -- ^ App型
  | ArrowPair Arrowterm Arrowterm -- ^ Pair型
  | ArrowProj ArrowSelector Arrowterm -- ^ Proj型
  | ArrowLam Arrowterm -- ^ Lam型
  | Arrow AEnv  Arrowterm -- ^ Pi型
  | ArrowEq Arrowterm Arrowterm Arrowterm -- ^ 型 項1 項2 型
  deriving (Eq)
-- | DTT.Preterm型に変換して得たテキストを加工している
instance Show Arrowterm where
  show term  = show $ arrow2DT term -- prestr2arrowstr ((filter (/= ' ') . show . arrow2DT) term) term

allReplace :: T.Text -> [(T.Text,T.Text)] -> T.Text
allReplace base=foldr (\(from,to) str' -> T.replace from to str') base

boundUpLim :: Arrowterm -> Int
boundUpLim term =
  case (betaReduce . arrowNotat) term of
    Conclusion a -> 0
    (ArrowSigma' a b) -> minimum $ (negate (length a) + boundUpLim b): map (\(num,a') -> (-num) +  boundUpLim a') (zip [0..] a)
    (ArrowPair h t)-> minimum [boundUpLim h,boundUpLim t]
    (ArrowApp a b) -> minimum[boundUpLim a,boundUpLim b]
    (ArrowProj s p) -> boundUpLim p
    (ArrowLam p) -> -1 + boundUpLim p
    (Arrow a b) -> minimum $ (negate (length a) + boundUpLim b): map (\(num,a') -> (-num) +  boundUpLim a') (zip [0..] a)
    (ArrowEq a b t) -> minimum [boundUpLim a,boundUpLim b]

-- | DTT.Preterm型に変換して得たテキストとArrowtermを比較して加工している
prestr2arrowstr :: String -> Arrowterm -> String
prestr2arrowstr prestr aTerm = undefined
    -- case aTerm of 
    --   (Conclusion p) -> dropWhile (not . (`elem` (['0'..'z']++['(',')','\8869']))) prestr 
    --   (Arrow env r) ->
    --     if null env
    --     then
    --       prestr2arrowstr  prestr r
    --     else
    --       let parentheses =  take (length env) (treatParentheses prestr  '(' ')' ) 
    --       in "[ " ++
    --           tail
    --             (foldr (
    --               (\ a b -> ", "  ++ a  ++ b) .
    --                 (\z ->
    --                   let str = (snd $ snd z)
    --                   in takeWhile (/= ':')  str ++":" ++  prestr2arrowstr (tail $ dropWhile (/= ':') str) (fst z)))
    --               "" (zip (reverse env) parentheses)) ++
    --           " ] => " ++ prestr2arrowstr  (tail $ drop ((fst . last) parentheses) prestr) r
    --   (ArrowSigma' env r) ->
    --     if null env
    --     then
    --       prestr2arrowstr  prestr r
    --     else
    --       let parentheses =  take (length env) (treatParentheses prestr  '(' ')' )
    --       in "[ " ++
    --           tail
    --             (foldr (
    --               (\ a b -> ", "  ++ a  ++ b) .
    --                 (\z ->
    --                   let str = (snd $ snd z)
    --                   in takeWhile (/= ':')  str ++":" ++  prestr2arrowstr (tail $ dropWhile (/= ':') str) (fst z)))
    --               "" (zip (reverse env) parentheses)) ++
    --           " ] × " ++ prestr2arrowstr  (tail $ drop ((fst . last) parentheses) prestr) r
    --   (ArrowApp h t) -> 
    --     case treatParentheses prestr '(' ')' of
    --       [(cnt,st)] -> 
    --         let tPst = treatParentheses st '(' ')' 
    --             args' = S.splitOn "," $T.unpack $allReplace (T.pack st) (map (\(num,str) -> (T.pack str,T.pack $ "(" ++ show num ++ ")")) tPst)
    --             (arg:args)  = map (\str -> T.unpack $ allReplace (T.pack str) (map (\(num,inSide) -> (T.pack $ "(" ++ (show num) ++ ")" ,T.pack $inSide)) tPst)) args'
    --             f = takeWhile (\ch -> ch /= '(') prestr
    --             result =  "" ++ prestr2arrowstr (f ++ (if null args then "" else "(" ++ drop (1 + length arg) st ++ ")"))  h ++ ("(" ++prestr2arrowstr arg t++")")
    --         in result
    --       _ -> "表示エラー : " ++ prestr
    --   (ArrowProj s t) -> 
    --     let parentheses = head (treatParentheses prestr '(' ')' )
    --     in take 2 prestr ++"(" ++ prestr2arrowstr (snd parentheses) t ++ ")"
    --   (ArrowLam t) -> 
    --     takeWhile (/= '.') prestr ++ ".(" ++ prestr2arrowstr  (tail $ dropWhile (/= '.') prestr) t ++")"
    --   (ArrowPair h t) ->
    --     let contents = init $ tail $prestr
    --         trCon = treatParentheses contents  '(' ')' 
    --         strs = S.splitOn "," $ T.unpack $ allReplace (T.pack contents) (map (\(num,str) -> (T.pack str,T.pack $ "(" ++ (show num) ++ ")")) trCon)
    --         hstr:tstr = map (\str' -> T.unpack $ allReplace (T.pack str') (map (\(num,str) -> (T.pack $ "(" ++ (show num) ++ ")",T.pack str)) trCon)) strs 
    --     in
    --       "("++prestr2arrowstr hstr h++","++prestr2arrowstr (head tstr) t++")"
    --   (ArrowEq a b t) -> prestr

-- | tP input :(u1:A)→(u2:B(u1))×C(u2,u1) output : [(6,"(u1:A)"),(17,"(u2:B(u1))"),(26,"(u2,u1)")]
treatParentheses :: String -> Char -> Char  -> [(Int,String)]
treatParentheses str l r=
  map snd $ filter (\((a,_),_)-> a /= 0) $ foldr (\ch (((num,cnt),(pl,st)) : lst) -> let num' = if ch == l then num - 1 else if ch ==  r then  num + 1 else num; in if (num == num' || (num > 0 &&num' >0)) then ((num',cnt+1),(pl,ch:st)):lst else ((num',cnt + 1),(length str - cnt,[])):((num,cnt),(pl,st)):lst) [((0,0),(0," "))] str

aVar :: Int -> Arrowterm
aVar num = Conclusion (DdB.Var num)

aCon :: T.Text -> Arrowterm
aCon txt =  Conclusion (DdB.Con txt)

aType :: Arrowterm
aType = Conclusion DdB.Type

dtToArrowSelector :: DdB.Selector -> ArrowSelector
dtToArrowSelector DdB.Fst = ArrowFst
dtToArrowSelector DdB.Snd = ArrowSnd

dtNotatSelector :: ArrowSelector -> DdB.Selector
dtNotatSelector ArrowFst = DdB.Fst
dtNotatSelector ArrowSnd = DdB.Snd

arrow2DT :: Arrowterm -> (DdB.Preterm)
arrow2DT (Conclusion a) = a
arrow2DT (ArrowSigma' [] t)= arrow2DT t
arrow2DT (ArrowSigma' (f:r) t)= arrow2DT (ArrowSigma' r (Conclusion (DdB.Sigma (arrow2DT f)  (arrow2DT t))))
arrow2DT (ArrowPair h t)= DdB.Pair (arrow2DT h) (arrow2DT t)
arrow2DT (ArrowApp a b) = DdB.App (arrow2DT a) (arrow2DT b)
arrow2DT (ArrowProj s p) = DdB.Proj (dtNotatSelector s) (arrow2DT p)
arrow2DT (ArrowLam p) = DdB.Lam (arrow2DT p)
arrow2DT (Arrow [] t) = arrow2DT t
arrow2DT (Arrow (f:r) t) = arrow2DT (Arrow r (Conclusion (DdB.Pi (arrow2DT f)  (arrow2DT t))))
arrow2DT (ArrowEq a b t) = DdB.Eq (arrow2DT a) (arrow2DT b) (arrow2DT t)

thereIsVar :: Int -> Arrowterm -> Bool 
thereIsVar num aTerm= case aTerm of
  Conclusion pr -> pr == DdB.Var num
  ArrowSigma' ars ar -> 
    let arsLen = length ars
    in or $ zipWith (\num' a ->  thereIsVar (num + arsLen - num') a) [0..] (ar:ars)
  ArrowApp ar ar' -> any (thereIsVar num) [ar,ar']
  ArrowPair ar ar' -> any (thereIsVar num) [ar,ar']
  ArrowProj as ar -> thereIsVar num ar
  ArrowLam ar -> thereIsVar (num+1) ar
  Arrow ars ar -> 
    let arsLen = length ars
    in or $ zipWith (\num' a ->  thereIsVar (num + arsLen - num') a) [0..] (ar:ars)
  ArrowEq ar ar' ar2 -> any (thereIsVar num) [ar,ar',ar2]

varsInaTerm :: Arrowterm -> [Int]
varsInaTerm aTerm = L.nub $ varsInaTerm' 0 $arrowNotat aTerm

varsInaTerm' :: Int ->Arrowterm ->  [Int]
varsInaTerm' base aTerm = 
  case aTerm of
    Conclusion (DdB.Var num) -> [num-base]
    Conclusion _ -> []
    ArrowSigma' ars ar -> concatMap (\(num,a) -> (varsInaTerm' (base+num) a)) (zip [0..] $reverse $ ar:ars)
    ArrowApp ar ar' -> concatMap (varsInaTerm' base) [ar,ar'] 
    ArrowPair ar ar' -> concatMap (varsInaTerm' base) [ar,ar'] 
    ArrowProj as ar -> varsInaTerm' base ar
    ArrowLam ar -> varsInaTerm' (base+1) ar
    Arrow ars ar -> concatMap (\(num,a) -> varsInaTerm' (base+num) a) (zip [0..] $reverse $ ar:ars)
    ArrowEq ar ar' ar2 -> concatMap (varsInaTerm' base) [ar,ar',ar2]  


-- | 入力された(DdB.Preterm)をArrowTermに変換する
dt2Arrow :: DdB.Preterm -> Arrowterm
dt2Arrow DdB.Type = Conclusion DdB.Type
dt2Arrow (DdB.Var i) = Conclusion $ DdB.Var i
dt2Arrow (DdB.Con i) = Conclusion $ DdB.Con i
dt2Arrow (DdB.Not i) =Arrow [dt2Arrow i] $Conclusion DdB.Bot
dt2Arrow (DdB.Pi h t) =
  case dt2Arrow t of
    Arrow env t' -> Arrow (env ++ [dt2Arrow h]) t'
    t' -> Arrow [dt2Arrow h] t'
dt2Arrow (DdB.Sigma h t) =
  case dt2Arrow t of
    ArrowSigma' env t' -> ArrowSigma' (env ++ [dt2Arrow h]) t'
    t' -> ArrowSigma' [dt2Arrow h] t'
dt2Arrow (DdB.App a b) =
  ArrowApp (dt2Arrow a) (dt2Arrow b)
dt2Arrow (DdB.Pair a b) =
  ArrowPair (dt2Arrow a) (dt2Arrow b)
dt2Arrow (DdB.Proj selector p) =
  ArrowProj (dtToArrowSelector selector) (dt2Arrow p)
dt2Arrow (DdB.Lam p) =
  ArrowLam (dt2Arrow p)
dt2Arrow (DdB.Eq a b t) =
  ArrowEq (dt2Arrow a) (dt2Arrow b) (dt2Arrow t)
dt2Arrow dt= Conclusion dt

arrowNotat :: Arrowterm -> Arrowterm
arrowNotat (Arrow [] a) = a
arrowNotat (Arrow a (Arrow b c)) = arrowNotat (Arrow (b ++ a) c)
arrowNotat (ArrowSigma' [] a) = a
arrowNotat (ArrowSigma' a (ArrowSigma' b c)) = arrowNotat (ArrowSigma' (b ++ a) c)
arrowNotat a = a

betaReduce :: Arrowterm -> Arrowterm
betaReduce aterm = case aterm of
    Conclusion t -> Conclusion t
    ArrowSigma' ars ar -> ArrowSigma' (map betaReduce ars) (betaReduce ar)
    ArrowApp ar ar' -> case betaReduce ar of 
      ArrowLam a -> betaReduce $ shiftIndices (arrowSubst a (shiftIndices ar' 1 0) (Conclusion $DdB.Var 0)) (-1) 0
      e -> ArrowApp e (betaReduce ar')
    ArrowPair ar ar' -> ArrowPair (betaReduce ar) (betaReduce ar')
    ArrowProj as ar ->  case betaReduce ar of
      ArrowPair x y -> case as of ArrowFst -> x ; ArrowSnd -> y
      e -> ArrowProj as e
    ArrowLam ar -> ArrowLam (betaReduce ar)
    Arrow ars ar -> Arrow (map betaReduce ars) (betaReduce ar)
    ArrowEq ar ar' ar2 ->  ArrowEq (betaReduce ar) (betaReduce ar') (betaReduce ar2)

--  In `fromDT2A`, I use `DdB.toDTT` and `DdB.toUDTT` and the term convert into `(DdB.Preterm)` -> `(DdB.Preterm DdB.UDTT)` -> `(DdB.Preterm)`.
--  This implementation let me using existed function, `DdB.betaReduce`.
--  `A.betaReduce` is used to format a term about an list
fromDT2A :: DdB.Preterm -> Arrowterm
fromDT2A = betaReduce . arrowNotat . dt2Arrow . DdB.betaReduce

shiftIndices :: Arrowterm -> Int -> Int -> Arrowterm
shiftIndices term d i= (arrowNotat . dt2Arrow) $ DdB.shiftIndices (arrow2DT term) d i

reduce :: Arrowterm -> Arrowterm
reduce = dt2Arrow . DdB.betaReduce . arrow2DT

type SUEnv = [(T.Text, DdB.Preterm)]
type AEnv = [Arrowterm]
type SAEnv = [(T.Text,Arrowterm)]
type Context = (SAEnv,AEnv)

contextLen :: Context -> Int
contextLen (sigCon,varCon) = length sigCon + length varCon 

{--
instance SimpleText SAEnv where
  toText sigs = toText (map (Data.Bifunctor.second (DdB.toUDTT . arrow2DT)) sigs)
--}

instance SimpleText SAEnv where
  toText sigs = undefined

data AJudgment =
  AJudgment
  SAEnv
  AEnv
  Arrowterm -- ^ term
  Arrowterm -- ^ type
    deriving (Eq,Show)

a2dtJudgment :: AJudgment -> DdB.Judgment
a2dtJudgment (AJudgment con env aterm atype) = DdB.Judgment (map (\(a,b)->(a,arrow2DT b)) con) (map arrow2DT env) (arrow2DT aterm) (arrow2DT atype)

dt2aJudgment :: DdB.Judgment -> AJudgment
dt2aJudgment (DdB.Judgment con env dtterm dttype) = AJudgment (map (\(a,b) -> (a, dt2Arrow b)) con) (map dt2Arrow env) (dt2Arrow dtterm) (dt2Arrow dttype)

typefromAJudgment :: AJudgment -> Arrowterm
typefromAJudgment ( AJudgment con env aterm atype) = atype

termfromAJudgment :: AJudgment -> Arrowterm
termfromAJudgment ( AJudgment con env aterm atype) = aterm

envfromAJudgment :: AJudgment -> Context
envfromAJudgment ( AJudgment con env aterm atype) = (con,env)

dnPr = DdB.Pi DdB.Type (DdB.Pi (DdB.Pi (DdB.Pi (DdB.Var 0) DdB.Bot) DdB.Bot) (DdB.Var 1))

subst :: DdB.Preterm -> DdB.Preterm -> DdB.Preterm -> DdB.Preterm 
subst preterm l i =
  if preterm == i then
    l
  else
    case preterm of
      DdB.Pi a b -> DdB.Pi (subst a l i) (subst b (DdB.shiftIndices l 1 0)  (DdB.shiftIndices i 1 0))
      DdB.Not m -> DdB.Not (subst m l i)
      DdB.Lam m -> DdB.Lam (subst m (DdB.shiftIndices l 1 0) (DdB.shiftIndices i 1 0))
      DdB.App m n    -> DdB.App (subst m l i) (subst n l i)
      DdB.Sigma a b  -> DdB.Sigma (subst a l i) (subst b (DdB.shiftIndices l 1 0) (DdB.shiftIndices i 1 0))
      DdB.Pair m n   -> DdB.Pair (subst m l i) (subst n l i)
      DdB.Proj s m   -> DdB.Proj s (subst m l i)
      DdB.Eq a m n   -> DdB.Eq (subst a l i) (subst m l i) (subst n l i)
      others -> others

arrowSubst :: Arrowterm -- ^ origin
  -> Arrowterm -- ^ 代入内容
  -> Arrowterm -- ^ 代入先
  -> Arrowterm
arrowSubst term i m= 
  dt2Arrow $ subst (arrow2DT term) (arrow2DT i) (arrow2DT m)

type Arrowrule = QT.DTTrule

downSide' ::  UDT.Tree Arrowrule AJudgment -> AJudgment
downSide' (UDT.Tree label node daughters) = node

changeDownSide' :: UDT.Tree Arrowrule AJudgment -> AJudgment -> UDT.Tree Arrowrule AJudgment
changeDownSide' (UDT.Tree label node daughters) newnode = UDT.Tree label newnode daughters

aTreeTojTree' :: UDT.Tree Arrowrule AJudgment -> UDT.Tree QT.DTTrule DdB.Judgment
aTreeTojTree' (UDT.Tree label node daughters)= UDT.Tree label (a2dtJudgment node) (map aTreeTojTree' daughters)

jTreeToaTree' :: UDT.Tree QT.DTTrule DdB.Judgment -> UDT.Tree Arrowrule AJudgment
jTreeToaTree' (UDT.Tree label node daughters)=UDT.Tree label (dt2aJudgment node) (map jTreeToaTree' daughters)

-- | generate free constraint from given word
genFreeCon ::  Arrowterm -- ^ term
  -> String -- ^ "hoge"
  ->  Arrowterm
genFreeCon term hoge =
  if isFreeCon term ( Conclusion $ DdB.Con $ T.pack hoge)
    then
       Conclusion $ DdB.Con $ T.pack hoge
    else
      genFreeCon term $hoge++hoge

-- | whether DdB.Con "hoge" is free or not
isFreeCon ::  Arrowterm  -- ^ term
  ->  Arrowterm  -- ^ DdB.Con "hoge"
  -> Bool
isFreeCon term con=
  let term' =  arrowSubst term con con
      term'' =  arrowSubst term ( Conclusion $ DdB.Var 0) con
  in term' == term''

--形だけ比較
canBeSame :: Int ->   Arrowterm ->   Arrowterm -> Bool
canBeSame _ (  Conclusion DdB.Top) (  Conclusion DdB.Top) = True
canBeSame _ (  Conclusion DdB.Bot) (  Conclusion DdB.Bot) = True
canBeSame _ (  Conclusion DdB.Type) (  Conclusion DdB.Type) = True
canBeSame lim (  Conclusion (DdB.Var anum)) (  Conclusion (DdB.Var anum')) =
  anum == anum' || anum <= lim
canBeSame _ (Conclusion (DdB.Con t)) (Conclusion (DdB.Con t')) = t == t'
canBeSame lim (  Conclusion (DdB.Var anum)) a = 
  case a of 
    ArrowEq _ _ _ -> False
    _ -> True
  -- anum <= lim
canBeSame lim a (  Conclusion (DdB.Var anum))  = 
  case a of 
    ArrowEq _ _ _ -> False
    _ -> True
  -- anum <= lim
canBeSame lim (  ArrowSigma' con a) (  ArrowSigma' con' a') =
  all (\(num,(s,t)) -> canBeSame (lim+num) s t) (zip [0..] (zip (a:con) (a':con')))
canBeSame lim (  ArrowApp a1 a2) (  ArrowApp a1' a2')=
   canBeSame lim a1 a1' && canBeSame lim a2 a2'
canBeSame lim (  ArrowProj s a) (  ArrowProj s' a') =
  s == s' && canBeSame lim a a'
canBeSame lim (  ArrowPair a1 a2) (  ArrowPair a1' a2')=
  canBeSame lim a1 a1' && canBeSame lim a2 a2'
canBeSame lim (  ArrowLam a) (  ArrowLam a') = canBeSame lim a a'
canBeSame lim (  Arrow con a) (  Arrow con' a') =
   all (\(num,(s,t)) -> canBeSame (lim+num) s t) (zip [0..] (zip (a:con) (a':con')))
canBeSame lim (  ArrowEq a b t) (  ArrowEq a' b' t') =
  canBeSame lim a a' && canBeSame lim b b' && canBeSame lim t t'
canBeSame lim other other' = False

--形だけ比較
canBeSame' :: Int ->   Arrowterm ->   Arrowterm -> [(Arrowterm,Arrowterm)]
canBeSame' _ (  Conclusion DdB.Top) (  Conclusion DdB.Top) = []
canBeSame' _ (  Conclusion DdB.Bot) (  Conclusion DdB.Bot) = []
canBeSame' _ (  Conclusion DdB.Type) (  Conclusion DdB.Type) = []
canBeSame' lim (  Conclusion (DdB.Var anum)) (  Conclusion (DdB.Var anum')) =[(aVar anum,aVar anum')]
canBeSame' _ (Conclusion (DdB.Con t)) (Conclusion (DdB.Con t')) = [(aCon t,aCon t')]
canBeSame' lim (  Conclusion (DdB.Var anum)) a = [(aVar anum,a)]
canBeSame' lim a (  Conclusion (DdB.Var anum))  = [(a,aVar anum)]
canBeSame' lim (  ArrowSigma' con a) (  ArrowSigma' con' a') =
  concatMap (\(num,(s,t)) -> canBeSame' (lim+num) s t) (zip [0..] (zip (a:con) (a':con')))
canBeSame' lim (  ArrowApp a1 a2) (  ArrowApp a1' a2')=
   canBeSame' lim a1 a1' ++ canBeSame' lim a2 a2'
canBeSame' lim (  ArrowProj s a) (  ArrowProj s' a') =
  if s == s' then canBeSame' lim a a' else [(ArrowProj s a, ArrowProj s' a')]
canBeSame' lim (  ArrowPair a1 a2) (  ArrowPair a1' a2')=
  canBeSame' lim a1 a1' ++ canBeSame' lim a2 a2'
canBeSame' lim (  ArrowLam a) (  ArrowLam a') = canBeSame' lim a a'
canBeSame' lim (  Arrow con a) (  Arrow con' a') =
   concatMap (\(num,(s,t)) -> canBeSame' (lim+num) s t) (zip [0..] (zip (a:con) (a':con')))
canBeSame' lim (  ArrowEq a b t) (  ArrowEq a' b' t') =
  canBeSame' lim a a' ++ canBeSame' lim b b' ++ canBeSame' lim t t'
canBeSame' lim other other' = [(other,other')]

addApp ::Int -> Arrowterm -> Arrowterm
-- addApp 0 base = base
-- addApp num base = addApp (num - 1) $ ArrowApp (shiftIndices base 1 ((minimum $ varsInaTerm base) -1)) (Conclusion $ DdB.Var 0)
addApp num base = foldl (\b a -> ArrowApp b (Conclusion $ DdB.Var a)) base $reverse [0..(num-1)]

addLam :: Int -> Arrowterm -> Arrowterm
addLam num term =
  if num > 0 then ArrowLam $ addLam (num - 1) (shiftIndices term 1 ((minimum $ varsInaTerm term) -1)) else term

addLam' :: Int -> Arrowterm -> Arrowterm
addLam' 0 term = term
addLam' num term = ArrowLam $ addLam' (num - 1) term

rmLam :: Int -> M.Maybe Arrowterm -> M.Maybe Arrowterm
rmLam 0 term = term
rmLam num term =
  case term of
    M.Just (ArrowLam a) -> maybe M.Nothing M.Just (rmLam (num-1) $ M.Just a)
    _ -> M.Nothing

sameCon :: Context -> Context -> Bool
sameCon (sigCon1,varCon1) (sigCon2,varCon2) = 
  let varCon = zip (reverse varCon1) (reverse varCon2)
      sigCon = zip (reverse sigCon1) (reverse sigCon2)
  in 
    foldl (\b (sig1,sig2) -> b && sig1 == sig2) (foldl (\b (var1,var2) -> b && arrowNotat var1 == arrowNotat var2) True varCon) sigCon

sameTerm :: (Context,Arrowterm) -> (Context,Arrowterm) -> Bool
sameTerm (con1,term1) (con2,term2) =
  let len1 = contextLen con1
      len2 =  contextLen con2
    in 
      (sameCon con1 con2) &&
        if len1 > len2 
          then  arrowNotat term1 == shiftIndices (arrowNotat term2) (len1- len2) 0 
          else   arrowNotat term2 == shiftIndices (arrowNotat term1) (len2- len1) 0
