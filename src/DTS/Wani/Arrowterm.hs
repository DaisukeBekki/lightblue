{-# LANGUAGE  TypeSynonymInstances, FlexibleInstances  #-}

module DTS.Wani.Arrowterm
(
  Arrowterm(..),
  AJudgement(..),
  aVar,
  aCon,
  aType,
  dt2Arrow,
  arrow2DT,
  arrowNotat,
  a2dtJudgement,
  dt2aJudgement,
  aTreeTojTree,
  jTreeToaTree,
  fromDT2A,
  SUEnv,
  AEnv,
  SAEnv,
  Context,
  contextLen,
  sameCon,
  sameTerm,
  envfromAJudgement,
  termfromAJudgement,
  typefromAJudgement,
  a2VNJudgment,
  shiftIndices,
  arrowSubst,
  ArrowSelector(..),
  upSide,
  downSide,
  reduce,
  subst,
  changeDownSide,
  -- Normterm(..),
  -- arrow2Normterm,
  genFreeCon,
  isFreeCon,
  canBeSame,
  aj,
  addLam,
  addApp,
  thereIsVar,
  varsInaTerm,
  betaReduce
) where

import qualified DTS.DTT as DT            -- DTT
import qualified DTS.UDTT as UD            -- DTT
import qualified Data.Text.Lazy as T      -- text
import qualified Data.List as L           -- base
import qualified DTS.Prover_daido.Judgement as J
import Interface.Text ( SimpleText(..) )
import qualified Data.Bifunctor
import qualified DTS.UDTTwithName as VN
import qualified Data.List.Split as S
import qualified Debug.Trace as D



data ArrowSelector = ArrowFst | ArrowSnd deriving (Eq, Show)
-- | Arrowterm
data Arrowterm =
  Conclusion DT.Preterm -- ^ 末尾の部分
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
  show term  = prestr2arrowstr ((filter (/= ' ') . show . arrow2DT) term) term

allReplace :: T.Text -> [(T.Text,T.Text)] -> T.Text
allReplace base=foldr (\(from,to) str' -> T.replace from to str') base

-- | DTT.Preterm型に変換して得たテキストとArrowtermを比較して加工している
prestr2arrowstr :: String -> Arrowterm -> String
prestr2arrowstr prestr aTerm =
    case aTerm of 
      (Conclusion p) -> dropWhile (not . (`elem` (['0'..'z']++['(',')','\8869']))) prestr 
      (Arrow env r) ->
        if null env
        then
          prestr2arrowstr  prestr r
        else
          let parentheses =  take (length env) (treatParentheses prestr  '(' ')' ) 
          in "[ " ++
              tail
                (foldr (
                  (\ a b -> ", "  ++ a  ++ b) .
                    (\z ->
                      let str = (snd $ snd z)
                      in takeWhile (/= ':')  str ++":" ++  prestr2arrowstr (tail $ dropWhile (/= ':') str) (fst z)))
                  "" (zip (reverse env) parentheses)) ++
              " ] => " ++ prestr2arrowstr  (tail $ drop ((fst . last) parentheses) prestr) r
      (ArrowSigma' env r) ->
        if null env
        then
          prestr2arrowstr  prestr r
        else
          let parentheses =  take (length env) (treatParentheses prestr  '(' ')' )
          in "[ " ++
              tail
                (foldr (
                  (\ a b -> ", "  ++ a  ++ b) .
                    (\z ->
                      let str = (snd $ snd z)
                      in takeWhile (/= ':')  str ++":" ++  prestr2arrowstr (tail $ dropWhile (/= ':') str) (fst z)))
                  "" (zip (reverse env) parentheses)) ++
              " ] × " ++ prestr2arrowstr  (tail $ drop ((fst . last) parentheses) prestr) r
      (ArrowApp h t) -> 
        case treatParentheses prestr '(' ')' of
          [(cnt,st)] -> 
            let tPst = treatParentheses st '(' ')' 
                args' = S.splitOn "," $T.unpack $allReplace (T.pack st) (map (\(num,str) -> (T.pack str,T.pack $ "(" ++ show num ++ ")")) tPst)
                (arg:args)  = map (\str -> T.unpack $ allReplace (T.pack str) (map (\(num,inSide) -> (T.pack $ "(" ++ (show num) ++ ")" ,T.pack $inSide)) tPst)) args'
                f = takeWhile (\ch -> ch /= '(') prestr
                result =  "" ++ prestr2arrowstr (f ++ (if null args then "" else "(" ++ drop (1 + length arg) st ++ ")"))  h ++ ("(" ++prestr2arrowstr arg t++")")
            in result
          _ -> "表示エラー : " ++ prestr
      (ArrowProj s t) -> 
        let parentheses = head (treatParentheses prestr '(' ')' )
        in take 2 prestr ++"(" ++ prestr2arrowstr (snd parentheses) t ++ ")"
      (ArrowLam t) -> 
        takeWhile (/= '.') prestr ++ ".(" ++ prestr2arrowstr  (tail $ dropWhile (/= '.') prestr) t ++")"
      (ArrowPair h t) ->
        let contents = init $ tail $prestr
            trCon = treatParentheses contents  '(' ')' 
            strs = S.splitOn "," $ T.unpack $ allReplace (T.pack contents) (map (\(num,str) -> (T.pack str,T.pack $ "(" ++ (show num) ++ ")")) trCon)
            hstr:tstr = map (\str' -> T.unpack $ allReplace (T.pack str') (map (\(num,str) -> (T.pack $ "(" ++ (show num) ++ ")",T.pack str)) trCon)) strs 
        in
          "("++prestr2arrowstr hstr h++","++prestr2arrowstr (head tstr) t++")"
      (ArrowEq a b t) -> prestr

-- | tP input :(u1:A)→(u2:B(u1))×C(u2,u1) output : [(6,"(u1:A)"),(17,"(u2:B(u1))"),(26,"(u2,u1)")]
treatParentheses :: String -> Char -> Char  -> [(Int,String)]
treatParentheses str l r=
  map snd $ filter (\((a,_),_)-> a /= 0) $ foldr (\ch (((num,cnt),(pl,st)) : lst) -> let num' = if ch == l then num - 1 else if ch ==  r then  num + 1 else num; in if (num == num' || (num > 0 &&num' >0)) then ((num',cnt+1),(pl,ch:st)):lst else ((num',cnt + 1),(length str - cnt,[])):((num,cnt),(pl,st)):lst) [((0,0),(0," "))] str

aVar :: Int -> Arrowterm
aVar num = Conclusion (DT.Var num)

aCon :: T.Text -> Arrowterm
aCon txt =  Conclusion (DT.Con txt)

aType :: Arrowterm
aType = Conclusion DT.Type

dtToArrowSelector :: DT.Selector -> ArrowSelector
dtToArrowSelector DT.Fst = ArrowFst
dtToArrowSelector DT.Snd = ArrowSnd

dtNotatSelector :: ArrowSelector -> DT.Selector
dtNotatSelector ArrowFst = DT.Fst
dtNotatSelector ArrowSnd = DT.Snd

arrow2DT :: Arrowterm -> DT.Preterm
arrow2DT (Conclusion a) = a
arrow2DT (ArrowSigma' [] t)= arrow2DT t
arrow2DT (ArrowSigma' (f:r) t)= arrow2DT (ArrowSigma' r (Conclusion (DT.Sigma (arrow2DT f)  (arrow2DT t))))
arrow2DT (ArrowPair h t)= DT.Pair (arrow2DT h) (arrow2DT t)
arrow2DT (ArrowApp a b) = DT.App (arrow2DT a) (arrow2DT b)
arrow2DT (ArrowProj s p) = DT.Proj (dtNotatSelector s) (arrow2DT p)
arrow2DT (ArrowLam p) = DT.Lam (arrow2DT p)
arrow2DT (Arrow [] t) = arrow2DT t
arrow2DT (Arrow (f:r) t) = arrow2DT (Arrow r (Conclusion (DT.Pi (arrow2DT f)  (arrow2DT t))))
arrow2DT (ArrowEq a b t) = DT.Eq (arrow2DT a) (arrow2DT b) (arrow2DT t)

thereIsVar :: Int -> Arrowterm -> Bool 
thereIsVar num aTerm= case aTerm of
  Conclusion pr -> pr == DT.Var num
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
    Conclusion (DT.Var num) -> [num-base]
    Conclusion _ -> []
    ArrowSigma' ars ar -> concatMap (\(num,a) -> (varsInaTerm' (base+num) a)) (zip [0..] $reverse $ ar:ars)
    ArrowApp ar ar' -> concatMap (varsInaTerm' base) [ar,ar'] 
    ArrowPair ar ar' -> concatMap (varsInaTerm' base) [ar,ar'] 
    ArrowProj as ar -> varsInaTerm' base ar
    ArrowLam ar -> varsInaTerm' (base+1) ar
    Arrow ars ar -> concatMap (\(num,a) -> varsInaTerm' (base+num) a) (zip [0..] $reverse $ ar:ars)
    ArrowEq ar ar' ar2 -> concatMap (varsInaTerm' base) [ar,ar',ar2]  


-- | 入力されたDT.PretermをArrowTermに変換する
dt2Arrow :: DT.Preterm -> Arrowterm
dt2Arrow DT.Type = Conclusion DT.Type
dt2Arrow (DT.Var i) = Conclusion $ DT.Var i
dt2Arrow (DT.Con i) = Conclusion $ DT.Con i
dt2Arrow (DT.Not i) =Arrow [dt2Arrow i] $Conclusion DT.Bot
dt2Arrow (DT.Pi h t) =
  case dt2Arrow t of
    Arrow env t' -> Arrow (env ++ [dt2Arrow h]) t'
    t' -> Arrow [dt2Arrow h] t'
dt2Arrow (DT.Sigma h t) =
  case dt2Arrow t of
    ArrowSigma' env t' -> ArrowSigma' (env ++ [dt2Arrow h]) t'
    t' -> ArrowSigma' [dt2Arrow h] t'
dt2Arrow (DT.App a b) =
  ArrowApp (dt2Arrow a) (dt2Arrow b)
dt2Arrow (DT.Pair a b) =
  ArrowPair (dt2Arrow a) (dt2Arrow b)
dt2Arrow (DT.Proj selector p) =
  ArrowProj (dtToArrowSelector selector) (dt2Arrow p)
dt2Arrow (DT.Lam p) =
  ArrowLam (dt2Arrow p)
dt2Arrow (DT.Eq a b t) =
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
      ArrowLam a -> betaReduce $ shiftIndices (arrowSubst a (shiftIndices ar' 1 0) (Conclusion $DT.Var 0)) (-1) 0
      e -> ArrowApp e (betaReduce ar')
    ArrowPair ar ar' -> ArrowPair (betaReduce ar) (betaReduce ar')
    ArrowProj as ar ->  case betaReduce ar of
      ArrowPair x y -> case as of ArrowFst -> x ; ArrowSnd -> y
      e -> ArrowProj as e
    ArrowLam ar -> ArrowLam (betaReduce ar)
    Arrow ars ar -> Arrow (map betaReduce ars) (betaReduce ar)
    ArrowEq ar ar' ar2 ->  ArrowEq (betaReduce ar) (betaReduce ar') (betaReduce ar2)

--  In `fromDT2A`, I use `DT.toDTT` and `DT.toUDTT` and the term convert into `DT.Preterm` -> `UD.Preterm` -> `DT.Preterm`.
--  This implementation let me using existed function, `UD.betaReduce`.
--  `A.betaReduce` is used to format a term about an list
fromDT2A :: DT.Preterm -> Arrowterm
fromDT2A = betaReduce . arrowNotat . dt2Arrow . DT.toDTT . UD.betaReduce . DT.toUDTT

shiftIndices :: Arrowterm -> Int -> Int -> Arrowterm
shiftIndices term d i= (arrowNotat . dt2Arrow . DT.toDTT) $ UD.shiftIndices ((DT.toUDTT . arrow2DT) term) d i

reduce :: Arrowterm -> Arrowterm
reduce = dt2Arrow . DT.toDTT . UD.betaReduce . DT.toUDTT . arrow2DT

type SUEnv = [(T.Text,DT.Preterm)]
type AEnv = [Arrowterm]
type SAEnv = [(T.Text,Arrowterm)]
type Context = (SAEnv,AEnv)

contextLen :: Context -> Int
contextLen (sigCon,varCon) = length sigCon + length varCon 

instance SimpleText SAEnv where
  toText sigs = toText (map (Data.Bifunctor.second (DT.toUDTT . arrow2DT)) sigs::UD.Signature)


data AJudgement =
  AJudgement
  Context -- ^ context
  Arrowterm -- ^ term
  Arrowterm -- ^ type
    deriving (Eq)

a2VNJudgment :: AJudgement -> VN.Judgment
a2VNJudgment aJ =
  case (aJ,a2dtJudgement aJ) of
    (AJudgement (sigEnv,varEnv) _ _,J.Judgement env preM preA) ->
      let uenv = map DT.toUDTT env
          preM' = DT.toUDTT preM
          preA' = DT.toUDTT preA
          uJ = UD.Judgment {UD.context = uenv, UD.term = preM', UD.typ = preA'}
      in case UD.fromDeBruijnJudgment uJ of
        VN.Judgment x0 pr pr' ->
          let (varEnv',sigEnv') = splitAt (length varEnv) x0
              sigEnv2 = reverse $ zipWith (curry (\(n,(_,s)) -> (VN.VarName 'c' n,s))) [0..] (reverse sigEnv')
          in VN.Judgment (varEnv' ++ sigEnv2) pr pr'


instance Show AJudgement where
  show (AJudgement (cons,vars) a_term a_type ) =
    let str = show $ Arrow (vars++map snd cons) a_term
        conVarsStr = foldl 
          (\st ch-> if ('[' `elem`  st) && (length (filter ('[' ==) st) == length (filter (']' ==) st )) then st else st ++[ch]  ) 
          "" 
          str
        f = length conVarsStr
        consStr = T.unpack $toText $reverse cons
        consLen = length cons        
        conVarStrs = S.splitOn "," $init $tail conVarsStr
        beforeReplaced = 
          consStr ++ 
          (if null vars then "" else (init $ foldr (\a b -> a ++ "," ++ b) "" $drop consLen conVarStrs))  ++
          " ト " ++ drop (f + length (" =>"::String)) str ++ 
          " : " ++ drop (f + length (" =>"::String)) (show $ Arrow (vars ++ map snd cons) a_type) ++ " "
        replaced = 
          T.unpack $ 
            foldr 
              (\(co,va) st -> T.replace (T.append va  $T.pack "=") (T.append co $T.pack "=") $T.replace (T.append va  $T.pack "]") (T.append co $T.pack "]") $T.replace (T.append va  $T.pack ")") (T.append co $T.pack ")") $T.replace (T.append va  $T.pack ",") (T.append co $T.pack ",") $T.replace (T.append va  $T.pack " ") (T.append co $T.pack " ") st) 
              (T.pack beforeReplaced) (zip (map fst cons) (map (\st -> T.pack $ head $S.splitOn ":"  $filter (/=' ') st) conVarStrs))
    in beforeReplaced

a2dtJudgement :: AJudgement -> J.Judgement
a2dtJudgement (AJudgement (con,env) aterm atype) = J.Judgement (map arrow2DT $ env ++ map snd con) (arrow2DT aterm) (arrow2DT atype)

dt2aJudgement ::  J.Judgement -> AJudgement
dt2aJudgement (J.Judgement env dtterm dttype) = AJudgement ([],map dt2Arrow env) (dt2Arrow dtterm) (dt2Arrow dttype)

typefromAJudgement :: AJudgement -> Arrowterm
typefromAJudgement ( AJudgement env aterm atype) = atype

termfromAJudgement :: AJudgement -> Arrowterm
termfromAJudgement ( AJudgement env aterm atype) = aterm

envfromAJudgement :: AJudgement -> Context
envfromAJudgement ( AJudgement env aterm atype) = env



dnPr = DT.Pi DT.Type (DT.Pi (DT.Pi (DT.Pi (DT.Var 0) DT.Bot) DT.Bot) (DT.Var 1))

classic  :: J.TEnv
classic = [dnPr]

subst :: DT.Preterm -> DT.Preterm -> DT.Preterm -> DT.Preterm
subst preterm l i =
  if preterm == i then
    l
  else
    case preterm of
      DT.Pi a b -> DT.Pi (subst a l i) (subst b (DT.toDTT (UD.shiftIndices (DT.toUDTT l) 1 0))  (DT.toDTT (UD.shiftIndices (DT.toUDTT i) 1 0)))
      DT.Not m -> DT.Not (subst m l i)
      DT.Lam m -> DT.Lam (subst m (DT.toDTT (UD.shiftIndices (DT.toUDTT l) 1 0)) (DT.toDTT (UD.shiftIndices (DT.toUDTT i) 1 0)))
      DT.App m n    -> DT.App (subst m l i) (subst n l i)
      DT.Sigma a b  -> DT.Sigma (subst a l i) (subst b (DT.toDTT (UD.shiftIndices (DT.toUDTT l) 1 0)) (DT.toDTT (UD.shiftIndices (DT.toUDTT i) 1 0)))
      DT.Pair m n   -> DT.Pair (subst m l i) (subst n l i)
      DT.Proj s m   -> DT.Proj s (subst m l i)
      DT.Eq a m n   -> DT.Eq (subst a l i) (subst m l i) (subst n l i)
      others -> others

aj :: (SUEnv,J.TEnv) -> DT.Preterm -> DT.Preterm -> AJudgement
aj (sig,vars) aterm atype =
  let hojo = dt2Arrow . DT.toDTT . UD.betaReduce . DT.toUDTT
      arrow_env = (map (Data.Bifunctor.second hojo) sig,map hojo vars)
      arrow_term = hojo aterm
      arrow_type = hojo atype
  in AJudgement arrow_env arrow_term arrow_type


arrowSubst :: Arrowterm -- ^ origin
  -> Arrowterm -- ^ 代入内容
  -> Arrowterm -- ^ 代入先
  -> Arrowterm
arrowSubst term i m= 
  dt2Arrow $ subst (arrow2DT term) (arrow2DT i) (arrow2DT m)

downSide ::  J.Tree AJudgement -> AJudgement
downSide (J.T _ downside _)=  downside
downSide (J.Error' judgement _) = judgement

changeDownSide :: J.Tree AJudgement -> AJudgement -> J.Tree AJudgement
changeDownSide (J.T label downside upside) newdownside = J.T label newdownside upside
changeDownSide (J.Error' a t) newdownside =  J.Error' newdownside t

upSide ::  J.Tree AJudgement -> [J.Tree AJudgement]
upSide (J.T _ _ upside)=  upside
upSide (J.Error' _ _) = []

aTreeTojTree :: J.Tree AJudgement -> J.Tree J.Judgement
aTreeTojTree (J.T label downside upside)=J.T label (a2dtJudgement downside) (map aTreeTojTree upside)
aTreeTojTree (J.Error' judgement text) = J.Error' (a2dtJudgement judgement) text

jTreeToaTree :: J.Tree J.Judgement -> J.Tree AJudgement
jTreeToaTree (J.T label downside upside)=J.T label (dt2aJudgement downside) (map jTreeToaTree upside)
jTreeToaTree (J.Error' judgement text) = J.Error' (dt2aJudgement judgement) text

-- | generate free constraint from given word
genFreeCon ::  Arrowterm -- ^ term
  -> String -- ^ "hoge"
  ->  Arrowterm
genFreeCon term hoge =
  if isFreeCon term ( Conclusion $ DT.Con $ T.pack hoge)
    then
       Conclusion $ DT.Con $ T.pack hoge
    else
      genFreeCon term $hoge++hoge

-- | whether DT.Con "hoge" is free or not
isFreeCon ::  Arrowterm  -- ^ term
  ->  Arrowterm  -- ^ DT.Con "hoge"
  -> Bool
isFreeCon term con=
  let term' =  arrowSubst term con con
      term'' =  arrowSubst term ( Conclusion $ DT.Var 0) con
  in term' == term''

--形だけ比較
canBeSame :: Int ->   Arrowterm ->   Arrowterm -> Bool
canBeSame _ (  Conclusion DT.Top) (  Conclusion DT.Top) = True
canBeSame _ (  Conclusion DT.Bot) (  Conclusion DT.Bot) = True
canBeSame _ (  Conclusion DT.Type) (  Conclusion DT.Type) = True
canBeSame lim (  Conclusion (DT.Var anum)) (  Conclusion (DT.Var anum')) =
  anum == anum' || anum <= lim
canBeSame _ (Conclusion (DT.Con t)) (Conclusion (DT.Con t')) = t == t'
canBeSame lim (  Conclusion (DT.Var anum)) a = 
  case a of 
    ArrowEq _ _ _ -> False
    _ -> True
  -- anum <= lim
canBeSame lim a (  Conclusion (DT.Var anum))  = 
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

addApp ::Int -> Arrowterm -> Arrowterm
addApp 0 base = base
addApp num base = addApp (num - 1) $ ArrowApp (shiftIndices base 1 0) (Conclusion $ DT.Var 0)

addLam :: Int -> Arrowterm -> Arrowterm
addLam 0 term = term
addLam num term = ArrowLam $ addLam (num - 1) term

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
