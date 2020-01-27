module DTS.Alligator.Prover
(
  prove,
  Arrowterm(..),
  AJudgement(..),
  arrow_notat,
  forward,
  deduce
) where

import qualified DTS.DTT as DT            -- DTT
import qualified DTS.UDTT as UD            -- DTT
import qualified Data.Text.Lazy as T      -- text
import qualified Data.List as L           -- base
import qualified Control.Applicative as M -- base
import qualified Control.Monad as M       -- base
import qualified DTS.UDTTwithName as VN
import qualified DTS.Prover.Judgement as J
import Data.Time　as Ti
import Debug.Trace
import System.Timeout

data ArrowSelector = Arrow_Fst | Arrow_Snd deriving (Eq, Show)

-- | Arrowterm
data Arrowterm =
  Conclusion DT.Preterm -- ^ 末尾の部分
  | Arrow_Sigma Arrowterm Arrowterm -- ^ Sigma型
  | Arrow_App Arrowterm Arrowterm -- ^ App型
  | Arrow_Pair Arrowterm Arrowterm -- ^ Pair型
  | Arrow_Proj ArrowSelector Arrowterm -- ^ Proj型
  | Arrow_Lam Arrowterm -- ^ Lam型
  | Arrow [Arrowterm]  Arrowterm -- ^ Pi型
  deriving (Eq)

-- | DTT.Preterm型に変換して得たテキストを加工している
instance Show Arrowterm where
  show term  = prestr2arrowstr ((filter (/= ' ') . show . arrow2DT) term) term

prestr2arrowstr :: String -> Arrowterm -> String
prestr2arrowstr prestr (Conclusion p) = (dropWhile (not . (`elem` (['0'..'z']++['(',')','\8869']))) prestr) --if (head prestr `elem` (['0'..'z']++['(',')'])) then prestr else tail prestr
prestr2arrowstr prestr (Arrow env r) =
  if length env == 0
  then
    (prestr2arrowstr  prestr r)
  else
    let parentheses =  take (length env) (treatParentheses prestr) --[(2,"q0")]
    in "[ " ++
        (tail
          (foldr
            (\a -> \b -> ", "  ++ a  ++ b)
            ""
            (map
              (\z ->
                let str = (init $ tail $ snd $ snd z)
                in (takeWhile (/= ':')  str) ++":" ++  (prestr2arrowstr (tail $ dropWhile (/= ':') str) $ fst z))
                $ zip (reverse env) parentheses))) ++
        " ] =>" ++ (prestr2arrowstr  (tail $ drop ((fst . last) parentheses) prestr) r)
prestr2arrowstr prestr (Arrow_Sigma h t) =
  let parentheses = head (treatParentheses prestr)
  in "(" ++ let str  = (tail $ init $ snd  parentheses) in ((takeWhile (/= ':')  str) ++":"++ (prestr2arrowstr (tail $ dropWhile (/= ':') str)  h)) ++ [' ',')','\215',' '] ++ (prestr2arrowstr  (drop ((fst  parentheses) + 1) prestr) t)
prestr2arrowstr prestr (Arrow_App h t) =
  let f_len = (length prestr) - (fst $ head $ treatParentheses (reverse prestr))
  in let f  = take f_len prestr in  (prestr2arrowstr f  h) ++ " " ++ (prestr2arrowstr  (init$ tail$drop (f_len) prestr) t)
prestr2arrowstr prestr (Arrow_Proj s t) =
  let parentheses = head (treatParentheses prestr)
  in (snd parentheses) ++"(" ++ (prestr2arrowstr (init $ tail (drop (fst parentheses) prestr)) t) ++ ")"
prestr2arrowstr prestr (Arrow_Lam t) =
  takeWhile (/= '.') prestr ++ ".(" ++ (prestr2arrowstr  (tail $ dropWhile (/= '.') prestr) t) ++")"
prestr2arrowstr prestr (Arrow_Pair h t) =
  let contents = init $ tail prestr
      index = fst $ head $ filter ((==",") . snd) $ treatParentheses contents
  in
     "("++(prestr2arrowstr (take (index - 1) contents) h)++","++(prestr2arrowstr (drop index contents) t)++")"

{-test
Arrow [Arrow_Proj Arrow_Fst (Conclusion DT.Type)] (Arrow_Proj Arrow_Snd (Conclusion (DT.Var 0)))
Arrow_Sigma (Conclusion $ DT.Con $ T.pack "p") (Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 1))
Arrow_Sigma (Arrow [(Arrow [(Conclusion (DT.Con (T.pack "p")))] (Conclusion (DT.Var 0))),Conclusion DT.Type](Conclusion (DT.Con (T.pack "p")))) (Conclusion (DT.Con (T.pack "q")))
(Arrow_App (Arrow [(Conclusion DT.Type)] (Conclusion (DT.Sigma DT.Type DT.Type))) (Conclusion $  DT.Pi DT.Type DT.Type))
Arrow [Arrow_Sigma (Conclusion $ DT.Pi (DT.Bot) (DT.Var 0)) (Conclusion $DT.Type), Arrow [Conclusion $ DT.Con $T.pack "q" ] (Conclusion (DT.Var 0))] (Arrow [Conclusion (DT.Con (T.pack "p"))] (Conclusion $ DT.Var 0))
Arrow_Pair (Arrow_Pair (Conclusion DT.Type) (Arrow_Sigma (Conclusion (DT.Type)) (Conclusion (DT.Con $ T.pack "p")))) (Arrow_Pair (Conclusion $ DT.Con $ T.pack "a") (Conclusion $ DT.Con $ T.pack "q"))
Arrow_Pair (Conclusion DT.Type) (Arrow_Pair (Conclusion DT.Type) (Conclusion (DT.Con $T.pack "p")))
Arrow_Pair (Arrow_Pair (Conclusion DT.Type) (Conclusion $ DT.Con $T.pack "y")) (Conclusion DT.Type)
 Arrow_Pair (Arrow_Pair (Conclusion DT.Type) (Conclusion $ DT.Con $T.pack "y")) (Arrow_Pair (Conclusion DT.Type) (Conclusion (DT.Con $T.pack "q")))
-}

treatParentheses :: String -> [(Int,String)]
treatParentheses str =
  let str'= (takeWhile (\x -> (x /='(') && (x /=')' && (x/=',')) ) str)
      lst =  (indexParentheses (drop (length str') str))
  in
    filter ((/=0) . fst) $ (length str' , str') : (map (\x -> ( ((snd $ fst x) + length str'),snd x)) $ filter (\x ->((fst $ fst x) /= 1) || ((snd x) == ",") || ((snd x) == ['\8869']) ) $ zip (zip lst $ map sum  $ tail  $ L.inits lst) $  devideParentheses $ filter (`elem` (['0'..'z']++['(',')','\8594','\215','\8869','\960','\955','.',',',' '])) (drop (length str')  str))

indexParentheses :: String -> [Int]
indexParentheses str =
  if length str > 0
  then
    let f = fst ( last (fst (head (filter (\x -> snd x == 0) (tail (map (\z -> (z,(foldr (\x -> \y -> if (snd x)=='(' then (y + 1) else (if (snd x)==')' then (y-1) else y) ) 0 z)))(L.inits (zip [1..] str))  ))))))
    in (f : (indexParentheses (drop f str)))
  else []

devideParentheses :: String -> [String]
devideParentheses str =
  if length str > 0
  then
    let f = fst (head (filter (\x -> snd x == 0) (tail (map (\z -> (z,(foldr (\x -> \y -> if x=='(' then (y + 1) else (if x==')' then (y-1) else y) ) 0 z)))(L.inits str)  ))))
    in (f : (devideParentheses (drop (length f)str)))
  else []

arrow_notat_selector :: DT.Selector -> ArrowSelector
arrow_notat_selector DT.Fst = Arrow_Fst
arrow_notat_selector DT.Snd = Arrow_Snd

dt_notat_selector :: ArrowSelector -> DT.Selector
dt_notat_selector Arrow_Fst = DT.Fst
dt_notat_selector Arrow_Snd = DT.Snd

arrow2DT :: Arrowterm -> DT.Preterm
arrow2DT (Conclusion a) = a
arrow2DT (Arrow_Sigma h t)= DT.Sigma (arrow2DT h) (arrow2DT t)
arrow2DT (Arrow_Pair h t)= DT.Pair (arrow2DT h) (arrow2DT t)
arrow2DT (Arrow_App a b) = DT.App (arrow2DT a) (arrow2DT b)
arrow2DT (Arrow_Proj s p) = DT.Proj (dt_notat_selector s) (arrow2DT p)
arrow2DT (Arrow_Lam p) = DT.Lam (arrow2DT p)
arrow2DT (Arrow [] t) = arrow2DT t
arrow2DT (Arrow (f:r) t) = arrow2DT (Arrow r (Conclusion (DT.Pi (arrow2DT f)  (arrow2DT t))))

to1Arrow :: Arrowterm -> Arrowterm
to1Arrow (Arrow env (Arrow env' conclusion)) = to1Arrow $ Arrow (env' ++ env) conclusion
to1Arrow term = term


-- | 入力されたDT.PretermをArrowTermに変換する
arrow_notat :: DT.Preterm -> Arrowterm
arrow_notat (DT.Type) = Conclusion $DT.Type
arrow_notat (DT.Var i) = Conclusion $ DT.Var i
arrow_notat (DT.Con i) = Conclusion $ DT.Con i
arrow_notat (DT.Not i) =
  Arrow [arrow_notat i] $Conclusion DT.Bot
arrow_notat (DT.Pi h t) =
  to1Arrow $ Arrow [arrow_notat h] (arrow_notat t)
arrow_notat (DT.Sigma h t) =
  Arrow_Sigma (arrow_notat h) (arrow_notat t)
arrow_notat (DT.App a b) =
  Arrow_App (arrow_notat a) (arrow_notat b)
arrow_notat (DT.Pair a b) =
  Arrow_Pair (arrow_notat a) (arrow_notat b)
arrow_notat (DT.Proj selector p) =
  Arrow_Proj (arrow_notat_selector selector) (arrow_notat p)
arrow_notat (DT.Lam p) =
  Arrow_Lam (arrow_notat p)
arrow_notat dt= Conclusion dt

shiftIndices :: Arrowterm -> Int -> Int -> Arrowterm
shiftIndices term d i= ((arrow_notat . DT.toDTT) $ UD.shiftIndices ((DT.toUDTT . arrow2DT) term) d i)

type TEnv = [DT.Preterm]
type SUEnv = [(T.Text,DT.Preterm)]
type AEnv = [Arrowterm]

{-test
AJudgement [Conclusion DT.Type,Conclusion $ DT.Con $ T.pack "p",Arrow_Sigma (Conclusion $ DT.Con $T.pack "q") (Conclusion $ DT.Var 0)] (Conclusion $ DT.Var 2) (Conclusion $ DT.Var 2)
-}
data AJudgement =
  AJudgement
  AEnv -- ^ context
  Arrowterm -- ^ term
  Arrowterm -- ^ type
    deriving (Eq)

instance Show AJudgement where
  show (AJudgement env a_term a_type ) =
    let str = show $ Arrow env a_term
        f = fst $ last $ fst $ head $ filter (\x -> snd x == 0) $ tail $ map (\z -> (z,(foldr (\x -> \y -> if (snd x)=='[' then y + 1 else (if (snd x)==']' then y-1 else y) ) 0 z))) $ L.inits (zip [1..] str)
    in
      if  length env == 0
      then
        " ト " ++ str ++ " : " ++  (show $ Arrow env a_type)
      else
        (init $ tail $take f str)++" ト " ++ (drop (f + (length " =>")) str) ++ " : " ++ (drop (f + (length " =>")) $ show $ Arrow env a_type)

a2dtJudgement :: AJudgement -> J.Judgement
a2dtJudgement (AJudgement env aterm atype) = J.Judgement (map arrow2DT env) (arrow2DT aterm) (arrow2DT atype)

typefromAJudgement :: AJudgement -> Arrowterm
typefromAJudgement ( AJudgement env aterm atype) = atype

termfromAJudgement :: AJudgement -> Arrowterm
termfromAJudgement ( AJudgement env aterm atype) = aterm

envfromAJudgement :: AJudgement -> [Arrowterm]
envfromAJudgement ( AJudgement env aterm atype) = env

dne =   DT.Lam  (DT.Pi (DT.Pi (DT.Pi (DT.Var 1)  DT.Bot) (DT.Bot)) (DT.Var 2))
efq = DT.Lam (DT.Pi DT.Bot (DT.Var 1))
dn_pr = DT.Pi (DT.Type) (DT.Pi (DT.Pi (DT.Pi (DT.Var 0) DT.Bot) DT.Bot) (DT.Var 1))

classic  :: TEnv
classic = [dn_pr]

-- getAxiom :: String -> TEnv
-- getAxiom "classic"= classic ++ []

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

arrow_subst :: Arrowterm -- ^ origin
  -> Arrowterm -- ^ 代入内容
  -> Arrowterm -- ^ 代入先
  -> Arrowterm
arrow_subst term i m= (arrow_notat) $ subst (arrow2DT term) (arrow2DT i) (arrow2DT m)

--deduce con f_context arrow_type depth =
{-
var_env = [DT.Sigma (DT.Var 0)  (DT.Var 2),DT.Con $ T.pack "p",DT.Con $ T.pack "q"]
sig_env = []
pre_type = DT.Con $ T.pack "p"
prove var_env sig_env pre_type

sig_env = []
var_env = [DT.Con $ T.pack "r",DT.Con $ T.pack "q",DT.Con $ T.pack "p"]
pre_type = DT.Pi (DT.Con $ T.pack "p") (DT.Con $ T.pack "p")
prove' var_env sig_env pre_type

sig_env = classic ++ DT.Bot
var_env = [DT.Con $ T.pack "r",DT.Con $ T.pack "q",DT.Con $ T.pack "p"]
pre_type = DT.Pi (DT.Con $ T.pack "p") (DT.Sigma (DT.Con $ T.pack "p") (DT.Con $ T.pack "p"))
prove' var_env sig_env pre_type

sig_env = classic ++ [DT.Bot]
p = DT.Con $ T.pack "p"
q = DT.Con $ T.pack "q"
r = DT.Con $ T.pack "r"
var_env = [r,q,p]
pre_type = DT.Pi (DT.Pi (DT.Sigma p q) r) (DT.Pi p (DT.Pi q r))
prove' var_env sig_env pre_type

sig_env = classic
p = DT.Con $ T.pack "p"
q = DT.Con $ T.pack "q"
r = DT.Con $ T.pack "r"
var_env = [r,q,p,DT.Pi p DT.Bot]
pre_type = DT.Bot
prove' var_env sig_env pre_type

sig_env = classic
p = DT.Con $ T.pack "p"
q = DT.Con $ T.pack "q"
r = DT.Con $ T.pack "r"
var_env = [r,q,p]
pre_type = DT.Pi p (DT.Pi (DT.Pi p DT.Bot) DT.Bot)
prove' var_env sig_env pre_type

sig_env = classic
p = DT.Con $ T.pack "p"
q = DT.Con $ T.pack "q"
r = DT.Con $ T.pack "r"
var_env = [r,q,p]
pre_type = DT.Pi (DT.Pi p DT.Bot) DT.Bot
prove' var_env sig_env pre_type

pre_type =  (DT.Pi p DT.Bot)
prove' var_env sig_env pre_type

pre_type =  DT.Bot
prove' var_env sig_env pre_type

var_env = [DT.Pi (DT.Pi (DT.Pi p DT.Bot) DT.Bot) (DT.Pi r DT.Bot),r,q,p]
pre_type = DT.Bot
prove' var_env sig_env pre_type

var_env = [DT.Pi p q,DT.Pi q r]
pre_type = DT.Pi p r
prove' var_env sig_env pre_type

var_env = [DT.Pi p q,DT.Pi q r,p]
pre_type = DT.Pi p r
prove' var_env sig_env pre_type


preterm = DT.Type
prove var_env sig_env preterm

-}

prove ::  TEnv -- ^ var_context ex)[(DT.Con (T.pack "prop")),(DT.Con (T.pack "set")),(DT.Con (T.pack "prop"))]
  -> TEnv -- ^ sig_context ex)[((T.pack "prop"),DT.Type),((T.pack "set"),DT.Type)] , classic
  -> DT.Preterm -- type ex) (DT.Pi (DT.Var 0) (DT.Sigma (DT.Var 0,DT.Var 3)))
  -> [J.Judgement] -- term  /変更:DT.Judgemnetにする
prove var_env sig_env pre_type =
  --TEnvをPTEnvに変える
  let arrow_terms = prove' var_env sig_env pre_type
  in  map (\(AJudgement env a_term a_type) -> J.Judgement (map arrow2DT env) (arrow2DT a_term) (arrow2DT a_type)) arrow_terms

prove' ::  TEnv -- ^ var_context ex)[(DT.Con (T.pack "prop")),(DT.Con (T.pack "set")),(DT.Con (T.pack "prop"))]
  -> TEnv -- ^ sig_context ex)[((T.pack "prop"),DT.Type),((T.pack "set"),DT.Type)] , classic
  -> DT.Preterm -- type ex) (DT.Pi (DT.Var 0) (DT.Sigma (DT.Var 0,DT.Var 3)))
  -> [AJudgement] -- term  /変更:DT.Judgemnetにする
prove' var_env sig_env pre_type =
  --TEnvをPTEnvに変える
  let var_env' = var_env ++ sig_env
      arrow_env = map (arrow_notat . DT.toDTT . UD.betaReduce . DT.toUDTT) var_env'
      arrow_type = (arrow_notat . DT.toDTT . UD.betaReduce . DT.toUDTT) pre_type
      arrow_terms = search_proof arrow_env arrow_type 1
  in  arrow_terms

pi_rules = [(DT.Type, DT.Type),  (DT.Type, DT.Kind),  (DT.Kind, DT.Kind),   (DT.Kind, DT.Type)]
sigma_rules = [(DT.Type,DT.Type)]

{-
test1 = Arrow [Arrow_Proj Arrow_Fst (Conclusion DT.Type)] (Arrow_Proj Arrow_Snd (Conclusion (DT.Var 0)))
test2 = Arrow_Sigma (Conclusion $ DT.Con $ T.pack "p") (Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 1))
test3 = Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 1)
test4 = Arrow_Sigma (Arrow [(Arrow [(Conclusion (DT.Con (T.pack "p")))] (Conclusion (DT.Var 0))),Conclusion DT.Type](Conclusion (DT.Con (T.pack "p")))) (Conclusion (DT.Con (T.pack "q")))
test5=(Arrow_Sigma (Conclusion $ DT.Con $ T.pack "p") (Arrow [Conclusion $ DT.Con $ T.pack "q"] (Arrow_Sigma (Conclusion $ DT.Var 1) (Conclusion $ DT.Var 3))))
context = [test1,test2,test3,test4,test5]
-}

{-
sigma_con = [Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Con $ T.pack "p"),(Conclusion $ DT.Con $ T.pack "q")]
context = forward_context sigma_con
-}

forward_context :: AEnv -> [AJudgement]
-- forward_context [] = []
-- forward_context (f:r) =
--   map (\(AJudgement env aterm atype) -> AJudgement (f:r) aterm atype) (forward f)  ++ forward_context r
--let boollst = map (\x -> shiftIndices (typefromAJudgement x) ((length con) - (length $ envfromAJudgement x)) 0 == arrow_type) context
forward_context context =
  map
    (\ (AJudgement env a_term a_type) -> AJudgement context (shiftIndices a_term ((length context) - (length env)) 0) (shiftIndices a_type ( (length context) - (length env)) 0))
    $ foldr
      (++)
      (foldr
        (++)
        []
        -- (map (\((varid',con_type),fr) -> [AJudgement (tail fr) con_type (Conclusion $ DT.Type)] ++ [AJudgement fr (gen_free_con con_type ("context" ++ (show (length context -varid'))) ) (shiftIndices con_type 1 0)]) $ zip (zip [1..] context) $ map reverse $ reverse $ tail $ L.inits $ reverse $ context))
        (map (\((varid',con_type),fr) -> [AJudgement (tail fr) con_type (Conclusion $ DT.Type)] ++ [AJudgement fr (Conclusion $ DT.Var 0 ) (shiftIndices con_type 1 0)]) $ zip (zip [1..] context) $ map reverse $ reverse $ tail $ L.inits $ reverse $ context))
      $ map ( \(f,fr) -> map (\(AJudgement env aterm atype) -> AJudgement (fr) aterm atype) (forward f) ) $ zip context $ map reverse $ reverse $ tail $ L.inits $ reverse $ context




-- | generate free constraint from given word
gen_free_con :: Arrowterm -- ^ term
  -> String -- ^ "hoge"
  -> Arrowterm
gen_free_con term hoge =
  if is_free_con term (Conclusion $ DT.Con $ T.pack hoge)
    then
      (Conclusion $ DT.Con $ T.pack hoge)
    else
      (gen_free_con term $hoge++hoge)

-- | whether DT.Con "hoge" is free or not
is_free_con :: Arrowterm -> -- ^ term
  Arrowterm -> -- ^ DT.Con "hoge"
  Bool
is_free_con term con=
  let term' = arrow_subst term con con
      term'' = arrow_subst term (Conclusion $ DT.Var 0) con
  in term' == term''


forward :: Arrowterm -> [AJudgement]
forward term =
  let base_con = gen_free_con term "base"
  in map (\(AJudgement con a_term a_type) -> AJudgement con (arrow_subst (shiftIndices a_term 1 0) (Conclusion $DT.Var 0) base_con ) (arrow_subst (shiftIndices a_type 1 0) (Conclusion $  DT.Var 0) base_con)) $ forward' [term] base_con term


forward' :: [Arrowterm] -- ^ origin
  ->   Arrowterm -- ^ base
  ->   Arrowterm -- ^ target
  ->  [AJudgement]
forward' context base (Arrow_Sigma h t) =
  let --t' = arrow_subst t (Arrow_Proj Arrow_Fst base) (Conclusion $ DT.Var 0)
      t' = shiftIndices (arrow_subst t  (Arrow_Proj Arrow_Fst base) (Conclusion $ DT.Var 0)) (-1) 0
      h_forward = forward' context (Arrow_Proj Arrow_Fst base) h
      t_forward = forward' context (Arrow_Proj Arrow_Snd base) t'
  -- in (if t_forward == [] then [AJudgement context (Arrow_Proj Arrow_Snd  base) t'] ++ ([AJudgement context t' (Conclusion $ DT.Type)]) else t_forward) ++ (if h_forward == [] then [AJudgement context (Arrow_Proj Arrow_Fst base) h]++ ([AJudgement context h (Conclusion $ DT.Type)]) else h_forward)
  in (if t_forward == [] then [AJudgement context (Arrow_Proj Arrow_Snd  base) t'] else t_forward) ++ (if h_forward == [] then [AJudgement context (Arrow_Proj Arrow_Fst base) h] else h_forward)
forward' context base (Arrow env (Arrow_Sigma h t)) =
  let term1 = add_Lam (length env) $ Arrow_Proj Arrow_Fst $ add_App (length env) base
      term2 = add_Lam (length env) $ Arrow_Proj Arrow_Snd $ add_App (length env) base
      t' = shiftIndices (arrow_subst t (shiftIndices term1 (length env) 0) (Conclusion $ DT.Var 0)) (-1) 0
      type1 = Arrow env h
      type2 = Arrow env t'
      h_forward = forward' context term1 type1
      t_forward = forward' context term2 type2
  in (if t_forward == [] then [AJudgement context term2 type2] else t_forward) ++ (if h_forward == [] then [AJudgement context term1 type1] else h_forward)
forward' context base  (Arrow a (Arrow b c)) =
  forward' context base (Arrow (b ++ a) c)
forward' context base arrowterm = []


add_App ::Int -> Arrowterm -> Arrowterm
add_App 0 base = base
add_App num base = add_App (num - 1) $ Arrow_App (shiftIndices base 1 0) (Conclusion $ DT.Var 0)

add_Lam :: Int -> Arrowterm -> Arrowterm
add_Lam 0 term = term
add_Lam num term = Arrow_Lam $ add_Lam (num - 1) term

maxdepth = 5


search_proof :: [Arrowterm] ->Arrowterm -> Int -> [AJudgement]
search_proof arrow_env arrow_type depth = deduceWithLog arrow_env arrow_type depth
--  L.nub $ foldr (++) [] (map (\d -> deduceWithLog con arrow_type d) [1..(maxdepth -1)])

membership :: [Arrowterm] ->  Arrowterm -> Int -> Either (String) ([AJudgement])
membership con arrow_type depth =
  let context = forward_context con
      boollst = map (\x -> shiftIndices (typefromAJudgement x) ((length con) - (length $ envfromAJudgement x)) 0 == arrow_type) context
  in
    if (or boollst)
      then
        Right $ map (fst) $ filter (snd) $ zip context boollst
      else
        Right $ []

{-
con = [Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Var 0),(Conclusion $ DT.Con $ T.pack "q")]
arrow_type = (Conclusion DT.Type)
depth = 1
arrow_term = Arrow [Conclusion $DT.Con $T.pack "q"] (Conclusion $ DT.Var 3)
pi_form con arrow_term arrow_type depth
-}

typecheck_env :: [Arrowterm] -> [Arrowterm] -> [Arrowterm] ->Int -> [[AJudgement]]
typecheck_env _ [] _ _= [[]]
typecheck_env con (f:r) arrow_types depth =
  let a_js = foldr (++) [] $ map (\arrow_type -> (withLog' typecheck) con f arrow_type depth) arrow_types in
    if a_js == []
    then
      []
    else
      foldr
      (++)
      []
      $map
      (\a_j ->
        let
          r_env' = f:(envfromAJudgement a_j)
          r_term' = map (\f' -> shiftIndices f' ((length r_env') - 1 - (length con)) 0) r
        in
          map -- ^[[c1,b1,a1],[c2,b1,a1]]
          (\r_js -> r_js ++ [a_j])
          (typecheck_env r_env' r_term' arrow_types depth)) -- ^ [[c1,b1],[c2,b1]]
      a_js  -- ^ [a1,a2]


pi_form :: [Arrowterm] -> Arrowterm ->  Arrowterm -> Int -> Either (String) ([AJudgement])
pi_form con (Arrow as b) arrow_type depth =
  if (arrow_type `elem`[Conclusion DT.Type,Conclusion DT.Kind]) && (depth < maxdepth)
  then
    let as_terms =typecheck_env con (reverse as) [Conclusion DT.Type,Conclusion DT.Kind] (depth + 1)
    in
      if as_terms == [] then
         Right []
      else
        let
            b_env = as ++ con
            b_term = b
            b_js =foldr (++) [] $ map (\dtterm -> withLog' typecheck b_env b_term (Conclusion dtterm) depth) [DT.Type,DT.Kind]
        in
          Right $
            map
            (\b_j ->AJudgement con (Arrow as b) (typefromAJudgement b_j))
            b_js
  else
    if depth < maxdepth
    then
      Left ("too deep @ pi_form " ++ show con ++" | "++ show arrow_type)
    else
      Right []
{-
con = [Arrow [Conclusion $ DT.Con $ T.pack "p",Conclusion $ DT.Con $ T.pack "r"] (Conclusion $ DT.Con $ T.pack "q"),Arrow [Conclusion $ DT.Con $ T.pack "p",Conclusion $ DT.Con $ T.pack "p"] (Conclusion $ DT.Con $ T.pack "q"),Arrow [Conclusion $ DT.Con $ T.pack "p"] (Conclusion $ DT.Con $ T.pack "q"),Conclusion $ DT.Con $ T.pack "p"]
arrow_term = Conclusion $ DT.Con $ T.pack "q"
arrow_type = Conclusion DT.Type
depth = 1
pi_form con arrow_term arrow_type depth
-}
pi_form con arrow_term arrow_type depth =
  if (arrow_type `elem`[Conclusion DT.Type,Conclusion DT.Kind]) && (depth < maxdepth)
  then
    let a_judgements = arrow_conclusion_b (forward_context con) arrow_term
        a_type_judgements =   foldr (++) [] $ map (\a_judgement ->withLog' typecheck con (typefromAJudgement a_judgement) arrow_type (depth+1)) a_judgements
    in
      if a_type_judgements == []
      then
        Right []
      else
        let arrow_envs = map (\(AJudgement _ (Arrow env _) _) -> reverse env) a_type_judgements
            arrow_judgements' = map (\arrow_env -> typecheck_env con arrow_env [Conclusion DT.Type,Conclusion DT.Kind] (depth + 1)) arrow_envs
            arrow_envs' = map (reverse . snd) $filter ((/=[]) . fst) $ zip arrow_judgements' arrow_envs
        in Right $ map (\env -> AJudgement (env ++ con) (shiftIndices arrow_term (length env)0 )arrow_type ) arrow_envs'
  else
    if depth < maxdepth
    then
      Left ("too deep @ pi_form " ++ show con ++" | "++ show arrow_type)
    else
      Right []

sigma_form:: [Arrowterm] -> Arrowterm ->  Arrowterm -> Int -> Either (String) ([AJudgement])
sigma_form con (Arrow_Sigma a b) arrow_type depth =
  if (arrow_type `elem`[Conclusion DT.Type,Conclusion DT.Kind]) && (depth < maxdepth)
  then
    let a_term = foldr (++) [] $ map (\dtterm -> withLog' typecheck con a (Conclusion dtterm) depth) [DT.Type,DT.Kind]
    in
      if a_term == []
      then
        Right []
      else
        let b_js = foldr (++) [] $ map (\dtterm -> withLog' typecheck (a:con) b (Conclusion dtterm) depth) [DT.Type,DT.Kind]
        in
          Right $
            map
            (\b_j -> AJudgement con (Arrow_Sigma a b) (typefromAJudgement b_j))
            b_js
  else
    if depth < maxdepth
    then
      Left ("too deep @ pi_form " ++ show con ++" | "++ show arrow_type)
    else
      Right []

sigma_form con arrow_term arrow_type depth = Right []

pi_intro :: [Arrowterm] ->  Arrowterm -> Int -> Either (String) ([AJudgement])
pi_intro con (Arrow a b) depth =
  if depth > maxdepth
  then
    Left ("too deep @ pi-intro " ++ show con ++" ト "++ show (Arrow a b))
  else
    if ((foldr (++) [] $ map (\dtterm -> withLog' typecheck con (Arrow a b) (Conclusion dtterm) depth) [DT.Type,DT.Kind]) == [])
    then
      Right []
    else
      let
          b_judgements = deduceWithLog (a ++ con) b (depth + 1)
          pi_a_b_judgements =
            map
              (\b_j ->
                let
                  env = con
                  a_term = add_Lam (length a) (termfromAJudgement b_j)
                  a_type = Arrow a b
                in
                  AJudgement env a_term a_type)
              b_judgements
      in
        Right pi_a_b_judgements
pi_intro  con arrow_type depth= Right []


arrow_conclusion_b :: [AJudgement] -> Arrowterm -> [AJudgement]
arrow_conclusion_b judgements b=
  filter
    (\j ->
      case typefromAJudgement j of
        Arrow env b' -> (shiftIndices b (length env) 0)==b'
        otherwise -> False)
    judgements

deduce_envs :: [Arrowterm] -> [AJudgement] -> Int-> [(AJudgement,[[AJudgement]])]
deduce_envs con a_judgements depth =
  let ajudges = map (\env -> sequence (map (\a -> deduceWithLog con a depth) env)) $ map (\(AJudgement _ _ (Arrow env _)) -> env) a_judgements
  in filter ((/= []) . snd) $zip a_judgements ajudges

app_as :: Arrowterm -> [Arrowterm] -> Arrowterm
app_as term [] = term
app_as term (f:r) =
  app_as (Arrow_App term f) r

subst_as_in_pi_elim :: Arrowterm -> [Arrowterm] -> Arrowterm
subst_as_in_pi_elim term [] = term
subst_as_in_pi_elim term (f:r) =
  subst_as_in_pi_elim (shiftIndices (arrow_subst term f (Conclusion $ DT.Var 0)) (-1) 0) r

pi_elim :: [Arrowterm] -> Arrowterm->Int->Either (String) ([AJudgement])
pi_elim con b1 depth =
  if depth > maxdepth
  then
    Left ("too deep @ pi_elim " ++ show con ++" ト "++ show b1)
  else
    let a_judgements = arrow_conclusion_b (forward_context con) b1
        a_type_terms = deduce_envs con a_judgements depth
    in
      Right
        $foldr
        (++)
        []
        $ map
          (\(base,ass) ->
              map
                (\as ->
                  let env' = con
                      a_terms' = app_as (termfromAJudgement base) $map (termfromAJudgement) as
                      a_type' = subst_as_in_pi_elim b1 $map (termfromAJudgement) as
                  in
                    AJudgement env' a_terms' a_type')
                ass
          )
          a_type_terms

sigma_intro :: [Arrowterm] ->  Arrowterm -> Int -> Either (String) ([AJudgement])
sigma_intro con  (Arrow_Sigma a b1) depth =
  if depth > maxdepth
  then
    Left ("too deep @ sigma_intro " ++ show con ++" ト "++ show (Arrow_Sigma a b1))
  else
    if foldr (++) [] (map (\dtterm ->withLog' typecheck con (Arrow_Sigma a b1) (Conclusion dtterm) depth) [DT.Type,DT.Kind]) == []
    then
      Right []
    else
      let a_term_judgements =deduceWithLog con a (depth + 1)
          a_b1_terms_judgements =
            map
            (\a_j ->
              let con2b =  (envfromAJudgement a_j)
              in
                (a_j,deduceWithLog con (shiftIndices (arrow_subst b1 (termfromAJudgement a_j) (Conclusion $ DT.Var 0)) (-1) 0) depth ))
            a_term_judgements
      in
        Right
          $ foldr
            (++)
            []
            $map
              (\(a_j,b1_js) ->
                map
                  (\b1_j ->
                    let
                      env = con
                      a_term = Arrow_Pair (termfromAJudgement a_j)  (termfromAJudgement b1_j)
                      a_type = (Arrow_Sigma a b1)
                    in AJudgement env a_term a_type)
                  b1_js)
              a_b1_terms_judgements

sigma_intro  con  arrow_type depth=Right []

deduceWithLog :: [Arrowterm] -> Arrowterm ->Int ->[AJudgement]
deduceWithLog = withLog deduce

withLog' :: ([Arrowterm] -> Arrowterm -> Arrowterm ->Int ->Either (String) ([AJudgement])) -> [Arrowterm] -> Arrowterm -> Arrowterm ->Int ->[AJudgement]
withLog' f con arrow_term arrow_type depth =
  case f con arrow_term arrow_type depth of
    (Right j) -> j
    (Left msg) -> trace msg []

withLog :: ([Arrowterm] -> Arrowterm ->Int ->Either (String) ([AJudgement])) -> [Arrowterm] -> Arrowterm ->Int ->[AJudgement]
withLog f con arrow_type depth =
  case f con arrow_type depth of
    (Right j) -> j
    (Left msg) -> trace msg []


{-
con = [Arrow [Conclusion $ DT.Con $ T.pack "q"] (Conclusion $ DT.Var 3),Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Var 0),(Conclusion $ DT.Con $ T.pack "q")]
depth = 1
arrow_type = Conclusion $ DT.Type
arrow_term = Conclusion $ DT.Var 3
typecheck con arrow_term arrow_type depth

arrow_term = Arrow [Arrow_Sigma (Conclusion $ DT.Var 2) (Conclusion $ DT.Var 4)] (Conclusion $ DT.Con $T.pack "q")
typecheck con arrow_term arrow_type depth

arrow_term = Arrow [Conclusion $ DT.Var 3] (Conclusion $ DT.Con $ T.pack "q")
typecheck con arrow_term arrow_type depth

arrow_term = Arrow_Sigma (Conclusion $ DT.Var 3) (Conclusion $ DT.Con $ T.pack "q")
typecheck con arrow_term arrow_type depth


-}
typecheck :: [Arrowterm] -> Arrowterm -> Arrowterm -> Int -> Either (String) ([AJudgement])
typecheck con (Conclusion DT.Bot) (Conclusion DT.Type) depth =
  Right [AJudgement con (Conclusion DT.Bot) (Conclusion DT.Type)]
typecheck con arrow_term arrow_type depth =
  if (depth < maxdepth)
  then
    let judgements = foldr (++) [] $ map (\f -> withLog f con arrow_type depth) [membership,pi_intro,pi_elim,sigma_intro]
        deducejudgements =filter (\a ->(termfromAJudgement a) == (shiftIndices arrow_term ((length $ envfromAJudgement a)-(length con) ) 0))$ judgements
    in
      Right $ deducejudgements ++ (foldr (++) [] $ map (\f -> withLog' f con arrow_term arrow_type depth) [pi_form,sigma_form])
  else
    Left ("too deep @ typecheck " ++ show con ++" | "++ show arrow_type)

{-
con = [Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Con $ T.pack "p"),(Conclusion $ DT.Con $ T.pack "q")]
depth = 1
arrow_type = (Conclusion $ DT.Con $ T.pack  "p")
deduce con arrow_type depth

arrow_type = (Conclusion $ DT.Var 2)
deduce con arrow_type depth

arrow_type = (Conclusion $ DT.Var 1)
deduce con arrow_type depth

arrow_type = (Conclusion $ DT.Var 0)
deduce con arrow_type depth

arrow_type = Arrow_Sigma (Conclusion $ DT.Var 1) (Conclusion $ DT.Var 3)
deduce con arrow_type depth


con = [Arrow [Conclusion $ DT.Var 2] (Conclusion $ DT.Var 2),Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Var 0),(Conclusion $ DT.Con $ T.pack "q")]
depth =  1
arrow_type = Arrow [Conclusion $ DT.Var 2] (Conclusion $ DT.Var 4)
deduce con arrow_type depth

con = [(Conclusion $ DT.Var 2),Arrow [Conclusion $ DT.Var 2] (Conclusion $ DT.Var 2),Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Var 0),(Conclusion $ DT.Con $ T.pack "q")]
depth =  1
arrow_type = Arrow [Conclusion $ DT.Var 3] (Conclusion $ DT.Var 5)
deduce con arrow_type depth


arrow_type = Arrow [Conclusion $ DT.Var 1] (Conclusion $ DT.Con $ T.pack "q")
pi_intro con arrow_type depth

arrow_type = Arrow [Conclusion $ DT.Var 1] (Conclusion $ DT.Var 2)
pi_intro con arrow_type depth


con = [Arrow [Conclusion $ DT.Var 0] (Conclusion $ DT.Var 2),(Conclusion $DT.Var 0),(Conclusion $ DT.Con $ T.pack "q")]
arrow_type =Conclusion $ DT.Var 2
deduce con arrow_type depth


test = Arrow [Conclusion $ DT.Con $ T.pack "q"] (Arrow [Conclusion $ DT.Con $ T.pack "q"] (Arrow_Sigma (Conclusion $ DT.Con $T.pack "s") (Conclusion $ DT.Con $T.pack "r")))
con =  [(Arrow [Conclusion $ DT.Con $ T.pack "p"] (Arrow_Sigma (Conclusion $ DT.Con $T.pack "s") (Conclusion $ DT.Con $T.pack "r"))),(Arrow [Conclusion $ DT.Con $ T.pack "q"] (Arrow_Sigma (Conclusion $ DT.Con $T.pack "s") (Conclusion $ DT.Con $T.pack "r"))),to1Arrow test,Conclusion $ DT.Con $ T.pack "q"]
b1 = (Arrow_Sigma (Conclusion $ DT.Con $T.pack "s") (Conclusion $ DT.Con $T.pack "r"))
deduce con b1 depth


con = [Arrow [Conclusion $ DT.Con $ T.pack "q"] (Conclusion $ DT.Var 3),Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Var 0),(Conclusion $ DT.Con $ T.pack "q")]
depth = 1
arrow_type = Arrow_Sigma (Conclusion $ DT.Var 2) (Conclusion $ DT.Var 3)
deduce con arrow_type depth
-}

--deduce (pi-intro + sigma-intro + membership + type-ax)
deduce :: [Arrowterm] -- ^ context
  -> Arrowterm -- ^  type
  -> Int -- ^ depth
  ->Either (String) ([AJudgement])


--type-ax
deduce _ (Conclusion DT.Kind) depth =
  if depth < maxdepth then Right [AJudgement [] (Conclusion DT.Type) (Conclusion DT.Kind)] else Left ("depth @ deduce - type-ax")
--
deduce con arrow_type depth =
  if (depth < maxdepth)
  then
    let judgements = map (\f -> withLog f con arrow_type depth) [membership,pi_intro,pi_elim,sigma_intro]
    in Right $ foldr (++) [] judgements
  else
    Left ("too deep @ deduce " ++ show con ++" | "++ show arrow_type)
