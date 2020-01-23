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
  in let f  = take f_len prestr in  (prestr2arrowstr f  h) ++ " " ++ (prestr2arrowstr  (drop (f_len) prestr) t)
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

-- | 入力されたDT.PretermをArrowTermに変換する
arrow_notat :: DT.Preterm -> Arrowterm
arrow_notat (DT.Type) = Conclusion $DT.Type
arrow_notat (DT.Var i) = Conclusion $ DT.Var i
arrow_notat (DT.Con i) = Conclusion $ DT.Con i
arrow_notat (DT.Not i) =
  Arrow [arrow_notat i] $Conclusion DT.Bot
arrow_notat (DT.Pi h t) =
  Arrow [arrow_notat h] (arrow_notat t)
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


typefromAJudgement :: AJudgement -> Arrowterm
typefromAJudgement ( AJudgement env aterm atype) = atype

termfromAJudgement :: AJudgement -> Arrowterm
termfromAJudgement ( AJudgement env aterm atype) = aterm

envfromAJudgement :: AJudgement -> [Arrowterm]
envfromAJudgement ( AJudgement env aterm atype) = env

dne =   DT.Lam ((DT.Pi (DT.Con (T.pack "Prop")) (DT.Pi (DT.Pi (DT.Pi (DT.Var 1)  DT.Bot) (DT.Bot)) (DT.Var 2))))
efq = DT.Lam (DT.Pi DT.Bot (DT.Var 1))

classic = [dne,efq]

getAxiom :: String -> TEnv
getAxiom "classic"= classic ++ []

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
arrow_subst term i m= arrow_notat $ subst (arrow2DT term) (arrow2DT i) (arrow2DT m)
{-
prove([p:prop,q:prop,r:prop,s:pi(X:false,p)],_X: (p \/ ~p) ).
reverse([p:prop,q:prop,r:prop,s:pi(X:false,p)],I_Context),get_act_contexts(Base_Context),append(I_Context,Base_Context,Context),initialize_fresh_vars(Context,[v,a]),standard_context_notat(Context,N_Context),reduce_context(N_Context,NR_Context),check_context(NR_Context),copy_term(NR_Context,[X:T1|Tail]),check_type(T1,_T3,Tail).
-}

prove ::  TEnv -- ^ var_context ex)[(DT.Con (T.pack "prop")),(DT.Con (T.pack "set")),(DT.Con (T.pack "prop"))]
  -> SUEnv -- ^ sig_context ex)[((T.pack "prop"),DT.Type),((T.pack "set"),DT.Type)]
  -> DT.Preterm -- type ex) (DT.Pi (DT.Var 0) (DT.Sigma (DT.Var 0,DT.Var 3)))
  -> DT.Preterm -- term
prove var_env sig_env preterm =
  --TEnvをPTEnvに変える
  let var_env' = (reverse var_env) ++ (getAxiom "classic")
      arrow_env = map (arrow_notat . DT.toDTT . UD.betaReduce . DT.toUDTT) var_env'
      arrow_type = (arrow_notat . DT.toDTT . UD.betaReduce . DT.toUDTT) preterm
      forward_env = forward_context arrow_env
      arrow_term = search_proof forward_env arrow_type 1
  in undefined
--initialize_fresh_vars

-- pi_rules = [(Type Set, Type Set),  (Type Set, Kind Type_prop),  (Kind Type_prop, Kind Type_prop),  (Type Prop, Type Prop),  (Type Set, Type Prop),  (Kind Type_prop, Type Prop)]
-- sigma_rules = [(Type Set,Type Prop),(Type Prop, Type Prop)]
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
test1 = Arrow [Arrow_Proj Arrow_Fst (Conclusion DT.Type)] (Arrow_Proj Arrow_Snd (Conclusion (DT.Var 0)))
test2 = Arrow_Sigma (Conclusion $ DT.Con $ T.pack "p") (Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 1))
test3 = Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 1)
test4=(Arrow_Sigma (Conclusion $ DT.Con $ T.pack "p") (Arrow [Conclusion $ DT.Con $ T.pack "q"] (Arrow_Sigma (Conclusion $ DT.Var 1) (Conclusion $ DT.Var 3))))
test5 = Arrow_Sigma (Arrow [(Arrow [(Conclusion (DT.Con (T.pack "p")))] (Conclusion (DT.Var 0))),Conclusion DT.Type](Conclusion (DT.Con (T.pack "p")))) (Conclusion (DT.Con (T.pack "q")))
context = [test1,test2,test3,test4,test5]


forward_context :: AEnv -> [AJudgement]
-- forward_context [] = []
-- forward_context (f:r) =
--   map (\(AJudgement env aterm atype) -> AJudgement (f:r) aterm atype) (forward f)  ++ forward_context r

forward_context context =
  foldr
    (++)
    (foldr
      (++)
      []
      (map (\((varid',con_type),fr) -> [AJudgement (tail fr) con_type (Conclusion $ DT.Type)] ++ [AJudgement fr (gen_free_con con_type ("context" ++ (show (length context -varid'))) ) con_type]) $ zip (zip [1..] context) $ map reverse $ reverse $ tail $ L.inits $ reverse $ context))
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
add_App num base = Arrow_App (shiftIndices base 1 0) (Conclusion $ DT.Var 0)

add_Lam :: Int -> Arrowterm -> Arrowterm
add_Lam 0 term = term
add_Lam num term = Arrow_Lam $ add_Lam (num - 1) term

maxdepth = 5
search_proof :: [AJudgement] -> Arrowterm -> Int -> [Arrowterm]
-- search_proof(forward_env,arrow_term,depth) =
--   --time check
--   if depth < maxdepth
--     then
--       let typelst = deduce(forward_env,arrowterm,depth)
--       in typelist ++ search_proof(forward_env,arrow_term,depth + 1)
--     else
--       []
search_proof forward_env arrow_type depth =
  foldr (++) [] (map (search_proof forward_env arrow_type) [1..depth])

{-
sigma_con = [Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Con $ T.pack "p"),(Conclusion $ DT.Con $ T.pack "q")]
context = forward_context sigma_con
arrow_type = (Conclusion DT.Type)
depth = 1
deduce sigma_con context arrow_type depth

sigma_con = [Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Con $ T.pack "p"),(Conclusion $ DT.Con $ T.pack "q")]
context = forward_context sigma_con
arrow_type = (Conclusion DT.Kind)
depth = 1
deduce sigma_con context arrow_type depth

sigma_con = [Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Con $ T.pack "p"),(Conclusion $ DT.Con $ T.pack "q")]
context = forward_context sigma_con
arrow_type = (Conclusion $ DT.Con $ T.pack  "p")
depth = 1
deduce sigma_con context arrow_type depth

sigma_con = [Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Con $ T.pack "p"),(Conclusion $ DT.Con $ T.pack "q")]
context = forward_context sigma_con
arrow_type = (Conclusion $ DT.Var 2)
depth = 1
deduce sigma_con context arrow_type depth

sigma_con = [Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Con $ T.pack "p"),(Conclusion $ DT.Con $ T.pack "q")]
context = forward_context sigma_con
arrow_type = (Conclusion $ DT.Var 1)
depth = 1
deduce sigma_con context arrow_type depth

sigma_con = [Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Con $ T.pack "p"),(Conclusion $ DT.Con $ T.pack "q")]
context = forward_context sigma_con
arrow_type = (Conclusion $ DT.Var 0)
depth = 1
deduce sigma_con context arrow_type depth

sigma_con = [Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Con $ T.pack "p"),(Conclusion $ DT.Con $ T.pack "q")]
context = forward_context sigma_con
arrow_type = Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2)
depth = 1
deduce sigma_con context arrow_type depth
-}


membership :: [Arrowterm] -> [AJudgement] ->  Arrowterm -> Int -> [AJudgement]
membership con context arrow_type depth =
  let boollst = map (\x -> shiftIndices (typefromAJudgement x) ((length con) - (length $ envfromAJudgement x)) 0 == arrow_type) context
  in
    if (or boollst)
      then
        map (fst) $ filter (snd) $ zip context boollst
      else
        []
--
-- pi_form :: [AJudgement]-> [Arrowterm] -> [Arrowterm] -> Arrowterm->Int->[Arrowterm]
--
-- pi_form context type_terms _ (Conclusion DT.Type) depth =
--   --type型を持つ項a1,...,anについて一つ一つextendした[(a1,[AJudgement]),(a2,[AJudgement])...]
--   --type型を持つ項a1,...,anについてありえる(as,b1),...,(as,bm)を並べた[[(Arrowterm,Arrowterm)]]
--   let extendedContexts = map (\aterm -> (aterm, (forward_context [aterm] ++ context))) type_terms
--       a_bss = map (\(aterm,aenv) -> (map (\b -> Arrow [aterm] b ) (deduce aenv (Conclusion DT.Type) (depth + 1)))) extendedContexts
--   in foldr (++) [] a_bss
-- pi_form context _ kind_terms (Conclusion DT.Kind) depth =
--   let extendedContexts = map (\aterm -> (aterm, (forward_context [aterm] ++ context))) kind_terms
--       a_bss = map (\(aterm,aenv) -> (map (\b -> Arrow [aterm] b ) (deduce aenv (Conclusion DT.Kind) (depth + 1)))) extendedContexts
--   in foldr (++) [] a_bss
-- pi_form _ _ _ _ _= []
--
-- norm_lab :: [Arrowterm] -> Arrowterm -> Arrowterm
-- norm_lab [] term = term
-- normlab (f:r) term = norm_lab r (Arrow_Lam term)
--
-- pi_intro :: [AJudgement] -> Arrowterm -> Int -> [Arrowterm]
-- pi_intro context (Arrow a b) depth =
--   let extendedContext = (forward_context a) ++ context
--       cs = deduce extendedContext b (depth + 1)
--   in map (norm_lab a) cs
--   --undefined
--
-- pi_elim :: [AJudgement]->Arrowterm->Int->[Arrowterm]
-- -- pi_elim context b1 depth =
-- pi_elim context arrow_type depth = undefined

--
deduce :: [Arrowterm] -- ^ context
  ->[AJudgement] -- ^ forward_context
  ->Arrowterm -- ^ term
  ->Int -- ^ depth
  ->[AJudgement]


--type-ax
deduce _  con (Conclusion DT.Kind) depth = if depth < maxdepth then [AJudgement [] (Conclusion DT.Type) (Conclusion DT.Kind)] else []
--
deduce con f_context arrow_type depth =
  let type_terms = [undefined]
      kind_terms = [undefined]
    in (membership con f_context arrow_type depth) -- ++ (pi_form context type_terms kind_terms arrow_type depth) ++ []
