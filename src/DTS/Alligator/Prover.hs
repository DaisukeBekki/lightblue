module DTS.Alligator.Prover
(
  prove,
  prove',
  proveWithEFQ',
  forward,
  deduce
) where

import qualified DTS.DTT as DT            -- DTT
import qualified DTS.UDTT as UD            -- DTT
import qualified DTS.Alligator.Arrowterm as A -- Arrowterm
import qualified Data.Text.Lazy as T      -- text
import qualified Data.List as L           -- base
import qualified DTS.Prover.Judgement as J
import qualified Debug.Trace as D
import qualified System.Timeout as ST


prove ::  A.TEnv -- ^ var_context ex)[(DT.Con (T.pack "prop")),(DT.Con (T.pack "set")),(DT.Con (T.pack "prop"))]
  -> A.TEnv -- ^ sig_context ex)[((T.pack "prop"),DT.Type),((T.pack "set"),DT.Type)] , classic
  -> DT.Preterm -- type ex) (DT.Pi (DT.Var 0) (DT.Sigma (DT.Var 0,DT.Var 3)))
  -> [J.Judgement] -- term
prove var_env sig_env pre_type =
  --A.TEnvをPA.TEnvに変える
  let arrow_terms = proveWithDNE' var_env sig_env pre_type
  in  map A.a2dtJudgement arrow_terms

prove' ::  A.TEnv -- ^ var_context ex)[(DT.Con (T.pack "prop")),(DT.Con (T.pack "set")),(DT.Con (T.pack "prop"))]
  -> A.TEnv -- ^ sig_context ex)[((T.pack "prop"),DT.Type),((T.pack "set"),DT.Type)] , classic
  -> DT.Preterm -- type ex) (DT.Pi (DT.Var 0) (DT.Sigma (DT.Var 0,DT.Var 3)))
  -> [A.AJudgement] -- term  /変更:DT.Judgemnetにする
prove' var_env sig_env pre_type =
  --A.TEnvをPA.TEnvに変える
  let var_env' = var_env ++ sig_env
      arrow_env = map (A.arrow_notat . DT.toDTT . UD.betaReduce . DT.toUDTT) var_env'
      arrow_type = (A.arrow_notat . DT.toDTT . UD.betaReduce . DT.toUDTT) pre_type
      arrow_terms = search_proof arrow_env arrow_type 1
  in  arrow_terms

proveWithEFQ' ::  A.TEnv -- ^ var_context ex)[(DT.Con (T.pack "prop")),(DT.Con (T.pack "set")),(DT.Con (T.pack "prop"))]
  -> A.TEnv -- ^ sig_context ex)[((T.pack "prop"),DT.Type),((T.pack "set"),DT.Type)] , classic
  -> DT.Preterm -- type ex) (DT.Pi (DT.Var 0) (DT.Sigma (DT.Var 0,DT.Var 3)))
  -> [A.AJudgement] -- term  /変更:DT.Judgemnetにする
proveWithEFQ' var_env sig_env pre_type =
  --A.TEnvをPA.TEnvに変える
  let var_env' = var_env ++ sig_env
      arrow_env = map (A.arrow_notat . DT.toDTT . UD.betaReduce . DT.toUDTT) var_env'
      arrow_type = (A.arrow_notat . DT.toDTT . UD.betaReduce . DT.toUDTT) pre_type
      arrow_terms = search_proof_with_EFQ arrow_env arrow_type 1
  in  arrow_terms

proveWithDNE' ::  A.TEnv -- ^ var_context ex)[(DT.Con (T.pack "prop")),(DT.Con (T.pack "set")),(DT.Con (T.pack "prop"))]
  -> A.TEnv -- ^ sig_context ex)[((T.pack "prop"),DT.Type),((T.pack "set"),DT.Type)] , classic
  -> DT.Preterm -- type ex) (DT.Pi (DT.Var 0) (DT.Sigma (DT.Var 0,DT.Var 3)))
  -> [A.AJudgement] -- term  /変更:DT.Judgemnetにする
proveWithDNE' var_env sig_env pre_type =
  --A.TEnvをPA.TEnvに変える
  let var_env' = var_env ++ sig_env
      arrow_env = map (A.arrow_notat . DT.toDTT . UD.betaReduce . DT.toUDTT) var_env'
      arrow_type = (A.arrow_notat . DT.toDTT . UD.betaReduce . DT.toUDTT) pre_type
      arrow_terms = search_proof_with_DNE arrow_env arrow_type 1
  in  arrow_terms

pi_rules = [(DT.Type, DT.Type),  (DT.Type, DT.Kind),  (DT.Kind, DT.Kind),   (DT.Kind, DT.Type)]
sigma_rules = [(DT.Type,DT.Type)]



forward_context :: [A.Arrowterm] -> [A.AJudgement]

-- forward_context [] = []
-- forward_context (f:r) =
--   map (\(A.AJudgement env aterm atype) -> A.AJudgement (f:r) aterm atype) (forward f)  ++ forward_context r
--let boollst = map (\x -> A.shiftIndices (A.typefromAJudgement x) ((length con) - (length $ A.envfromAJudgement x)) 0 == arrow_type) context
forward_context context =
  map
    (\ (A.AJudgement env a_term a_type) -> A.AJudgement context (A.shiftIndices a_term ((length context) - (length env)) 0) (A.shiftIndices a_type ( (length context) - (length env)) 0))
    $ foldr
      (++)
      (foldr
        (++)
        []
        -- (map (\((varid',con_type),fr) -> [A.AJudgement (tail fr) con_type (A.Conclusion $ DT.Type)] ++ [A.AJudgement fr (gen_free_con con_type ("context" ++ (show (length context -varid'))) ) (A.shiftIndices con_type 1 0)]) $ zip (zip [1..] context) $ map reverse $ reverse $ tail $ L.inits $ reverse $ context))
        (map (\((varid',con_type),fr) -> [A.AJudgement (tail fr) con_type (A.Conclusion $ DT.Type)] ++ [A.AJudgement fr (A.Conclusion $ DT.Var 0 ) (A.shiftIndices con_type 1 0)]) $ zip (zip [1..] context) $ map reverse $ reverse $ tail $ L.inits $ reverse $ context))
      $ map ( \(f,fr) -> map (\(A.AJudgement env aterm atype) -> A.AJudgement (fr) aterm atype) (forward f) ) $ zip context $ map reverse $ reverse $ tail $ L.inits $ reverse $ context




-- | generate free constraint from given word
gen_free_con :: A.Arrowterm -- ^ term
  -> String -- ^ "hoge"
  -> A.Arrowterm
gen_free_con term hoge =
  if is_free_con term (A.Conclusion $ DT.Con $ T.pack hoge)
    then
      (A.Conclusion $ DT.Con $ T.pack hoge)
    else
      (gen_free_con term $hoge++hoge)

-- | whether DT.Con "hoge" is free or not
is_free_con :: A.Arrowterm  -- ^ term
  -> A.Arrowterm  -- ^ DT.Con "hoge"
  -> Bool
is_free_con term con=
  let term' = A.arrow_subst term con con
      term'' = A.arrow_subst term (A.Conclusion $ DT.Var 0) con
  in term' == term''


forward :: A.Arrowterm -> [A.AJudgement]
forward term =
  let base_con = gen_free_con term "base"
  in map (\(A.AJudgement con a_term a_type) -> A.AJudgement con (A.arrow_subst (A.shiftIndices a_term 1 0) (A.Conclusion $DT.Var 0) base_con ) (A.arrow_subst (A.shiftIndices a_type 1 0) (A.Conclusion $  DT.Var 0) base_con)) $ forward' [term] base_con term


forward' :: [A.Arrowterm] -- ^ origin
  ->   A.Arrowterm -- ^ base
  ->   A.Arrowterm -- ^ target
  ->  [A.AJudgement]
forward' context base (A.Arrow_Sigma h t) =
  let --t' = A.arrow_subst t (A.Arrow_Proj A.Arrow_Fst base) (A.Conclusion $ DT.Var 0)
      t' = A.shiftIndices (A.arrow_subst t  (A.Arrow_Proj A.Arrow_Fst base) (A.Conclusion $ DT.Var 0)) (-1) 0
      h_forward = forward' context (A.Arrow_Proj A.Arrow_Fst base) h
      t_forward = forward' context (A.Arrow_Proj A.Arrow_Snd base) t'
  -- in (if t_forward == [] then [A.AJudgement context (A.Arrow_Proj A.Arrow_Snd  base) t'] ++ ([A.AJudgement context t' (A.Conclusion $ DT.Type)]) else t_forward) ++ (if h_forward == [] then [A.AJudgement context (A.Arrow_Proj A.Arrow_Fst base) h]++ ([A.AJudgement context h (A.Conclusion $ DT.Type)]) else h_forward)
  in (if t_forward == [] then [A.AJudgement context (A.Arrow_Proj A.Arrow_Snd  base) t'] else t_forward) ++ (if h_forward == [] then [A.AJudgement context (A.Arrow_Proj A.Arrow_Fst base) h] else h_forward)
forward' context base (A.Arrow env (A.Arrow_Sigma h t)) =
  let term1 = add_Lam (length env) $ A.Arrow_Proj A.Arrow_Fst $ add_App (length env) base
      term2 = add_Lam (length env) $ A.Arrow_Proj A.Arrow_Snd $ add_App (length env) base
      t' = A.shiftIndices (A.arrow_subst t (A.shiftIndices term1 (length env) 0) (A.Conclusion $ DT.Var 0)) (-1) 0
      type1 = A.Arrow env h
      type2 = A.Arrow env t'
      h_forward = forward' context term1 type1
      t_forward = forward' context term2 type2
  in (if t_forward == [] then [A.AJudgement context term2 type2] else t_forward) ++ (if h_forward == [] then [A.AJudgement context term1 type1] else h_forward)
forward' context base  (A.Arrow a (A.Arrow b c)) =
  forward' context base (A.Arrow (b ++ a) c)
forward' context base arrowterm = []


add_App ::Int -> A.Arrowterm -> A.Arrowterm
add_App 0 base = base
add_App num base = add_App (num - 1) $ A.Arrow_App (A.shiftIndices base 1 0) (A.Conclusion $ DT.Var 0)

add_Lam :: Int -> A.Arrowterm -> A.Arrowterm
add_Lam 0 term = term
add_Lam num term = A.Arrow_Lam $ add_Lam (num - 1) term

maxdepth = 9

maxtime = 100000


search_proof :: [A.Arrowterm] ->A.Arrowterm -> Int -> [A.AJudgement]
search_proof arrow_env arrow_type depth = deduceWithLog arrow_env arrow_type depth
--  L.nub $ foldr (++) [] (map (\d -> deduceWithLog con arrow_type d) [1..(maxdepth -1)])

search_proof_with_EFQ :: [A.Arrowterm] ->A.Arrowterm -> Int -> [A.AJudgement]
search_proof_with_EFQ arrow_env arrow_type depth = deduceWithEFQWithLog arrow_env arrow_type depth

search_proof_with_DNE :: [A.Arrowterm] ->A.Arrowterm -> Int -> [A.AJudgement]
search_proof_with_DNE arrow_env arrow_type depth = deduceWithDNEWithLog arrow_env arrow_type depth



membership :: [A.Arrowterm] ->  A.Arrowterm -> Int -> Either (String) ([A.AJudgement])
membership con arrow_type depth =
  let context = forward_context con
      boollst = map (\x -> A.shiftIndices (A.typefromAJudgement x) ((length con) - (length $ A.envfromAJudgement x)) 0 == arrow_type) context
  in
    if (or boollst)
      then
        Right $ map (fst) $ filter (snd) $ zip context boollst
      else
        Right $ []





pi_form :: [A.Arrowterm] -> A.Arrowterm ->  A.Arrowterm -> Int -> Either (String) ([A.AJudgement])
pi_form con (A.Arrow as b) arrow_type depth =
  if (arrow_type `elem`[A.Conclusion DT.Type,A.Conclusion DT.Kind]) && (depth < maxdepth)
  then
    if length as == 1
    then
      let a = head as
          a_term = foldr (++) [] $ map (\dtterm -> withLog' typecheck con a (A.Conclusion dtterm) (depth + 1)) [DT.Type,DT.Kind]
      in
        if a_term == []
        then Right []
        else
          let b_env = a: con
              b_js =  withLog' typecheck b_env b arrow_type (depth + 1)
          in
            Right $ map (\b_j -> A.AJudgement con (A.Arrow as b) arrow_type) b_js
    else
      let a = last as
          a_term = foldr (++) [] $map (\dtterm -> withLog' typecheck con a (A.Conclusion dtterm) (depth + 1)) [DT.Type,DT.Kind]
      in
        if a_term == [] then
          Right []
        else
          let
              b_env = a : con
              b_term = A.Arrow (init as) b
              b_js =  withLog' typecheck b_env b_term arrow_type (depth + 1)
          in
            Right $ map (\b_j ->A.AJudgement con (A.Arrow as b) (A.typefromAJudgement b_j)) b_js

  else
    if depth < maxdepth
    then
      Left ("too deep @ pi_form " ++ show con ++" | "++ show arrow_type)
    else
      Right []
pi_form con arrow_term arrow_type depth = Right []

sigma_form:: [A.Arrowterm] -> A.Arrowterm ->  A.Arrowterm -> Int -> Either (String) ([A.AJudgement])
sigma_form con (A.Arrow_Sigma a b) arrow_type depth =
  if (arrow_type `elem`[A.Conclusion DT.Type,A.Conclusion DT.Kind]) && (depth < maxdepth)
  then
    let a_term = foldr (++) [] $ map (\dtterm -> withLog' typecheck con a (A.Conclusion dtterm) depth) [DT.Type,DT.Kind]
    in
      if a_term == []
      then
        Right []
      else
        let b_js = foldr (++) [] $ map (\dtterm -> withLog' typecheck (a:con) b (A.Conclusion dtterm) depth) [DT.Type,DT.Kind]
        in
          Right $
            map
            (\b_j -> A.AJudgement con (A.Arrow_Sigma a b) (A.typefromAJudgement b_j))
            b_js
  else
    if depth < maxdepth
    then
      Left ("too deep @ pi_form " ++ show con ++" | "++ show arrow_type)
    else
      Right []

sigma_form con arrow_term arrow_type depth = Right []

pi_intro :: [A.Arrowterm] ->  A.Arrowterm -> Int -> Either (String) ([A.AJudgement])
pi_intro con (A.Arrow a b) depth =
  if depth > maxdepth
  then
    Left ("too deep @ pi-intro " ++ show con ++" ト "++ show (A.Arrow a b))
  else
    if ((foldr (++) [] $ map (\dtterm -> withLog' typecheck con (A.Arrow a b) (A.Conclusion dtterm) depth) [DT.Type,DT.Kind]) == [])
    then
      Right []
    else
      let
          b_judgements = deduceWithDNEWithLog (a ++ con) b (depth + 1)
          pi_a_b_judgements =
            map
              (\b_j ->
                let
                  env = con
                  a_term = add_Lam (length a) (A.termfromAJudgement b_j)
                  a_type = A.Arrow a b
                in
                  A.AJudgement env a_term a_type)
              b_judgements
      in
        Right pi_a_b_judgements
pi_intro  con arrow_type depth= Right []

tail_is_b :: A.Arrowterm -> A.Arrowterm -> Bool
tail_is_b (A.Arrow env b') b=
  if (A.shiftIndices b (length env) 0)==b'
  then
    True
  else
    case b' of
      A.Arrow b1 b2 -> tail_is_b (A.Arrow b1 b2) (A.shiftIndices b (length env) 0)
      otherwise -> False
tail_is_b _ _ =False

head_is_type :: A.Arrowterm -> Bool
head_is_type (A.Arrow env _)=
  if last env == (A.Conclusion $ DT.Type)
  then
    True
  else
    case last env of
      A.Arrow b1 b2 -> head_is_type (A.Arrow b1 b2)
      otherwise -> False
head_is_type _=False

arrow_conclusion_b :: [A.AJudgement] -> A.Arrowterm -> [A.AJudgement]
arrow_conclusion_b judgements b=
  filter (\j -> tail_is_b (A.typefromAJudgement j) b) judgements

arrow_conclusion_b' :: [A.Arrowterm] -> A.Arrowterm -> Int -> [A.AJudgement]
arrow_conclusion_b' con b depth=
  if [] /= (withLog' typecheck con b (A.Conclusion $ DT.Type) depth)
  then
    map
      (\(A.AJudgement env a_term a_type) -> A.AJudgement env (A.Arrow_App a_term b) a_type)
      $filter
        (\(A.AJudgement _ _ j_type) ->
          case  j_type of
            A.Arrow j_a j_b -> (tail_is_b j_b (A.Conclusion $ DT.Var 0)) && (head_is_type j_type)
            _ -> False)
        (forward_context con)
  else
    []

deduce_envs :: [A.Arrowterm] -> [A.AJudgement] -> Int-> [(A.AJudgement,[[A.AJudgement]])]
deduce_envs con a_judgements depth =
  let ajudges = map (\env -> mapM (\a -> deduceWithLog con a depth) env) $ map (\(A.AJudgement _ _ (A.Arrow env _)) -> env) a_judgements
  in filter ((/= []) . snd) $zip a_judgements ajudges

app_as :: A.Arrowterm -> [A.Arrowterm] -> A.Arrowterm
app_as term [] = term
app_as term (f:r) =
  app_as (A.Arrow_App term f) r

subst_as_in_pi_elim :: A.Arrowterm -> [A.Arrowterm] -> A.Arrowterm
subst_as_in_pi_elim term [] = term
subst_as_in_pi_elim term (f:r) =
  subst_as_in_pi_elim (A.shiftIndices (A.arrow_subst term f (A.Conclusion $ DT.Var 0)) (-1) 0) r

pi_elim :: [A.Arrowterm] -> A.Arrowterm->Int->Either (String) ([A.AJudgement])
pi_elim con b1 depth =
  if depth > maxdepth
  then
    Left ("too deep @ pi_elim " ++ show con ++" ト "++ show b1)
  else
    let a_judgements = arrow_conclusion_b (forward_context con) b1
                        -- ++ arrow_conclusion_b' con b1 (depth + 1)
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
                      a_terms' = app_as (A.termfromAJudgement base) $map (A.termfromAJudgement) as
                      a_type' = subst_as_in_pi_elim b1 $map (A.termfromAJudgement) as
                  in
                    A.AJudgement env' a_terms' a_type')
                ass
          )
          a_type_terms

sigma_intro :: [A.Arrowterm] ->  A.Arrowterm -> Int -> Either (String) ([A.AJudgement])
sigma_intro con  (A.Arrow_Sigma a b1) depth =
  if depth > maxdepth
  then
    Left ("too deep @ sigma_intro " ++ show con ++" ト "++ show (A.Arrow_Sigma a b1))
  else
    if foldr (++) [] (map (\dtterm ->withLog' typecheck con (A.Arrow_Sigma a b1) (A.Conclusion dtterm) depth) [DT.Type,DT.Kind]) == []
    then
      Right []
    else
      let a_term_judgements =deduceWithLog con a (depth + 1)
          a_b1_terms_judgements =
            map
            (\a_j ->
              let con2b =  con
                  base = (gen_free_con b1 "base")
                  b_type = A.reduce $ A.arrow_subst (A.shiftIndices (A.arrow_subst b1 base (A.Conclusion $ DT.Var 0)) (-1) 0) (A.termfromAJudgement a_j) base
--shiftIndices (A.arrow_subst b1 (A.termfromAJudgement a_j) (A.Conclusion $ DT.Var 0)) (-1) 0
              in
                (a_j,deduceWithLog con b_type depth ))
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
                      a_term = A.Arrow_Pair (A.termfromAJudgement a_j)  (A.termfromAJudgement b1_j)
                      a_type = (A.Arrow_Sigma a b1)
                    in A.AJudgement env a_term a_type)
                  b1_js)
              a_b1_terms_judgements

sigma_intro  con  arrow_type depth=Right []

deduceWithLog :: [A.Arrowterm] -> A.Arrowterm ->Int ->[A.AJudgement]
deduceWithLog = withLog deduce

deduceWithEFQWithLog :: [A.Arrowterm] -> A.Arrowterm ->Int ->[A.AJudgement]
deduceWithEFQWithLog = withLog deduceWithEFQ

deduceWithDNEWithLog :: [A.Arrowterm] -> A.Arrowterm ->Int ->[A.AJudgement]
deduceWithDNEWithLog = withLog deduceWithDNE

withLog' :: ([A.Arrowterm] -> A.Arrowterm -> A.Arrowterm ->Int ->Either (String) ([A.AJudgement])) -> [A.Arrowterm] -> A.Arrowterm -> A.Arrowterm ->Int ->[A.AJudgement]
withLog' f con arrow_term arrow_type depth =
  case f con arrow_term arrow_type depth of
    (Right j) -> j
    (Left msg) -> []--D.trace msg []

withLog :: ([A.Arrowterm] -> A.Arrowterm ->Int ->Either (String) ([A.AJudgement])) -> [A.Arrowterm] -> A.Arrowterm ->Int ->[A.AJudgement]
withLog f con arrow_type depth =
  case f con arrow_type depth of
    (Right j) -> j
    (Left msg) -> [] --D.trace msg []



typecheck :: [A.Arrowterm] -- ^ context
  -> A.Arrowterm -- ^ term
  -> A.Arrowterm -- ^ type
  -> Int -- ^ depth
  -> Either (String) ([A.AJudgement])

typecheck con (A.Conclusion DT.Bot) (A.Conclusion DT.Type) depth =
  Right [A.AJudgement con (A.Conclusion DT.Bot) (A.Conclusion DT.Type)]
typecheck con arrow_term arrow_type depth =
  if (depth < maxdepth)
  then
    let judgements = foldr (++) [] $ map (\f -> withLog f con arrow_type depth) [membership,pi_intro,pi_elim,sigma_intro]
        deducejudgements =filter (\a ->(A.termfromAJudgement a) == (A.shiftIndices arrow_term ((length $ A.envfromAJudgement a)-(length con) ) 0))$ judgements
    in
      Right $ deducejudgements ++ (foldr (++) [] $ map (\f -> withLog' f con arrow_term arrow_type depth) [pi_form,sigma_form])
  else
    Left ("too deep @ typecheck " ++ show con ++" | "++ show arrow_type)


deduceWithEFQ :: [A.Arrowterm] -- ^ context
  -> A.Arrowterm -- ^  type
  -> Int -- ^ depth
  ->Either (String) ([A.AJudgement])
deduceWithEFQ con arrow_type depth=
  if (depth < maxdepth)
  then
    let judgements = map (\f -> withLog f con arrow_type depth) [efq,membership,pi_intro,pi_elim,sigma_intro]
    in Right $ foldr (++) [] judgements
  else
    Left ("too deep @ deducewithEFQ " ++ show con ++" | "++ show arrow_type)


efq :: [A.Arrowterm] -> A.Arrowterm->Int->Either (String) ([A.AJudgement])
efq con b depth =
  if depth > maxdepth
  then
    Left ("too deep @ efq " ++ show con ++" ト "++ show b)
  else
    if [] /= (withLog' typecheck con b (A.Conclusion DT.Type) (depth + 1))
    then
      let bot_js = deduceWithLog con (A.Conclusion DT.Bot) (depth + 1)
      in
        if bot_js == []
        then
          Right []
        else
          Right
            $map
            (\(A.AJudgement env a_term a_type) -> (A.AJudgement env (A.Arrow_App (A.Conclusion $ DT.Con $T.pack "efq") a_term) b))
            bot_js
    else
      Right []

deduceWithDNE :: [A.Arrowterm] -- ^ context
  -> A.Arrowterm -- ^  type
  -> Int -- ^ depth
  ->Either (String) ([A.AJudgement])
deduceWithDNE con arrow_type depth=
  if (depth < maxdepth)
  then
    if (arrow_type == A.Conclusion DT.Bot)
    then
      let judgements = map (\f -> withLog f con arrow_type depth) [membership,pi_intro,pi_elim,sigma_intro]
      in Right $ foldr (++) [] judgements
    else
      let judgements = map (\f -> withLog f con arrow_type depth) [dne,membership,pi_intro,pi_elim,sigma_intro]
      in Right $ foldr (++) [] judgements
  else
    Left ("too deep @ deducewithEFQ " ++ show con ++" | "++ show arrow_type)


dne :: [A.Arrowterm] -> A.Arrowterm->Int->Either (String) ([A.AJudgement])
dne con b depth =
  if depth > maxdepth
  then
    Left ("too deep @ dne " ++ show con ++" ト "++ show b)
  else
      if [] /= (withLog' typecheck con b (A.Conclusion DT.Type) (depth + 1))
      then
        let dne_js = deduceWithLog con (A.Arrow [A.Arrow [b] (A.Conclusion DT.Bot)] (A.Conclusion DT.Bot)) (depth + 1)
        in
          if dne_js == []
          then
            Right []
          else
            Right
              $map
              (\(A.AJudgement env a_term a_type) -> (A.AJudgement env (A.Arrow_App (A.Conclusion $ DT.Con $T.pack "dne") a_term) b))
              dne_js
      else
        Right []
--deduce (pi-intro + sigma-intro + membership + type-ax)
{-
justtimeout :: IO [Char]
justtimeout =
  -- (ST.timeout 0 (return "finished on time"))
  --   >>=(\x -> "s")
  timetest >>= (\x -> case x of
    Just iochar -> return iochar
    Nothing -> return []
  )
  -- case (timetest) of
  --   Just iochar -> (iochar >>= (\x -> x))
  --   Nothing -> Nothing

timetest :: IO (Maybe [Char])
timetest = ST.timeout 0 (return "finished on time")

inttest :: IO (Maybe Int)
inttest = ST.timeout 0 (return $ 1+1)
-}
deduce :: [A.Arrowterm] -- ^ context
  -> A.Arrowterm -- ^  type
  -> Int -- ^ depth
  ->Either (String) ([A.AJudgement])

--type-ax
deduce _ (A.Conclusion DT.Kind) depth =
  if depth < maxdepth then Right [A.AJudgement [] (A.Conclusion DT.Type) (A.Conclusion DT.Kind)] else Left ("depth @ deduce - type-ax")
--
deduce con arrow_type depth =
  if (depth < maxdepth)
  then
    let judgements = foldl (\js -> (\f -> if (js == []) then (withLog f con arrow_type depth) else js)) [] [membership,pi_intro,pi_elim,sigma_intro]
    in Right  judgements
  else
    Left ("too deep @ deduce " ++ show con ++" | "++ show arrow_type)
-- deduce con arrow_type depth =
--   if (depth < maxdepth)
--   then
--     let judgements = map (\f -> withLog f con arrow_type depth) [membership,pi_intro,pi_elim,sigma_intro]
--     in Right $ foldr (++) [] judgements
--   else
--     Left ("too deep @ deduce " ++ show con ++" | "++ show arrow_type)
