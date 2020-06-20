module DTS.Alligator.ProofTree
(
  prove,
  settingDef,
  settingDNE,
  settingEFQ,
  ProofMode(..),
  Setting(..),
  announce
) where

import qualified DTS.DTT as DT            -- DTT
import qualified DTS.UDTT as UD            -- DTT
import qualified DTS.Alligator.Arrowterm as A -- Arrowterm
import qualified Data.Text.Lazy as T      -- text
import qualified Data.List as L           -- base
import qualified DTS.Prover.Judgement as J
import qualified Debug.Trace as D

import qualified Data.Text.Lazy.IO as T
import qualified Interface.HTML as HTML

data ProofMode = Plain | WithDNE | WithEFQ deriving (Show,Eq)
data Setting = Setting {mode :: ProofMode,falsum :: Bool,maxdepth :: Int,maxtime :: Int}



settingDef = Setting{mode = Plain,falsum = True,maxdepth = 9,maxtime = 100000}
settingDNE = Setting{mode = WithDNE,falsum = True,maxdepth = 9,maxtime = 100000}
settingEFQ = Setting{mode = WithEFQ,falsum = True,maxdepth = 9,maxtime = 100000}


-- aannounce :: [J.Tree A.AJudgement] -> IO()
-- aannounce [] = putStrLn "Nothing to announce"
-- aannounce atrees = announce (map A.aTreeTojTree atrees)
--
-- announce :: [J.Tree J.Judgement] -> IO()
-- announce [] = putStrLn "Nothing to announce"
-- announce jtrees = do
--   let jtree = head jtrees
--   putStrLn HTML.htmlHeader4MathML
--   T.putStrLn HTML.startMathML
--   T.putStrLn $J.treeToMathML jtree
--   T.putStrLn HTML.endMathML
--   putStrLn HTML.htmlFooter4MathML

announce :: [J.Tree J.Judgement] -> IO T.Text
announce [] = return $ T.pack "Nothing to announce"
announce jtrees = do
  let jtree = head jtrees
  return
    $ T.append (T.pack HTML.htmlHeader4MathML) $
      T.append HTML.startMathML $
      T.append (J.treeToMathML jtree) $
      T.append HTML.endMathML
       (T.pack HTML.htmlFooter4MathML)

prove ::  A.TEnv -- ^ var_context ex)[(DT.Con (T.pack "prop")),(DT.Con (T.pack "set")),(DT.Con (T.pack "prop"))]
  -> A.TEnv -- ^ sig_context ex)[((T.pack "prop"),DT.Type),((T.pack "set"),DT.Type)] , classic
  -> DT.Preterm -- type ex) (DT.Pi (DT.Var 0) (DT.Sigma (DT.Var 0,DT.Var 3)))
  -> Setting
  -> [J.Tree J.Judgement] -- term
prove var_env sig_env pre_type setting=
  let var_env' = var_env ++ sig_env
      arrow_env = map (A.arrowNotat . DT.toDTT . UD.betaReduce . DT.toUDTT) var_env'
      arrow_type = (A.arrowNotat . DT.toDTT . UD.betaReduce . DT.toUDTT) pre_type
      arrow_terms = searchProof arrow_env arrow_type 1 setting
  in  map A.aTreeTojTree arrow_terms

forwardContext :: [A.Arrowterm] -> [J.Tree A.AJudgement]

forwardContext context = do
  let forwarded = concat $ zipWith (\ f fr -> map (\aTree -> let (A.AJudgement env aterm atype) = A.downSide aTree in A.changeDownSide aTree (A.AJudgement fr aterm atype)) (forward f fr)) context (init $ L.tails  context)
      adoptVarRuleAndType = concatMap (\fr -> J.VAR ( A.AJudgement (tail fr) (head fr) (A.Conclusion  DT.Type)): [J.VAR $ A.AJudgement fr (A.Conclusion $ DT.Var 0 ) (A.shiftIndices (head fr) 1 0)])$ init $ L.tails  context
      connected = forwarded ++ adoptVarRuleAndType
  map
    (\aTree  -> let (A.AJudgement env aTerm a_type) = A.downSide aTree; d = length context - length env in A.changeDownSide aTree $A.AJudgement context (A.shiftIndices aTerm d 0) (A.shiftIndices a_type d 0))
    connected

  --[  u0:p, u1:(u2:p)→q  ト [  u3:p ] =>q : type,  u0:p, u1:(u2:p)→q  ト u1 : [  u3:p ] =>q,  u0:p, u1:(u2:p)→q  ト p : type,  u0:p, u1:(u2:p)→q  ト u0 : p]

-- | generate free constraint from given word
genFreeCon :: A.Arrowterm -- ^ term
  -> String -- ^ "hoge"
  -> A.Arrowterm
genFreeCon term hoge =
  if isFreeCon term (A.Conclusion $ DT.Con $ T.pack hoge)
    then
      A.Conclusion $ DT.Con $ T.pack hoge
    else
      genFreeCon term $hoge++hoge

-- | whether DT.Con "hoge" is free or not
isFreeCon :: A.Arrowterm  -- ^ term
  -> A.Arrowterm  -- ^ DT.Con "hoge"
  -> Bool
isFreeCon term con=
  let term' = A.arrowSubst term con con
      term'' = A.arrowSubst term (A.Conclusion $ DT.Var 0) con
  in term' == term''

--term = A.Arrow [A.Conclusion $ DT.Con $ T.pack "p"] (A.Arrow_Sigma (A.Conclusion $ DT.Con $ T.pack "q") (A.Conclusion $ DT.Con $T.pack "r"))
forward :: A.Arrowterm -> [A.Arrowterm] -> [J.Tree A.AJudgement]
forward term env=
  let baseCon = genFreeCon term "base"
      forwarded' = forward' env baseCon term
  in map
    (\aTreef
      -> let
          aTree = aTreef (A.Conclusion $DT.Var 0)
          (A.AJudgement con aTerm aType) = A.downSide aTree
          newJ = A.AJudgement
                  con
                  (A.arrowSubst (A.shiftIndices aTerm 1 0) (A.Conclusion $DT.Var 0) baseCon)
                  (A.arrowSubst (A.shiftIndices aType 1 0) (A.Conclusion $  DT.Var 0) baseCon)
          in A.changeDownSide aTree newJ)
    forwarded'


forward' :: [A.Arrowterm] -- ^ origin
  ->   A.Arrowterm -- ^ base
  ->   A.Arrowterm -- ^ target
  ->  [A.Arrowterm ->J.Tree A.AJudgement]
forward' context base (A.Arrow_Sigma h t) =
  let t' = A.shiftIndices (A.arrowSubst t  (A.Arrow_Proj A.Arrow_Fst base) (A.Conclusion $ DT.Var 0)) (-1) 0
      hForward = forward' context (A.Arrow_Proj A.Arrow_Fst base) h
      tForward = forward' context (A.Arrow_Proj A.Arrow_Snd base) t'
      hTree base' = J.SigE (A.AJudgement context (A.Arrow_Proj A.Arrow_Fst base) h) (J.VAR (A.AJudgement context base' (A.Arrow_Sigma h t)))
      tTree base'=  J.SigE (A.AJudgement context (A.Arrow_Proj A.Arrow_Snd  base) t') (J.VAR (A.AJudgement context base' (A.Arrow_Sigma h t)))
  in (if null tForward then [tTree] else tForward) ++ (if null hForward  then [hTree] else hForward)
forward' context base (A.Arrow env (A.Arrow_Sigma h t)) =
  let term1 = addLam (length env) $ A.Arrow_Proj A.Arrow_Fst $ addApp (length env) base
      term2 = addLam (length env) $ A.Arrow_Proj A.Arrow_Snd $ addApp (length env) base
      t' = A.shiftIndices (A.arrowSubst t (A.shiftIndices term1 (length env) 0) (A.Conclusion $ DT.Var 0)) (-1) 0
      type1 = case h of (A.Arrow henv hcon) -> A.Arrow (henv ++ env) hcon ; _ -> A.Arrow env h
      type2 = case t' of (A.Arrow tenv tcon) -> A.Arrow (tenv ++ env) tcon; _ ->A.Arrow env t'
      hForward = forward' context term1 type1
      tForward = forward' context term2 type2
      tTree base'= J.SigE (A.AJudgement context term2 type2) (J.VAR (A.AJudgement context base' $ A.Arrow env (A.Arrow_Sigma h t)))
      hTree base'= J.SigE (A.AJudgement context term1 type1) (J.VAR (A.AJudgement context base' $ A.Arrow env (A.Arrow_Sigma h t)))
      result = (if null tForward  then [tTree] else tForward) ++ (if null hForward then [hTree] else hForward)
  in
    result

forward' context base  (A.Arrow a (A.Arrow b c)) =
  forward' context base (A.Arrow (b ++ a) c)
forward' context base arrowterm = []


addApp ::Int -> A.Arrowterm -> A.Arrowterm
addApp 0 base = base
addApp num base = addApp (num - 1) $ A.Arrow_App (A.shiftIndices base 1 0) (A.Conclusion $ DT.Var 0)

addLam :: Int -> A.Arrowterm -> A.Arrowterm
addLam 0 term = term
addLam num term = A.Arrow_Lam $ addLam (num - 1) term

searchProof :: [A.Arrowterm] ->A.Arrowterm -> Int -> Setting-> [J.Tree A.AJudgement]
searchProof a b c setting = L.nub $ deduceWithLog a b c setting

membership :: [A.Arrowterm] ->  A.Arrowterm -> Int -> Setting -> Either String [J.Tree A.AJudgement]
membership con arrow_type depth setting=
  let context = forwardContext con
      matchlst = filter
        (\xTree -> let x = A.downSide xTree in A.shiftIndices (A.typefromAJudgement x) (length con - length (A.envfromAJudgement x)) 0 == arrow_type)
        context
  in Right matchlst

piForm :: [A.Arrowterm] -> A.Arrowterm ->  A.Arrowterm -> Int -> Setting -> Either String [J.Tree A.AJudgement]
piForm con (A.Arrow as b) arrow_type depth setting
  | depth > (maxdepth setting) = Left ("too deep @ piForm " ++ show con ++" | "++ show arrow_type)
  | arrow_type `notElem` [A.Conclusion DT.Type,A.Conclusion DT.Kind] = Right []
  | otherwise=
      let a = if length as == 1 then head as else last as
          aTerm = concatMap (\dtterm -> withLog' typecheck con a (A.Conclusion dtterm) (depth + 1) setting) [DT.Type,DT.Kind]
      in
        if null aTerm
        then Right []
        else
          let bEnv = a: con
              bJs =  withLog' typecheck bEnv (if length as == 1 then b else A.Arrow (init as) b) arrow_type (depth + 1) setting
              treeAB = zip (concatMap (replicate (length bJs)) aTerm) (cycle bJs)
          in
            Right $ map (\(aTree,bTree) -> let x = A.downSide bTree in J.PiF (A.AJudgement con (A.Arrow as b) (A.typefromAJudgement x)) aTree bTree) treeAB

piForm con arrow_term arrow_type depth setting = Right []

sigmaForm:: [A.Arrowterm] -> A.Arrowterm ->  A.Arrowterm -> Int -> Setting -> Either String [J.Tree A.AJudgement]
sigmaForm con (A.Arrow_Sigma a b) arrow_type depth setting
  | depth > (maxdepth setting) = Left ("too deep @ piForm " ++ show con ++" | "++ show arrow_type)
  | arrow_type `notElem`[A.Conclusion DT.Type,A.Conclusion DT.Kind] = Right []
  | otherwise =
    let aTerm = concatMap (\dtterm -> withLog' typecheck con a (A.Conclusion dtterm) depth setting) [DT.Type,DT.Kind]
    in
      if null aTerm
      then
        Right []
      else
        let bJs = concatMap (\dtterm -> withLog' typecheck (a:con) b (A.Conclusion dtterm) depth setting) [DT.Type,DT.Kind]
            treeAB = zip (concatMap (replicate (length bJs)) aTerm) (cycle bJs)
        in   Right $ map (\(aTree,bTree) -> let x = A.downSide bTree in J.SigF (A.AJudgement con (A.Arrow_Sigma a b) (A.typefromAJudgement x)) aTree bTree) treeAB

sigmaForm con arrow_term arrow_type depth setting= Right []

piIntro :: [A.Arrowterm] ->  A.Arrowterm -> Int -> Setting -> Either String [J.Tree A.AJudgement]
piIntro con (A.Arrow a b) depth setting
  | depth > (maxdepth setting) = Left ("too deep @ piIntro " ++ show con ++" ト "++ show (A.Arrow a b))
  | null (concatMap (\dtterm -> withLog' typecheck con (A.Arrow a b) (A.Conclusion dtterm) depth setting) [DT.Type,DT.Kind]) = Right []
  | otherwise =
      let
          bJs = deduceWithLog (a ++ con) b (depth + 1) setting
          typeArrow = L.nub $ concatMap (\dtterm -> withLog' typecheck con (A.Arrow a b) (A.Conclusion dtterm) (depth + 1) setting) [DT.Type,DT.Kind]
          treeTB = zip (concatMap (replicate (length bJs)) typeArrow) (cycle bJs)
          piABJs =
            map
              (\(tJ,bJ) ->
                let
                  env = con
                  aTerm = addLam (length a) (A.termfromAJudgement $ A.downSide  bJ)
                  aType = A.Arrow a b
                in
                  J.PiI (A.AJudgement env aTerm aType) tJ bJ )
              treeTB
      in Right piABJs

piIntro  con arrow_type depth setting = Right []

tailIsB :: A.Arrowterm -> A.Arrowterm -> Bool
tailIsB (A.Arrow env b') b=
  (A.shiftIndices b (length env) 0==b') || (case b' of A.Arrow b1 b2 -> tailIsB (A.Arrow b1 b2) (A.shiftIndices b (length env) 0);_ -> False)
tailIsB _ _ =False

headIsType :: A.Arrowterm -> Bool
headIsType (A.Arrow env _) = (last env == A.Conclusion DT.Type) || (case last env of A.Arrow b1 b2 -> headIsType (A.Arrow b1 b2) ; _ -> False)
headIsType _=False

arrowConclusionB :: [A.AJudgement] -> A.Arrowterm -> [J.Tree A.AJudgement]
arrowConclusionB judgements b= map J.VAR $filter (\j -> tailIsB (A.typefromAJudgement j) b) judgements

deduceEnvs :: [A.Arrowterm] -> [J.Tree A.AJudgement] -> Int -> Setting -> [(J.Tree A.AJudgement,[[J.Tree A.AJudgement]])]
deduceEnvs con aJudgements depth setting=
  let ajudges = map ((mapM (\(f:r) -> deduceWithLog (r++con) f depth setting) . (\(A.AJudgement _ _ (A.Arrow env _)) -> init $ L.tails env)) . A.downSide)  aJudgements
  in filter ((/= []) . snd) $zip aJudgements ajudges

appAs :: A.Arrowterm -> [A.Arrowterm] -> A.Arrowterm
appAs= foldl A.Arrow_App

substAsInPiElim:: A.Arrowterm -> [(Int,A.Arrowterm)] -> A.Arrowterm
substAsInPiElim term [] = term
substAsInPiElim term ((fn,f):r) =A.arrowSubst (substAsInPiElim term  r) f (A.Conclusion $ DT.Var fn)

  -- A.shiftIndices (subst_as_in_pi_elim (A.shiftIndices (A.arrowSubst term f (A.Conclusion $ DT.Var 0)) (-1) 0) r) 1 0

piElim :: [A.Arrowterm] -> A.Arrowterm -> Int -> Setting -> Either String [J.Tree A.AJudgement]
piElim con b1 depth setting
  | depth > (maxdepth setting) = Left ("too deep @ piElim " ++ show con ++" ト "++ show b1)
  | otherwise =
    let aJudgements = arrowConclusionB (map A.downSide $ forwardContext con) b1 -- ++ arrowConclusionB' con b1 (depth + 1)
        a_type_terms = deduceEnvs con aJudgements depth setting
    in
      Right
        $concatMap
          (\(base,ass) ->
              map
                (\as ->
                  let env' = con
                      aTerms' = appAs (A.termfromAJudgement $ A.downSide base) $map (A.termfromAJudgement . A.downSide) as
                      a_type' = substAsInPiElim b1 $ zip [0..] $map (A.termfromAJudgement . A.downSide) as
                  in
                    J.PiE (A.AJudgement env' aTerms' a_type') base (head as)
                )
              ass
          )
          a_type_terms

sigmaIntro :: [A.Arrowterm] ->  A.Arrowterm -> Int -> Setting -> Either String [J.Tree A.AJudgement]
sigmaIntro con  (A.Arrow_Sigma a b1) depth setting
  | depth > (maxdepth setting) = Left ("too deep @ sigmaIntro " ++ show con ++" ト "++ show (A.Arrow_Sigma a b1))
  | null $ concatMap (\dtterm ->withLog' typecheck con (A.Arrow_Sigma a b1) (A.Conclusion dtterm) depth setting) [DT.Type,DT.Kind] = Right []
  | otherwise =
      let aTermJudgements =deduceWithLog con a (depth + 1) setting
          typeAS = L.nub $ concatMap (\dtterm -> withLog' typecheck con (A.Arrow_Sigma a b1) (A.Conclusion dtterm) (depth + 1) setting) [DT.Type,DT.Kind]
          aB1TermsJudgements =
            concatMap
            (\aJ ->
              let con2b =  con
                  base = genFreeCon b1 "base"
                  b_type = A.reduce $ A.arrowSubst (A.shiftIndices (A.arrowSubst b1 base (A.Conclusion $ DT.Var 0)) (-1) 0) (A.termfromAJudgement $ A.downSide aJ) base
                  treeAB = map (\bJ -> (aJ,bJ)) (deduceWithLog con b_type depth setting)
              in
              zip (concatMap (replicate (length treeAB)) typeAS) (cycle treeAB))
            aTermJudgements
      in
        Right
          $ map
              (\(tJ,(aJ,b1J)) ->
                    let
                      env = con
                      aTerm = A.Arrow_Pair (A.termfromAJudgement $ A.downSide aJ)  (A.termfromAJudgement $ A.downSide b1J)
                      a_type = A.Arrow_Sigma a b1
                    in J.SigI (A.AJudgement env aTerm a_type) aJ b1J)
--(J.Treeで途中まで実装してしまったので)とりあえず型チェックした内容(tJ)を削っている
              aB1TermsJudgements

sigmaIntro  con  arrow_type depth setting =Right []

deduceWithLog :: [A.Arrowterm] -> A.Arrowterm ->Int -> Setting ->[J.Tree A.AJudgement]
deduceWithLog = withLog deduce

withLog' :: ([A.Arrowterm] -> A.Arrowterm -> A.Arrowterm ->Int -> Setting ->Either String [J.Tree A.AJudgement]) -> [A.Arrowterm] -> A.Arrowterm -> A.Arrowterm ->Int -> Setting ->[J.Tree A.AJudgement]
withLog' f con arrow_term arrow_type depth setting =
  case f con arrow_term arrow_type depth setting of
    Right j -> j
    Left msg -> []

withLog :: ([A.Arrowterm] -> A.Arrowterm ->Int -> Setting ->Either String [J.Tree A.AJudgement]) -> [A.Arrowterm] -> A.Arrowterm ->Int -> Setting ->[J.Tree A.AJudgement]
withLog f con arrow_type depth setting =
  case f con arrow_type depth setting of
    Right j -> j
    Left msg -> []



typecheck :: [A.Arrowterm] -- ^ context
  -> A.Arrowterm -- ^ term
  -> A.Arrowterm -- ^ type
  -> Int -- ^ depth
  -> Setting
  -> Either String [J.Tree A.AJudgement]

typecheck con arrow_term arrow_type depth setting
  | depth > (maxdepth setting) = Left ("too deep @ typecheck " ++ show con ++" | "++ show arrow_type)
  | falsum setting && arrow_term == A.Conclusion DT.Bot && arrow_type == A.Conclusion DT.Type = Right [J.BotF (A.AJudgement con arrow_term arrow_type)]
  | otherwise =
    let judgements = concatMap (\f -> withLog f con arrow_type depth setting) [membership,piIntro,piElim,sigmaIntro]
        deducejudgements =filter (\a ->A.termfromAJudgement (A.downSide a) == A.shiftIndices arrow_term (length (A.envfromAJudgement $ A.downSide a)- length con ) 0) judgements
    in
      Right $ deducejudgements ++ concatMap (\f -> withLog' f con arrow_term arrow_type depth setting) [piForm,sigmaForm]


efq :: [A.Arrowterm] -> A.Arrowterm -> Int -> Setting ->Either String [J.Tree A.AJudgement]
efq con b depth setting
  | depth > maxdepth setting = Left  $ "too deep @ efq " ++ show con ++" ト "++ show b
  | null (withLog' typecheck con b (A.Conclusion DT.Type) (depth + 1) setting ) = Right []
  | otherwise =
    if null (withLog membership con (A.Conclusion DT.Bot) depth setting)
    then
      case deduceWithLog con (A.Conclusion DT.Bot) (depth + 1) setting of
          [] -> Right []
          botJs ->
            Right
              $map
              (\dTree -> let (A.AJudgement env aTerm a_type) = A.downSide dTree in J.SigE (A.AJudgement env (A.Arrow_App (A.Conclusion $ DT.Con $T.pack "efq") aTerm) b) dTree)---(\dTree -> let (A.AJudgement env aTerm a_type) = A.downSide dTree in J.NotF (A.AJudgement env (A.Arrow_App (A.Conclusion $ DT.Con $T.pack "efq") aTerm) b) dTree)
              botJs
    else
      Right
        $map
        (\dTree -> let (A.AJudgement env aTerm a_type) = A.downSide dTree in J.SigE (A.AJudgement env (A.Arrow_App (A.Conclusion $ DT.Con $T.pack "efq") aTerm) b) dTree)
        (withLog membership con (A.Conclusion DT.Bot) depth setting)

--(J.Treeで途中まで実装してしまったので)とりあえずEFQの代わりにNotFを使っている

dne :: [A.Arrowterm] -> A.Arrowterm -> Int -> Setting -> Either String [J.Tree A.AJudgement]
dne con b depth setting
  | depth > maxdepth setting = Left $ "too deep @ dne " ++ show con ++" ト "++ show b
  | null (withLog' typecheck con b (A.Conclusion DT.Type) (depth + 1) setting) = Right []
  | otherwise =
      case deduceWithLog con (A.Arrow [A.Arrow [b] (A.Conclusion DT.Bot)] (A.Conclusion DT.Bot)) (depth + 1) setting of
        [] -> Right []
        dneJs ->
              Right
                $map
                (\dTree -> let (A.AJudgement env aTerm a_type) = A.downSide dTree in J.SigE (A.AJudgement env (A.Arrow_App (A.Conclusion $ DT.Con $T.pack "dne") aTerm) b) dTree)
                dneJs
--(J.Treeで途中まで実装してしまったので)とりあえずDNEの代わりにNotFを使っている

-- | deduce (pi-intro + sigma-intro + membership + type-ax)
deduce :: [A.Arrowterm] -- ^ context
  -> A.Arrowterm -- ^  type
  -> Int -- ^ depth
  -> Setting
  ->Either String [J.Tree A.AJudgement]

--type-ax
deduce _ (A.Conclusion DT.Kind) depth setting
  | depth > maxdepth setting = Left  "depth @ deduce - type-ax"
  | otherwise = Right [J.VAR $A.AJudgement [] (A.Conclusion DT.Type) (A.Conclusion DT.Kind)]
-- --
deduce con arrow_type depth setting
  | depth > maxdepth setting = Left ("too deep @ deduce " ++ show con ++" | "++ show arrow_type)
  | otherwise =
      let judgements = foldl
            (\js f -> if null js then withLog f con arrow_type depth setting else js)
            []
            ([membership,piIntro,piElim,sigmaIntro] ++ [dne | arrow_type /= A.Conclusion DT.Bot && mode setting == WithDNE] ++ [efq | arrow_type /= A.Conclusion DT.Bot && mode setting == WithEFQ])
    in Right  judgements
