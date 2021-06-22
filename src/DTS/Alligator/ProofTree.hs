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
import qualified DTS.Prover_daido.Judgement  as J
import qualified Debug.Trace as D

import qualified Data.Text.Lazy.IO as T
import qualified Interface.HTML as HTML
import qualified Data.Maybe as M

data ProofMode = Plain | WithDNE | WithEFQ deriving (Show,Eq)
data Setting = Setting {mode :: ProofMode,falsum :: Bool,maxdepth :: Int,maxtime :: Int,debug :: Bool,nglst :: [(Int,A.Arrowterm)],typecheckTerm :: Maybe A.Arrowterm}



settingDef = Setting{mode = Plain,falsum = True,maxdepth = 9,maxtime = 100000,debug = False,nglst=[],typecheckTerm = Nothing}
settingDNE = Setting{mode = WithDNE,falsum = True,maxdepth = 9,maxtime = 100000,debug = True,nglst=[],typecheckTerm = Nothing}
settingEFQ = Setting{mode = WithEFQ,falsum = True,maxdepth = 9,maxtime = 100000,debug = False,nglst=[],typecheckTerm = Nothing}

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
      arrow_env = map (A.dtToArrow . DT.toDTT . UD.betaReduce . DT.toUDTT) var_env'
      arrow_type = (A.dtToArrow . DT.toDTT . UD.betaReduce . DT.toUDTT) pre_type
      -- notate_type = con_arrow arrow_type
      arrow_terms = searchProof arrow_env arrow_type 1 setting
  in  debugLog arrow_env arrow_type 0 setting "goal" $map A.aTreeTojTree arrow_terms

forwardContext :: [A.Arrowterm] -> [J.Tree A.AJudgement]
forwardContext context = do
  let forwarded = concat $ zipWith (\ f fr -> map (\aTree -> let (A.AJudgement env aterm atype) = A.downSide aTree in A.changeDownSide aTree (A.AJudgement fr aterm atype)) (forward f fr)) context (init $ L.tails  context)
      adoptVarRuleAndType = concatMap (\fr -> (J.T J.VAR( A.AJudgement (tail fr) (head fr) (A.Conclusion  DT.Type)) []): [J.T J.VAR(A.AJudgement fr (A.Conclusion $ DT.Var 0 ) (A.shiftIndices (head fr) 1 0)) [] ] )$ init $ L.tails  context
      connected = forwarded ++ adoptVarRuleAndType
  map
    (\aTree  -> let (A.AJudgement env aTerm a_type) = A.downSide aTree; d = length context - length env in A.changeDownSide aTree $A.AJudgement context (A.shiftIndices aTerm d 0) (A.shiftIndices a_type d 0))
    connected
  --[  u0:p, u1:(u2:p)→q  ト [  u3:p ] =>q : type,  u0:p, u1:(u2:p)→q  ト u1 : [  u3:p ] =>q,  u0:p, u1:(u2:p)→q  ト p : type,  u0:p, u1:(u2:p)→q  ト u0 : p]

--term = A.Arrow [A.Conclusion $ DT.Con $ T.pack "p"] (A.Arrow_Sigma (A.Conclusion $ DT.Con $ T.pack "q") (A.Conclusion $ DT.Con $T.pack "r"))
forward :: A.Arrowterm -> [A.Arrowterm] -> [J.Tree A.AJudgement]
forward term env=
  let baseCon = A.genFreeCon term "base"
      forwarded' = forward' env baseCon term
  in map
    (\aTreef
      -> let
          aTree = aTreef (A.Conclusion $DT.Var 0)
          (A.AJudgement con aTerm aType) = A.downSide aTree
          newJ = A.AJudgement
                  con
                  (A.arrowSubst (A.shiftIndices aTerm 1 0) (A.Conclusion $ DT.Var 0) baseCon)
                  (A.arrowSubst (A.shiftIndices aType 1 0) (A.Conclusion $  DT.Var 0) baseCon)
          in A.changeDownSide aTree newJ)
    forwarded'

forward' :: [A.Arrowterm] -- ^ origin
  ->   A.Arrowterm -- ^ base
  ->   A.Arrowterm -- ^ target
  ->  [A.Arrowterm ->J.Tree A.AJudgement]
forward' context base (A.Arrow_Sigma h t) =
  let fstbase =  A.Arrow_Proj A.Arrow_Fst base
      sndbase =  A.Arrow_Proj A.Arrow_Snd base
      t' = A.shiftIndices (A.arrowSubst t  fstbase (A.Conclusion $ DT.Var 0)) (-1) 0
      hForward = forward' context fstbase h
      tForward = forward' context sndbase t'
      hTree base' = J.T J.SigE (A.AJudgement context fstbase h) [J.T J.VAR(A.AJudgement context base' (A.Arrow_Sigma h t)) []]
      tTree base'=  J.T J.SigE (A.AJudgement context sndbase t') [J.T J.VAR(A.AJudgement context base' (A.Arrow_Sigma h t)) []]
  in (if null tForward then [tTree] else tForward) ++ (if null hForward  then [hTree] else hForward)
forward' context base (A.Arrow env (A.Arrow_Sigma h t)) =
  let lenEnv = length env
      term1 = addLam lenEnv $ A.Arrow_Proj A.Arrow_Fst $ addApp lenEnv base
      term2 = addLam lenEnv $ A.Arrow_Proj A.Arrow_Snd $ addApp lenEnv base
      t' = A.shiftIndices (A.arrowSubst t (A.shiftIndices term1 lenEnv 0) (A.Conclusion $ DT.Var 0)) (-1) 0
      type1 = case h of (A.Arrow henv hcon) -> A.Arrow (henv ++ env) hcon ; _ -> A.Arrow env h
      type2 = case t' of (A.Arrow tenv tcon) -> A.Arrow (tenv ++ env) tcon; _ ->A.Arrow env t'
      hForward = forward' context term1 type1
      tForward = forward' context term2 type2
      tTree base'= J.T J.SigE (A.AJudgement context term2 type2) [J.T J.VAR(A.AJudgement context base' $ A.Arrow env (A.Arrow_Sigma h t)) []]
      hTree base'= J.T J.SigE (A.AJudgement context term1 type1) [J.T J.VAR(A.AJudgement context base' $ A.Arrow env (A.Arrow_Sigma h t)) []]
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

headIsType :: A.Arrowterm -> Bool
headIsType (A.Arrow env _) = (last env == A.Conclusion DT.Type) || (case last env of A.Arrow b1 b2 -> headIsType (A.Arrow b1 b2) ; _ -> False)
headIsType _=False

match :: Int -> A.Arrowterm -> A.Arrowterm -> Bool
match =A.canBeSame

tailIsB :: A.Arrowterm -> A.Arrowterm -> (Int,Bool)
tailIsB (A.Arrow env b) (A.Arrow env' b')=
  let d = length env - length env'
  in  (d
      ,
      d >= 0
      &&
      match (length env) b (A.shiftIndices b' d 0)
      &&
      and (map (\((s,num),t) -> match (num +　d) s t) $zip (zip (take (length env') env) [0..]) (map (\c' -> A.shiftIndices c' d 0) env')))
tailIsB (A.Arrow env b) b'=
  let result = (length env,match (length env) b (A.shiftIndices b' (length env) 0))
  in {--D.trace ("match "++(show $ length env)++(show (A.Arrow env b))++" b : "++(show b)++" b' : "++(show $ (A.shiftIndices b' (length env) 0))++" result: "++(show result))--}
    result
tailIsB _ _ =(0,False)

arrowConclusionB :: A.AJudgement -> A.Arrowterm -> ((Int,Bool),A.AJudgement)
arrowConclusionB j b=
  -- let foroutut = D.trace ("arrowConclusion j:"++(show j)++" b:"++(show b))  [] in
  (tailIsB (A.typefromAJudgement j) b,j)

-- | pi型の後ろの方がbと一致するか
arrowConclusionBs :: [A.AJudgement] -> A.Arrowterm -> [(Int,J.Tree A.AJudgement)]
arrowConclusionBs judgements b=
  map
  (\((num ,b),j )-> (num,J.T J.VAR j []))
  $filter
    (snd . fst)
    $map (\j -> arrowConclusionB j b) judgements

deduceEnv :: [A.Arrowterm] -> (Int,J.Tree A.AJudgement) -> Int -> Setting -> (J.Tree A.AJudgement,[[J.Tree A.AJudgement]])
deduceEnv con (num,aJudgement) depth setting=
  case  A.downSide aJudgement of
    A.AJudgement _ _ (A.Arrow env _) ->
        let deduceTargetAndCons = reverse $take num $reverse $ init $ L.tails env
            proofForEnv = map (\(f:r) -> deduceWithLog (r++con) f depth setting) deduceTargetAndCons
        in
          if or $map (null) proofForEnv
          then (aJudgement,[])
          else (aJudgement,proofForEnv)
    _ -> (aJudgement, [])

  -- let ajudge = ((mapM (\(f:r) -> deduceWithLog (r++con) f depth setting). (\(A.AJudgement _ _ (A.Arrow env _)) -> init $ L.tails env)) . A.downSide) ajudgement

deduceEnvs :: [A.Arrowterm] -> [(Int,J.Tree A.AJudgement)] -> Int -> Setting -> [(J.Tree A.AJudgement,[[J.Tree A.AJudgement]])]
deduceEnvs con aJudgements depth setting=
  let ajudges = map (\aj ->deduceEnv con aj depth setting) aJudgements
  in filter ((/= []) . snd) ajudges

appAs :: A.Arrowterm -> [A.Arrowterm] -> A.Arrowterm
appAs= foldl A.Arrow_App

substAsInPiElim:: A.Arrowterm -> [A.Arrowterm] -> A.Arrowterm
-- substAsInPiElim term [] = term
-- substAsInPiElim term ((fn,f):r) =A.arrowSubst (substAsInPiElim term  r) f (A.Conclusion $ DT.Var fn)
-- substAsInPiElim term [] = term
substAsInPiElim (A.Arrow env t) args
  | length env < length args = undefined
  | otherwise =
    let beforeSubst =
          case (length env - length args) of
            0 -> t
            d -> A.Arrow (reverse $drop d $reverse env) t
        afterSubst = foldr (\(num,a) -> \tt -> A.arrowSubst tt (A.shiftIndices a (length args) (0)) (A.Conclusion $ DT.Var num)) beforeSubst $zip [0..] args
    in
      A.shiftIndices afterSubst (0-length args) (length args)
substAsInPiElim _ _= undefined

searchProof :: [A.Arrowterm] ->A.Arrowterm -> Int -> Setting-> [J.Tree A.AJudgement]
searchProof a b c setting= L.nub $ deduceWithLog a b c setting

membership :: [A.Arrowterm] ->  A.Arrowterm -> Int -> Setting -> Either String [J.Tree A.AJudgement]
membership con arrow_type depth setting=
  debugLog con arrow_type depth setting "membership"  $
    let context = forwardContext con
        matchlst = filter
          (\xTree -> let x = A.downSide xTree in A.shiftIndices (A.typefromAJudgement x) (length con - length (A.envfromAJudgement x)) 0 == arrow_type)
          context
    in Right matchlst

piForm :: [A.Arrowterm] -> A.Arrowterm ->  A.Arrowterm -> Int -> Setting -> Either String [J.Tree A.AJudgement]
piForm con (A.Arrow as b) arrow_type depth setting
  | depth > (maxdepth setting) = debugLogWithTerm con (A.Arrow as b) arrow_type depth setting "piFormハズレ1"  $ Left ("too deep @ piForm " ++ show con ++" | "++ show arrow_type)
  | arrow_type `notElem` [A.Conclusion DT.Type,A.Conclusion DT.Kind] = debugLogWithTerm con (A.Arrow as b) arrow_type depth setting "piFormハズレ2"  $Right []
  | otherwise=
      debugLogWithTerm con (A.Arrow as b) arrow_type depth setting "piForm1"  $
        let a = if length as == 1 then head as else last as
            aTerm = concatMap (\dtterm -> withLog' typecheck con a (A.Conclusion dtterm) depth setting) [DT.Type,DT.Kind]
        in
          if null aTerm
          then Right []
          else
            let bEnv = a: con
                bJs =  withLog' typecheck bEnv (if length as == 1 then b else A.Arrow (init as) b) arrow_type (depth + 1) setting
                treeAB = zip (concatMap (replicate (length bJs)) aTerm) (cycle bJs)
            in
               Right $ map (\(aTree,bTree) -> let x = A.downSide bTree in J.T J.PiF (A.AJudgement con (A.Arrow as b) (A.typefromAJudgement x)) [aTree,bTree]) treeAB

piForm con arrow_term arrow_type depth setting = Right []

sigmaForm:: [A.Arrowterm] -> A.Arrowterm ->  A.Arrowterm -> Int -> Setting -> Either String [J.Tree A.AJudgement]
sigmaForm con (A.Arrow_Sigma a b) arrow_type depth setting
  | depth > (maxdepth setting) = debugLogWithTerm con (A.Arrow_Sigma a b) arrow_type  depth setting "sigmaFormハズレ1"  $ Left ("too deep @ piForm " ++ show con ++" | "++ show arrow_type)
  | arrow_type `notElem`[A.Conclusion DT.Type,A.Conclusion DT.Kind] = debugLogWithTerm con (A.Arrow_Sigma a b) arrow_type  depth setting"sigmaFormハズレ"  $Right []
  | otherwise =
    debugLogWithTerm con (A.Arrow_Sigma a b) arrow_type  depth setting "sigmaForm1"  $
      let aTerm = concatMap (\dtterm -> withLog' typecheck con a (A.Conclusion dtterm) depth setting) [DT.Type,DT.Kind]
      in
        if null aTerm
        then
          Right []
        else
          let bJs = concatMap (\dtterm -> withLog' typecheck (a:con) b (A.Conclusion dtterm) depth setting) [DT.Type,DT.Kind]
              treeAB = zip (concatMap (replicate (length bJs)) aTerm) (cycle bJs)
          in  Right $ map (\(aTree,bTree) -> let x = A.downSide bTree in J.T J.SigF (A.AJudgement con (A.Arrow_Sigma a b) (A.typefromAJudgement x)) [aTree,bTree]) treeAB

sigmaForm con arrow_term arrow_type depth setting = Right []

piIntro :: [A.Arrowterm] ->  A.Arrowterm -> Int -> Setting -> Either String [J.Tree A.AJudgement]
piIntro con (A.Arrow a b) depth setting
  | depth > (maxdepth setting) = debugLog con (A.Arrow a b) depth setting "piIntroハズレ1"  $Left ("too deep @ piIntro " ++ show con ++" ト "++ show (A.Arrow a b))
  -- | null (concatMap (\dtterm -> withLog' typecheck con (A.Arrow a b) (A.Conclusion dtterm) (depth + 1) setting) [DT.Type,DT.Kind]) = debugLog con (A.Arrow a b) depth setting "piIntroハズレ2" $Right []
  | otherwise =
      debugLog con (A.Arrow a b) depth setting "piIntro1"  $
        let
            bJs = deduceWithLog (a ++ con) b (depth + 1) setting
            typeArrow =  L.nub $ concatMap (\dtterm -> withLog' typecheck con (A.Arrow a b) (A.Conclusion dtterm) (depth + 1) setting) [DT.Type,DT.Kind]
            treeTB = zip (concatMap (replicate (length bJs)) typeArrow) (cycle bJs)
            piABJs =
              map
                (\(tJ,bJ) ->
                  let
                    env = con
                    aTerm = addLam (length a) (A.termfromAJudgement $ A.downSide  bJ)
                    aType = A.Arrow a b
                  in
                    J.T J.PiI (A.AJudgement env aTerm aType) [tJ,bJ] )
                treeTB
        in Right piABJs

piIntro  con arrow_type depth setting = debugLog con arrow_type depth setting "piIntro2"  $Right []


-- A.arrowSubst (substAsInPiElim term  r) f (A.Conclusion $ DT.Var fn)
piElim :: [A.Arrowterm] -> A.Arrowterm -> Int -> Setting -> Either String [J.Tree A.AJudgement]
piElim con b1 depth setting
  | depth > (maxdepth setting) = debugLog con b1 depth setting "piElimハズレ"$  Left ("too deep @ piElim " ++ show con ++" ト "++ show b1)
  | typecheckTerm setting /= Nothing =
      case typecheckTerm setting of
      --   Just f a ->
      --     let atree = deduceWithLog con a depth setting
      --         justaType = case aTypes of [] -> Nothing;a:_ -> Just (A.typefromAJudgement $ A.downSide a)
      --     in
      --       case justaType of
      --         Nothing -> debugLog con b1 depth setting ("piElim deduce失敗 : "++(show a))$  Right []
      --         Just aType ->
      --           let fType = A.arrowNotat $A.Arrow [aType] b1
      --               ftree = withLog' typecheck con f fType depth setting
      --
        Just (A.Arrow_App (A.Conclusion (DT.Var fn)) x) ->
          let fType = A.shiftIndices (con !! fn) (fn+1) 0in
            case fType of
              A.Arrow (a:r) b ->
                if b == b1
                  then
                    let proofForx = withLog' typecheck con x a (depth + 1) setting
                        ftree = J.T J.VAR (A.AJudgement con (A.Conclusion (DT.Var fn)) fType) []
                        downside = A.AJudgement con (A.Arrow_App (A.Conclusion (DT.Var fn)) x) (A.arrowNotat $ A.Arrow r b)
                        result = map (\xtree -> J.T J.PiE downside [ftree,xtree]) proofForx
                    in debugLogWithTerm con (A.Arrow_App (A.Conclusion (DT.Var fn)) x) b1 depth setting "piElimtypecheck"$ Right result
                  else
                    debugLogWithTerm con (A.Arrow_App (A.Conclusion (DT.Var fn)) x) b1 depth setting "piElimtypecheck ハズレ1"$ Right []
              c -> debugLogWithTerm con c b1 depth setting "piElimtypecheck ハズレ2"$ Right []
        Just term-> debugLogWithTerm con term b1 depth setting "piElimtypecheck ハズレ3"$ Right []
        _ -> Right [] --ここに来ることはない
  | otherwise =
    let aJudgements = arrowConclusionBs (map A.downSide $ forwardContext con) b1 --
        a_type_terms = deduceEnvs con aJudgements (depth+1) setting
    in debugLog con b1 depth setting ("piElim1" ++ show aJudgements) $Right
          $concatMap
            (\(base,ass) ->
                M.mapMaybe
                  (\as ->
                    let env' = con
                        args = map (A.termfromAJudgement . A.downSide) as
                        aTerms' = appAs (A.termfromAJudgement $ A.downSide base) args
                        a_type' = substAsInPiElim (A.typefromAJudgement $A.downSide base) $  args
                    in
                      if a_type' == b1
                      then
                        Just $ J.T J.PiE (A.AJudgement env' aTerms' a_type') [base,head as]
                      else
                        Nothing
                  )
                (sequence ass)
            )
            (a_type_terms)

sigmaIntro :: [A.Arrowterm] ->  A.Arrowterm -> Int -> Setting -> Either String [J.Tree A.AJudgement]
sigmaIntro con  (A.Arrow_Sigma a b1) depth setting
  | depth > (maxdepth setting) = debugLog con (A.Arrow_Sigma a b1) depth setting "sigmaIntro深さ"$Left ("too deep @ sigmaIntro " ++ show con ++" ト "++ show (A.Arrow_Sigma a b1))
  -- | null $ concatMap (\dtterm ->withLog' typecheck con (A.Arrow_Sigma a b1) (A.Conclusion dtterm) depth setting) [DT.Type,DT.Kind] = Right []
  | otherwise =
    debugLog con (A.Arrow_Sigma a b1) depth setting "sigmaIntro1"  $
      let aTermJudgements' =deduceWithLog con a (depth + 1) setting
          substedB b a= let base = A.genFreeCon b "base" in A.reduce $ A.arrowSubst (A.shiftIndices (A.arrowSubst b1 base (A.Conclusion $ DT.Var 0)) (-1) 0) a base
          dependencyCheck = case aTermJudgements' of [] -> False ; (a:_) -> substedB b1 (A.termfromAJudgement $ A.downSide a) == A.shiftIndices b1 (-1) 0
          aTermJudgements =
            if dependencyCheck
            then
              (D.trace (L.replicate (depth+1) '\t'++"後件に影響ないから一個だけ見る" ++ (show  $ map A.downSide aTermJudgements')) (take 1 $reverse aTermJudgements'))
            else
              (D.trace (L.replicate (depth+1) '\t'++"この辺見ていくよー" ++ (show  $ map A.downSide aTermJudgements')) aTermJudgements')
          -- typeAS = L.nub $ concatMap (\dtterm -> withLog' typecheck con (A.Arrow_Sigma a b1) (A.Conclusion dtterm) (depth + 1) setting) [DT.Type,DT.Kind]
          aB1TermsJudgements =
            concatMap
            (\aJ ->
              let b_type = D.trace (L.replicate (depth+1) '\t'++"ここ見てるよ"　++ (show aJ)) substedB b1 (A.termfromAJudgement $ A.downSide aJ)
              -- let con2b =  con
              --     base = A.genFreeCon b1 "base"
                  -- b_type = D.trace (L.replicate (depth+1) '\t'++"ここ見てるよ"　++ (show aJ)) A.reduce $ A.arrowSubst (A.shiftIndices (A.arrowSubst b1 base (A.Conclusion $ DT.Var 0)) (-1) 0) (A.termfromAJudgement $ A.downSide aJ) base
                  treeAB = map (\bJ -> (aJ,bJ)) (deduceWithLog con b_type (depth+2) setting)
              in treeAB)
              -- zip (concatMap (replicate (length treeAB)) typeAS) (cycle treeAB))
            (reverse aTermJudgements)
      in
        Right
          $ map
              (\(aJ,b1J) ->
                    let
                      env = con
                      aTerm = A.Arrow_Pair (A.termfromAJudgement $ A.downSide aJ)  (A.termfromAJudgement $ A.downSide b1J)
                      a_type = A.Arrow_Sigma a b1
                    in J.T J.SigI (A.AJudgement env aTerm a_type) [aJ,b1J])
--(J.Treeで途中まで実装してしまったので)とりあえず型チェックした内容(tJ)を削っている
              aB1TermsJudgements

sigmaIntro  con  arrow_type depth setting = debugLog con arrow_type depth setting "sigmaIntroハズレ"  $Right []

eqIntro  :: [A.Arrowterm] -> A.Arrowterm -> Int -> Setting -> Either String [J.Tree A.AJudgement]
eqIntro con (A.Arrow_Eq t a b) depth setting
  | a /= b = debugLog con (A.Arrow_Eq t a b) depth setting "eqIntro1"  $ Right []
  | otherwise =
    debugLog con (A.Arrow_Eq t a b) depth setting "eqIntro2"  $
      case withLog' typecheck con a t (depth + 1) setting of
        [] ->  Right []
        typeTree ->
                Right
                  $map
                  (\dTree -> let (A.AJudgement env aTerm a_type) = A.downSide dTree in J.T J.SigE (A.AJudgement con (A.Conclusion $ DT.Con $T.pack $ "eqIntro(" ++ (show a) ++ ")("++ (show t) ++")" ) (A.Arrow_Eq t a a)) [dTree])
                  typeTree
eqIntro con b depth setting= debugLog con b depth setting "eqIntro3"  $　Right []

eqElim :: [A.Arrowterm] -> A.Arrowterm -> Int -> Setting -> Either String [J.Tree A.AJudgement]
eqElim  con target depth setting
  | depth > (maxdepth setting) = debugLog con target depth setting "eqElim深さ"  $Left ("too deep @ eqElim " ++ show con ++" ト "++ show target)
  | not $ any (A.canBeSame 0 (A.Arrow_Eq (A.Conclusion $DT.Var 0) (A.Conclusion $DT.Var 0) (A.Conclusion $DT.Var 0)) )   con  = debugLog con target depth setting "eqElimハズレ"  $　Right []
  | otherwise =
    debugLog con target depth setting "eqElim1"  $
      case target of
        A.Conclusion (DT.Var varnum) ->                                                                           -- | shiftIndices m d i-- add d to all the indices that is greater than or equal to i within m (=d-place shift)
            let eqAboutDttermLst = M.catMaybes $map (\(dnum,term) -> case A.shiftIndices term (varnum - dnum) 0 of; (A.Arrow_Eq _ var1 var2) -> if var1 == (A.Conclusion $ DT.Var varnum) then Just (dnum,var2) else (if (var2 == (A.Conclusion $DT.Var varnum))then Just (dnum,var1) else Nothing) ; _ -> Nothing) (zip [(varnum-1),(varnum-2)..0] con) in
  -- filter (\(dnum,term) ->case term of;A.Arrow_Eq (A.Conclusion $DT.Var num0) (A.Conclusion $DT.Var num1) _ -> True;_ -> False) eqlist in A.shiftIndices var1 (-dnum) dnum
            let varLst = M.mapMaybe
                      (\(varid,dterm') ->
                        case deduceWithLog con dterm' (depth + 1) setting of
                          [] -> Nothing
                          dtermJs ->
                            let dtermJ = head dtermJs in
                            Just $ J.T J.PiE
                                (A.AJudgement con (A.Conclusion$ DT.Var varid{-暫定-}) target)
                                [
                                  (J.T J.CON(A.AJudgement con (A.Conclusion$ DT.Var varid) (A.Arrow_Eq (A.Conclusion DT.Type) target dterm')) []),
                                  dtermJ
                                ]
                      )
                      eqAboutDttermLst
                sigmaLst = [] -- sigmaelimをforward_contextでしているため
                piLst = [] -- varで結局拾える
                  in  Right $varLst ++ sigmaLst ++ piLst
        _ -> debugLog con target depth setting "eqElim2"  $Right[] --target が DT.App (f) (a)のときの対応

efq :: [A.Arrowterm] -> A.Arrowterm -> Int -> Setting ->Either String [J.Tree A.AJudgement]
efq con b depth setting
  | depth > maxdepth setting = debugLog con b depth setting "efqハズレ1" $ Left  $ "too deep @ efq " ++ show con ++" ト "++ show b
  | null (withLog' typecheck con b (A.Conclusion DT.Type) (depth + 1) setting ) = debugLog con b depth setting "efqハズレ2"$  Right []
  | otherwise =
    if null (withLog membership con (A.Conclusion DT.Bot) depth setting)
    then
      case deduceWithLog con (A.Conclusion DT.Bot) (depth + 1) setting of
          [] -> debugLog con b depth setting "efq1" $Right []
          botJs -> debugLog con b depth setting "efq2"  $
            Right
              $map
              (\dTree -> let (A.AJudgement env aTerm a_type) = A.downSide dTree in J.T J.SigE (A.AJudgement env (A.Arrow_App (A.Conclusion $ DT.Con $T.pack "efq") aTerm) b) [dTree])---(\dTree -> let (A.AJudgement env aTerm a_type) = A.downSide dTree in J.NotF (A.AJudgement env (A.Arrow_App (A.Conclusion $ DT.Con $T.pack "efq") aTerm) b) dTree)
              botJs
    else debugLog con b depth setting "efq3"  $
      Right
        $map
        (\dTree -> let (A.AJudgement env aTerm a_type) = A.downSide dTree in J.T J.SigE (A.AJudgement env (A.Arrow_App (A.Conclusion $ DT.Con $T.pack "efq") aTerm) b) [dTree])
        (withLog membership con (A.Conclusion DT.Bot) depth setting)
--(J.Treeで途中まで実装してしまったので)とりあえずEFQの代わりにNotFを使っている

dne :: [A.Arrowterm] -> A.Arrowterm -> Int -> Setting -> Either String [J.Tree A.AJudgement]
dne con b depth setting
  | depth > maxdepth setting = debugLog con b depth setting "dne深さ"  $ Left $ "too deep @ dne " ++ show con ++" ト "++ show b
  | b==A.Conclusion DT.Type =debugLog con b depth setting "dneハズレ2"  $ Right []
  | case b of (A.Arrow [A.Arrow _ (A.Conclusion DT.Bot)] (A.Conclusion DT.Bot)) -> True ; _ -> False=  debugLog con b depth setting "dne2重"  $Right []
  | b `elem` (map (\(num,term) -> A.shiftIndices term (length con - num) 0) (nglst setting)) = debugLog con b depth setting "dne2回め"  $Right []
  | null (debugLog con b depth setting "dneが使えるか確認" (withLog' typecheck con b (A.Conclusion DT.Type) (depth + 1) setting)) = debugLog con b depth setting "dneハズレ1"  $Right []
  | otherwise =
    debugLog con b depth setting "dne1"  $
      case deduceWithLog con (A.Arrow [A.Arrow [b] (A.Conclusion DT.Bot)] (A.Conclusion DT.Bot)) (depth + 1) setting{nglst = ((length con,b) : nglst setting)} of
        [] ->  Right []
        dneJs ->
              Right
                $map
                (\dTree -> let (A.AJudgement env aTerm a_type) = A.downSide dTree in J.T J.SigE (A.AJudgement env (A.Arrow_App (A.Conclusion $ DT.Con $T.pack "dne") aTerm) b) [dTree])
                dneJs
--(J.Treeで途中まで実装してしまったので)とりあえずDNEの代わりにNotFを使っている


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
    Right j -> {-D.trace ("found  : "++ (show j))-} j
    Left msg -> {-D.trace "reset"-} []

typecheck :: [A.Arrowterm] -- ^ context
  -> A.Arrowterm -- ^ term
  -> A.Arrowterm -- ^ type
  -> Int -- ^ depth
  -> Setting
  -> Either String [J.Tree A.AJudgement]

typecheck con arrow_term arrow_type depth setting
  | depth > (maxdepth setting) = Left ("too deep @ typecheck " ++ show con ++" | "++ show arrow_type)
  | falsum setting && arrow_term == A.Conclusion DT.Bot && arrow_type == A.Conclusion DT.Type = Right [J.T J.BotF (A.AJudgement con arrow_term arrow_type) []]
  | arrow_type == A.Conclusion DT.Kind = if arrow_term == A.Conclusion DT.Type then Right [J.T J.CON(A.AJudgement con arrow_term arrow_type) []] else Right []
  | arrow_term == A.Conclusion DT.Kind = debugLogWithTerm con arrow_term arrow_type depth setting ("kindは証明項にはならないと思う")  $Right []
  | otherwise =
    debugLogWithTerm con arrow_term arrow_type depth setting ("typecheck : ")  $
    let formjudgements = concatMap (\f -> withLog' f con arrow_term arrow_type (depth+1) setting{typecheckTerm = Just arrow_term}) [piForm,sigmaForm]
        judgements = foldl
          (\js f ->
            let js' = filter (\a ->A.termfromAJudgement (A.downSide a) == A.shiftIndices arrow_term (length (A.envfromAJudgement $ A.downSide a)- length con ) 0) js
            in if null js' then withLog f con arrow_type (depth+1) setting{typecheckTerm = Just arrow_term} else {-debugLogWithTerm con arrow_term arrow_type depth setting ("found : ")  $-} js')
          formjudgements
          ([membership,eqIntro,piIntro,sigmaIntro,piElim,eqElim] ++ [dne | arrow_type /= A.Conclusion DT.Bot && mode setting == WithDNE] ++ [efq | arrow_type /= A.Conclusion DT.Bot && mode setting == WithEFQ])
        --
        -- concatMap (\f -> withLog f con arrow_type depth setting) [membership,piIntro,piElim,sigmaIntro]
        -- deducejudgements =filter (\a ->A.termfromAJudgement (A.downSide a) == A.shiftIndices arrow_term (length (A.envfromAJudgement $ A.downSide a)- length con ) 0) judgements
    in
      -- Right $ if null deducejudgements ++ concatMap (\f -> withLog' f con arrow_term arrow_type depth setting) [piForm,sigmaForm]
      debugLogWithTerm con arrow_term arrow_type depth setting ("found : ")  $Right judgements


-- | deduce (pi-intro + sigma-intro + membership + type-ax)
deduce :: [A.Arrowterm] -- ^ context
  -> A.Arrowterm -- ^  type
  -> Int -- ^ depth
  -> Setting
  ->Either String [J.Tree A.AJudgement]

--type-ax
deduce _ (A.Conclusion DT.Kind) depth setting
  | depth > maxdepth setting = Left  "depth @ deduce - type-ax"
  | otherwise = Right [J.T J.VAR(A.AJudgement [] (A.Conclusion DT.Type) (A.Conclusion DT.Kind)) []]
-- --
deduce con arrow_type depth setting
  | depth > maxdepth setting = Left ("too deep @ deduce " ++ show con ++" | "++ show arrow_type)
  | otherwise =
      let judgements = foldl
            (\js f -> if null js then withLog f con arrow_type depth setting{typecheckTerm = Nothing} else js)
            []
            ([membership,eqIntro,piIntro,sigmaIntro,piElim,eqElim] ++ [dne | arrow_type /= A.Conclusion DT.Bot && mode setting == WithDNE] ++ [efq | arrow_type /= A.Conclusion DT.Bot && mode setting == WithEFQ])
    in (if null judgements then debugLog con arrow_type depth setting "deduce failed" else D.trace (L.replicate depth '\t' ++  (show depth) ++ " deduced :  " ++ (show $ A.downSide $ head judgements))) $Right  judgements

debugLog :: {-(Show a) =>-} [A.Arrowterm] -> A.Arrowterm -> Int -> Setting -> String -> a -> a
debugLog con=
  debugLogWithTerm con (A.Conclusion $ DT.Con $T.pack "?")

debugLogWithTerm :: {-(Show a) =>-} [A.Arrowterm] -> A.Arrowterm -> A.Arrowterm -> Int -> Setting -> String -> a -> a
debugLogWithTerm con term target depth setting label answer=
  if debug setting
    then
      D.trace
        (L.replicate depth '\t' ++  (show depth) ++ " " ++ label ++ " " ++ (show $ A.AJudgement con term target)++ " ")
        answer
    else answer
