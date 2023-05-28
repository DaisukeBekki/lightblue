{-# LANGUAGE OverloadedStrings #-}
module DTS.Wani.Forward (forwardContext) where

import qualified Data.List as L 

import qualified DTS.DTT as DT            -- DTT
import qualified DTS.Wani.Arrowterm as A
import qualified DTS.Prover_daido.Judgement  as J

import qualified DTS.Wani.WaniBase as B 


import qualified Debug.Trace as D

eqIntroElimTerm :: A.Arrowterm
eqIntroElimTerm = 
  A.Arrow 
    (
      [A.ArrowEq (A.aVar 4) (A.aVar 0) (A.aVar 3),A.aVar 3,A.ArrowEq (A.aVar 2) (A.aVar 0) (A.aVar 1),A.aVar 1,A.aVar 0,A.aType]
      -- [A.ArrowEq (A.Conclusion $ DT.Var 4) (A.Conclusion $ DT.Var 1) (A.Conclusion $ DT.Var 3),
      -- A.ArrowEq (A.Conclusion $ DT.Var 3) (A.Conclusion $ DT.Var 1) (A.Conclusion $ DT.Var 2)]++ 
      -- (map A.Conclusion [DT.Var 2,DT.Var 1,DT.Var 0,DT.Type])
    ) 
    (A.ArrowEq (A.Conclusion $ DT.Var 5) (A.Conclusion $ DT.Var 3) (A.Conclusion $ DT.Var 1))

eqIntro :: A.Arrowterm
eqIntro = 
  A.Arrow 
    (map A.Conclusion [DT.Var 0,DT.Type])
    (A.ArrowEq (A.Conclusion $ DT.Var 1) (A.Conclusion $ DT.Var 0) (A.Conclusion $ DT.Var 0))

forwardContext :: A.Context -> B.Result
forwardContext (sigCon,varCon) = 
    let (sigNames,sigTerms) = unzip sigCon
        context' = varCon ++ sigTerms
        varsigmaForwarded =
          (concat$
            zipWith
              (\f fr ->
                let varLen =  length fr - length sigNames
                    fIsVar = varLen > 0
                    (varCon',sigTerms') = splitAt varLen fr
                    sigCon' = reverse $ zip (reverse sigNames) (reverse sigTerms')
                    newCon = (sigCon',varCon')
                    sigmaForwarded = sigmaForward f newCon
                    varForwarded =                 
                      if fIsVar then
                          -- [J.T J.VAR (A.AJudgement  (sigCon',tail varCon') f (A.Conclusion  DT.Type)) [] | case A.betaReduce f of A.Conclusion DT.Type -> False; A.Conclusion (DT.Con _)-> False;_ ->True] ++
                          [J.T J.VAR(A.AJudgement newCon (A.Conclusion $ DT.Var 0 ) (A.shiftIndices f 1 0)) [] ]
                        else
                          let (sigName,sigTerm) = head sigCon' in
                            -- [J.T J.VAR (A.AJudgement (tail sigCon',[]) sigTerm (A.Conclusion  DT.Type)) [] | sigTerm /= A.Conclusion DT.Type] ++
                            [J.T J.CON (A.AJudgement newCon (A.Conclusion $ DT.Con sigName) (A.shiftIndices sigTerm 1 0)) [] ]
                in  sigmaForwarded ++ varForwarded
              )
              context'
              (L.tails  context')) 
        trees =   
          let eqIntroTree = J.T J.VAR (A.AJudgement (sigCon,varCon) (A.Conclusion $DT.Con "forwardEqIntro") eqIntro) []
              sigmaForwarded = eqIntroTree :
                    map
                      (\aTree  ->
                        case aTree of
                          J.T label (A.AJudgement env aTerm aType) upSides -> --assert label `elem` [J.Var,J.Con]
                            let d = (length sigCon + length varCon) - (length (fst env ) + length (snd env ) )
                                downSide' = A.AJudgement (sigCon,varCon) (A.betaReduce $A.shiftIndices aTerm d 0) (A.shiftIndices aType d 0)
                                upSides' = 
                                  map 
                                    (\upside -> 
                                      case upside of 
                                        J.T upLabel (A.AJudgement _ upATerm upAType) [] ->
                                          J.T upLabel (A.AJudgement (sigCon,varCon) (A.betaReduce $ A.shiftIndices upATerm d 0) (A.shiftIndices upAType (d + 1) 0)) []
                                        _ -> upside
                                    )
                                    upSides
                            in
                              J.T label downSide' upSides'
                          _ -> aTree
                      )
                      varsigmaForwarded
          in  (eqForwards sigmaForwarded)  ++ sigmaForwarded
    in --D.trace (show trees) 
        B.resultDef{B.trees = trees}

eqForwards :: [J.Tree A.AJudgement] -> [J.Tree A.AJudgement]
eqForwards =  concatMap eqForward

eqForward :: J.Tree A.AJudgement -> [J.Tree A.AJudgement]
eqForward tree = 
  case A.downSide tree of 
    A.AJudgement aContext aTerm aType ->
      let eqForwardedTypes = filter (\at -> case at of A.Arrow (_:_) _ -> True; _ -> False) $snd $forEq aContext aType
          eqForwardedTerm = A.ArrowApp (A.aCon "eqElim") (aTerm)
      in map (\eqForwardedType -> J.T J.EqE (A.AJudgement aContext eqForwardedTerm eqForwardedType) [tree]) eqForwardedTypes

sigmaForward :: B.AType -> A.Context -> [J.Tree A.AJudgement]
sigmaForward aType (sigCon,varCon) = 
  let baseCon = A.genFreeCon aType "base"
      forwarded' = sigmaForward' aType baseCon aType (sigCon,varCon)
      trees = 
        map
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
  in trees

sigmaForward' :: A.Arrowterm -> B.AType -> B.AType -> A.Context -> [A.Arrowterm -> J.Tree A.AJudgement]
sigmaForward' originType baseTerm aType (sigEnv,varEnv) = case A.arrowNotat aType of
  A.ArrowSigma' [h] t -> 
    let fstbase =  A.ArrowProj A.ArrowFst baseTerm
        sndbase =  A.ArrowProj A.ArrowSnd baseTerm
        t' = A.shiftIndices (A.arrowSubst t  fstbase (A.Conclusion $ DT.Var 0)) (-1) 0
        hForward = sigmaForward' originType fstbase h (sigEnv,varEnv)
        tForward = sigmaForward' originType sndbase t' (sigEnv,varEnv)
        hTree term' = J.T J.SigE (A.AJudgement (sigEnv,varEnv) fstbase h) [J.T J.VAR(A.AJudgement (sigEnv,varEnv) term' originType ) []]
        tTree term'=  J.T J.SigE (A.AJudgement (sigEnv,varEnv) sndbase t') [J.T J.VAR(A.AJudgement (sigEnv,varEnv) term' originType ) []]
    in (if null tForward then [tTree] else tForward) ++ (if null hForward  then [hTree] else hForward)
  A.ArrowSigma' hs tLast -> 
    let fstbase =  A.ArrowProj A.ArrowFst baseTerm
        sndbase =  A.ArrowProj A.ArrowSnd baseTerm
        h:hrest = reverse hs
        t = A.ArrowSigma' (reverse hrest) tLast
        t' =  A.shiftIndices (A.arrowSubst t  fstbase (A.Conclusion $ DT.Var 0)) (-1) 0
        hForward = sigmaForward' originType fstbase h (sigEnv,varEnv)
        tForward = sigmaForward' originType sndbase t' (sigEnv,varEnv)
        hTree term' = J.T J.SigE (A.AJudgement (sigEnv,varEnv) fstbase h) [J.T J.VAR(A.AJudgement (sigEnv,varEnv) term' originType ) []]
    in tForward ++ (if null hForward  then [hTree] else hForward)
  A.Arrow env (A.ArrowSigma' [h] t) -> 
    let lenEnv = length env
        term1 = A.ArrowProj A.ArrowFst $ A.addApp lenEnv baseTerm
        term2 = A.addLam lenEnv $ A.ArrowProj A.ArrowSnd $ A.addApp lenEnv baseTerm
        t' = A.shiftIndices (A.arrowSubst t (A.shiftIndices term1 lenEnv 0) (A.Conclusion $ DT.Var 0)) (-1) 0
        type1 = case h of (A.Arrow henv hcon) -> A.Arrow (henv ++ env) hcon ; _ -> A.Arrow env h
        type2 = case t' of (A.Arrow tenv tcon) -> A.Arrow (tenv ++ env) tcon; _ ->A.Arrow env t'
        hForward = sigmaForward' originType (A.addLam lenEnv $  term1) type1 (sigEnv,varEnv)
        tForward = sigmaForward' originType term2 type2 (sigEnv,varEnv)
        tTree term'= J.T J.SigE (A.AJudgement (sigEnv,varEnv) term2 type2) [J.T J.VAR(A.AJudgement (sigEnv,varEnv) term' $ A.Arrow env originType) []]
        hTree term'= J.T J.SigE (A.AJudgement (sigEnv,varEnv) (A.addLam lenEnv $  term1) type1) [J.T J.VAR(A.AJudgement (sigEnv,varEnv) term' $ A.Arrow env originType) []]
        result = (if null tForward  then [tTree] else tForward) ++ (if null hForward  then [hTree] else hForward)
    in
      result
  A.Arrow env (A.ArrowSigma' hs tLast) -> 
    let lenEnv = length env
        term1 = A.ArrowProj A.ArrowFst $ A.addApp lenEnv baseTerm
        term2 = A.addLam lenEnv $ A.ArrowProj A.ArrowSnd $ A.addApp lenEnv baseTerm
        h:hrest = reverse hs
        t = A.ArrowSigma' (reverse hrest) tLast
        t' = A.shiftIndices (A.arrowSubst t (A.shiftIndices term1 lenEnv 0) (A.Conclusion $ DT.Var 0)) (-1) 0
        type1 = case h of (A.Arrow henv hcon) -> A.Arrow (henv ++ env) hcon ; _ -> A.Arrow env h
        type2 = A.Arrow env t'
        hForward = sigmaForward' originType (A.addLam lenEnv $  term1) type1 (sigEnv,varEnv)
        tForward = sigmaForward' originType term2 type2 (sigEnv,varEnv)
        hTree term'= J.T J.SigE (A.AJudgement (sigEnv,varEnv) (A.addLam lenEnv $  term1) type1) [J.T J.VAR(A.AJudgement (sigEnv,varEnv) term' $ A.Arrow env originType ) []]
        result = tForward ++ (if null hForward  then [hTree] else hForward)
    in
      result
  _ ->  []


searchType :: A.Context -> B.ATerm -> Maybe B.AType
searchType (sigCon,varCon) term =
  case term of
    A.Conclusion (DT.Type) -> Just (A.Conclusion DT.Kind)
    A.Conclusion (DT.Var num) -> if (num >= length varCon) then D.trace (show $ A.AJudgement (sigCon,varCon) term (A.aCon "notFound1")) Nothing else Just $A.shiftIndices (varCon !! num) (num+1) 0
    A.Conclusion (DT.Con txt) -> lookup txt sigCon
    A.Conclusion (DT.Kind) -> Nothing
    A.Conclusion (DT.Bot) -> Just (A.Conclusion DT.Type)
    A.Conclusion (DT.Top) -> Just (A.Conclusion DT.Type)
    A.ArrowSigma' ars ar -> case searchType (sigCon,(ars ++ varCon)) ar  of--雑
        Just t->
          case A.arrowNotat t of 
            A.Conclusion DT.Type ->  Just (A.Conclusion DT.Type)
            A.Conclusion DT.Kind -> Just (A.Conclusion DT.Kind)
            t -> {--D.trace (show $ A.AJudgement (sigCon,varCon) term (A.aCon "notFound2"))--} Nothing
        _ -> {--D.trace (show $ A.AJudgement (sigCon,varCon) term (A.aCon "notFound3"))--} Nothing
    A.Arrow ars ar -> 
      case searchType (sigCon,(ars ++ varCon)) ar of --雑
        Just t->
          case A.arrowNotat t of 
            A.Conclusion DT.Type ->  Just (A.Conclusion DT.Type)
            A.Conclusion DT.Kind -> Just (A.Conclusion DT.Kind)
            t -> {--D.trace (show $ A.AJudgement (sigCon,varCon) term (A.aCon "notFound4"))--} Nothing
        _ -> {--D.trace (show $ A.AJudgement (sigCon,varCon) term (A.aCon "notFound5"))--} Nothing
    A.ArrowApp ar ar' ->
      let fType = searchType (sigCon,varCon) ar
          xType = searchType (sigCon,varCon) ar'
      in case (fType,xType) of (Just (A.Arrow (h:t) res),Just x) -> Just $ A.shiftIndices (A.arrowSubst (A.Arrow (init (h:t)) res) x (A.aVar 0)) (-1) 0; _ ->{-- D.trace (show $ A.AJudgement (sigCon,varCon) term (A.aCon "notFound"))--} Nothing --雑
    A.ArrowPair ar ar' -> 
      let arType = searchType (sigCon,varCon) ar
          arType' = searchType (sigCon,varCon) ar' 
      in case (arType,arType') of
        (Just t,Just t') -> Just (A.arrowNotat $A.ArrowSigma' [t] (t'))
        _ -> {--D.trace (show $ A.AJudgement (sigCon,varCon) term (A.aCon "notFound6"))--} Nothing
    A.ArrowProj as ar ->  case A.betaReduce ar of
      A.ArrowPair x y -> searchType (sigCon,varCon) (case as of A.ArrowFst -> x ; A.ArrowSnd -> y)
      u -> 
        case (searchType (sigCon,varCon) u) of
          Just (A.ArrowSigma' h t) ->
            case as of 
              A.ArrowFst -> Just (last h) 
              A.ArrowSnd -> Just $ A.arrowNotat $ A.shiftIndices (A.arrowSubst (A.ArrowSigma' (init h) t) (A.ArrowProj A.ArrowFst u) (A.aVar 0)) (-1) 1
          _ -> {--D.trace (show $ A.AJudgement (sigCon,varCon) term (A.aCon "notFound7"))--} Nothing
    A.ArrowLam ar -> {--D.trace (show $ A.AJudgement (sigCon,varCon) term (A.aCon "notFound8"))--} Nothing -- 引数部分の型がないから
    A.ArrowEq ar ar' ar2 ->  Just A.aType -- 雑
    _ -> {--D.trace (show $ A.AJudgement (sigCon,varCon) term (A.aCon "notFound9"))--} Nothing

--forwardContext後に使う
forEq :: A.Context -> B.AType -> ([B.AType],[B.AType])
forEq context term =
  case searchType context term of
    Just tType -> 
        let this = if (searchType context tType) == Just A.aType then  [A.Arrow [A.ArrowEq (A.shiftIndices tType 1 0) (A.aVar 0) (A.shiftIndices term 1 0),tType] (A.aVar 1)] else [] in 
            case term of
              A.ArrowPair ar ar' -> D.trace ("ペアは型にはならない@forEq") undefined
              A.ArrowLam ar -> D.trace ("ラムダは型にはならない@forEq") undefined
              A.ArrowSigma' ars ar -> {-D.trace ("シグマは崩してるほうを見る@forEq")-} ([],[])
              A.Conclusion t ->  (this,[])
              A.ArrowApp ar ar' -> 
                let arEqTypes' = map (\r -> let (l,l') = forEq context r in l ++ l') [ar,ar']
                    arEqTypes = 
                      sequence 
                      (case arEqTypes' of 
                        [[],(h:t)] -> [[A.Arrow [] ar],(h:t)]; 
                        [(h:t),[]] -> [(h:t),[A.Arrow [] ar']];
                         ts -> ts
                      )
                in (this, 
                  map
                  (\pair ->
                    case pair of
                      [A.Arrow fArg fRes,A.Arrow xArg xRes] -> 
                        let A.Arrow xArg' xRes' = A.shiftIndices (A.Arrow xArg xRes) (length fArg) 0
                            arg = xArg' ++ fArg
                            res = A.ArrowApp (A.shiftIndices fRes (length xArg) 0) (xRes')
                        in A.Arrow arg res
                      _ -> D.trace ("forEq ここに来るはずはない") undefined)
                  arEqTypes)
              A.ArrowProj ars ar -> 
                let arEqType = let (l,l') = forEq context ar in l ++ l'
                in  (this,
                  map
                  (\arTerm ->
                    case arTerm of
                      A.Arrow arArg arRes -> --D.trace (show (A.AJudgement context (A.aCon "?") term) ++ " arRes " ++ (show $ arRes))$ 
                        let arg = arArg
                            res = A.ArrowProj ars arRes
                        in A.Arrow arg res
                      _ -> D.trace ("forEq ここに来るはずはない") undefined
                    )
                  arEqType)
              A.Arrow [] ar -> forEq context ar       
              A.Arrow ars ar -> 
                let h = last ars
                    hEqTypes = (\r -> let (l,l') = forEq context r in l ++ l') h
                    tEqTypes = let (l,l') = forEq (case context of (sigCon,varCon)-> (sigCon,h:varCon)) (A.Arrow (init ars) ar) in l++l'
                    arEqTypes = sequence (case [hEqTypes,tEqTypes] of [[],(h':t')] -> [[A.Arrow [] h],(h':t')]; [(h':t'),[]] -> [(h':t'),[(A.Arrow (init ars) ar)]]; ts -> ts)
                in  (this,
                  map
                  (\pair ->
                    case pair of
                      [A.Arrow hArg hRes,A.Arrow tArg tRes] -> --D.trace (show (A.AJudgement context (A.aCon "?") term) ++ " arRes " ++ (show $ arRes))$ 
                        let arg = tArg++(hRes:hArg)
                            res = tRes
                        in --D.trace (show (A.AJudgement context (A.aCon "?") term) ++ " hArg " ++ (show hArg) ++ " hRes " ++ (show hRes) ++ " tArg "++(show tArg) ++ " tRes "++(show tRes)) $
                          A.Arrow arg res
                      _ -> D.trace ("forEq ここに来るはずはない") undefined
                    )
                  arEqTypes)
              A.ArrowEq ar ar' ar2 -> 
                let aEqTypes' = (\r -> let (l,l') = forEq context r in l ++ l') ar'
                    bEqTypes' = (\r -> let (l,l') = forEq context r in l ++ l') ar2
                    notnull = not $ null ({-tEqTypes' ++ -}aEqTypes' ++ bEqTypes')
                    aEqTypes = case (notnull,aEqTypes') of (True,[]) -> [A.Arrow [] ar']; (_,ts) -> ts
                    bEqTypes = case (notnull,bEqTypes') of (True,[]) -> [A.Arrow [] ar2]; (_,ts) -> ts
                    arEqTypes = sequence [{-tEqTypes,-}aEqTypes,bEqTypes]
                in  (this, 
                  map
                  (\tup ->
                    case tup of
                      [A.Arrow aArg aRes,A.Arrow bArg bRes] -> --D.trace (show (A.AJudgement context (A.aCon "?") term) ++ " arRes " ++ (show $ arRes))$ 
                        let A.Arrow aArg' aRes' = case (A.Arrow aArg aRes)  of A.Arrow h t->A.Arrow h t;t -> A.Arrow [] t
                            A.Arrow bArg' bRes' = case A.shiftIndices (A.Arrow bArg bRes) ((length $  aArg)) 0 of A.Arrow h t->A.Arrow h t;t -> A.Arrow [] t
                            arg = bArg' ++ aArg'
                            res = A.ArrowEq (A.shiftIndices ar (length $aArg ++ bArg) 0) (A.shiftIndices aRes' (length bArg) 0) bRes'
                        in --D.trace (show (A.AJudgement context (A.aCon "?") term) ++ " hArg " ++ (show hArg) ++ " hRes " ++ (show hRes) ++ " tArg "++(show tArg) ++ " tRes "++(show tRes)) $
                          A.Arrow arg res
                      _ -> D.trace ("forEq ここに来るはずはない") undefined
                    )
                  arEqTypes)
    Nothing -> ([],[])
