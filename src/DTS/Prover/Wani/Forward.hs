{-# LANGUAGE OverloadedStrings #-}
module DTS.Prover.Wani.Forward (forwardContext) where

import qualified Data.List as L 

import qualified DTS.DTTdeBruijn as DdB
import qualified DTS.Prover.Wani.Arrowterm as A
import qualified Interface.Tree as UDT
import qualified DTS.QueryTypes as QT

import qualified DTS.Prover.Wani.WaniBase as B 


import qualified Debug.Trace as D

eqIntroElimTerm :: A.Arrowterm
eqIntroElimTerm = 
  A.Arrow 
    (
      [A.ArrowEq (A.aVar 4) (A.aVar 0) (A.aVar 3),A.aVar 3,A.ArrowEq (A.aVar 2) (A.aVar 0) (A.aVar 1),A.aVar 1,A.aVar 0,A.aType]
      -- [A.ArrowEq (A.Conclusion $ DdB.Var 4) (A.Conclusion $ DdB.Var 1) (A.Conclusion $ DdB.Var 3),
      -- A.ArrowEq (A.Conclusion $ DdB.Var 3) (A.Conclusion $ DdB.Var 1) (A.Conclusion $ DdB.Var 2)]++ 
      -- (map A.Conclusion [DdB.Var 2,DdB.Var 1,DdB.Var 0,DdB.Type])
    ) 
    (A.ArrowEq (A.Conclusion $ DdB.Var 5) (A.Conclusion $ DdB.Var 3) (A.Conclusion $ DdB.Var 1))

eqIntro :: A.Arrowterm
eqIntro = 
  A.Arrow 
    (map A.Conclusion [DdB.Var 0,DdB.Type])
    (A.ArrowEq (A.Conclusion $ DdB.Var 1) (A.Conclusion $ DdB.Var 0) (A.Conclusion $ DdB.Var 0))

searchType :: A.Context -> B.ATerm -> Maybe B.AType
searchType (sigCon,varCon) term =
  case term of
    A.Conclusion (DdB.Type) -> Just (A.Conclusion DdB.Kind)
    A.Conclusion (DdB.Var num) -> if (num >= length varCon) then D.trace (show $ A.AJudgment sigCon varCon term (A.aCon "notFound1")) Nothing else Just $A.shiftIndices (varCon !! num) (num+1) 0
    A.Conclusion (DdB.Con txt) -> lookup txt sigCon
    A.Conclusion (DdB.Kind) -> Nothing
    A.Conclusion (DdB.Bot) -> Just (A.Conclusion DdB.Type)
    A.Conclusion (DdB.Top) -> Just (A.Conclusion DdB.Type)
    A.ArrowSigma' ars ar -> case searchType (sigCon,(ars ++ varCon)) ar  of--雑
        Just t->
          case A.arrowNotat t of 
            A.Conclusion DdB.Type ->  Just (A.Conclusion DdB.Type)
            A.Conclusion DdB.Kind -> Just (A.Conclusion DdB.Kind)
            t -> {--D.trace (show $ A.AJudgement (sigCon,varCon) term (A.aCon "notFound2"))--} Nothing
        _ -> {--D.trace (show $ A.AJudgement (sigCon,varCon) term (A.aCon "notFound3"))--} Nothing
    A.Arrow ars ar -> 
      case searchType (sigCon,(ars ++ varCon)) ar of --雑
        Just t->
          case A.arrowNotat t of 
            A.Conclusion DdB.Type ->  Just (A.Conclusion DdB.Type)
            A.Conclusion DdB.Kind -> Just (A.Conclusion DdB.Kind)
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
                        -- let A.Arrow xArg' xRes' = A.shiftIndices (A.Arrow xArg xRes) (length fArg) 0
                        --     arg = xArg' ++ fArg
                        --     res = A.ArrowApp (A.shiftIndices fRes (length xArg) 0) (xRes')
                        -- in A.Arrow arg res
                        case A.shiftIndices (A.Arrow xArg xRes) (length fArg) 0 of
                          A.Arrow xArg' xRes' -> let arg = xArg' ++ fArg
                                                     res = A.ArrowApp (A.shiftIndices fRes (length xArg) 0) (xRes')
                                                 in A.Arrow arg res
                          _ -> A.Arrow xArg xRes -- | ToDo: これは間違い。どうにかする
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

forwardContext :: A.SAEnv -> A.AEnv -> B.Result
forwardContext sig var = 
    let (sigNames,sigTerms) = unzip sig
        context' = var ++ sigTerms
        varsigmaForwarded =
          (concat$
            zipWith
              (\f fr ->
                let varLen =  length fr - length sigNames
                    fIsVar = varLen > 0
                    (varCon',sigTerms') = splitAt varLen fr
                    sigCon' = reverse $ zip (reverse sigNames) (reverse sigTerms')
                    sigmaForwarded = sigmaForward2 f sigCon' varCon'
                    varForwarded =                 
                      if fIsVar then
        --                   -- [J.T J.VAR (A.AJudgement  (sigCon',tail varCon') f (A.Conclusion  DdB.Type)) [] | case A.betaReduce f of A.Conclusion DdB.Type -> False; A.Conclusion (DdB.Con _)-> False;_ ->True] ++
                          [UDT.Tree QT.Var (A.AJudgment sigCon' varCon' (A.Conclusion $ DdB.Var 0 ) (A.shiftIndices f 1 0)) [] ]
                        else 
                          let (sigName,sigTerm) = head sigCon' in
        --                     -- [J.T J.VAR (A.AJudgement (tail sigCon',[]) sigTerm (A.Conclusion  DdB.Type)) [] | sigTerm /= A.Conclusion DdB.Type] ++
                            [UDT.Tree QT.Con (A.AJudgment sigCon' varCon' (A.Conclusion $ DdB.Con sigName) (A.shiftIndices sigTerm 1 0)) [] ]
                in  sigmaForwarded ++ varForwarded
              )
              context'
              (L.tails  context')) 
        trees =   
          let eqIntroTree = UDT.Tree QT.Var (A.AJudgment sig var (A.Conclusion $DdB.Con "forwardEqIntro") eqIntro) []
              sigmaForwarded = eqIntroTree :
                    map
                      (\aTree  ->
                        case aTree of
                          UDT.Tree label (A.AJudgment sig_a var_a aTerm aType) upSides -> --assert label `elem` [J.Var,J.Con]
                            let d = (length sig + length var) - (length sig_a + length var_a)
                                downSide' = A.AJudgment sig var (A.betaReduce $A.shiftIndices aTerm d 0) (A.shiftIndices aType d 0)
                                upSides' = 
                                  map 
                                    (\upside -> 
                                      case upside of 
                                        UDT.Tree upLabel (A.AJudgment _ _ upATerm upAType) [] ->
                                          UDT.Tree upLabel (A.AJudgment sig var (A.betaReduce $ A.shiftIndices upATerm d 0) (A.shiftIndices upAType (d + 1) 0)) []
                                        _ -> upside
                                    )
                                    upSides
                            in UDT.Tree label downSide' upSides'
                          _ -> aTree
                      )
                      varsigmaForwarded
          in (eqForwards2' sigmaForwarded)  ++ sigmaForwarded
    in B.resultDef{B.trees = trees}


eqForwards2' :: [UDT.Tree A.Arrowrule A.AJudgment] -> [UDT.Tree A.Arrowrule A.AJudgment]
eqForwards2' =  concatMap eqForward2

eqForward2 :: UDT.Tree A.Arrowrule A.AJudgment -> [UDT.Tree A.Arrowrule A.AJudgment]
eqForward2 tree =
  case A.downSide' tree of 
    A.AJudgment sig var aTerm aType ->
      let eqForwardedTypes = filter (\at -> case at of A.Arrow (_:_) _ -> True; _ -> False) $snd $forEq (sig,var) aType
          eqForwardedTerm = A.ArrowApp (A.aCon "eqElim") (aTerm)
      in map (\eqForwardedType -> UDT.Tree QT.IqE (A.AJudgment sig var eqForwardedTerm eqForwardedType) [tree]) eqForwardedTypes

sigmaForward2 :: B.AType -> A.SAEnv -> A.AEnv -> [UDT.Tree A.Arrowrule A.AJudgment]
sigmaForward2 aType sig var =
  let baseCon = A.genFreeCon aType "base"
      forwarded' = sigmaForward2' aType baseCon aType sig var
      trees = 
        map
        (\aTreef
          -> let
              aTree = aTreef (A.Conclusion $DdB.Var 0)
              (A.AJudgment sig var aTerm aType) = A.downSide' aTree
              newJ = A.AJudgment
                      sig var
                      (A.arrowSubst (A.shiftIndices aTerm 1 0) (A.Conclusion $ DdB.Var 0) baseCon)
                      (A.arrowSubst (A.shiftIndices aType 1 0) (A.Conclusion $  DdB.Var 0) baseCon)
              in A.changeDownSide' aTree newJ)
        forwarded'
  in trees

sigmaForward2' :: A.Arrowterm -> B.AType -> B.AType -> A.SAEnv -> A.AEnv -> [A.Arrowterm -> UDT.Tree A.Arrowrule A.AJudgment]
sigmaForward2' originType baseTerm aType sig var = case A.arrowNotat aType of 
  A.ArrowSigma' [h] t ->
    let fstbase =  A.ArrowProj A.ArrowFst baseTerm
        sndbase =  A.ArrowProj A.ArrowSnd baseTerm
        t' = A.shiftIndices (A.arrowSubst t  fstbase (A.Conclusion $ DdB.Var 0)) (-1) 0
        hForward = sigmaForward2' originType fstbase h sig var
        tForward = sigmaForward2' originType sndbase t' sig var
        hTree term' = UDT.Tree QT.SigmaE (A.AJudgment sig var fstbase h) [UDT.Tree QT.Var (A.AJudgment sig var term' originType ) []]
        tTree term'=  UDT.Tree QT.SigmaE (A.AJudgment sig var sndbase t') [UDT.Tree QT.Var (A.AJudgment sig var term' originType ) []]
    in (if null tForward then [tTree] else tForward) ++ (if null hForward  then [hTree] else hForward)
  A.ArrowSigma' hs tLast ->
    let fstbase =  A.ArrowProj A.ArrowFst baseTerm
        sndbase =  A.ArrowProj A.ArrowSnd baseTerm
        h:hrest = reverse hs
        t = A.ArrowSigma' (reverse hrest) tLast
        t' =  A.shiftIndices (A.arrowSubst t  fstbase (A.Conclusion $ DdB.Var 0)) (-1) 0
        hForward = sigmaForward2' originType fstbase h sig var
        tForward = sigmaForward2' originType sndbase t' sig var
        hTree term' = UDT.Tree QT.SigmaE (A.AJudgment sig var fstbase h) [UDT.Tree QT.Var (A.AJudgment sig var term' originType ) []]
    in tForward ++ (if null hForward  then [hTree] else hForward)
  A.Arrow env (A.ArrowSigma' [h] t) ->
    let lenEnv = length env
        term1 = A.ArrowProj A.ArrowFst $ A.addApp lenEnv baseTerm
        term2 = A.addLam lenEnv $ A.ArrowProj A.ArrowSnd $ A.addApp lenEnv baseTerm
        t' = A.shiftIndices (A.arrowSubst t (A.shiftIndices term1 lenEnv 0) (A.Conclusion $ DdB.Var 0)) (-1) 0
        type1 = case h of (A.Arrow henv hcon) -> A.Arrow (henv ++ env) hcon ; _ -> A.Arrow env h
        type2 = case t' of (A.Arrow tenv tcon) -> A.Arrow (tenv ++ env) tcon; _ ->A.Arrow env t'
        hForward = sigmaForward2' originType (A.addLam lenEnv $  term1) type1 sig var
        tForward = sigmaForward2' originType term2 type2 sig var
        tTree term'= UDT.Tree QT.SigmaE (A.AJudgment sig var term2 type2) [UDT.Tree QT.Var (A.AJudgment sig var term' $ A.Arrow env originType) []]
        hTree term'= UDT.Tree QT.SigmaE (A.AJudgment sig var (A.addLam lenEnv $  term1) type1) [UDT.Tree QT.Var (A.AJudgment sig var term' $ A.Arrow env originType) []]
        result = (if null tForward  then [tTree] else tForward) ++ (if null hForward  then [hTree] else hForward)
    in
      result
  A.Arrow env (A.ArrowSigma' hs tLast) -> 
    let lenEnv = length env
        term1 = A.ArrowProj A.ArrowFst $ A.addApp lenEnv baseTerm
        term2 = A.addLam lenEnv $ A.ArrowProj A.ArrowSnd $ A.addApp lenEnv baseTerm
        h:hrest = reverse hs
        t = A.ArrowSigma' (reverse hrest) tLast
        t' = A.shiftIndices (A.arrowSubst t (A.shiftIndices term1 lenEnv 0) (A.Conclusion $ DdB.Var 0)) (-1) 0
        type1 = case h of (A.Arrow henv hcon) -> A.Arrow (henv ++ env) hcon ; _ -> A.Arrow env h
        type2 = A.Arrow env t'
        hForward = sigmaForward2' originType (A.addLam lenEnv $  term1) type1 sig var
        tForward = sigmaForward2' originType term2 type2 sig var
        hTree term'= UDT.Tree QT.SigmaE (A.AJudgment sig var (A.addLam lenEnv $  term1) type1) [UDT.Tree QT.Var (A.AJudgment sig var term' $ A.Arrow env originType ) []]
        result = tForward ++ (if null hForward  then [hTree] else hForward)
    in
      result
  _ ->  []