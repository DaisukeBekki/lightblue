{-# LANGUAGE OverloadedStrings #-}

module DTS.Wani.Backward (
    deduce
) where

import qualified DTS.DTT as DT            -- DTT
import qualified DTS.Wani.Arrowterm as A -- Aterm
import qualified DTS.Wani.Forward as F  --Forward Inference
import qualified DTS.Prover_daido.Judgement  as J -- Judgement

import qualified Data.List as L -- List
import qualified Debug.Trace as D -- Trace
import qualified Data.Text.Lazy as T -- Text
import qualified Data.Maybe as M -- Maebe

import qualified DTS.Wani.WaniBase as B

-- | Execute proof search.
deduce :: B.DeduceRule
deduce con arrowType depth setting 
  | depth > B.maxdepth setting =
      B.resultDef{B.errMsg = "depth @ deduce",B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}} -- Set `B.rStatus` to update the maximum depth used.
  | any (\(con',aType')->A.contextLen con == (A.contextLen con')&&A.sameCon con con'&& A.sameTerm (con,arrowType) (con',aType')) (B.deduceNgLst (B.sStatus setting)) = 
      B.debugLog con arrowType depth setting "Avoid endless loops."  (B.resultDef{B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}})
  | otherwise = 
    case arrowType of
      A.Conclusion DT.Kind -> -- The only term for `kind` is `type`.
        B.resultDef{
          B.trees = [J.T J.VAR(A.AJudgement con (A.aType) (A.Conclusion DT.Kind)) []],
          B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}}
      _ -> 
        let result' = foldl -- If certain conditions are met, the proof may be rounded up without moving on to the next proof search.
              (\rs f -> 
                if (B.allProof (B.sStatus setting)) || (null (B.trees rs)) 
                then 
                  let result = f con (A.arrowNotat arrowType) depth setting{B.sStatus = B.rStatus rs}
                  in B.mergeResult rs result -- `B.mergeResult` updates the status of a newly executed proof search
                else rs)

              (B.resultDef{
                  B.rStatus = 
                    B.mergeStatus 
                      (B.sStatus setting) 
                      (B.statusDef{B.usedMaxDepth = depth,B.deduceNgLst = (con,arrowType) : (B.deduceNgLst $B.sStatus setting)})} ) 
                      -- Currently, `arrowType` proof search is performed under environment `con`, and to prevent infinite loops, it is set to round up when `arrowType` proof search is needed under environment `con`(★).
              ( -- The stronger the rule, the later the timing of application is turned back. For example, `dne` can be used for any term, thus turning the execution later. This setting takes effect in combination with the rounding up of proof search using `B.allProof`.
                [membership,piIntro,sigmaIntro,piElim] 
                  ++ [dne | arrowType /= A.Conclusion DT.Bot && B.mode setting == B.WithDNE]
                  ++ [efq | arrowType /= A.Conclusion DT.Bot && B.mode setting == B.WithEFQ]
              )
            result = result'{B.rStatus = (B.rStatus result'){B.deduceNgLst = B.deduceNgLst$B.sStatus setting}}{B.trees = L.nub$B.trees result'} -- excludes duplicate proof trees, and restore deduceNgList to its original state from ★ state
        in 
          if null (B.trees result) -- for debug
            then
              (if B.debug setting then B.debugLog con arrowType depth setting "deduce failed " else id) result
            else
              (if B.debug setting then D.trace (L.replicate (2*depth) ' ' ++  show depth ++ " deduced:  " ++ show (map A.downSide (B.trees result))) else id) result

-- | Execute typecheck
typecheck :: B.TypecheckRule 
typecheck con arrowTerm arrowType depth setting 
  | depth > B.maxdepth setting = B.resultDef{B.errMsg = "depth @ typecheck",B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}}
  | any (\(con',aTerm',aType')->A.sameCon con con'&& A.sameTerm (con,arrowTerm) (con',aTerm') && A.sameTerm (con,arrowType) (con',aType')) (B.failedlst (B.sStatus setting)) = B.debugLogWithTerm con arrowTerm arrowType depth setting "Failed in the past."  (B.resultDef{B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}})
  | B.falsum setting && arrowTerm == A.Conclusion DT.Bot && arrowType == A.aType = B.resultDef{B.trees = [J.T J.BotF (A.AJudgement con arrowTerm arrowType) []],B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}} -- if `B.falsum` is true, the type for `false` is `type`.
  | arrowTerm == A.Conclusion DT.Kind = B.debugLogWithTerm con arrowTerm arrowType depth setting "kind cannot be a term."  B.resultDef{B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}}
  | arrowType == A.Conclusion DT.Kind = if arrowTerm == A.aType then B.resultDef{B.trees = [J.T J.CON(A.AJudgement con arrowTerm arrowType) []],B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}} else B.resultDef{B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}}
  | otherwise = B.debugLogWithTerm con arrowTerm arrowType depth setting "typeCheck" $
      let arrowType' = A.arrowNotat arrowType
          formResult = 
            foldl  -- `typecheck` exits when one term-type pair is found.
            (\r f -> if null (B.trees r) then f con arrowTerm arrowType' depth setting{B.sStatus = B.rStatus r} else r)
            B.resultDef{B.rStatus = B.mergeStatus (B.sStatus setting) (B.statusDef{B.usedMaxDepth = depth})}
            [piForm,sigmaForm,eqForm]
          piElimResult = if null (B.trees formResult) then piElimTypeCheckApp con arrowTerm [] arrowType' depth setting{B.sStatus = B.rStatus formResult} else formResult -- It is complicated to handle when there is a function application in a term, so it is separately handled by `piElimTypeCheckApp`.
          result = foldl --  looking for a proof tree obtained by proof search that matches the proof term.
            (\r f -> 
              let r' = r{B.trees = filter (\a ->A.sameTerm (A.envfromAJudgement $ A.downSide a,A.termfromAJudgement (A.downSide a)) (con,A.arrowNotat arrowTerm)) (B.trees r)}
              in 
              if null (B.trees r') then  f con arrowType' depth setting{B.sStatus = B.rStatus r'} else r')
            piElimResult
            (
              [membership,piIntro,sigmaIntro] 
                ++ [dne | arrowType /= A.Conclusion DT.Bot && B.mode setting == B.WithDNE] 
                ++ [efq | arrowType /= A.Conclusion DT.Bot && B.mode setting == B.WithEFQ]
            )
          resultStatus = B.rStatus result
          failedlst = if null (B.trees result) then (con,arrowTerm,arrowType):(B.failedlst resultStatus) else B.failedlst resultStatus
          status = resultStatus{B.failedlst =failedlst}
      in 
        (if B.debug setting then D.trace (L.replicate (2*depth) ' ' ++  show depth ++ " termFound :  " ++ show (map A.downSide $L.nub $ B.trees result)) else id) result{B.rStatus = status}{B.trees = L.nub $B.trees result}


-- | This function refers to the results of forward inference using sigmaElim and eqElim and returns a list of proof trees with matching type parts.
membership :: B.DeduceRule 
membership con aType depth setting = 
  let forwardResult = F.forwardContext con
      forwardTrees = B.trees forwardResult
      matchLst =  L.nub $
        filter 
          (\xTree -> 
            let x = A.downSide xTree 
                xCon =A.envfromAJudgement x
                xType =   A.typefromAJudgement x
            in 
                  A.sameTerm (con,aType) (xCon,xType))
            $ forwardTrees  
  in B.debugLog con aType depth setting "membership" (forwardResult{B.trees = matchLst,B.rStatus = B.sStatus setting})

-- | not used
eqIntro :: B.DeduceRule 
eqIntro con aType depth setting = 
  case aType of
    A.ArrowEq t a b -> 

      if a /= b then  B.debugLog con (A.ArrowEq t a b) depth setting "eqIntro1"  B.resultDef{B.rStatus = B.sStatus setting}
        else
          let typechecked =  typecheck con a t depth setting
              trees =  
                map 
                (\dTree -> 
                    let (A.AJudgement env aTerm a_type) = A.downSide dTree in 
                      J.T J.SigE (A.AJudgement con (A.aCon $T.pack $ "eqIntro(" ++ show (A.AJudgement con a t) ++ ")" ) (A.ArrowEq t a a)) [dTree])  -- 等号ラベルundefined
                (B.trees typechecked)
          in B.debugLog con aType depth setting "eqIntro2" typechecked{B.trees = trees}
    _ -> B.debugLog con aType depth setting "eqIntro3" B.resultDef{B.rStatus = B.sStatus setting}

piIntro :: B.DeduceRule 
-- | piIntro
-- +------------------------+----------------------------------+
-- | input                  | \[ \Gamma \vdash ? : A -> B  \]  |
-- +========================+==================================+
-- | con                    | \[ \Gamma \]                     |
-- +------------------------+----------------------------------+
-- | aType                  | \[ A -> B \]                     |
-- +========================+==================================+
-- | bTrees                 | proof trees for                  |
-- |                        | \[ \Gamma, a:A \vdash ? : B\]    |
-- +------------------------+----------------------------------+
-- | typeChecked            | check if \[ A->B \]  is a type   |
-- +------------------------+----------------------------------+
piIntro con aType depth setting = 
  case (con,aType) of
    ((sigCon,varCon),A.Arrow a b) -> 
      let bTrees = deduce (sigCon,a ++ varCon) b (depth+1) setting{B.sStatus = (B.sStatus setting)}
          typeChecked = 
            foldl 
            (\r dtTerm -> 
              let newR = typecheck con aType (A.Conclusion dtTerm) depth setting{B.sStatus = (B.rStatus r)}
              in newR{B.trees = B.trees newR ++ B.trees r}
            )
             B.resultDef{B.rStatus = B.sStatus setting} 
             [DT.Type,DT.Kind]
          tbTreePairs = zip (concatMap (replicate (length (B.trees bTrees))) (B.trees typeChecked)) (cycle $B.trees bTrees)
          piABTrees = map 
            (\(tTree,bTree) -> 
              let aTerm = A.betaReduce $ A.addLam (length a) (A.termfromAJudgement $ A.downSide bTree) in 
                J.T J.PiI (A.AJudgement con aTerm aType) [tTree,bTree]
              ) 
            tbTreePairs
          result = bTrees{B.rStatus = (B.mergeStatus (B.rStatus bTrees) (B.rStatus typeChecked)),B.trees = piABTrees} 
      in B.debugLog con aType depth setting "piIntro1"  result
    _ -> B.debugLog con aType depth setting "piIntro2"  B.resultDef{B.rStatus = B.sStatus setting}

tailIsB :: A.Arrowterm -> A.Arrowterm -> (Int,Bool) -- `tailisB` should use Maybe Monad.
-- | tailIsB
-- +------------------------+----------------------------------+
-- | input                  | \[ A -> B -> C, B -> C \]        |
-- +========================+==================================+
-- | output                 | (1(= 2 - 1),True)                |
-- +------------------------+----------------------------------+
tailIsB (A.Arrow env b) (A.Arrow env' b')=
  let d = length env - length env'
  in  (d
      ,
      d >= 0
      &&
      A.canBeSame (length env) b (A.shiftIndices b' d 0)
      &&
      all (\((s,num),t) -> A.canBeSame (num + d) s t) 
        (zip (zip (take (length env') env) [0..]) (map (\c' -> A.shiftIndices c' d 0) env')))
-- | tailIsB
-- +------------------------+----------------------------------+
-- | input                  | \[ A -> B -> C, C \]             |
-- +========================+==================================+
-- | output                 | (2(= 2 - 0),True)                |
-- +------------------------+----------------------------------+
tailIsB (A.Arrow env b) b'=
  let result =  (length env,A.canBeSame (length env) b (A.shiftIndices b' (length env) 0))
  in result
tailIsB _ _ =(0,False)

arrowConclusionBs :: [A.AJudgement] -> A.Arrowterm -> [(Int,J.Tree A.AJudgement)]
-- | arrowConclusionBs (Would the input be appropriate? Why this function uses not [J.Tree A.AJudgement] but [A.AJudgement]?)
-- +------------------------+--------------------------------------------------------------+
-- | input                  | \[ \Gamma \vdash a : A -> B, \Gamma \vdash a : A -> C \], B  |
-- +========================+==============================================================+
-- | output                 | \[(1,tree for \Gamma \vdash a : A -> B )\]                   |
-- +------------------------+--------------------------------------------------------------+
arrowConclusionBs judgements b=
  let arrowConclusionB j b = (tailIsB (A.typefromAJudgement j) b,j) in
    map
    (\((num ,b),j )-> (num,J.T J.VAR j [])) -- It is necessary to check if there is any problem with the certification tree being thrown away.
    $filter
      (snd . fst)
      $map (`arrowConclusionB` b) judgements

deduceEnv :: A.Context -> (Int,J.Tree A.AJudgement) -> Int -> B.Setting -> (J.Tree A.AJudgement,[[B.Result]])

-- | deduceEnv
-- +------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
-- | input                  | [entity:type, event:(x0:entity)→ type, x:entity, girl:(x0:entity)→ type, man:(x0:entity)→ type] u4:girl(x), u5:[ x4:entity, u6:girl(x4) ] => man(x4) |- u4:girl(x), u5:[ x4:entity, u6:girl(x4) ] => man(x4) ,2 |
-- +========================+=================================================================================================================================================================================================================+
-- | parentLsts             |  [(2,[1]),(1,[0]),(0,[])]                                                                                                                                        |
-- +------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
-- | childrenLLsts          |  [[],[-1],[0,1],[-1,-1,-1]]                                                                                                                                         |
-- +========================+=====================================================================================================================================================================+
-- | a                      |  ((1,[0]),[-1])   This is for girl(x4), the fst element means that is refer to one previous var (x4:entity) and the snd element means no consequent refer to this var (u6:girl(x4)). |
-- +------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
-- | b'                     | ([girl(x4),man(x4)],[[result for entity],[result with null Tree]])   This if for girl(x). the fst element shows consequents and the snd, prooftrees for antecedents  |
-- +------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
-- | substLsts              |  [(deducedLst,[[(0,[Result for x4 with prooftree 1])]]),(deducedLst,[[(0,[Result for x4 with prooftree 2])]])]  They are sets of proof of the precedent to be adopted|
-- +========================+=====================================================================================================================================================================+
-- | a                      |  ((0,[]),[0,1])  This is for entity, the fst element means that is refer to no previous var and the snd element means 2 consequents refer to this var (entity). |
-- +------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
-- | b'                     | ([entity,girl(x4),man(x4)],[[result with null Tree]])   This if for girl(x). the fst element shows consequents and the snd, prooftrees for antecedents  |
-- +------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
-- | clueLst'               | [girl(_),man(_)] In looking for proof of the target, check how it is used in the consequents for a hint. 
-- +------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
-- | deducedLstaTypeAndClues| [(([Result for null Tree],entity),[[  x0:entity ] => girl(x0),[  x0:entity ] => man(x0)])]  The snd fst element is the type after substituting the proof term of the previous case
-- +------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
-- | deducedLsts'           |  I believe we can use 'clueLst' better in this function.
--
-- | result'                | proofs for antecedents

deduceEnv (sEnv,vEnv) (num,aJudgement) depth setting= 
  case  A.arrowNotat $A.typefromAJudgement $A.downSide aJudgement of
    (A.Arrow env b) ->
        let as =  drop (length env - num) env
            b1 = A.arrowNotat $ A.Arrow (take (length env - num) env) b
            parentLsts = reverse $ zipWith (\term num -> (num,A.varsInaTerm term)) (reverse (b1:as)) [0..] 
            childrenLsts =  map (\l -> map (\(num,l) -> if num `elem` l then num  else -1) (zip [0..] $reverse l)) $L.inits $map snd parentLsts
            (_,result') = 
                foldr
                  (\a b' -> case (a,b') of 
                    (((argId,parentLst),childrenLst),(b1':as',deducedLsts))-> 
                      let substLsts = 
                            map 
                            (\deducedLst -> 
                              (deducedLst,sequence$
                              map 
                                (\num -> --
                                  (zip [num,num..] (let chosen = deducedLst !! num in map (\t -> chosen{B.trees = [t]}) (B.trees chosen) ))) 
                                (filter (\num -> 0<= num && num < length deducedLst) parentLst)
                              )
                            )
                            deducedLsts;
                          clueLst' = 
                            map 
                              (\num -> 
                                A.shiftIndices (A.arrowNotat $A.arrowSubst (as' !! num ) (A.aVar (-1)) (A.aVar num)) 1 (-1)) 
                              $ filter (-1 <) childrenLst;
                          deducedLstaTypeAndClues = 
                            concatMap 
                              (\(deducedLst,substLst) -> 
                                  map
                                    (\substNote ->
                                      let (dLst,aType') = 
                                            foldr 
                                              (\(beforeVarNum,afterResult) (deducedLst',t) ->
                                                ((take beforeVarNum deducedLst') ++ (afterResult:(drop (beforeVarNum + 1) deducedLst')), 
                                                  A.betaReduce $ A.arrowSubst t (A.shiftIndices (A.termfromAJudgement $ A.downSide $head $B.trees afterResult) (argId) 0) (A.aVar (beforeVarNum)))
                                              )
                                              (deducedLst,b1')
                                              (reverse substNote) -- reverse is used to apply a var from an earlier antecedent.
                                      in
                                        ((dLst,A.shiftIndices aType' (-argId) 0),map (A.Arrow [aType']) clueLst')
                                    )
                                    substLst
                              )
                              substLsts;            
                          deducedLsts' =  filter (\dLst -> not $ null dLst) $
                            map 
                              (\((dLst,aType'),clueLst) -> 
                                let deduced = deduce (sEnv,vEnv) aType' (depth + 1) setting{B.sStatus = (B.rStatus (head dLst)){B.allProof = True && (length as')>0}};
                                in
                                  if null (B.trees deduced) then [] else (if null clueLst then deduced{B.trees =[head $ B.trees deduced]} else deduced){B.rStatus = (B.rStatus deduced){B.allProof = B.allProof $B.sStatus setting}}:dLst)
                              deducedLstaTypeAndClues
                      in   (as',deducedLsts')
                    (c,d) -> d
                    ) 
                  ((reverse $ b1:as),[[B.resultDef{B.rStatus = B.sStatus setting}]]) 
                  (tail$zip parentLsts childrenLsts) 
            statusArgTreesLst =   map (\rs -> (B.rStatus $ head rs,sequence $ init $map B.trees rs)) result'
            status = foldr (\s s' -> B.mergeStatus s s') (B.sStatus B.settingDef) (map fst statusArgTreesLst) 
            deduceTargetAndCons = take num $reverse $ init $ L.tails env 
            proofForEnv = 
              init$
              foldl 
              (\(r:rs) (f:e) -> 
                  deduce (sEnv,e++vEnv) f (depth+1) setting{B.sStatus = (B.rStatus r)}:(r:rs)
              )
              [B.resultDef{B.rStatus = (B.sStatus setting){B.allProof = True}}]
              deduceTargetAndCons
            lastStatus = (B.rStatus $head proofForEnv){B.allProof = B.allProof $B.sStatus setting}
        in 
          (aJudgement,map init result')
    _ -> (aJudgement, [])


deduceEnvs :: A.Context -> [(Int,J.Tree A.AJudgement)] -> Int -> B.Setting -> [(J.Tree A.AJudgement,[[B.Result]])]
-- | deduceEnvs
-- +------------------------+----------------------------------------------------------------------------+
-- | input                  | \Gamma , [(1, \Gamma \vdash x : B -> A)]                                   |
-- +========================+============================================================================+
-- | output                 | tp be updated     |
-- +------------------------+----------------------------------------------------------------------------+
deduceEnvs con aJudgements depth setting= 
  let ajudges = map (\aj ->deduceEnv con aj depth setting) aJudgements
  in filter (\(t,r)-> not $null r) ajudges

substAsInPiElim:: A.Arrowterm -> [A.Arrowterm] -> A.Arrowterm
substAsInPiElim (A.Arrow env t) args
  | length env < length args = undefined
  | otherwise =
    let beforeSubst = 
          case  length env - length args of
            0 -> t
            d -> A.Arrow (reverse $drop (length args) $reverse env) t
        afterSubst = 
          foldr 
          (\ (num,a) tt -> A.arrowSubst tt (A.shiftIndices a (length args) 0) (A.aVar num)) 
          beforeSubst $ 
          reverse$zip [0..] $args
    in
       A.shiftIndices afterSubst (negate (length args)) (length args)
substAsInPiElim _ _= undefined


-- | piElim
-- +------------------------+----------------------------------------------------------------------------+
-- | input                  | \[ \Gamma \vdash ? : A \]                                                  |
-- +========================+============================================================================+
-- | dAndaTrees             | \[ (1, \Gamma \vdash x : B -> A),(2, \Gamma \vdash y : c -> D -> A) \]     |
-- +------------------------+----------------------------------------------------------------------------+
-- | asResults              | \[ A \]                                 |
-- +------------------------+----------------------------------------------------------------------------+
-- | forwarded              | results from forward inference          |
-- +------------------------+----------------------------------------------------------------------------+
-- | dAndaTrees             | [(Int,J.Tree A.AJudgement)]             |
-- |                        | Number of arguments that must be applied|
-- |                        | Proof trees for function part           |
-- +------------------------+----------------------------------------------------------------------------+
-- | asResults              | checking...                             |
-- +------------------------+----------------------------------------------------------------------------+
piElim :: B.DeduceRule 
piElim con aType depth setting 
  | otherwise = 
      let forwarded = F.forwardContext con
          dAndaTrees = arrowConclusionBs (map A.downSide $B.trees forwarded) aType
          asResults =  deduceEnvs con dAndaTrees depth setting{B.sStatus = (B.sStatus setting)}
          result =
            foldl
            (\oR (base,rss) -> 
              foldl
              (\ooR rs ->
                let status = foldl B.mergeStatus (B.rStatus ooR) (map B.rStatus rs) 
                    aSets = mapM B.trees rs
                    newTree = 
                      M.mapMaybe 
                      (\aSet ->
                        let 
                            args = map (\a -> let aj = A.downSide a in (A.termfromAJudgement aj)) aSet
                            aTerms = A.betaReduce $ foldl A.ArrowApp (A.termfromAJudgement $ A.downSide base) args
                            aType' =  substAsInPiElim (A.typefromAJudgement $A.downSide base) args
                        in  if (not $ null aSet ) && (aType' == aType) then Just (J.T J.PiE (A.AJudgement con aTerms aType') [base,head aSet]) else Nothing --表示の都合上引数を一個だけ扱いにしている                   
                      )
                      aSets
                in ooR{B.rStatus = status,B.trees =  newTree ++ B.trees ooR}
              )
              oR
              rss
            )
            B.resultDef{B.rStatus = B.sStatus setting} 
            asResults
      in B.debugLog con aType depth setting "piElim1"  result{B.rStatus = (B.rStatus result)}

-- | Implementation of type checking when a function application is in term
piElimTypeCheckApp  :: A.Context -> A.Arrowterm -> [A.Arrowterm] -> A.Arrowterm -> Int -> B.Setting -> B.Result
piElimTypeCheckApp con (A.ArrowApp f x) argLst b1 depth setting = 
  case  A.arrowNotat f of
  A.Conclusion (DT.Var fn) -> 
    let fType = A.shiftIndices (snd con !! fn) (fn+1) 0 in case fType of
      A.Arrow lst b -> 
        let d = length lst - length (x:argLst) in 
          if d < 0 then B.debugLogWithTerm con (A.ArrowApp f x) b1 depth setting "piElimtypecheck / 引数の数が多い" B.resultDef{B.rStatus = B.sStatus setting}
          else if b1 /= (if d == 0 then b else A.arrowNotat $A.Arrow (drop (length lst - d) lst) b) then B.debugLogWithTerm con (A.ArrowApp f x) b1 depth setting "piElimtypecheck 返り値の型が合わないv" B.resultDef{B.rStatus = B.sStatus setting}
            else 
              let typeLst = take (1 + length argLst) lst
                  xType = last typeLst
                  typeChecked = typecheck con x xType (depth) setting{B.sStatus = (B.sStatus setting)}
                  xTree = B.trees typeChecked
                  ftree = J.T J.VAR (A.AJudgement con  (A.betaReduce f) (A.Arrow lst b)) []
                  rType = substAsInPiElim (A.Arrow lst b) [x]
                  downside = A.AJudgement con (A.betaReduce $A.ArrowApp f x) rType
                  result = typeChecked{B.trees = [J.T J.PiE downside [ftree,head xTree]| not (null xTree)],B.rStatus = (B.sStatus setting)} --xについての木ができたらなんでもいいから一個だけにしてる
              in B.debugLogWithTerm con (A.ArrowApp f x) (rType) depth setting "piElimtypecheck1"  result
      c -> B.debugLogWithTerm con c b1 depth setting "piElimtypecheck / this function is not of pi-type" B.resultDef{B.rStatus = B.sStatus setting}
  A.Conclusion (DT.Con txt) -> 
    let fType = lookup txt (fst con) in case fType of
      Just (A.Arrow lst b) -> 
        let d = length lst - length (x:argLst) in 
          if  d < 0 then B.debugLogWithTerm con (A.ArrowApp f x) b1 depth setting "piElimtypecheck 引数の数が多い" B.resultDef{B.rStatus = B.sStatus setting}
          else if b1 /= (if d == 0 then b else A.arrowNotat $A.Arrow (drop (length lst - d) lst) b) then B.debugLogWithTerm con (A.ArrowApp f x) b1 depth setting "piElimtypecheck 返り値の型が合わない" B.resultDef{B.rStatus = B.sStatus setting}
            else 
              let typeLst = take (1 + length argLst) lst
                  xType =  last typeLst
                  typeChecked = typecheck con x xType (depth) setting{B.sStatus = (B.sStatus setting)}
                  xTree = B.trees typeChecked
                  ftree = J.T J.VAR (A.AJudgement con (A.betaReduce f) (A.Arrow lst b)) []
                  rType =  substAsInPiElim (A.Arrow lst b) [x]
                  downside = A.AJudgement con (A.betaReduce $ A.ArrowApp f x) rType
                  result = typeChecked{B.trees = [J.T J.PiE downside [ftree,head xTree]| not (null xTree)],B.rStatus = (B.sStatus setting)} --xについての木ができたらなんでもいいから一個だけにしてる
              in B.debugLogWithTerm con (A.ArrowApp f x) (rType) depth setting "piElimtypecheck3"  result
      _ -> B.debugLogWithTerm con (A.ArrowApp f x) b1 depth setting "piElimtypecheck / this function is not of pi-type" B.resultDef{B.rStatus = B.sStatus setting}
  A.ArrowApp f' y -> 
    let nested = piElimTypeCheckApp con (A.ArrowApp f' y) (x:argLst) b1 depth setting in
      case B.trees nested of
        [] -> B.debugLogWithTerm con (A.ArrowApp f x) b1 depth setting "piElimtypecheckApp 引数の方が一致しなかった" B.resultDef{B.rStatus = B.rStatus nested}
        (J.T J.PiE (A.AJudgement con te (A.Arrow lst b)) trs):_ ->  
          let typeLst = take (1 + length argLst) lst
              xType = last typeLst
              typeChecked = typecheck con x xType (depth + 1) setting{B.sStatus = (B.sStatus setting)}
              xTree = B.trees typeChecked
              rType = A.arrowNotat $ A.Arrow ((init typeLst) ++ drop (1 + length argLst) lst) b
              downside = A.AJudgement con (A.betaReduce $ A.ArrowApp f x) rType
              result = typeChecked{B.trees = [J.T J.PiE downside [J.T J.PiE (A.AJudgement con te (A.Arrow lst b)) trs,head xTree] | not (null xTree)],B.rStatus = (B.sStatus setting)}
          in
            B.debugLogWithTerm con (A.ArrowApp f x) rType depth setting "piElimtypecheck2"  result
        _ -> B.debugLogWithTerm con (A.ArrowApp f x) b1 depth setting "piElimtypecheckApp ここには来ないはず" B.resultDef{B.rStatus = B.rStatus nested}
  _ -> B.debugLogWithTerm con (A.ArrowApp f x) b1 depth setting "piElimtypecheckApp 引数を受け取る型でない" B.resultDef{B.rStatus = B.sStatus setting}
piElimTypeCheckApp con aTerm _ b1 depth setting = B.debugLogWithTerm con aTerm b1 depth setting "piElimtypecheckApp / No functin application in term" B.resultDef{B.rStatus = B.sStatus setting}

-- | case aType of A.ArrowApp _ (A.Conclusion _) -> True ;A.ArrowApp (A.Conclusion _) _ -> True ; _ -> False = undefined について考慮が必要かも
-- | haddock
eqElim :: B.DeduceRule 
eqElim con aType depth setting 
  | not $ any (A.canBeSame 0 (A.ArrowEq (A.aVar 0) (A.aVar 0) (A.aVar 0)) )   (snd con ++ map snd (fst con))  
    = B.debugLog con aType depth setting "eqElimハズレ"  B.resultDef{B.rStatus = B.sStatus setting}
  | aType == A.Conclusion  DT.Bot = B.debugLog con aType depth setting "eqElim bot"  B.resultDef{B.rStatus = B.sStatus setting}
  | case  aType of A.ArrowApp  (A.Conclusion _) _ -> True ;A.ArrowApp _ (A.Conclusion _) -> True ;(A.Conclusion _) -> True;A.ArrowEq _ _ (A.Conclusion _) -> True ; _ -> False = 
      let connum txt con' = (A.contextLen con')- 1 - (M.fromMaybe (A.contextLen con' - 1) $ lookup txt (zip (map fst $ reverse $ fst con') [0..]))
          (target,(deduceType,varnum)) = 
            case aType of 
              A.ArrowEq t x (A.Conclusion (DT.Var varnum')) -> (A.aVar varnum',(A.ArrowEq t x,varnum'))
              A.ArrowEq t x (A.Conclusion (DT.Con txt)) -> (A.aCon txt,(A.ArrowEq t x,connum txt con))
              A.Conclusion (DT.Var varnum')-> (aType,(id,varnum' ))
              A.Conclusion (DT.Con txt) -> (aType,(id,connum txt con))
              A.ArrowApp f (A.Conclusion (DT.Var varnum')) -> (A.aVar varnum',(A.ArrowApp f,varnum'))
              A.ArrowApp f (A.Conclusion (DT.Con txt)) -> (A.aCon txt,(A.ArrowApp f,connum txt con))
              A.ArrowApp (A.Conclusion (DT.Var varnum')) x -> (A.aVar varnum',(\g -> A.ArrowApp g x,varnum'))
              A.ArrowApp (A.Conclusion (DT.Con txt)) x -> (A.aCon txt,(\g -> A.ArrowApp g x,connum txt con))
              _ -> D.trace ("here : "++ show aType) undefined
          eqAboutDtTermLst' = 
            M.mapMaybe 
              (\(A.AJudgement _ te ty) ->
                case ty of
                  (A.ArrowEq _ var1 var2) -> 
                    if var1 == target then Just (te,var2)
                      else (if var2 == target then Just (te,var1) else Nothing)
                  _ -> Nothing
              ) 
              (map A.downSide $B.trees $ F.forwardContext con) 
          varR = foldl
            (\r (e,dTerm) ->
              let deduced = deduce con (deduceType dTerm) (depth + 1) setting{B.sStatus = B.rStatus r} 
                  trees = 
                      map  
                      (\dtermJ ->
                        J.T J.PiE
                        (A.AJudgement con 
                          (A.ArrowApp (A.ArrowApp (A.aCon "eqElim") e) (A.termfromAJudgement $A.downSide dtermJ)) aType)
                        [J.T J.CON(A.AJudgement con e (A.ArrowEq (A.aType) aType dTerm)) [],dtermJ]                               
                      ) 
                      (B.trees deduced)
              in deduced{B.trees = trees ++ B.trees r}
            ) 
            B.resultDef{B.rStatus = B.sStatus setting}
            eqAboutDtTermLst'
          result = varR
      in B.debugLog con aType depth setting "eqElim1" result
  |otherwise = B.debugLog con aType depth setting "eqElim2" B.resultDef{B.rStatus = B.sStatus setting}

nestdne :: (A.Context,B.AType) -> (A.Context,B.ATerm) -> Bool
nestdne (con1,aType1) (con2,aType2) =
  if A.contextLen con1 == (A.contextLen con2)+1
  then 
    case (aType2,con1,con2) of
      (A.Arrow [aType] (A.Conclusion DT.Bot),(sEnv1,vEnv1),(sEnv2,vEnv2)) ->  
          (A.sameTerm (con1,aType1) (con2,aType)) && 
            (con1 == (sEnv2,(A.Arrow [aType] (A.Conclusion DT.Bot)):vEnv2))
      _ -> False
  else False

dne :: B.DeduceRule 
dne con aType depth setting 
  | aType==A.aType =B.debugLog con aType depth setting "dneハズレ2"  B.resultDef{B.rStatus = B.sStatus setting}
  | case aType of (A.Arrow [A.Arrow _ (A.Conclusion DT.Bot)] (A.Conclusion DT.Bot)) -> True ; _ -> False=  
    B.debugLog con aType depth setting "dne2重"  B.resultDef{B.rStatus = B.sStatus setting}
  | otherwise =
    let typeChecked = B.debugLog con aType depth setting "dneが使えるか確認" (typecheck con aType A.aType depth setting) in 
    if null (B.trees typeChecked) then B.debugLog con aType depth setting "dneハズレ1"  typeChecked 
      else
        let deduced = deduce con 
              (A.Arrow [A.Arrow [aType] (A.Conclusion DT.Bot)] (A.Conclusion DT.Bot)) (depth + 1) 
              setting{B.sStatus = (B.rStatus typeChecked)}
            trees = 
              map 
              (\dTree -> let (A.AJudgement env aTerm a_type) = A.downSide dTree 
                in J.T J.SigE (A.AJudgement env (A.ArrowApp (A.aCon $T.pack " dne ") aTerm) aType) [dTree]) 
              (B.trees deduced)
            deducedStatus = B.rStatus deduced
            status = deducedStatus{B.deduceNgLst = tail $B.deduceNgLst deducedStatus}
        in  deduced{B.trees = trees,B.rStatus = status}

efq :: B.DeduceRule 
efq con aType depth setting = undefined
{- すぐには使わないから後回し
efq :: A.Context -> A.Arrowterm -> Int -> Setting ->Either String [J.Tree A.AJudgement]
efq con b depth setting
  | depth > maxdepth setting = debugLog con b depth setting "efqハズレ1" $ Left  $ "too deep @ efq " ++ show con ++" ト "++ show b
  | null (withLog' typecheck con b (A.Conclusion DT.Type) {-表示の際は depth+1-}(depth) setting ) = debugLog con b depth setting "efqハズレ2"$  Right []
  | otherwise =
    if null (withLog membership con (A.Conclusion DT.Bot) depth setting)
    then
      case deduceWithLog con (A.Conclusion DT.Bot) (depth + 1) setting of
          [] -> debugLog con b depth setting "efq1" $Right []
          botJs -> debugLog con b depth setting "efq2"  $
            Right
              $map
              (\dTree -> let (A.AJudgement env aTerm a_type) = A.downSide dTree in J.T J.SigE (A.AJudgement env (A.ArrowApp (A.Conclusion $ DT.Con $T.pack "efq") aTerm) b) [dTree])---(\dTree -> let (A.AJudgement env aTerm a_type) = A.downSide dTree in J.NotF (A.AJudgement env (A.ArrowApp (A.Conclusion $ DT.Con $T.pack "efq") aTerm) b) dTree)
              botJs
    else debugLog con b depth setting "efq3"  $
      Right
        $map
        (\dTree -> let (A.AJudgement env aTerm a_type) = A.downSide dTree in J.T J.SigE (A.AJudgement env (A.ArrowApp (A.Conclusion $ DT.Con $T.pack "efq") aTerm) b) [dTree])
        (withLog membership con (A.Conclusion DT.Bot) depth setting)
-}
piForm :: B.TypecheckRule
piForm con aTerm aType depth setting
  | aType `notElem` [A.aType,A.Conclusion DT.Kind] = B.debugLogWithTerm con aTerm aType depth setting "piFormハズレ1" B.resultDef{B.rStatus = B.sStatus setting}
  | otherwise = case (con,A.arrowNotat aTerm) of 
    ((sigCon,varCon),A.Arrow as b )-> 
      let a = last as
          aTypeChecked = 
            foldl 
            (\r dtTerm -> 
              let newR = typecheck con a (A.Conclusion dtTerm) depth setting{B.sStatus = B.rStatus r}
              in newR{B.trees = B.trees newR ++ B.trees r}
            )
             B.resultDef{B.rStatus = B.sStatus setting} 
             [DT.Type,DT.Kind]
          result = 
            if null (B.trees aTypeChecked) then  B.resultDef{B.rStatus = B.sStatus setting}
              else 
                let bEnv = (sigCon,a:varCon)
                    bJs = typecheck bEnv (A.arrowNotat $A.Arrow (init as) b) aType depth setting{B.sStatus = B.rStatus aTypeChecked}
                    treeAB = zip (concatMap (replicate (length $B.trees bJs)) $B.trees aTypeChecked) (cycle (B.trees bJs))
                    trees = 
                      map 
                      (\(aTree,bTree) -> 
                        let x = A.downSide bTree 
                        in J.T J.PiF (A.AJudgement con (A.Arrow as b) (A.typefromAJudgement x)) [aTree,bTree] )
                      treeAB
                in bJs{B.trees = trees}
      in B.debugLogWithTerm con (A.Arrow as b) aType depth setting "piForm1" result
    _ -> B.debugLogWithTerm con aTerm aType depth setting "piFormハズレ" B.resultDef{B.rStatus = B.sStatus setting}

-- | sigma-formation
--
-- +------------------------+------------+----------+----------+
-- | Header row, column 1   | Header 2   | Header 3 | Header 4 |
-- | (possible message)     |            |          |          |
-- +========================+============+==========+==========+
-- | sigmaForm hazure       | column 2   | column 3 | column 4 |
-- +------------------------+------------+----------+----------+
-- | body row 2             | Cells may span columns.          |
-- +------------------------+------------+---------------------+
-- | body row 3             | Cells may  | \[                  |
-- +------------------------+ span rows. | f(n) = \sum_{i=1}   |
-- | body row 4             |            | \]                  |
-- +------------------------+------------+---------------------+
sigmaForm :: B.TypecheckRule
sigmaForm con aTerm aType depth setting
  | aType `notElem` [A.aType,A.Conclusion DT.Kind] = B.debugLogWithTerm con aTerm aType depth setting "sigmaFormハズレ1" B.resultDef{B.rStatus = B.sStatus setting}
  | otherwise = case (con,A.arrowNotat aTerm) of 
    ((sigCon,varCon),A.ArrowSigma' as b) -> 
      let a = last as
          aTypeChecked = 
            foldl 
            (\r dtTerm -> 
              if a == A.aType && dtTerm == DT.Type
                then
                  r
                else
                  let newR = typecheck con a (A.Conclusion dtTerm) {-表示の際は depth+1-}depth setting{B.sStatus = B.rStatus r}
                  in newR{B.trees = B.trees newR ++ B.trees r}
            )
             B.resultDef{B.rStatus = B.sStatus setting} 
             [DT.Type,DT.Kind]
          result = 
            if null (B.trees aTypeChecked) then  B.resultDef{B.rStatus = B.sStatus setting}
              else 
                let bEnv = (sigCon,a:varCon)
                    bJs = typecheck bEnv (A.arrowNotat $A.ArrowSigma' (init as) b) aType depth setting{B.sStatus = B.rStatus aTypeChecked}
                    treeAB = zip (concatMap (replicate (length $B.trees bJs)) $B.trees aTypeChecked) (cycle (B.trees bJs))
                    trees = 
                      map 
                      (\(aTree,bTree) -> 
                        let x = A.downSide bTree 
                        in J.T J.SigF (A.AJudgement con (A.ArrowSigma' as b) (A.typefromAJudgement x)) [aTree,bTree] )
                      treeAB
                in bJs{B.trees = trees}
      in B.debugLogWithTerm con (A.ArrowSigma' as b) aType  depth setting "sigmaForm2" result        
    _ -> B.debugLogWithTerm con aTerm aType depth setting "sigmaでない@sigmaFormハズレ" B.resultDef{B.rStatus = B.sStatus setting}

eqForm :: B.TypecheckRule
eqForm con aTerm aType depth setting
  | aType `notElem` [A.aType,A.Conclusion DT.Kind] = B.debugLogWithTerm con aTerm aType depth setting "eqFormハズレ1" B.resultDef{B.rStatus = B.sStatus setting}
  | otherwise = case (con,A.arrowNotat aTerm) of 
    ((sigCon,varCon),A.ArrowEq t a b )-> 
      let tTypeChecked = typecheck con t A.aType depth setting 
          abTypeChecked = 
            foldl 
            (\(r:rr) gterm ->
              case B.trees r of
                [] -> (r:rr)
                _ -> (typecheck con gterm t depth setting{B.sStatus = B.rStatus r}):(r:rr)
            )
            [tTypeChecked]
            [a,b]
          result = case abTypeChecked of
            [tRes,aRes,bRes] ->
              if any (null . B.trees) abTypeChecked 
                then  B.resultDef{B.rStatus = B.sStatus setting}
                else 
                  let treeABT = sequence $ map B.trees abTypeChecked
                      trees = 
                        map 
                        (\[tTree,aTree,bTree] -> J.T J.EqF (A.AJudgement con aTerm (aType)) [tTree,aTree,bTree] )
                        treeABT
                  in bRes{B.trees = trees}
            _ -> B.resultDef{B.rStatus = B.sStatus setting}
      in B.debugLogWithTerm con aTerm aType depth setting "eqForm1" result
    _ -> B.debugLogWithTerm con aTerm aType depth setting "eqFormハズレ" B.resultDef{B.rStatus = B.sStatus setting}


sigmaIntro :: B.DeduceRule 
sigmaIntro con aType depth setting = --B.debugLog con aType depth setting "sigmaIntroパス" B.resultDef{B.rStatus = B.sStatus setting}{-}
  case A.arrowNotat aType of 
  A.ArrowSigma' as b1 -> 
    let parentLsts = reverse $ zipWith (\term num -> (num,A.varsInaTerm term)) (reverse (b1:as)) [0..] 
        childrenLsts = map (\l -> map (\(num,l) -> if num `elem` l then num  else -1) (zip [0..] $reverse l)) $L.inits $map snd parentLsts
        (_,result') = --D.trace ("parentLsts : " ++ show parentLsts)
            foldr 
              (\((argId,parentLst),childrenLst) (b1':as',deducedLsts)-> 
                let substLsts = -- D.trace ("sigmaIntroで見ている部分@589 : " ++ show b1'  ++ " deducedLsts " ++ (show $ map (map (map (A.varsInaTerm . A.termfromAJudgement. A.downSide) . B.trees)) deducedLsts) ++ " parentLst : " ++ show parentLst ++"argid : "++show argId)
                      map
                      (\deducedLst -> --D.trace ("sigmaIntroで見ている部分 : " ++ show b1'{-(A.shiftIndices b1' 1 0)-} ++ " 親リスト : "++ (show $filter (\num -> 0<= num && num < length deducedLst) parentLst))
                        (deducedLst,sequence$  
                        map 
                          (\num -> 
                            (zip [num,num..] (let chosen = deducedLst !! num in map (\t -> chosen{B.trees = [t]}) (B.trees chosen) ))) 
                          (filter (\num -> 0<= num && num < length deducedLst) parentLst)))
                      deducedLsts;
                    clueLst' =map (\num -> A.shiftIndices (A.arrowNotat $A.arrowSubst (as' !! num) (A.aVar (-1)) (A.aVar num)) 1 (-1)) $ filter (-1 <) childrenLst;
                    deducedLstaTypeAndClues = 
                      concatMap 
                        (\(deducedLst,substLst) -> 
                            map
                              (\substNote ->
                                let (dLst,aType') = 
                                      foldr 
                                        (\(beforeVarNum,afterResult) (deducedLst',t) -> --D.trace ("beforeVarNum : " ++ (show beforeVarNum) ++ " t : " ++ show (A.varsInaTerm t) ++ " 代入内容 : "++show (A.shiftIndices (A.termfromAJudgement $ A.downSide $head $B.trees afterResult) (argId) 0) ++ " 代入先 "　++ (show $ beforeVarNum)　
                                             -- ++ " 結果 : " ++ (show $ A.arrowSubst t (A.Conclusion $ DT.Con "here"{-A.termfromAJudgement $ A.downSide $head $B.trees afterResult-}) (A.aVar ( beforeVarNum {-- argId-})))
                                            --)
                                          ((take beforeVarNum deducedLst') ++ (afterResult:(drop (beforeVarNum + 1) deducedLst')), 
                                            A.betaReduce $ A.arrowSubst t (A.shiftIndices (A.termfromAJudgement $ A.downSide $head $B.trees afterResult) (argId) 0) (A.aVar (beforeVarNum)))
                                        )
                                        (deducedLst,b1')
                                        (reverse substNote)
                                in --D.trace ("sigmaIntroで見ている部分@599 : " ++ show (A.varsInaTerm b1') ++ " aType' " ++ (show (A.shiftIndices aType' (-argId) 0)))$
                                  ((dLst,A.shiftIndices aType' (-argId) 0),map (A.Arrow [aType']) clueLst')
                              )
                              substLst
                        )
                        substLsts;            
                    deducedLsts' =  filter (\dLst -> not $ null dLst) $
                      map 
                        (\((dLst,aType'),clueLst) -> 
                          let deduced = deduce con aType' (depth + 1) setting{B.sStatus = (B.rStatus (head dLst)){B.allProof = (length as')>0}};
                          in
                            if null (B.trees deduced) then [] else (if null clueLst then deduced{B.trees =[head $ B.trees deduced]} else deduced){B.rStatus = (B.rStatus deduced){B.allProof = B.allProof $B.sStatus setting}}:dLst)
                        deducedLstaTypeAndClues
                in (as',deducedLsts')) 
              ((reverse $ b1:as),[[B.resultDef{B.rStatus = B.sStatus setting}]]) 
              (zip parentLsts childrenLsts)
        statusArgTreesLst =  map (\rs -> (B.rStatus $ head rs,sequence $ init $map B.trees rs)) result'
        trees = 
          concatMap 
            (\(_,argTreesLst) ->
              map 
              (\argTrees -> 
                let upside = argTrees; 
                    h:t = map (A.termfromAJudgement . A.downSide) argTrees;
                    term = foldr (\a b -> A.ArrowPair a b) h (reverse t);
                in J.T J.SigI (A.AJudgement con term aType) upside )
              argTreesLst
            )
            statusArgTreesLst
        status = foldr (\s s' -> B.mergeStatus s s') (B.sStatus B.settingDef) (map fst statusArgTreesLst) 
        result = B.resultDef{B.rStatus = status, B.trees = trees}
    in B.debugLog con (A.ArrowSigma' as b1) depth setting "sigmaIntro" result
  _ -> B.debugLog con aType depth setting "sigmaIntroハズレ" B.resultDef{B.rStatus = B.sStatus setting}
