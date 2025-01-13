{-# LANGUAGE OverloadedStrings #-}

module DTS.Prover.Wani.Backward (
    deduce
) where

import qualified DTS.DTTdeBruijn as DdB
import qualified DTS.Prover.Wani.Arrowterm as A -- Aterm
import qualified DTS.Prover.Wani.Forward as F  --Forward Inference
import qualified Interface.Tree as UDT
import qualified DTS.QueryTypes as QT

import qualified Data.List as L -- List
import qualified Debug.Trace as D -- Trace
import qualified Data.Text.Lazy as T -- Text
import qualified Data.Maybe as M -- Maebe

import qualified DTS.Prover.Wani.WaniBase as B

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


nestdne :: (A.Context,B.AType) -> (A.Context,B.ATerm) -> Bool
nestdne (con1,aType1) (con2,aType2) =
  if A.contextLen con1 == (A.contextLen con2)+1
  then 
    case (aType2,con1,con2) of
      (A.Arrow [aType] (A.Conclusion DdB.Bot),(sEnv1,vEnv1),(sEnv2,vEnv2)) ->  
          (A.sameTerm (con1,aType1) (con2,aType)) && 
            (con1 == (sEnv2,(A.Arrow [aType] (A.Conclusion DdB.Bot)):vEnv2))
      _ -> False
  else False

-- | Execute proof search.
deduce :: B.DeduceRule
deduce sig var arrowType depth setting 
  | depth > B.maxdepth setting =
      B.resultDef{B.errMsg = "depth @ deduce",B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}} -- Set `B.rStatus` to update the maximum depth used.
  | any (\(con',aType')->A.contextLen (sig,var) == (A.contextLen con')&&A.sameCon (sig,var) con'&& A.sameTerm ((sig,var),arrowType) (con',aType')) (B.deduceNgLst (B.sStatus setting)) = 
      B.debugLog (sig,var) arrowType depth setting "Avoid endless loops."  (B.resultDef{B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}})
  | otherwise = 
    case arrowType of
      A.Conclusion DdB.Kind -> -- The only term for `kind` is `type`.
        B.resultDef{
          B.trees = [UDT.Tree QT.Var (A.AJudgment sig var (A.aType) (A.Conclusion DdB.Kind)) []],
          B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}}
      _ -> 
        let result' = foldl -- If certain conditions are met, the proof may be rounded up without moving on to the next proof search.
              (\rs f -> 
                if (B.allProof (B.sStatus setting)) || (null (B.trees rs)) 
                then 
                  let result = f sig var (A.arrowNotat arrowType) depth setting{B.sStatus = B.rStatus rs}
                  in B.mergeResult rs result -- `B.mergeResult` updates the status of a newly executed proof search
                else rs)

              (B.resultDef{
                  B.rStatus = 
                    B.mergeStatus 
                      (B.sStatus setting) 
                      (B.statusDef{B.usedMaxDepth = depth,B.deduceNgLst = ((sig,var),arrowType) : (B.deduceNgLst $B.sStatus setting)})} ) 
                      -- Currently, `arrowType` proof search is performed under environment `con`, and to prevent infinite loops, it is set to round up when `arrowType` proof search is needed under environment `con`(★).
              ( -- The stronger the rule, the later the timing of application is turned back. For example, `dne` can be used for any term, thus turning the execution later. This setting takes effect in combination with the rounding up of proof search using `B.allProof`.
                [membership',piIntro',sigmaIntro',piElim'] 
                  ++ [dne' | arrowType /= A.Conclusion DdB.Bot && B.mode setting == B.WithDNE]
                  ++ [efq' | arrowType /= A.Conclusion DdB.Bot && B.mode setting == B.WithEFQ]
              )
            result = result'{B.rStatus = (B.rStatus result'){B.deduceNgLst = B.deduceNgLst$B.sStatus setting}}{B.trees = L.nub$B.trees result'} -- excludes duplicate proof trees, and restore deduceNgList to its original state from ★ state
        in 
          if null (B.trees result) -- for debug
            then
              (if B.debug setting then B.debugLog (sig,var) arrowType depth setting "deduce failed " else id) result
            else
              (if B.debug setting then D.trace (L.replicate (2*depth) ' ' ++  show depth ++ " deduced:  " ++ show (map A.downSide' (B.trees result))) else id) result

-- | Execute typecheck
typecheck' :: B.TypecheckRule
typecheck' sig var arrowTerm arrowType depth setting
  | depth > B.maxdepth setting = B.resultDef{B.errMsg = "depth @ typecheck",B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}}
  | any (\(con',aTerm',aType')->A.sameCon (sig,var) con'&& A.sameTerm ((sig,var),arrowTerm) (con',aTerm') && A.sameTerm ((sig,var),arrowType) (con',aType')) (B.failedlst (B.sStatus setting)) = B.debugLogWithTerm (sig,var) arrowTerm arrowType depth setting "Failed in the past."  (B.resultDef{B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}})
  | B.falsum setting && arrowTerm == A.Conclusion DdB.Bot && arrowType == A.aType = B.resultDef{B.trees = [UDT.Tree QT.Var (A.AJudgment sig var arrowTerm arrowType) []],B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}} -- if `B.falsum` is true, the type for `false` is `type`.
  | arrowTerm == A.Conclusion DdB.Kind = B.debugLogWithTerm (sig,var) arrowTerm arrowType depth setting "kind cannot be a term."  B.resultDef{B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}}
  | arrowType == A.Conclusion DdB.Kind = if arrowTerm == A.aType then B.resultDef{B.trees = [UDT.Tree QT.Con (A.AJudgment sig var arrowTerm arrowType) []],B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}} else B.resultDef{B.rStatus = B.mergeStatus (B.sStatus setting) B.statusDef{B.usedMaxDepth = depth}}
  | otherwise = B.debugLogWithTerm (sig,var) arrowTerm arrowType depth setting "typeCheck" $
      let arrowType' = A.arrowNotat arrowType
          formResult = 
            foldl  -- `typecheck` exits when one term-type pair is found.
            (\r f -> if null (B.trees r) then f sig var arrowTerm arrowType' depth setting{B.sStatus = B.rStatus r} else r)
            B.resultDef{B.rStatus = B.mergeStatus (B.sStatus setting) (B.statusDef{B.usedMaxDepth = depth})}
            [piForm',sigmaForm',eqForm']
          piElimResult = if null (B.trees formResult) then piElimTypeCheckApp' sig var arrowTerm [] arrowType' depth setting{B.sStatus = B.rStatus formResult} else formResult -- It is complicated to handle when there is a function application in a term, so it is separately handled by `piElimTypeCheckApp`.
          result = foldl --  looking for a proof tree obtained by proof search that matches the proof term.
            (\r f -> 
              let r' = r{B.trees = filter (\a ->A.sameTerm (A.envfromAJudgment $ A.downSide' a,A.termfromAJudgment (A.downSide' a)) ((sig,var),A.arrowNotat arrowTerm)) (B.trees r)}
              in 
              if null (B.trees r') then  f sig var arrowType' depth setting{B.sStatus = B.rStatus r'} else r')
            piElimResult
            (
              [membership',piIntro',sigmaIntro'] 
                ++ [dne' | arrowType /= A.Conclusion DdB.Bot && B.mode setting == B.WithDNE] 
                ++ [efq' | arrowType /= A.Conclusion DdB.Bot && B.mode setting == B.WithEFQ]
            )
          resultStatus = B.rStatus result
          failedlst = if null (B.trees result) then ((sig,var),arrowTerm,arrowType):(B.failedlst resultStatus) else B.failedlst resultStatus
          status = resultStatus{B.failedlst =failedlst}
      in 
        (if B.debug setting then D.trace (L.replicate (2*depth) ' ' ++  show depth ++ " termFound :  " ++ show (map A.downSide' $L.nub $ B.trees result)) else id) result{B.rStatus = status}{B.trees = L.nub $B.trees result}

-- | This function refers to the results of forward inference using sigmaElim and eqElim and returns a list of proof trees with matching type parts.
membership' :: B.DeduceRule
membership' sig var aType depth setting = 
  let forwardResult = F.forwardContext sig var
      forwardTrees = B.trees forwardResult
      matchLst =  L.nub $
        filter 
          (\xTree -> 
            let x = A.downSide' xTree 
                xCon =A.envfromAJudgment x
                xType =   A.typefromAJudgment x
            in 
                  A.sameTerm ((sig,var),aType) (xCon,xType))
            $ forwardTrees  
  in B.debugLog (sig,var) aType depth setting "membership" (forwardResult{B.trees = matchLst,B.rStatus = B.sStatus setting})

piIntro' :: B.DeduceRule 
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
piIntro' sig var aType depth setting = 
  case aType of
    A.Arrow a b -> 
      let bTrees = deduce sig (a ++ var) b (depth+1) setting{B.sStatus = (B.sStatus setting)}
          typeChecked = 
            foldl 
            (\r dtTerm -> 
              let newR = typecheck' sig var aType (A.Conclusion dtTerm) depth setting{B.sStatus = (B.rStatus r)}
              in newR{B.trees = B.trees newR ++ B.trees r}
            )
             B.resultDef{B.rStatus = B.sStatus setting} 
             [DdB.Type,DdB.Kind]
          tbTreePairs = zip (concatMap (replicate (length (B.trees bTrees))) (B.trees typeChecked)) (cycle $B.trees bTrees)
          piABTrees = map 
            (\(tTree,bTree) -> 
              let aTerm = A.betaReduce $ A.addLam (length a) (A.termfromAJudgment $ A.downSide' bTree) in 
                UDT.Tree QT.PiI (A.AJudgment sig var aTerm aType) [tTree,bTree]
              )
            tbTreePairs
          result = bTrees{B.rStatus = (B.mergeStatus (B.rStatus bTrees) (B.rStatus typeChecked)),B.trees = piABTrees} 
      in B.debugLog (sig,var) aType depth setting "piIntro1"  result
    _ -> B.debugLog (sig,var) aType depth setting "piIntro2"  B.resultDef{B.rStatus = B.sStatus setting}


arrowConclusionBs' :: [UDT.Tree A.Arrowrule A.AJudgment] -> A.Arrowterm -> [(Int,UDT.Tree A.Arrowrule A.AJudgment)]
-- | arrowConclusionBs (Would the input be appropriate? Why this function uses not [J.Tree A.AJudgement] but [A.AJudgement]?)
-- +------------------------+--------------------------------------------------------------+
-- | input                  | \[ \Gamma \vdash a : A -> B, \Gamma \vdash a : A -> C \], B  |
-- +========================+==============================================================+
-- | output                 | \[(1,tree for \Gamma \vdash a : A -> B )\]                   |
-- +------------------------+--------------------------------------------------------------+
arrowConclusionBs' trees b= 
  let arrowConclusionB t b = (tailIsB (A.typefromAJudgment $ A.downSide' t) b,t) in
    map
    (\((num ,b),t )-> (num,t))
    $filter
      (snd . fst)
      $map (`arrowConclusionB` b) trees

deduceEnv' :: A.SAEnv -> A.AEnv -> (Int,UDT.Tree A.Arrowrule A.AJudgment) -> Int -> B.Setting -> (UDT.Tree A.Arrowrule A.AJudgment,[[B.Result]])

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

deduceEnv'  sEnv vEnv  (num,aJudgment) depth setting= 
  case  A.arrowNotat $A.typefromAJudgment $A.downSide' aJudgment of
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
                                                  A.betaReduce $ A.arrowSubst t (A.shiftIndices (A.termfromAJudgment $ A.downSide' $head $B.trees afterResult) (argId) 0) (A.aVar (beforeVarNum)))
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
                                let deduced = deduce sEnv vEnv aType' (depth + 1) setting{B.sStatus = (B.rStatus (head dLst)){B.allProof = True && (length as')>0}};
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
                  deduce sEnv (e++vEnv) f (depth+1) setting{B.sStatus = (B.rStatus r)}:(r:rs)
              )
              [B.resultDef{B.rStatus = (B.sStatus setting){B.allProof = True}}]
              deduceTargetAndCons
            lastStatus = (B.rStatus $head proofForEnv){B.allProof = B.allProof $B.sStatus setting}
        in (aJudgment,map init result')
    _ -> (aJudgment, [])


deduceEnvs' :: A.SAEnv -> A.AEnv -> [(Int,UDT.Tree A.Arrowrule A.AJudgment)] -> Int -> B.Setting -> [(UDT.Tree A.Arrowrule A.AJudgment,[[B.Result]])]
-- | deduceEnvs
-- +------------------------+----------------------------------------------------------------------------+
-- | input                  | \Gamma , [(1, \Gamma \vdash x : B -> A)]                                   |
-- +========================+============================================================================+
-- | output                 | tp be updated     |
-- +------------------------+----------------------------------------------------------------------------+
deduceEnvs' sig var aJudgments depth setting=
  let ajudges = map (\aj ->deduceEnv' sig var aj depth setting) aJudgments
  in filter (\(t,r)-> not $null r) ajudges

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
piElim' :: B.DeduceRule
piElim' sig var aType depth setting
  | otherwise = 
      let forwarded = F.forwardContext sig var
          dAndaTrees = arrowConclusionBs' (B.trees forwarded) aType
          asResults =  deduceEnvs' sig var dAndaTrees depth setting{B.sStatus = (B.sStatus setting)}
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
                            args = map (\a -> let aj = A.downSide' a in (A.termfromAJudgment aj)) aSet
                            aTerms = A.betaReduce $ foldl A.ArrowApp (A.termfromAJudgment $ A.downSide' base) args
                            aType' =  substAsInPiElim (A.typefromAJudgment $A.downSide' base) args
                        in  if (not $ null aSet ) && (aType' == aType) then Just (UDT.Tree QT.PiE (A.AJudgment sig var aTerms aType') [base,head aSet]) else Nothing --表示の都合上引数を一個だけ扱いにしている                   
                      )
                      aSets
                in ooR{B.rStatus = status,B.trees =  newTree ++ B.trees ooR}
              )
              oR
              rss
            )
            B.resultDef{B.rStatus = B.sStatus setting} 
            asResults
      in B.debugLog (sig,var) aType depth setting "piElim1"  result{B.rStatus = (B.rStatus result)}

-- | Implementation of type checking when a function application is in term
piElimTypeCheckApp'  :: A.SAEnv -> A.AEnv -> A.Arrowterm -> [A.Arrowterm] -> A.Arrowterm -> Int -> B.Setting -> B.Result
piElimTypeCheckApp' sig var (A.ArrowApp f x) argLst b1 depth setting = 
  case  A.arrowNotat f of
  A.Conclusion (DdB.Var fn) -> 
    let fType = A.shiftIndices (var !! fn) (fn+1) 0 in case fType of
      A.Arrow lst b ->
        let d = length lst - length (x:argLst) in 
          if d < 0 then B.debugLogWithTerm (sig,var) (A.ArrowApp f x) b1 depth setting "piElimtypecheck / 引数の数が多い" B.resultDef{B.rStatus = B.sStatus setting}
          else if b1 /= (if d == 0 then b else A.arrowNotat $A.Arrow (drop (length lst - d) lst) b) then B.debugLogWithTerm (sig,var) (A.ArrowApp f x) b1 depth setting "piElimtypecheck 返り値の型が合わないv" B.resultDef{B.rStatus = B.sStatus setting}
            else 
              let typeLst = take (1 + length argLst) lst
                  xType = last typeLst
                  typeChecked = typecheck' sig var x xType depth setting{B.sStatus = (B.sStatus setting)}
                  xTree = B.trees typeChecked
                  ftree = UDT.Tree QT.Var (A.AJudgment sig var (A.betaReduce f) (A.Arrow lst b)) []
                  rType = substAsInPiElim (A.Arrow lst b) [x]
                  downside = A.AJudgment sig var (A.betaReduce $A.ArrowApp f x) rType
                  result = typeChecked{B.trees = [UDT.Tree QT.PiE downside [ftree,head xTree]| not (null xTree)],B.rStatus = (B.sStatus setting)} --xについての木ができたらなんでもいいから一個だけにしてる
              in B.debugLogWithTerm (sig,var) (A.ArrowApp f x) (rType) depth setting "piElimtypecheck1"  result
      c -> B.debugLogWithTerm (sig,var) c b1 depth setting "piElimtypecheck / this function is not of pi-type" B.resultDef{B.rStatus = B.sStatus setting}
  A.Conclusion (DdB.Con txt) ->
    let fType = lookup txt sig in case fType of
      Just (A.Arrow lst b) ->
        let d = length lst - length (x:argLst) in 
          if  d < 0 then B.debugLogWithTerm (sig,var) (A.ArrowApp f x) b1 depth setting "piElimtypecheck 引数の数が多い" B.resultDef{B.rStatus = B.sStatus setting}
          else if b1 /= (if d == 0 then b else A.arrowNotat $A.Arrow (drop (length lst - d) lst) b) then B.debugLogWithTerm (sig,var) (A.ArrowApp f x) b1 depth setting "piElimtypecheck 返り値の型が合わない" B.resultDef{B.rStatus = B.sStatus setting}
            else
              let typeLst = take (1 + length argLst) lst
                  xType =  last typeLst
                  typeChecked = typecheck' sig var x xType (depth) setting{B.sStatus = (B.sStatus setting)}
                  xTree = B.trees typeChecked
                  ftree = UDT.Tree QT.Var (A.AJudgment sig var (A.betaReduce f) (A.Arrow lst b)) []
                  rType =  substAsInPiElim (A.Arrow lst b) [x]
                  downside = A.AJudgment sig var (A.betaReduce $ A.ArrowApp f x) rType
                  result = typeChecked{B.trees = [UDT.Tree QT.PiE downside [ftree,head xTree]| not (null xTree)],B.rStatus = (B.sStatus setting)} --xについての木ができたらなんでもいいから一個だけにしてる
              in B.debugLogWithTerm (sig,var) (A.ArrowApp f x) (rType) depth setting "piElimtypecheck3"  result
      _ -> B.debugLogWithTerm (sig,var) (A.ArrowApp f x) b1 depth setting "piElimtypecheck / this function is not of pi-type" B.resultDef{B.rStatus = B.sStatus setting}
  A.ArrowApp f' y ->
    let nested = piElimTypeCheckApp' sig var (A.ArrowApp f' y) (x:argLst) b1 depth setting in
      case B.trees nested of
        [] -> B.debugLogWithTerm (sig,var) (A.ArrowApp f x) b1 depth setting "piElimtypecheckApp 引数の方が一致しなかった" B.resultDef{B.rStatus = B.rStatus nested}
        (UDT.Tree QT.PiE (A.AJudgment sig var te (A.Arrow lst b)) trs):_ ->
          let typeLst = take (1 + length argLst) lst
              xType = last typeLst
              typeChecked = typecheck' sig var x xType (depth + 1) setting{B.sStatus = (B.sStatus setting)}
              xTree = B.trees typeChecked
              rType = A.arrowNotat $ A.Arrow ((init typeLst) ++ drop (1 + length argLst) lst) b
              downside = A.AJudgment sig var (A.betaReduce $ A.ArrowApp f x) rType
              result = typeChecked{B.trees = [UDT.Tree QT.PiE downside [UDT.Tree QT.PiE (A.AJudgment sig var te (A.Arrow lst b)) trs,head xTree] | not (null xTree)],B.rStatus = (B.sStatus setting)}
          in
            B.debugLogWithTerm (sig,var) (A.ArrowApp f x) rType depth setting "piElimtypecheck2"  result
        _ -> B.debugLogWithTerm (sig,var) (A.ArrowApp f x) b1 depth setting "piElimtypecheckApp ここには来ないはず" B.resultDef{B.rStatus = B.rStatus nested}
  _ -> B.debugLogWithTerm (sig,var) (A.ArrowApp f x) b1 depth setting "piElimtypecheckApp 引数を受け取る型でない" B.resultDef{B.rStatus = B.sStatus setting}
piElimTypeCheckApp' sig var aTerm _ b1 depth setting = B.debugLogWithTerm (sig,var) aTerm b1 depth setting "piElimtypecheckApp / No functin application in term" B.resultDef{B.rStatus = B.sStatus setting}

dne' :: B.DeduceRule
dne' sig var aType depth setting
  | aType==A.aType =B.debugLog (sig,var) aType depth setting "dneハズレ2"  B.resultDef{B.rStatus = B.sStatus setting}
  | case aType of (A.Arrow [A.Arrow _ (A.Conclusion DdB.Bot)] (A.Conclusion DdB.Bot)) -> True ; _ -> False=  
    B.debugLog (sig,var) aType depth setting "dne2重"  B.resultDef{B.rStatus = B.sStatus setting}
  | otherwise = 
    let typeChecked = B.debugLog (sig,var) aType depth setting "dneが使えるか確認" (typecheck' sig var aType A.aType depth setting) in 
    if null (B.trees typeChecked) then B.debugLog (sig,var) aType depth setting "dneハズレ1"  typeChecked 
      else 
        let deduced = deduce sig var 
              (A.Arrow [A.Arrow [aType] (A.Conclusion DdB.Bot)] (A.Conclusion DdB.Bot)) (depth + 1) 
              setting{B.sStatus = (B.rStatus typeChecked)}
            trees = 
              map 
              (\dTree -> let (A.AJudgment sig_d var_d aTerm a_type) = A.downSide' dTree 
                in UDT.Tree QT.SigmaE (A.AJudgment sig_d var_d (A.ArrowApp (A.aCon $T.pack " dne ") aTerm) aType) [dTree]) 
              (B.trees deduced)
            deducedStatus = B.rStatus deduced
            status = deducedStatus{B.deduceNgLst = tail $B.deduceNgLst deducedStatus}
        in  deduced{B.trees = trees,B.rStatus = status}

efq' :: B.DeduceRule
efq' sig var aType depth setting
  | aType == (A.Conclusion DdB.Bot) =  
    B.debugLog (sig,var) aType depth setting "efq2重"  B.resultDef{B.rStatus = B.sStatus setting}
  | otherwise = 
      let deduced = deduce sig var (A.Conclusion DdB.Bot) (depth + 1) setting
          trees = 
              map 
              (\dTree -> let (A.AJudgment sig_d var_d aTerm a_type) = A.downSide' dTree 
                in UDT.Tree QT.SigmaE (A.AJudgment sig_d var_d (A.ArrowApp (A.aCon $T.pack " efq ") aTerm) aType) [dTree]) 
              (B.trees deduced)
          status = B.rStatus deduced
      in  deduced{B.trees = trees}

piForm' :: B.TypecheckRule
piForm' sig var aTerm aType depth setting
  -- | aType `notElem` [A.aType,A.Conclusion DdB.Kind] = B.debugLogWithTerm con aTerm aType depth setting "piFormハズレ1" B.resultDef{B.rStatus = B.sStatus setting}
  | otherwise = case A.arrowNotat aTerm of 
    A.Arrow as b ->
      let a = last as
          aTypeChecked = 
            foldl 
            (\r dtTerm -> 
              let newR = typecheck' sig var a (A.Conclusion dtTerm) depth setting{B.sStatus = B.rStatus r}
              in newR{B.trees = B.trees newR ++ B.trees r}
            )
             B.resultDef{B.rStatus = B.sStatus setting} 
             [DdB.Type,DdB.Kind]
          result = 
            if null (B.trees aTypeChecked) then  B.resultDef{B.rStatus = B.sStatus setting}
              else 
                let bJs = typecheck' sig (a:var) (A.arrowNotat $A.Arrow (init as) b) aType depth setting{B.sStatus = B.rStatus aTypeChecked}
                    treeAB = zip (concatMap (replicate (length $B.trees bJs)) $B.trees aTypeChecked) (cycle (B.trees bJs))
                    trees = 
                      map 
                      (\(aTree,bTree) -> 
                        let x = A.downSide' bTree 
                        in UDT.Tree QT.PiF (A.AJudgment sig var (A.Arrow as b) (A.typefromAJudgment x)) [aTree,bTree] )
                      treeAB
                in bJs{B.trees = trees}
      in B.debugLogWithTerm (sig,var) (A.Arrow as b) aType depth setting "piForm1" result
    _ -> B.debugLogWithTerm (sig,var) aTerm aType depth setting "piFormハズレ" B.resultDef{B.rStatus = B.sStatus setting}

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
sigmaForm' :: B.TypecheckRule
sigmaForm' sig var aTerm aType depth setting
  | aType `notElem` [A.aType,A.Conclusion DdB.Kind] = B.debugLogWithTerm (sig,var) aTerm aType depth setting "sigmaFormハズレ1" B.resultDef{B.rStatus = B.sStatus setting}
  | otherwise = case A.arrowNotat aTerm of 
    A.ArrowSigma' as b ->
      let a = last as
          aTypeChecked = 
            foldl 
            (\r dtTerm -> 
              if a == A.aType && dtTerm == DdB.Type
                then
                  r
                else
                  let newR = typecheck' sig var a (A.Conclusion dtTerm) {-表示の際は depth+1-}depth setting{B.sStatus = B.rStatus r}
                  in newR{B.trees = B.trees newR ++ B.trees r}
            )
             B.resultDef{B.rStatus = B.sStatus setting} 
             [DdB.Type,DdB.Kind]
          result = 
            if null (B.trees aTypeChecked) then  B.resultDef{B.rStatus = B.sStatus setting}
              else 
                let bJs = typecheck' sig (a:var) (A.arrowNotat $A.ArrowSigma' (init as) b) aType depth setting{B.sStatus = B.rStatus aTypeChecked}
                    treeAB = zip (concatMap (replicate (length $B.trees bJs)) $B.trees aTypeChecked) (cycle (B.trees bJs))
                    trees = 
                      map 
                      (\(aTree,bTree) -> 
                        let x = A.downSide' bTree 
                        in UDT.Tree QT.SigmaF (A.AJudgment sig var (A.ArrowSigma' as b) (A.typefromAJudgment x)) [aTree,bTree] )
                      treeAB
                in bJs{B.trees = trees}
      in B.debugLogWithTerm (sig,var) (A.ArrowSigma' as b) aType  depth setting "sigmaForm2" result        
    _ -> B.debugLogWithTerm (sig,var) aTerm aType depth setting "sigmaでない@sigmaFormハズレ" B.resultDef{B.rStatus = B.sStatus setting}

eqForm' :: B.TypecheckRule
eqForm' sig var aTerm aType depth setting
  | aType `notElem` [A.aType,A.Conclusion DdB.Kind] = B.debugLogWithTerm (sig,var) aTerm aType depth setting "eqFormハズレ1" B.resultDef{B.rStatus = B.sStatus setting}
  | otherwise = case A.arrowNotat aTerm of 
    A.ArrowEq t a b -> 
      let tTypeChecked = typecheck' sig var t A.aType depth setting 
          abTypeChecked = 
            foldl 
            (\(r:rr) gterm ->
              case B.trees r of
                [] -> (r:rr)
                _ -> (typecheck' sig var gterm t depth setting{B.sStatus = B.rStatus r}):(r:rr)
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
                        (\[tTree,aTree,bTree] -> UDT.Tree QT.IqF (A.AJudgment sig var aTerm (aType)) [tTree,aTree,bTree] )
                        treeABT
                  in bRes{B.trees = trees}
            _ -> B.resultDef{B.rStatus = B.sStatus setting}
      in B.debugLogWithTerm (sig,var) aTerm aType depth setting "eqForm1" result
    _ -> B.debugLogWithTerm (sig,var) aTerm aType depth setting "eqFormハズレ" B.resultDef{B.rStatus = B.sStatus setting}

sigmaIntro' :: B.DeduceRule 
sigmaIntro' sig var aType depth setting = 
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
                          (filter (\num -> 0<= num && num < length deducedLst) parentLst))
                      )
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
                                             -- ++ " 結果 : " ++ (show $ A.arrowSubst t (A.Conclusion $ DdB.Con "here"{-A.termfromAJudgement $ A.downSide $head $B.trees afterResult-}) (A.aVar ( beforeVarNum {-- argId-})))
                                            --)
                                          ((take beforeVarNum deducedLst') ++ (afterResult:(drop (beforeVarNum + 1) deducedLst')), 
                                            A.betaReduce $ A.arrowSubst t (A.shiftIndices (A.termfromAJudgment $ A.downSide' $head $B.trees afterResult) (argId) 0) (A.aVar (beforeVarNum)))
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
                          let deduced = deduce sig var aType' (depth + 1) setting{B.sStatus = (B.rStatus (head dLst)){B.allProof = (length as')>0}};
                          in
                            if null (B.trees deduced) then [] else (if null clueLst then deduced{B.trees =[head $ B.trees deduced]} else deduced){B.rStatus = (B.rStatus deduced){B.allProof = B.allProof $B.sStatus setting}}:dLst
                        )
                        deducedLstaTypeAndClues
                in (as',deducedLsts')
              ) 
              ((reverse $ b1:as),[[B.resultDef{B.rStatus = B.sStatus setting}]]) 
              (zip parentLsts childrenLsts)
        statusArgTreesLst =  map (\rs -> (B.rStatus $ head rs,sequence $ init $map B.trees rs)) result'
        trees = 
          concatMap 
            (\(_,argTreesLst) ->
              map 
              (\argTrees -> 
                let upside = argTrees; 
                    h:t = map (A.termfromAJudgment . A.downSide') argTrees;
                    term = foldr (\a b -> A.ArrowPair a b) h (reverse t);
                in UDT.Tree QT.SigmaI (A.AJudgment sig var term aType) upside 
              )
              argTreesLst
            )
            statusArgTreesLst
        status = foldr (\s s' -> B.mergeStatus s s') (B.sStatus B.settingDef) (map fst statusArgTreesLst) 
        result = B.resultDef{B.rStatus = status, B.trees = trees}
    in B.debugLog (sig,var) (A.ArrowSigma' as b1) depth setting "sigmaIntro" result
  _ -> B.debugLog (sig,var) aType depth setting "sigmaIntroハズレ" B.resultDef{B.rStatus = B.sStatus setting}
