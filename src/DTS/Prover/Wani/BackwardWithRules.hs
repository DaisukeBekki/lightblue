{-# LANGUAGE OverloadedStrings #-}
module DTS.Prover.Wani.BackwardWithRules
(
  -- * deduce
  deduce
) where

import qualified DTS.DTTdeBruijn as DdB   -- DTT
import qualified DTS.UDTTdeBruijn as UDdB
import qualified DTS.Prover.Wani.Arrowterm as A -- Aterm
import qualified DTS.Prover.Wani.BackwardRules as BR
import qualified Interface.Tree as UDT
import qualified DTS.QueryTypes as QT

import qualified DTS.Prover.Wani.WaniBase as WB 
import qualified DTS.Prover.Wani.Forward as F

import qualified Data.Text.Lazy as T 
import qualified Data.List as L 
import qualified Debug.Trace as D
import qualified Data.Maybe as M

import qualified Data.Time.Clock as Time

debugLog :: WB.Goal -> WB.Depth -> WB.Setting -> T.Text -> a -> a
debugLog (WB.Goal sig var maybeTerm proofTypes) depth setting = 
  WB.debugLogWithTerm (sig,var) (maybe (A.Conclusion $ DdB.Con $T.pack "?") id maybeTerm) (head proofTypes) depth setting

-- | to be updated
-- | sortSubGoalSets
-- | summary : Rank subgoalsets to reduce computation time
sortSubGoalSets :: IO [WB.SubGoalSet] -> IO [WB.SubGoalSet]
sortSubGoalSets = id

-- | ruleResultToSubGoalsets
-- | summary : Extract available subgoalsets and prepare debug output
ruleResultToSubGoalsets :: WB.Depth -> Bool -> IO [([WB.SubGoalSet],T.Text)] -> IO [WB.SubGoalSet]
ruleResultToSubGoalsets depth debugEnabled ruleResultsIO = ruleResultsIO >>= \ruleResults ->
  let 
    (nullsubgoalsets,notNullSubGoalsets) = L.partition (null .fst ) ruleResults 
    f = concatMap fst
    subgoalsets = 
        (
          if debugEnabled 
            then D.trace (
              (concatMap (\(_,msg) -> if T.null msg then [] else (concat [(L.replicate (2*depth) ' '),(show depth)," ",T.unpack msg])) nullsubgoalsets) ++ 
              (unlines $map (\set -> concat [L.replicate (2*depth) ' ',show depth,"-acceptable ",show set]) notNullSubGoalsets)) f   
            else f
        )
        notNullSubGoalsets
  in -- | summary : Reconfigure subgoals so that there is only one type in the arrowType section
    return $ concatMap
        (\(WB.SubGoalSet rule maybeTree subgoals' downside) ->
          let 
            subgoalsLst = 
              sequence $ map
                (\(WB.SubGoal(WB.Goal sig var justTerm arrowTypes) substLst clue) -> 
                  map
                  (\arrowType ->
                    WB.SubGoal
                      (WB.Goal sig var (M.maybe M.Nothing (M.Just . A.betaReduce . A.arrowNotat) justTerm) [(A.betaReduce . A.arrowNotat) arrowType]) 
                      substLst
                      clue
                  )
                  arrowTypes
                )
                subgoals'
          in map (\subgoals -> WB.SubGoalSet rule maybeTree subgoals downside) subgoalsLst
        )
        subgoalsets

constructResultWithResultsets :: QT.DTTrule -> (M.Maybe (UDT.Tree QT.DTTrule A.AJudgment)) -> [[WB.Result]] -> (A.AJudgment,WB.SubstLst)  -> WB.Setting -> WB.Result -> WB.Result
constructResultWithResultsets rule maybeTree resultsets dSide setting resultDef = 
  let 
    constructResultWithResultset ((A.AJudgment sig var aTerm aType),substLstForDside) resultset = 
        let resultBase = foldl WB.mergeResult resultDef resultset
            downside = 
              let gijiGoal = (WB.Goal sig var (M.Just aTerm) [aType])
                  resultsLen = length resultset
                  WB.Goal _ _ (M.Just aTerm') [aType'] = {-- D.trace ("gijigoal : "++(show gijiGoal)++ " resultset "++(show resultset) ++ " bound "++(show $A.varsInaTerm aType)) $--}  maybe gijiGoal id $
                        snd $
                            foldl
                            (\(targetId,maybeGoal') (WB.SubstSet lst target num) -> -- D.trace ("maybeGoal "++(show maybeGoal') ++ " / " ++ (show $ WB.SubstSet lst target num)) $
                                maybe
                                (targetId-1,M.Nothing)
                                (\goal' -> (
                                  targetId-1, 
                                  if resultsLen > num then  updateGoalWithAntecedent (WB.SubstSet lst target num) ((reverse resultset) !! num) setting (targetId,goal') else (D.trace ("error in constructResultWithResultset : num-" ++ (show num) ++ " resulstset "++(show resultset)) M.Nothing)
                                  )
                                )
                                maybeGoal'
                            )
                            (-1, M.Just gijiGoal)
                            (reverse $ L.sortOn (\(WB.SubstSet _ _ num) -> num) substLstForDside)
              in A.AJudgment sig var aTerm' aType'
            trees = map (head . WB.trees) resultset
            tree = UDT.Tree rule downside (maybe trees (\tree -> tree:trees) maybeTree)
        in resultBase{WB.trees = [tree]}
    resultsets' = map (constructResultWithResultset dSide) resultsets
  in foldl WB.mergeResult resultDef resultsets'

updateGoalWithAntecedent :: WB.SubstSet -> WB.Result -> WB.Setting -> (Int,WB.Goal) -> M.Maybe WB.Goal
updateGoalWithAntecedent (WB.SubstSet _ before _) result setting (targetId,(WB.Goal sig var maybeTerm arrowTypes))
  | targetId > (-1) = M.Nothing
  | ((length (WB.trees result)) > 1) = M.Nothing
  | otherwise = 
      let resultDownSide = A.downSide' (head (WB.trees result))
          envDiff =  A.contextLen (sig,var) - A.contextLen (A.envfromAJudgment resultDownSide)
          after = A.shiftIndices (A.termfromAJudgment $ A.downSide' (head (WB.trees result))) (envDiff) 0
          arrowTypes' =  map (\arrowType -> A.betaReduce $ A.arrowSubst arrowType after before) arrowTypes
          maybeTerm' = maybe M.Nothing (\term -> M.Just (A.betaReduce $ A.arrowSubst term after before)) maybeTerm
      in M.Just $ WB.Goal sig var maybeTerm' arrowTypes'

-- | subgoalToGoalWithAntecedents
-- | input : 
-- |   results : 1 dummy + antecedents (ex : [result for 2nd subgoal,result for the 1st subgoal,resultDef])
-- |   goal : WB.Goal with var -2 replacing the proof term of the leftmost subgoal and var -3 replacing the proof term of the second left subgoal ...
subgoalToGoalWithAntecedents :: [WB.Result] ->  WB.SubGoal-> WB.Depth -> WB.Setting -> M.Maybe WB.Goal
-- subgoalToGoalWithAntecedents [resultDef] (WB.SubGoal goal _ _) setting = M.Just goal
subgoalToGoalWithAntecedents [] (WB.SubGoal goal _ _) _ setting = M.Nothing
subgoalToGoalWithAntecedents results (WB.SubGoal goal substLst (pos,res)) depth setting = -- D.trace ("subgoalToGoalWithAntecedents results:" ++ (show results) ++ " / goal:" ++ (show goal) ++ "clue :" ++ (show (pos,res))) $ 
  let myId = -1 -- negate $ (1 + length results)
      goalWithClue = 
        maybe
          (M.Just goal)
          (\(before,after) -> 
            let clueLst = L.nub $ filter ((A.aVar (-1) ==) . fst) (map (\(fst_,snd_) ->(A.betaReduce $ A.arrowNotat fst_,A.betaReduce $ A.arrowNotat snd_) ) $ A.canBeSame' 0 before after)
                justMyTerm = if length clueLst == 1 then M.Just (snd $ head clueLst) else M.Nothing
            in case goal of
              WB.Goal sig var M.Nothing proofTypes -> maybe ((if depth < WB.debug setting then debugLog goal depth setting (T.pack ("remove this goal due to the clue " ++ (show clueLst) ++ " : ")) else id) (M.Nothing)) (\myTerm -> let newGoal = WB.Goal sig var (M.Just myTerm) proofTypes in (if depth < WB.debug setting then debugLog newGoal depth setting (T.pack ("update" ++ (show goal) ++  " with clue " ++ (show (pos,res)) ++ " : ")) else id) (M.Just newGoal)) justMyTerm
              WB.Goal sig var _ proofTypes -> (if depth < WB.debug setting then D.trace ("it already has a term so I won't update the term with clue") else id) (M.Just goal)
          )
          res
      resultsLen = length results
  in snd $
      foldl
      (\(targetId,maybeGoal') (WB.SubstSet lst target num) -> -- D.trace ("maybeGoal "++(show maybeGoal') ++ " / " ++ (show $ WB.SubstSet lst target num)) $
          maybe
          (targetId-1,M.Nothing)
          (\goal' -> (
            targetId-1, 
            if resultsLen > num then  updateGoalWithAntecedent (WB.SubstSet lst target num) (results !! num) setting (targetId,goal') else (D.trace "error in subgoalToGoalWithAntecedents" M.Nothing)
            )
          )
          maybeGoal'
      )
      (-1, goalWithClue)
      (reverse $ L.sortOn (\(WB.SubstSet _ _ num) -> num) substLst)

-- | deduceWithSubGoalset
-- | summary : search or check proof terms for a type in input `[WB.SubGoalSet]`
-- |
-- | 1. prepare debug output
-- | 2. 

deduceWithSubGoalset :: WB.SubGoalSet -> WB.Depth -> WB.Setting -> WB.Result -> IO WB.Result
deduceWithSubGoalset (WB.SubGoalSet rule maybeTree subgoals dSide) depth setting resultDef = 
    --deduceWithAntecedentsAndSubGoal :: Subgoal -> [WB.Result] -> IO [[WB.Result]]
    let deduceWithAntecedentsAndSubGoal subgoal results= 
            case subgoalToGoalWithAntecedents results subgoal depth setting of
                M.Just goal -> 
                    deduce' goal depth setting >>= \newResult -> return (map (\tree -> (newResult{WB.trees = [tree]}):results) (L.nub $ WB.trees newResult))
                M.Nothing -> return []
        -- deduceWithAntecedentsetAndSubGoal :: IO [[WB.Result]] -> Subgoal -> IO [[WB.Result]]
        deduceWithAntecedentsetAndSubGoal resultsetIOs subgoal = resultsetIOs >>= \resultset -> foldMap (deduceWithAntecedentsAndSubGoal subgoal) resultset
        resultsetIO = (if depth < WB.debug setting then (D.trace (L.replicate (2*depth) ' ' ++ "with " ++ (show rule) ++ ", want to prove "  ++ (show subgoals)) ) else id) $
            foldl deduceWithAntecedentsetAndSubGoal (return [[resultDef]]) subgoals >>= \resultset' -> return (map (reverse . init) resultset')
    in 
      resultsetIO >>= \resultset -> return $ constructResultWithResultsets rule maybeTree resultset dSide setting resultDef

-- | deduceWithSubGoalsets
-- | summary : search or check proof terms for a type in input `[WB.SubGoalSet]`
-- | 
-- | detail :
-- | 1. No need for check depth
-- | 2. check if allProof is needed
-- | 3. execute `deduceWithSubGoalset` for each subgoalset
-- | 4. leave only the result matches the target.
deduceWithSubGoalsets :: [WB.SubGoalSet] -> WB.Depth -> WB.Setting -> WB.Result -> M.Maybe A.Arrowterm -> A.Arrowterm -> IO WB.Result
deduceWithSubGoalsets subgoalsets depth setting resultDef justTerm arrowType = 
    let resultIO' = 
            foldl
                (\rsIO subgoalset -> 
                  rsIO >>= \rs ->
                    if (WB.allProof (WB.sStatus setting)) || (null $ WB.trees rs)
                        then 
                          (deduceWithSubGoalset subgoalset depth setting{WB.sStatus = WB.mergeStatus (WB.rStatus rs) WB.statusDef{WB.allProof = True}} resultDef)
                            >>= \result -> return $ WB.mergeResult rs result
                        else rsIO
                )
                (return resultDef)
                subgoalsets
        treeIO = resultIO' >>= \result' -> return $
                  filter 
                  (\tree -> 
                      let A.AJudgment sig' var' term' type' = A.downSide' tree 
                      in -- `arrowNotat` and `betaReduece` are performed uniformly here. Even if normalization is not considered when creating a rule, the following ensures that the comparison is valid.
                          (maybe True (\term -> (A.arrowNotat . A.betaReduce) term' == (A.arrowNotat . A.betaReduce) term) justTerm) && ((A.arrowNotat . A.betaReduce) type' == (A.arrowNotat . A.betaReduce) arrowType)
                  ) $
                  L.nub$ WB.trees result'
    in treeIO >>= \trees -> resultIO' >>= \result' -> return $ result'{WB.rStatus = (WB.rStatus result'){WB.deduceNgLst = WB.deduceNgLst$WB.sStatus setting}}{WB.trees = trees}


-- | deduce'
-- | summary : search or check proof terms for a type in input `goal`
-- | 
-- | detail :
-- | 1. Check depth
-- | 2. Assert that there is only one type to prove
-- | 3. Check `deduceNgLst` (which is updated with context-types pair targeted in shallow nodes)
-- | 4. Check `failedlst` (which is updated with context-term-types tuple which are targeted in shallow nodes or failed before)
-- | 5. If typecheck with specific proof terms (Bot, Type or Kind) is needed, return result without rule adoption
-- | 6. If deduce with specific proof type(Kind) is needed, return result without rule adoption
-- | 7. Find subgoalsets with rules
-- | 8. Perform deduce recursion on subgoalsets
-- | 9. Restore deduceNgLst to that passed as input
-- | 10. If typecheck failed, update `failedlst` with context-term-types tuple.
deduce':: WB.Goal -> WB.Depth -> WB.Setting -> IO WB.Result
deduce' goal depth setting
  | depth > WB.maxdepth setting =
      return $ debugLog goal depth setting "depth @ deduce : " WB.resultDef{WB.errMsg = "depth @ deduce",WB.rStatus = WB.mergeStatus (WB.sStatus setting) WB.statusDef{WB.usedMaxDepth = depth}} -- Set `B.rStatus` to update the maximum depth used.
  | (let WB.Goal _ _ _ typeLst = goal in length typeLst /= 1) =
      return $ debugLog goal depth setting "typeLst has 0 or more than 2 elements : " WB.resultDef{WB.errMsg = "typeLst has 0 or more than 2 elements.",WB.rStatus = WB.mergeStatus (WB.sStatus setting) WB.statusDef{WB.usedMaxDepth = depth}}
  -- | maybe (let WB.Goal sig var term [arrowType] = goal in any (\(con,aType) -> A.contextLen (sig,var) == (A.contextLen con) && A.sameCon (sig,var) con && A.sameTerm ((sig,var),arrowType) (con,aType)) (WB.deduceNgLst (WB.sStatus setting))) (\arrowTerm -> False) (WB.termFromGoal goal) = 
  --     debugLog goal depth setting (T.concat ["avoidloop(ng) : ",(T.pack $ show (WB.deduceNgLst (WB.sStatus setting)))]) WB.resultDef{WB.errMsg = "avoid loop.",WB.rStatus = WB.mergeStatus (WB.sStatus setting) WB.statusDef{WB.usedMaxDepth = depth}}
  | maybe False (\arrowTerm -> let WB.Goal sig var _ [arrowType] = goal in any (\(con,aType,aTerm) -> A.contextLen (sig,var) == (A.contextLen con) && A.sameCon (sig,var) con && A.sameTerm ((sig,var),arrowType) (con,aType) && A.sameTerm ((sig,var),arrowTerm) (con,aTerm)) (WB.failedlst (WB.sStatus setting))) (WB.termFromGoal goal) = 
      return $ debugLog goal depth setting (T.concat ["avoidloop(failed) : ",(T.pack $ show (WB.failedlst (WB.sStatus setting)))]) WB.resultDef{WB.errMsg = "avoid loop.",WB.rStatus = WB.mergeStatus (WB.sStatus setting) WB.statusDef{WB.usedMaxDepth = depth}}
  | otherwise =
      Time.getCurrentTime >>= \currentTime ->
        if maybe False (\timeLimit -> timeLimit < currentTime) (WB.timeLimit setting)
        then
          return $ debugLog goal depth setting (T.concat ["timelimit : ",(T.pack $ show (WB.failedlst (WB.sStatus setting)))]) WB.resultDef{WB.errMsg = "time limit",WB.rStatus = WB.mergeStatus (WB.sStatus setting) WB.statusDef{WB.usedMaxDepth = depth}}
        else
          let 
            WB.Goal sig var justTerm [arrowType] = debugLog goal depth setting "current goal : " goal
          in 
            case justTerm of
              M.Just (A.Conclusion DdB.Bot) ->
                if arrowType == A.aType && WB.falsum setting
                  then return $ WB.resultDef{WB.trees = [UDT.Tree QT.BotF (A.AJudgment sig var (A.Conclusion DdB.Bot) arrowType) []],WB.rStatus = WB.mergeStatus (WB.sStatus setting) WB.statusDef{WB.usedMaxDepth = depth}} -- if `B.falsum` is true, the type for `false` is `type`.
                  else return $ WB.resultDef{WB.rStatus = WB.mergeStatus (WB.sStatus setting) WB.statusDef{WB.usedMaxDepth = depth}}
              M.Just (A.Conclusion DdB.Top) ->
                if arrowType == A.aType
                  then return $ WB.resultDef{WB.trees = [UDT.Tree QT.TopF (A.AJudgment sig var (A.Conclusion DdB.Top) arrowType) []],WB.rStatus = WB.mergeStatus (WB.sStatus setting) WB.statusDef{WB.usedMaxDepth = depth}}
                  else return $ WB.resultDef{WB.rStatus = WB.mergeStatus (WB.sStatus setting) WB.statusDef{WB.usedMaxDepth = depth}}
              M.Just (A.Conclusion DdB.Type) ->
                if arrowType == A.Conclusion DdB.Kind
                  then return $ WB.resultDef{WB.trees = [UDT.Tree QT.Con (A.AJudgment sig var (A.Conclusion DdB.Type) arrowType) []],WB.rStatus = WB.mergeStatus (WB.sStatus setting) WB.statusDef{WB.usedMaxDepth = depth}}
                  else return $ WB.resultDef{WB.rStatus = WB.mergeStatus (WB.sStatus setting) WB.statusDef{WB.usedMaxDepth = depth}}
              M.Just (A.Conclusion DdB.Kind) ->
                return $ WB.debugLogWithTerm (sig,var) (A.Conclusion DdB.Kind) arrowType depth setting "kind cannot be a term."  WB.resultDef{WB.rStatus = WB.mergeStatus (WB.sStatus setting) WB.statusDef{WB.usedMaxDepth = depth}}
              _ -> -- M.Nothing or M.Just term
                case arrowType of 
                  A.Conclusion DdB.Kind -> -- The only term for `kind` is `type`. but the term is not `type` due to the antecedent
                    return $ debugLog goal depth setting "the only term for `kind` is `type`" WB.resultDef{WB.rStatus = WB.mergeStatus (WB.sStatus setting) WB.statusDef{WB.usedMaxDepth = depth}}
                  _ -> 
                    let subgoalsetsIO = sortSubGoalSets $ (ruleResultToSubGoalsets depth $ depth < WB.debug setting) $ sequence $ 
                          map
                            (\rule -> rule goal setting)
                            ( -- Because of `sortSubGoalSets`, there is no need to care about rule order. (Before `sortSubGoalSets`, The stronger the rule, the later to be set. For example, `dne` can be used for any term, thus turning the execution later. This setting takes effect in combination with the rounding up of proof search using `B.allProof`.)
                              [BR.piForm,BR.sigmaForm,BR.eqForm,BR.membership,BR.askOracle,BR.piIntro,BR.sigmaIntro,BR.piElim,BR.topIntro] 
                              ++ [BR.dne | arrowType /= A.Conclusion DdB.Bot && WB.mode setting == WB.WithDNE]
                              ++ [BR.efq | arrowType /= A.Conclusion DdB.Bot && WB.mode setting == WB.WithEFQ]
                            )
                        resultIO = 
                            let resultDef = -- update `deduceNgLst` and `failedlst` to be used in deeper search
                                    WB.resultDef{WB.rStatus = WB.mergeStatus (WB.sStatus setting) (WB.statusDef{WB.usedMaxDepth = depth,WB.deduceNgLst = ((sig,var),arrowType) : (WB.deduceNgLst $WB.sStatus setting),WB.failedlst = maybe (WB.failedlst $WB.sStatus setting) (\arrowTerm -> (((sig,var),arrowTerm,arrowType) : (WB.failedlst $WB.sStatus setting))) justTerm})} -- Currently, `arrowType` proof search is performed under environment `con`, and to prevent infinite loops, it is set to round up when `arrowType` proof search is needed under environment `con`(★).
                            in subgoalsetsIO >>= \subgoalsets -> deduceWithSubGoalsets subgoalsets (depth+1) setting resultDef justTerm arrowType
                    in resultIO >>= \result ->
                      if null (WB.trees result)
                        then
                          return $ (if depth < WB.debug setting then WB.debugLog (sig,var) arrowType depth setting "deduce failed " else id) result{WB.rStatus = (WB.rStatus result){WB.failedlst = maybe (WB.failedlst $WB.sStatus setting) (\arrowTerm -> (((sig,var),arrowTerm,arrowType) : (WB.failedlst $WB.sStatus setting))) justTerm}}
                        else
                          return $ (if depth < WB.debug setting then D.trace (L.replicate (2*depth) ' ' ++  show depth ++ " deduced:  " ++ show (map A.downSide' (WB.trees result))) else id) result

-- | deduce
-- | summary : deduce' wrapper
deduce :: WB.DeduceRule
deduce sig var arrowType depth setting = 
  deduce' (WB.Goal sig var M.Nothing [arrowType]) depth setting
    >>= \result ->  (if depth < WB.debug setting then (D.trace ("result :" ++ (show result))) else id ) (return result)
