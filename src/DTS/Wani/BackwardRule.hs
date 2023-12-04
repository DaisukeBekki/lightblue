{-# LANGUAGE OverloadedStrings #-}
module DTS.Wani.BackwardRule
(
  SubstSet(..),
  Goal(..),
  SubGoalSet(..),
  ProofType(..),
  piIntro,
  deduce
) where

import qualified DTS.DTT as DT
import qualified DTS.Wani.Arrowterm as A
import qualified DTS.Prover_daido.Judgement  as J

import qualified DTS.Wani.WaniBase as WB 
import qualified DTS.Wani.Forward as F

import qualified Data.Text.Lazy as T 
import qualified Data.List as L 
import qualified Debug.Trace as D
import qualified Data.Maybe as M

import qualified Interface.HTML as HTML
import qualified Data.Bifunctor

type ProofTerm = A.Arrowterm
type ProofType = A.Arrowterm
-- data ProofType' = Single A.Arrowterm | TypeOrKind deriving (Eq,Show)

data SubstSet = SubstSet (M.Maybe ProofTerm) ProofType deriving (Eq)

instance Show SubstSet where
  show term = case term of
    SubstSet substTerm target -> 
      let termText = maybe "?_subst" show substTerm
      in "[ "++ termText ++ " / (" ++ (show target) ++")]"

type SubstLst = [SubstSet]

data Goal = Goal A.Context (Maybe ProofTerm) [ProofType]
  deriving (Eq)

instance Show Goal where
  show term = case term of
    Goal con maybeTerm proofTypes -> 
      let wrappedTerm = maybe (A.Conclusion $ DT.Con "?") id maybeTerm
      in concat $ map (\(num,proofType) -> " / GoalOption-" ++ (show num) ++ "/" ++ (show $ length proofTypes)++" " ++ (show $ A.AJudgement con wrappedTerm proofType)) $ zip [1..] proofTypes

conFromGoal :: Goal -> A.Context
conFromGoal (Goal con _ _) = con

termFromGoal :: Goal -> Maybe ProofTerm
termFromGoal (Goal _ maybeProofTerm _) = maybeProofTerm

typesFromGoal :: Goal -> [ProofType]
typesFromGoal (Goal _ _ proofTypes) = proofTypes

type SubGoal = (Goal,SubstLst)

goalFromSubGoal :: SubGoal -> Goal
goalFromSubGoal = fst

substLstFromSubGoal :: SubGoal -> SubstLst
substLstFromSubGoal = snd

data SubGoalSet = SubGoalSet T.Text [J.Tree A.AJudgement] [SubGoal] [(ProofType,(ProofType,SubstLst))]
  deriving (Eq,Show)

treeFromSubGoalSet :: SubGoalSet -> [J.Tree A.AJudgement]
treeFromSubGoalSet subGoalSet = case subGoalSet of SubGoalSet _ trees subgoals clues -> trees

subGoalsFromSubGoalSet :: SubGoalSet -> [SubGoal]
subGoalsFromSubGoalSet subGoalSet = case subGoalSet of SubGoalSet _ trees subgoals clues -> subgoals

cluesFromSubGoalSet :: SubGoalSet -> [(ProofType,(ProofType,SubstLst))]
cluesFromSubGoalSet subGoalSet = case subGoalSet of SubGoalSet _ trees subgoals clues -> clues

type Rule = Goal -> WB.Depth -> WB.Setting  -> [SubGoalSet]

debugLog :: Goal -> WB.Depth -> WB.Setting -> T.Text -> a -> a
debugLog goal depth setting = WB.debugLogWithTerm (conFromGoal goal) (A.Conclusion $ DT.Con $T.pack "?") (head $ typesFromGoal goal) depth setting

-- debugLogSubGoalSet :: SubGoalSet -> WB.Depth -> WB.Setting -> T.Text -> a -> a
-- debugLogSubGoalSet subGoalSet = let goals = conFromGoal $ goalFromSubGoal $ subGoalsFromSubGoalSet subGoalSet in debugLog goal 

debugLogSubgoalSet :: SubGoalSet -> WB.Depth -> WB.Setting  -> T.Text -> a -> a
debugLogSubgoalSet subGoalSet depth setting label answer =
  if WB.debug setting 
    then
      D.trace
        ((if WB.allProof (WB.sStatus setting) then "all " else "")++  L.replicate (2*depth) ' ' ++ show depth ++ " " ++ T.unpack label ++ " subgoals are(is) ..." ++ (show subGoalSet))
        answer
    else answer

backwardToforward :: Goal -> WB.Depth -> WB.Setting -> WB.Result
backwardToforward goal depth setting =
  let con = conFromGoal goal
      forwardResult = F.forwardContext con
      forwardTrees = WB.trees forwardResult
      matchLst =  L.nub $
        filter 
          (\xTree -> 
            let x = A.downSide xTree 
                xCon =A.envfromAJudgement x
                xType =   A.typefromAJudgement x
            in 
                or $ map (\aType -> A.sameTerm (con,aType) (xCon,xType)) (typesFromGoal goal))
            $ forwardTrees  
  in debugLog goal depth setting "backwardToforward" (forwardResult{WB.trees = matchLst,WB.rStatus = WB.sStatus setting})


-- pi 型の前件及び型付きラムダ型の型部分は正規化されているものとする
piIntro :: Rule
piIntro goal depth setting =
  case (conFromGoal goal,typesFromGoal goal) of
    ((sigCon,varCon),[A.Arrow as b]) ->
        let trees = []
            subgoalsWithTerm = map (\a -> (Goal (sigCon,varCon) (Just a) (map A.Conclusion [DT.Type,DT.Kind]),[])) as
            termForB = maybe Nothing (\g -> case g of (A.ArrowLam' _ term) -> Just (A.betaReduce term)) (termFromGoal goal)
            subgoals = (Goal (sigCon,as ++ varCon) termForB [b],[]):subgoalsWithTerm
            clues = []
            subgoalsets = [SubGoalSet "piIntro" trees subgoals clues]
        in subgoalsets
    _ -> []

dne :: Rule
dne goal depth setting = []

efq :: Rule
efq goal depth setting = []


deduce :: Goal -> WB.Depth -> WB.Setting -> WB.Result
deduce goal depth setting 
  | depth > WB.maxdepth setting =
      WB.resultDef{WB.errMsg = "depth @ deduce",WB.rStatus = WB.mergeStatus (WB.sStatus setting) WB.statusDef{WB.usedMaxDepth = depth}}
  | any (\(con',aType')->let con = conFromGoal goal ; arrowTypes = typesFromGoal goal in A.contextLen con == (A.contextLen con')&&A.sameCon con con'&& or (map (\arrowType -> A.sameTerm (con,arrowType) (con',aType')) arrowTypes)) (WB.deduceNgLst (WB.sStatus setting)) = 
      debugLog goal depth setting "Avoid endless loops."  (WB.resultDef{WB.rStatus = WB.mergeStatus (WB.sStatus setting) WB.statusDef{WB.usedMaxDepth = depth}})
  | any (== A.Conclusion DT.Kind) (typesFromGoal goal) = let con = conFromGoal goal in WB.resultDef{ WB.trees = [J.T J.VAR(A.AJudgement con (A.aType) (A.Conclusion DT.Kind)) []], WB.rStatus = WB.mergeStatus (WB.sStatus setting) WB.statusDef{WB.usedMaxDepth = depth}}  
  | otherwise = 
      let con = conFromGoal goal
          arrowType = case typesFromGoal goal of [proofType] -> proofType ; _ -> undefined
          forwardedresult = backwardToforward goal depth setting
          subgoalsSetLst = concatMap 
            (\f -> 
              let subgoalsets = f goal depth setting
              in map (\subgoalset -> debugLogSubgoalSet subgoalset depth setting "here" subgoalset) subgoalsets
            )
            ( -- The stronger the rule, the later the timing of application is turned back. For example, `dne` can be used for any term, thus turning the execution later. This setting takes effect in combination with the rounding up of proof search using `B.allProof`.
              -- [membership,piIntro,sigmaIntro,piElim]
              [piIntro] 
                ++ [dne | arrowType /= A.Conclusion DT.Bot && WB.mode setting == WB.WithDNE]
                ++ [efq | arrowType /= A.Conclusion DT.Bot && WB.mode setting == WB.WithEFQ]
            )
          -- 
          subgoalsLstWithPriority = 
            map 
            subGoalsFromSubGoalSet case of -> (Goal,SubstLst) 
            subgoalsSetLst

          backwardresult = WB.resultDef
      --     -- backwardresult= foldl
      --     --   (\rs subgoalsSetsFromEachRules -> 
      --     --     let result = foldl
      --     --           (\rs' subgoalSets-> let
      --     --               subgoals = debugLogSubgoalSet subgoalSets depth setting "subgoals" (subGoalsFromSubGoalSet subgoalSets)
      --     --             in if all (null . snd) subgoals
      --     --               then 
      --     --                 foldl 
      --     --                 (\rs2' subgoal ->
      --     --                   deduce (goalFromSubGoal subgoal) (depth+1) setting
      --     --                   )
      --     --               else undefined
      --     --           )
      --     --           rs
      --     --           subgoalsSetsFromEachRules
      --     --     in result)
      --     --   (WB.resultDef{
      --     --     WB.rStatus = 
      --     --       WB.mergeStatus 
      --     --         (WB.sStatus setting) 
      --     --         (WB.statusDef{WB.usedMaxDepth = depth,WB.deduceNgLst = (con,arrowType) : (WB.deduceNgLst $WB.sStatus setting)})} ) 
      --     --         -- Currently, `arrowType` proof search is performed under environment `con`, and to prevent infinite loops, it is set to round up when `arrowType` proof search is needed under environment `con`(★).
      --     --   $ subgoalsSetLst
      --       -- if null subgoalsLst then WB.resultDef{WB.errMsg="a"} else WB.resultDef{WB.errMsg="b"} 
          result' = WB.mergeResult forwardedresult backwardresult
          result = result'{WB.rStatus = (WB.rStatus result'){WB.deduceNgLst = WB.deduceNgLst$WB.sStatus setting}}{WB.trees = L.nub$WB.trees result'} -- excludes duplicate proof trees, and restore deduceNgList to its original state from ★ state
      in result
        
        
      --   -- if null (WB.trees result) -- for debug
      --   --   then
      --   --     (if WB.debug setting then WB.debugLog con arrowType depth setting "deduce failed " else id) result
      --   --   else
      --   --     (if WB.debug setting then D.trace (L.replicate (2*depth) ' ' ++  show depth ++ " deduced:  " ++ show (map A.downSide (WB.trees result))) else id) result
