{-# LANGUAGE OverloadedStrings #-}
module DTS.Prover.Wani.BackwardRule
(
  Goal(..),
  conFromGoal,
  termFromGoal,
  typesFromGoal,
  Rule(..),
  SubGoal(..),
  goalFromSubGoal,
  cluesFromSubGoal,
  substLstFromSubGoal,
  SubstSet(..),
  treeFromSubGoalSet,
  dsideFromSubGoalSet,
  subGoalsFromSubGoalSet,
  SubstLst(..),
  Clue(..),
  SubGoalSet(..),
  ProofType(..),
  ProofTerm(..),
  -- * Rules
  piIntro,
  piElim,
  piForm,
  sigmaIntro,
  sigmaForm,
  membership,
  ax,
  dne,
  -- * deduce
  deduce
) where

import qualified DTS.UDTTdeBruijn as UDdB
import qualified DTS.Prover.Wani.Arrowterm as A -- Aterm
import qualified Interface.Tree as UDT
import qualified DTS.QueryTypes as QT
import DTS.Labels (DTT)                   -- UDTT

import qualified DTS.Prover.Wani.WaniBase as WB 
import qualified DTS.Prover.Wani.Forward as F

import qualified Data.Text.Lazy as T 
import qualified Data.List as L 
import qualified Debug.Trace as D
import qualified Data.Maybe as M

import qualified Interface.HTML as HTML
import qualified Data.Bifunctor

-- | term in judgments
type ProofTerm = A.Arrowterm

-- | type in judgments
type ProofType = A.Arrowterm

-- Goal sig var  M.Nothing [targetType]
data Goal = 
  Goal 
    A.SAEnv -- ^ sigs
    A.AEnv  -- ^ vars
    (Maybe ProofTerm) -- ^ maybe term ( if typecheck is needed, it will be Just *, and it will be Nothing if deduce is needed.)
    [ProofType] -- ^ In most cases, this value is a list with only one element. Only when you want to indicate the term is a type, it takes the form [type,kind].
  deriving (Eq)

instance Show Goal where
  show term = case term of
    Goal sig var maybeTerm proofTypes -> 
      let wrappedTerm = maybe (A.Conclusion $ UDdB.Con "?") id maybeTerm
      in L.intercalate " / " $ map (\(num,proofType) -> "GoalOption-" ++ (show num) ++ "/" ++ (show $ length proofTypes) ++ (show $ A.AJudgment sig var wrappedTerm proofType)) $ zip [1..] proofTypes

conFromGoal :: Goal -> (A.SAEnv,A.AEnv)
conFromGoal (Goal sig var _ _) = (sig,var)

termFromGoal :: Goal -> Maybe ProofTerm
termFromGoal (Goal _ _ maybeProofTerm _) = maybeProofTerm

typesFromGoal :: Goal -> [ProofType]
typesFromGoal (Goal _ _ _ proofTypes) = proofTypes

type Rule = Goal -> ([SubGoalSet],T.Text)

data SubGoal = 
  SubGoal 
    Goal
    SubstLst
    Clue
  deriving (Eq,Show)

-- | In deduce, the proof search is performed from the left side of the upper row. 
-- | When there is a target to which the proof term resulting from the left proof search to be assigned, a substSet is created for each such target.

-- | Suppose we want to execute proofsearch of \( \text{pochi:entity,taro:entity,} u_1 \text{: man(taro) } \vdash \text{ ? : [x:entity,man(x)] } \) and use SigmaIntro, there will be 2 minigoals, \( \text{pochi:entity,taro:entity,} u_1 \text{: man(taro)} \vdash ?_1 \text{ : entity}\) and  \( \text{pochi:entity,taro:entity,} u_1 \text{: man(taro)}  \vdash ?_2 \text{ : man(}?_1\text{)}\).
-- | After the former is completed, \( ?_1\) can be \( \text{pochi}\) or \( \text{taro} \). Rule provides 1 SubstSet for \( ?_1 \) in the latter minigoal. 
data SubstSet = 
  SubstSet 
    [ProofTerm] -- ^ In the subgoalSet resulting from the output of the `Rule`, this value is initially specified as []. As the left-hand proof search proceeds, this value is changed to list of proofterms.
    ProofType -- ^ The target is notated as `A.aVar` number. The way the numbers are assigned follow De Bruijn. That is, 0 for itself and 1+ for the left-minigoal-proofterm.
  deriving (Eq)

instance Show SubstSet where
  show term = case term of
    SubstSet substTerms target -> 
      let termText = show substTerms
      in "[ "++ termText ++ " / (" ++ (show target) ++")]"

type SubstLst = [SubstSet]

{-|
  The clue \(([(a,b)],\text{Maybe } (c,d)) \) in 1 `SubGoal` means the following.

  1. the proofterm for this Subgoal is mentioned in the type of \(b\) and the form is \(a\).

  2. \(c \equiv d \)
-}
type Clue = ([(ProofTerm,ProofType)],Maybe (ProofTerm,ProofTerm))

goalFromSubGoal :: SubGoal -> Goal
goalFromSubGoal (SubGoal goal substLst clue) = goal

substLstFromSubGoal :: SubGoal -> SubstLst
substLstFromSubGoal (SubGoal goal substLst clue) = substLst

cluesFromSubGoal :: SubGoal -> Clue
cluesFromSubGoal (SubGoal goal substLst clue) = clue

data SubGoalSet = 
  SubGoalSet 
    QT.DTTrule -- ^ label of rule
    (Maybe (UDT.Tree QT.DTTrule A.AJudgment))  -- ^ forwardedTree; a tree for function in `piElim` or the upside tree for `membership`
    [SubGoal]  -- ^ list of subgoals; Proofsearch is performed starting with the one in the front
    A.AJudgment -- ^ judgement for the downside; The part that needs to be updated based on the upside is indicated as `A.aVar` num. The left-most proofterm in the upside is `A.aVar` -1, the second proof term from the left is `A.aVar` -2, and so on, decreasing in number.
  deriving (Eq,Show)

labelFromSubGoalSet :: SubGoalSet -> QT.DTTrule
labelFromSubGoalSet subGoalSet = case subGoalSet of SubGoalSet label _ _ _ -> label

treeFromSubGoalSet :: SubGoalSet -> Maybe (UDT.Tree QT.DTTrule A.AJudgment)
treeFromSubGoalSet subGoalSet = case subGoalSet of SubGoalSet _ tree subgoals dside -> tree

subGoalsFromSubGoalSet :: SubGoalSet -> [SubGoal]
subGoalsFromSubGoalSet subGoalSet = case subGoalSet of SubGoalSet _ tree subgoals dside -> subgoals

dsideFromSubGoalSet :: SubGoalSet -> A.AJudgment
dsideFromSubGoalSet subGoalSet = case subGoalSet of SubGoalSet _ tree subgoals dside -> dside

debugLog :: Goal -> WB.Depth -> WB.Setting -> T.Text -> a -> a
debugLog goal depth setting = WB.debugLogWithTerm (conFromGoal goal) (A.Conclusion $ UDdB.Con $T.pack "?") (head $ typesFromGoal goal) depth setting

debugLogSubgoalSet :: SubGoalSet -> WB.Depth -> WB.Setting  -> QT.DTTrule -> a -> a
debugLogSubgoalSet subGoalSet depth setting label answer =
  if WB.debug setting 
    then
      D.trace
        ((if WB.allProof (WB.sStatus setting) then "all " else "")++  L.replicate (2*depth) ' ' ++ show depth ++ " " ++ (show label) ++ " subgoals are(is) ..." ++ (show subGoalSet))
        answer
    else answer

exitMessage :: ExitReason -> QT.DTTrule ->T.Text
exitMessage reasonCode label = 
  let message = case reasonCode of
        MultipleTypes -> "Goal has multiple types"
        TypeMisMatch aterm -> T.concat [T.pack $ show aterm, " is not acceptable"]
        TermMisMatch aterm -> T.concat [T.pack $ show aterm, " is not acceptable"]
        Other output -> output
  in T.concat [message," in ",T.pack (show label)]


data ExitReason = MultipleTypes | TypeMisMatch ProofType | TermMisMatch (M.Maybe ProofTerm) | Other T.Text

backwardToforward :: Goal -> WB.Depth -> WB.Setting -> WB.Result
backwardToforward goal depth setting =
  let (sig,var) = conFromGoal goal
      forwardResult = F.forwardContext sig var
      forwardTrees = WB.trees forwardResult
      matchLst =  L.nub $
        filter 
          (\xTree -> 
            let x = A.downSide' xTree 
                xCon =A.envfromAJudgment x
                xType =   A.typefromAJudgment x
            in 
                or $ map (\aType -> A.sameTerm ((sig,var),aType) (xCon,xType)) (typesFromGoal goal))
            $ forwardTrees  
  in debugLog goal depth setting "backwardToforward" (forwardResult{WB.trees = matchLst,WB.rStatus = WB.sStatus setting})

-- | piIntro rule
--
-- +----------+----------------+-------------------------------------------------------------------------------------------+
-- | deduce   | input goal     | \(\Gamma \ \vdash \text{Nothing}:[l] \ (\equiv [[ A1,  ... , An ] -> B])\)                |
-- |          +----------------+-------------------------------------------------------------------------------------------+
-- |          | assertion      | \( l \text{ is } \Pi  \text{ type } \)                                                    |
-- |          +----------------+-------------------------------------------------------------------------------------------+
-- |          | forwardedTree  | \( [] \)                                                                                  |
-- |          +----------------+-------------------------------------------------------------------------------------------+
-- |          | subgoals       | \( [ \text{subgoal1} , \text{subgoal2} ] \)                                               |
-- |          |                +----------------+----------+---------------------------------------------------------------+
-- |          |                |                | goal     | \(\Gamma\vdash\text{Just} ([ A1,..., An ] ->B):[type,kind] \) |
-- |          |                |                +----------+---------------------------------------------------------------+
-- |          |                | subgoal1       | substLst | \( [ ]  \)                                                    |
-- |          |                |                +----------+---------------------------------------------------------------+
-- |          |                |                | clue     | \(([ ],\text{Nothing})\)                                      |
-- |          |                +----------------+----------+---------------------------------------------------------------+
-- |          |                |                | goal     | \( \Gamma , A1, ... , An  \vdash  \text{Nothing} :  [ B ] \)  |
-- |          |                |                +----------+---------------------------------------------------------------+
-- |          |                | subgoal2       | substLst | \( [ ] \)                                                     |
-- |          |                |                +----------+---------------------------------------------------------------+
-- |          |                |                | clue     | \( ([ ],\text{Nothing}) \)                                    |
-- |          +----------------+----------------+----------+---------------------------------------------------------------+
-- |          |   dside        | \[ \Gamma \vdash (\lambda (n). t) : [ [ A1, A2 , ... , An ]  -> B ] \]                    |
-- |          +----------------+-------------------------------------------------------------------------------------------+
-- |          |   subgoalSet1  |  \(\text{"piIntro" Nothing subgoals dside} \)                                             |
-- |          +----------------+-------------------------------------------------------------------------------------------+
-- |          | output         | \[ [ \text{subgoalSet1} ] \]                                                              |
-- +----------+----------------+-------------------------------------------------------------------------------------------+
-- | typecheck|input           | \(\Gamma \vdash \text{ Just }t1\ (\equiv \lambda (n). t): [l](\equiv[[ A1,...,An ]->B)]\) |
-- |          +----------------+-------------------------------------------------------------------------------------------+
-- |          | assertion      | \( l \text{ is } \Pi \text{ type and } t1 \text{ is } \lambda \text{ type}\)              |
-- |          +----------------+-------------------------------------------------------------------------------------------+
-- |          | forwardedTree  | \( [] \)                                                                                  |
-- |          +----------------+-------------------------------------------------------------------------------------------+
-- |          | subgoals       | \( [ \text{subgoal1},\text{subgoal2} ] \)                                                 |
-- |          +                +----------------+----------+---------------------------------------------------------------+
-- |          |                | subgoal1       | goal     | \(\Gamma \vdash \text{Just} [A1,..., An] ->B : [type,kind]\)  |
-- |          |                |                +----------+---------------------------------------------------------------+
-- |          |                |                | substLst | \( [] \)                                                      |
-- |          |                |                +----------+---------------------------------------------------------------+
-- |          |                |                | clue     | \( ([],\text{Nothing}) \)                                     |
-- |          |                +----------------+----------+---------------------------------------------------------------+
-- |          |                | subgoal2       | goal     | \(\Gamma , A1, A2, ... , An  \vdash \text{Just} t :  [ B ] \) |
-- |          |                |                +----------+---------------------------------------------------------------+
-- |          |                |                | substLst | \( [] \)                                                      |
-- |          |                |                +----------+---------------------------------------------------------------+
-- |          |                |                | clue     | \( ([],\text{Nothing}) \)                                     |
-- |          +----------------+----------------+----------+---------------------------------------------------------------+
-- |          | dSide          | \( \Gamma \vdash (\lambda (n).t) : head(l)\)                                              |
-- |          +----------------+-------------------------------------------------------------------------------------------+
-- |          | subgoalSet     | \( \text{"piIntro" Nothing subgoals dSide}\)                                              |
-- |          +----------------+-------------------------------------------------------------------------------------------+
-- |          | output         | \([\text{subgoalSet}]\)                                                                   |
-- +----------+----------------+-------------------------------------------------------------------------------------------+
-- pi 型の前件及び型付きラムダ型の型部分は正規化されているものとする
piIntro :: Rule

-- | piElim rule
--
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
-- | deduce   | input goal     | \(\Gamma \ \vdash \text{Nothing}:[l] \ (\equiv [love(x,y)])\)                                                    |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | assertion      | \( len(l)=1 \text{ and } head (l) \not \equiv kind \)                                                            |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | forwardTrees   | \( [tree1, tree2, tree3 ] \)                                                                                     |
-- |          |                +-------+----------------------------------------------------------------------------------------------------------+
-- |          |                | tree1 | \( \text{tree of } \Gamma \vdash_F f1 : [u1 : Ha1 , ... , un : Han] -> love(u1,un) \)                    |
-- |          |                |       +---------------+---------+--------------------------------------------------------------------------------+
-- |          |                |       | subgoal1      | goal    | \( \Gamma \vdash \text{Nothing} : [ Ha1 ]  \)                                  |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | substLst| \( [] \)                                                                       |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | clue    | \( ( [], \text{Just} (love(var(-1),var(-n)),love(x,y))) \)                     |
-- |          |                |       +---------------+---------+--------------------------------------------------------------------------------+
-- |          |                |       | subgoal2      | goal    | \( \Gamma \vdash \text{Nothing} : [ Ha2 ]   \)                                 |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | substLst| \( [] \)                                                                       |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | clue    | \( ([], \text{Nothing}) \)                                                     |
-- |          |                |       +---------------+---------+--------------------------------------------------------------------------------+
-- |          |                |       | subgoaln      | goal    | \( \Gamma \vdash \text{Nothing} : [ Han ]  \)                                  |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | substLst| \( [] \)                                                                       |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | clue    | \( ( [], \text{Just} (love(var(n-2) , var(-1)) ,love(x,y)) ) \)                |
-- |          |                |       +---------------+---------+--------------------------------------------------------------------------------+
-- |          |                |       | dSide         | \(\Gamma  \vdash f1 [var(-1),var(-2),...,var(-n)] : love(x,y)  \)                        |
-- |          |                |       +---------------+---------+--------------------------------------------------------------------------------+
-- |          |                |       | subgoalSet1   | \( \text{"piElim" (Just tree1) subgoals dside} \)                                        |
-- |          |                +-------+---------------+------------------------------------------------------------------------------------------+
-- |          |                | tree2 | \( \text{tree of } \Gamma \vdash_F f2 : [u1 : Hb1 , ... , um : Hbm] -> u2(x,u1) \)                       |
-- |          |                |       +---------------+---------+--------------------------------------------------------------------------------+
-- |          |                |       | subgoal1      | goal    | \( \Gamma \vdash \text{Nothing} : [ Hb1 ]  \)                                  |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | substLst| \( [] \)                                                                       |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | clue    | \( ( [], \text{Just} (var(-2)(x, var(-1)),love(x,y))) \)                       |
-- |          |                |       +---------------+---------+--------------------------------------------------------------------------------+
-- |          |                |       | subgoal2      | goal    | \( \Gamma \vdash \text{Nothing} : [ Hb2 ]   \)                                 |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | substLst| \( [] \)                                                                       |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | clue    | \( ([], \text{Just} (var(-1) (x, var(0)),love(x,y))) \)                        |
-- |          |                |       +---------------+---------+--------------------------------------------------------------------------------+
-- |          |                |       | subgoalm      | goal    | \( \Gamma \vdash \text{Nothing} : [ Hbm ]  \)                                  |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | substLst| \( [] \)                                                                       |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | clue    | \( ( [], \text{Just} (love(var(n-2) , var(-1)) ,love(x,y)) ) \)                |
-- |          |                |       +---------------+---------+--------------------------------------------------------------------------------+
-- |          |                |       | dSide         | \(\Gamma  \vdash f2 [var(-1),var(-2),...,var(-n)] : love(x,y)  \)                        |
-- |          |                |       +---------------+---------+--------------------------------------------------------------------------------+
-- |          |                |       | subgoalSet1   | \( \text{"piElim" (Just tree2) subgoals dside} \)                                        |
-- |          |                +-------+---------------+------------------------------------------------------------------------------------------+
-- |          |                | tree3 | \( \text{tree of } \Gamma \vdash_F f3 : [u1 : Hc1 , ... , ul : u2(u1)] -> love(u1,y) \)                  |
-- |          |                |       +---------------+---------+--------------------------------------------------------------------------------+
-- |          |                |       | subgoal1      | goal    | \( \Gamma \vdash \text{Nothing} : [ Hc1 ]  \)                                  |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | substLst| \( [] \)                                                                       |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | clue    | \( ( [(var(-2)(var(-1)),var(-l))], \text{Just} (love(var -1,y), love(x,y))) \) |
-- |          |                |       +---------------+---------+--------------------------------------------------------------------------------+
-- |          |                |       | subgoal2      | goal    | \( \Gamma \vdash \text{Nothing} : [ Hc2 ]   \)                                 |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | substLst| \( [] \)                                                                       |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | clue    | \( ([(var(-1)(var(0)), var(-(l-1)))], \text{Nothing} \)                        |
-- |          |                |       +---------------+---------+--------------------------------------------------------------------------------+
-- |          |                |       | subgoall      | goal    | \( \Gamma \vdash \text{Nothing} : [ u2(u1) ]  \)                               |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | substLst| \( [([], var(-1)), ([], var (-2))] \)                                          |
-- |          |                |       |               +---------+--------------------------------------------------------------------------------+
-- |          |                |       |               | clue    | \( ( [], \text{Nothing}  ) \)                                                  |
-- |          |                |       +---------------+---------+--------------------------------------------------------------------------------+
-- |          |                |       | dSide         | \(\Gamma  \vdash f3 [var(-1),var(-2),...,var(-n)] : love(x,y)  \)                        |
-- |          |                |       +---------------+---------+--------------------------------------------------------------------------------+
-- |          |                |       | subgoalSet1   | \( \text{"piElim" (Just tree3) subgoals dside} \)                                        |
-- |          +----------------+-------+----------------------------------------------------------------------------------------------------------+
-- |          | output         | \( [ subgoalSet1, subgoalSet2, subgoalSet3 ] \)                                                                  |
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
-- | typecheck| input goal     | \( \Gamma \vdash t( \equiv Just f1 [h1, h2, ... , x, ... , hn]) : [l] \ (\equiv [[ A1,  ... , An ] -> B])\)      |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | assertion      | \( len(l)=1 \text{ and } head (l) \not \equiv kind \text{ and } t \text{ is Just App type}\)                     |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | tree           | \( \text{tree of } \Gamma \vdash f1 : [ H1, H2, ... , Hm, ... , Hn] -> a (\equiv love(var (n-m),y) \)            |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | assertion      | \( a [h1/ var(n-1)][h2/var(n-2)]...[x/var(n-m)]...[hn/var(0)] \equiv love(x,y) \)                                |
-- |          +----------------+-----------+--------+---------------------------------------------------------------------------------------------+
-- |          | subgoals       | subgoal1  | goal   | \( \Gamma \vdash \text{Just} h1 : [H1] \)                                                   |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           |substLst| \( [] \)                                                                                    |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           | clue   | \( ([],\text{Nothing}) \)                                                                   |
-- |          |                +-----------+--------+---------------------------------------------------------------------------------------------+
-- |          |                | subgoal2  | goal   | \( \Gamma \vdash \text{Just} h1 : [H2] \)                                                   |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           |substLst| \( [] \)                                                                                    |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           | clue   | \( ([],\text{Nothing}) \)                                                                   |
-- |          |                +-----------+--------+---------------------------------------------------------------------------------------------+
-- |          |                | subgoalm  | goal   | \( \Gamma \vdash \text{Just} h1 : [Hm] \)                                                   |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           |substLst| \( [] \)                                                                                    |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           | clue   | \( ([],\text{Nothing}) \)                                                                   |
-- |          |                +-----------+--------+---------------------------------------------------------------------------------------------+
-- |          |                | subgoaln  | goal   | \( \Gamma \vdash \text{Just} h1 : [Hn] \)                                                   |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           |substLst| \( [] \)                                                                                    |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           | clue   | \( ([],\text{Nothing}) \)                                                                   |
-- |          +----------------+-----------+--------+---------------------------------------------------------------------------------------------+
-- |          | dSide          | \( \Gamma \vdash f1 [h1, h2, ... , x, ... , hn] : love(x,y) \)                                                   |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | subgoalSet     | \( \text{"piElim" (Just tree1) subgoals dside} \)                                                                |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | output         | \( [subgoalSet] \)                                                                                               |
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
-- pi 型の前件及び型付きラムダ型の型部分は正規化されているものとする
piElim :: Rule

-- | piForm
-- 
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
-- | typecheck| input goal     | \( \Gamma \vdash  t (\equiv \text{Just } [ A1, man(var (0)) , ... , An ]  -> B) : [ s ] \)                       |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | assertion      | \( t \text{ is Just } \Pi \text{ type and (members of s are only type or kind) } \)                              |
-- |          +----------------+-----------+--------+---------------------------------------------------------------------------------------------+
-- |          | subgoals       | subgoal1  | goal   | \( \Gamma \vdash \text{Just } A1 : [type,kind] \)                                           |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           |substLst| \( [] \)                                                                                    |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           | clue   | \( (([(var -2 , (man (var -1)))],\text{Nothing})) \)                                        |
-- |          |                +-----------+--------+---------------------------------------------------------------------------------------------+
-- |          |                | subgoal2  | goal   | \( \Gamma , A1 \vdash \text{Just} man(var (-1)) :  [ type , kind ]  \)                      |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           |substLst| \( [([],var(-1))] \)                                                                        |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           | clue   | \( ([],\text{Nothing}) \)                                                                   |
-- |          |                +-----------+--------+---------------------------------------------------------------------------------------------+
-- |          |                | subgoaln  | goal   | \( \Gamma  , A1, man (var(0)), ...  \vdash \text{Just } An : [type,kind] \)                 |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           |substLst| \( [] \)                                                                                    |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           | clue   | \( ([],\text{Nothing}) \)                                                                   |
-- |          |                +-----------+--------+---------------------------------------------------------------------------------------------+
-- |          |                | subgoalB  | goal   | \( \Gamma , A1, man (var(0)), ... , An \vdash \text{Just } B : [s] \)                       |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           |substLst| \( [] \)                                                                                    |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           | clue   | \( ([],\text{Nothing}) \)                                                                   |
-- |          +----------------+-----------+--------+---------------------------------------------------------------------------------------------+
-- |          | dSide          | \( \Gamma \vdash [A1,man(var(0)),...,An] -> B  : s \)                                                            |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | subgoalSet     | \( \text{"piForm" (Nothing) [subgoal] dside} \)                                                                  |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | output         | \( [subgoalSet] \)                                                                                               |
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
piForm :: Rule

-- | sigmaIntro
-- 
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
-- | deduce   | input goal     | \(\Gamma \ \vdash \text{Nothing}:[l] \ (\equiv [ \Sigma ([A1, man(var 0),...,An],B)])\)                          |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | assertion      | \( len(l)=1  \)                                                                                                  |
-- |          +----------------+-------------+----------+-----------------------------------------------------------------------------------------+
-- |          | subgoals       |  subgoal1   |goal      | \( \Gamma \vdash \text{Nothing }: [A1 ]\)                                               |
-- |          |                |             +----------+-----------------------------------------------------------------------------------------+
-- |          |                |             | substLst | \( [] \)                                                                                |
-- |          |                |             +----------+-----------------------------------------------------------------------------------------+
-- |          |                |             | clue     | \( ([ (var -2, (man (var -1))) ] ,\text{Nothing})\)                                     |
-- |          |                +-------------+----------+-----------------------------------------------------------------------------------------+
-- |          |                | subgoal2    | goal     | \( \Gamma , A1 \vdash \text{Nothing } : [man (var (0))]\)                               |
-- |          |                |             +----------+-----------------------------------------------------------------------------------------+
-- |          |                |             | substLst | \( [([],var(-1))] \)                                                                     |
-- |          |                |             +----------+-----------------------------------------------------------------------------------------+
-- |          |                |             | clue     | \( ([  ] ,\text{Nothing})\)                                                             |
-- |          |                +-------------+----------+-----------------------------------------------------------------------------------------+
-- |          |                | subgoaln    | goal     | \( \Gamma , A1 ,man (var(0)) ... \vdash \text{Nothing } : [An]\)                        |
-- |          |                |             +----------+-----------------------------------------------------------------------------------------+
-- |          |                |             | substLst | \( [] \)                                                                                |
-- |          |                |             +----------+-----------------------------------------------------------------------------------------+
-- |          |                |             | clue     | \( ([  ] ,\text{Nothing})\)                                                             |
-- |          |                +-------------+----------+-----------------------------------------------------------------------------------------+
-- |          |                | subgoalB    | goal     | \( \Gamma , A1 ,man(var(0)), ... , An \vdash \text{Nothing } : [B]\)                    |
-- |          |                |             +----------+-----------------------------------------------------------------------------------------+
-- |          |                |             | substLst | \( [] \)                                                                                |
-- |          |                |             +----------+-----------------------------------------------------------------------------------------+
-- |          |                |             | clue     | \( ([  ] ,\text{Nothing})\)                                                             |
-- |          +----------------+-------------+----------+-----------------------------------------------------------------------------------------+
-- |          | dSide          | \( \Gamma \vdash (var -2, ( var -3, (...,( var -(n-1) ,var -n)...))) : \Sigma [A1,man(var(0)),...,An] ,B \)      |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | subgoalSet     | \( \text{"sigmaIntro" Nothing subgoals dSide} \)                                                                 |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | output         | \( [subgoalSet] \)                                                                                               |
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
-- | typecheck| input goal     | \(\Gamma \vdash t (\equiv Just (h1,(h2,(...,(hn,hb))))) :[l] \ (\equiv [ \Sigma ([A1, man(var 0),...,An],B)])\)  |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | assertion      | \(  \text{len}(l)=1 \text{ and t is Just pair type} \)                                                           |
-- |          +----------------+-------------+----------+-----------------------------------------------------------------------------------------+
-- |          | subgoals       |  subgoal1   |goal      | \( \Gamma \vdash \text{Just} h1 : [A1]\)                                                |
-- |          |                |             +----------+-----------------------------------------------------------------------------------------+
-- |          |                |             | substLst | \( [] \)                                                                                |
-- |          |                |             +----------+-----------------------------------------------------------------------------------------+
-- |          |                |             | clue     | \( ([ (var -2, (man (var -1))) ] ,\text{Nothing})\)                                     |
-- |          |                +-------------+----------+-----------------------------------------------------------------------------------------+
-- |          |                | subgoal2    | goal     | \( \Gamma, A1 \vdash \text{Just} h2: [man(var 0)]\)                                     |
-- |          |                |             +----------+-----------------------------------------------------------------------------------------+
-- |          |                |             | substLst | \( [([],var(-1))] \)                                                                     |
-- |          |                |             +----------+-----------------------------------------------------------------------------------------+
-- |          |                |             | clue     | \( ([  ] ,\text{Nothing})\)                                                             |
-- |          |                +-------------+----------+-----------------------------------------------------------------------------------------+
-- |          |                | subgoaln    | goal     | \( \Gamma, A1, man(var 0) ... \vdash \text{Just} hn : [An]\)                            |
-- |          |                |             +----------+-----------------------------------------------------------------------------------------+
-- |          |                |             | substLst | \( [] \)                                                                                |
-- |          |                |             +----------+-----------------------------------------------------------------------------------------+
-- |          |                |             | clue     | \( ([  ] ,\text{Nothing})\)                                                             |
-- |          |                +-------------+----------+-----------------------------------------------------------------------------------------+
-- |          |                | subgoalB    | goal     | \( \Gamma, A1, man(var 0) ... An \vdash \text{Just} hb : [B]\)                          |
-- |          |                |             +----------+-----------------------------------------------------------------------------------------+
-- |          |                |             | substLst | \( [] \)                                                                                |
-- |          |                |             +----------+-----------------------------------------------------------------------------------------+
-- |          |                |             | clue     | \( ([  ] ,\text{Nothing})\)                                                             |
-- |          +----------------+-------------+----------+-----------------------------------------------------------------------------------------+
-- |          | dSide          | \( \Gamma \vdash (var -2, ( var -3, (...,( var -(n-1) ,var -n)...))) : \Sigma [A1,man(var(0)),...,An] ,B \)      |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | subgoalSet     | \( \text{"sigmaIntro" Nothing subgoals dSide} \)                                                                 |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | output         | \( [subgoalSet] \)                                                                                               |
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+

sigmaIntro :: Rule


-- | sigmaForm
-- 
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
-- | typecheck| input goal     | \( \Gamma \vdash  t (\equiv \text{Just } \Sigma ([ A1, man(var 0) , ... , An ], B))) : s \)                      |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | assertion      | \( t \text{ is Just } \Sigma \text{ type and (s is a list contains type) } \)                                    |
-- |          +----------------+-----------+--------+---------------------------------------------------------------------------------------------+
-- |          | subgoals       | subgoal1  | goal   | \( \Gamma \vdash \text{Just } A1 : [type,kind] \)                                           |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           |substLst| \( [] \)                                                                                    |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           | clue   | \( (([(var -2 , (man (var -1)))],\text{Nothing})) \)                                        |
-- |          |                +-----------+--------+---------------------------------------------------------------------------------------------+
-- |          |                | subgoal2  | goal   | \( \Gamma , A1 \vdash \text{Just} man(var (0)) :  [ type , kind ]  \)                       |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           |substLst| \( [([],var(-1))] \)                                                                         |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           | clue   | \( ([],\text{Nothing}) \)                                                                   |
-- |          |                +-----------+--------+---------------------------------------------------------------------------------------------+
-- |          |                | subgoaln  | goal   | \( \Gamma  , A1, man (var(0)), ...  \vdash \text{Just } An : [type,kind] \)                 |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           |substLst| \( [] \)                                                                                    |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           | clue   | \( ([],\text{Nothing}) \)                                                                   |
-- |          |                +-----------+--------+---------------------------------------------------------------------------------------------+
-- |          |                | subgoalB  | goal   | \( \Gamma , A1, man (var(0)), ... , An \vdash \text{Just } B : [\text{type}] \)             |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           |substLst| \( [] \)                                                                                    |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           | clue   | \( ([],\text{Nothing}) \)                                                                   |
-- |          +----------------+-----------+--------+---------------------------------------------------------------------------------------------+
-- |          | dSide          | \( \Gamma \vdash \Sigma [A1,man(var(0)),...,An] -> B  : type \)                                                  |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | subgoalSet     | \( \text{"sigmaForm" (Nothing) subgoals dSide} \)                                                                |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | output         | \( [subgoalSet] \)                                                                                               |
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
sigmaForm :: Rule


-- | membership
-- 
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
-- | deduce   | input goal     | \( \Gamma \vdash  \text{Nothing} : l \)                                                                          |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | assertion      | \( l \equiv [H] \)                                                                                               |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | forwardedTree  | \( [tree1,tree2,...,treen] \)                                                                                    |
-- |          |                +-----------+------------------------------------------------------------------------------------------------------+
-- |          |                | tree1     | \( \text{tree of} \Gamma \vdash h1 : H\)                                                             |
-- |          |                |           +-----------+------------------------------------------------------------------------------------------+
-- |          |                |           | subgoals  | \( [] \)                                                                                 |
-- |          |                |           +-----------+------------------------------------------------------------------------------------------+
-- |          |                |           | dSide     | \( \Gamma \vdash h1 : H \)                                                               |
-- |          |                |           +-----------+------------------------------------------------------------------------------------------+
-- |          |                |           |subgoalSet | \( \text{ "membership" tree1 [ ] dside } \)                                              |
-- |          |                +-----------+-----------+------------------------------------------------------------------------------------------+
-- |          |                | tree2     | \( \text{tree of } \Gamma \vdash h2 : H\)                                                            |
-- |          |                |           +-----------+------------------------------------------------------------------------------------------+
-- |          |                |           | subgoals  | \( [] \)                                                                                 |
-- |          |                |           +-----------+------------------------------------------------------------------------------------------+
-- |          |                |           | dSide     | \( \Gamma \vdash h2 : H \)                                                               |
-- |          |                |           +-----------+------------------------------------------------------------------------------------------+
-- |          |                |           |subgoalSet | \( \text{ "membership" tree2 [ ] dside } \)                                              |
-- |          +----------------+-----------+-----------+------------------------------------------------------------------------------------------+
-- |          | output         | \( [subgoalSet1,subgoalSet2,...,subgoalSetn] \)                                                                  |
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
-- | typecheck| input goal     | \( \Gamma \vdash  \text{Just} t : l \)                                                                           |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | assertion      | \( l \equiv [H] \)                                                                                               |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | forwardedTree  | \( [tree1,tree2,...,treen] \)                                                                                    |
-- |          |                +-----------+------------------------------------------------------------------------------------------------------+
-- |          |                | tree1     | \( \text{tree of } \Gamma \vdash h1 : H\)                                                            |
-- |          |                |           +-----------+------------------------------------------------------------------------------------------+
-- |          |                |           | assertion | \( h1 \equiv t\)                                                                         |
-- |          |                |           +-----------+------------------------------------------------------------------------------------------+
-- |          |                |           | subgoals  | \( [] \)                                                                                 |
-- |          |                |           +-----------+------------------------------------------------------------------------------------------+
-- |          |                |           | dSide     | \( \Gamma \vdash h1 : H \)                                                               |
-- |          |                |           +-----------+------------------------------------------------------------------------------------------+
-- |          |                |           |subgoalSet | \( \text{ "membership" tree1 [ ] dside } \)                                              |
-- |          |                +-----------+-----------+------------------------------------------------------------------------------------------+
-- |          |                | tree2     | \( \text{tree of} \Gamma \vdash h2 : H\)                                                             |
-- |          |                |           +-----------+------------------------------------------------------------------------------------------+
-- |          |                |           | assertion | \( h2 \equiv t \)                                                                        |
-- |          |                |           +-----------+------------------------------------------------------------------------------------------+
-- |          |                |           | subgoals  | \( [] \)                                                                                 |
-- |          |                |           +-----------+------------------------------------------------------------------------------------------+
-- |          |                |           | dSide     | \( \Gamma \vdash h2 : H \)                                                               |
-- |          |                |           +-----------+------------------------------------------------------------------------------------------+
-- |          |                |           |subgoalSet | \( \text{ "membership" tree2 [ ] dside } \)                                              |
-- |          +----------------+-----------+-----------+------------------------------------------------------------------------------------------+
-- |          | output         | \( [subgoalSet1,subgoalSet2,...,subgoalSetn] \)                                                                  |
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
membership :: Rule


-- | ax
--
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
-- | deduce   | input goal     | \( \Gamma \vdash  \text{Nothing} : l \)                                                                          |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | assertion      | \( l \equiv [kind] \)                                                                                            |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | dSide          | \( \Gamma \vdash type : kind \)                                                                                  |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | subgoalSet     | \( \text{"ax" Nothing [ ] dside} \)                                                                              |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | output         | \( [subgoalSet] \)                                                                                               |
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
-- | typecheck| input goal     | \( \Gamma \vdash  \text{Just} t : l \)                                                                           |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | assertion      | \( l \equiv [kind] \text{ and } t \equiv \text{type}\)                                                           |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | dSide          | \( \Gamma \vdash type : kind \)                                                                                  |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | subgoalSet     | \( \text{"ax" Nothing [ ] dside} \)                                                                              |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | output         | \( [subgoalSet] \)                                                                                               |
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
ax :: Rule

-- | dne
-- 
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
-- | deduce   | input goal     | \( \Gamma \vdash  \text{Nothing} : l \)                                                                          |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | assertion      | \( l \equiv [H] \text{ and }H \not \equiv \bot \text{ and }H \not \equiv \text{type and H is not already DNEd }\)|
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | forwardedtrees | \( [] \)                                                                                                         |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | subgoal        | \( \Gamma \vdash \text{Nothing} : [\neg \neg H] \)                                                               |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | dSide          | \(\Gamma \vdash dne(var(-2)) : H \)                                                                              |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | subgoalSet     | \( \text{"dne" Nothing [ subgoal1 ] dside} \)                                                                    |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | output         | \( [subgoalSet] \)                                                                                               |
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
-- | typecheck| input goal     | \( \Gamma \vdash  \text{Just} t : l \)                                                                           |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | assertion      | \( t \text{ is dne(a) and } l \equiv [H] \)                                                                      |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | forwardedtrees | \( [] \)                                                                                                         |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | subgoal        | \( \Gamma \vdash \text{Just} a : [\neg \neg H] \)                                                                |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | dSide          | \(\Gamma \vdash dne(a)  : H \)                                                                                   |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | subgoalSet     | \( \text{"dne" Nothing [ subgoal1 ] dside} \)                                                                    |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | output         | \( [subgoalSet] \)                                                                                               |
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
dne :: Rule

-- | If isAllow is true, this function returns l only when lst includes goaltype. If isAllow is false, this function returns l only when lst excludes goaltype.
acceptableType :: QT.DTTrule -> Goal -> Bool -> [ProofType] -> (M.Maybe ProofType,T.Text) 
acceptableType label goal isAllow lst =
  case typesFromGoal goal of
    [l] -> if isAllow ==  (any (l==) lst) then (Just l,"") else (Nothing,exitMessage (TypeMisMatch l) label)
    _ -> (Nothing,exitMessage MultipleTypes label)

-- e1:entity -> type , [u0:entity,u1:entity,u2:entity->type,u3:(var 0)(var 2),u4:(var 4)(var 2)]=>x
-- convertToSubstableTerm ((var 0)(var 2),3) 0 -> u3:(var -3)(var -1)
-- convertToSubstableTerm ((var 4)(var 2),4) 0 -> u4:(var 0)(var -2)
-- convertToSubstableTerm ((var 0)(var 2),3) 1 -> u3:(var -2)(var 0)
-- convertToSubstableTerm ((var 4)(var 2),3) 1 -> u4:(var 1)(var -1)
convertToSubstableTerm :: (ProofType,Int) -> Int -> ProofType
convertToSubstableTerm (childTerm,idAboutChild) idAboutTarget = A.shiftIndices childTerm (idAboutTarget-idAboutChild) 0 

--  input           |  u5:[ y0:entity, y1:var 1(var 0),y2:entity ->type,y3:var3(var 1)] => var 1(var 3) 
--  parentLsts      |  [(4,[1,3]),(3,[1,3]),(2,[]),(1,[0,1]),(0,[])]     

membership goal = 
  case acceptableType QT.Var goal False [] of
    (Nothing,message) -> ([],message)
    (Just arrowType,_) -> 
      let (sig,var) = conFromGoal goal
          forwardedTrees = WB.trees $ F.forwardContext sig var
          trees = L.nub $
            filter 
              (\tree ->
                  let
                    x = A.downSide' tree
                    xCon = A.envfromAJudgment x
                    xType = A.typefromAJudgment x
                  in
                    A.sameTerm ((sig,var),arrowType) (xCon,xType))
              forwardedTrees
          subgoalsets =
            map
              (\tree ->
                let
                  dSide = A.downSide' tree
                in SubGoalSet QT.Var (M.Just tree) [] dSide
              )
              trees
      in (subgoalsets,"")


{--
  in B.debugLog (sig,var) aType depth setting "membership" (forwardResult{B.trees = matchLst,B.rStatus = B.sStatus setting})


--}

piElim goal =
  case acceptableType QT.PiE goal False [(A.Conclusion UDdB.Kind)] of
    (Nothing,message) -> ([],message)
    (Just arrowType,_) -> 
      let (sig,var) = conFromGoal goal
          maybeTerm = termFromGoal goal
          justTerms = let -- [b,a,f] for (f(a))(b)
            maybeTermsInAppTerm appTerm =
                case appTerm of 
                  A.ArrowApp f t ->
                    t : (maybeTermsInAppTerm f)
                  f -> [f]
            in maybe [] maybeTermsInAppTerm maybeTerm
      in if length justTerms == 1 
        then ([],exitMessage (TermMisMatch maybeTerm) QT.PiE)
        else
          let
            argNumAndFunctions = -- return [(the num of args,functionTree) pair]
              let forwarded = F.forwardContext sig var
                  argNumAndFunction functionTree r = -- return Maybe (the num of args,functionTree) pair "the num of args" is the num `function` need to gain `r`
                    let canBeFunctionJudgment = A.downSide' functionTree
                        canBeFunctionTerm = A.termfromAJudgment canBeFunctionJudgment
                    in
                      case A.typefromAJudgment canBeFunctionJudgment of
                        A.Arrow env b -> 
                          case r of
                            A.Arrow env' b' -> 
                              let d = length env - length env'
                              in 
                                if d >= 0
                                    &&
                                    A.canBeSame (length env) b (A.shiftIndices b' d 0)
                                    &&
                                    ( null justTerms
                                      ||
                                    (((length justTerms) == (d+1)) && canBeFunctionTerm == last justTerms)
                                    )
                                    &&
                                    all (\((s,num),t) -> A.canBeSame (num + d) s t) 
                                      (zip (zip (take (length env') env) [0..]) (map (\c' -> A.shiftIndices c' d 0) env'))
                                then M.Just (d,functionTree) 
                                else M.Nothing
                            b' -> 
                              if 
                                A.canBeSame (length env) b (A.shiftIndices b' (length env) 0) 
                                &&
                                (
                                null justTerms
                                  ||
                                (((length justTerms) == (1 + (length env))) && canBeFunctionTerm == last justTerms)
                                )
                              then M.Just (length env,functionTree) 
                              else M.Nothing
                        _ -> M.Nothing
              in
                M.mapMaybe 
                  (\forwardedTree -> argNumAndFunction forwardedTree arrowType) 
                  (WB.trees forwarded)
            subgoalsetForFunctions =
              let subgoalsetForFunction (argNum,functionTree) = 
                    let functionJudgment = A.downSide' functionTree
                        dSide = 
                          let
                            arrowTerm = A.betaReduce $ foldl A.ArrowApp (A.termfromAJudgment functionJudgment) $ reverse $ if null justTerms then map A.aVar  [(-argNum)..(-1)] else init justTerms
                          in A.AJudgment sig var arrowTerm arrowType
                        unformattedFunctionType = -- format the function ; if the (function,result) is ([a,b]->c,b->c), the term will be ([a]->([b]->c))
                          case A.typefromAJudgment functionJudgment of
                            A.Arrow formerLst latter ->
                              let notArgFormerNum = length formerLst - argNum
                              in A.Arrow (drop notArgFormerNum formerLst) (A.arrowNotat $ A.Arrow (take notArgFormerNum formerLst) latter)  
                        parentLsts = -- ex : [(4,[1,3]),(3,[1,3]),(2,[]),(1,[0,1]),(0,[])] for [ y0:entity, y1:var 1(var 0),y2:entity ->type,y3:var3(var 1)] => var 1(var 3) 
                          case unformattedFunctionType of A.Arrow args result -> reverse $ zipWith (\term num -> (num,A.varsInaTerm term)) (reverse (result:args)) [0..]
                        subgoalForArg idInLstFromOld = 
                          case unformattedFunctionType of
                            A.Arrow args result ->
                              let targetArg = A.shiftIndices ((reverse args) !! idInLstFromOld) (-idInLstFromOld) 0
                                  goal = Goal sig var (if null justTerms then M.Nothing else (M.Just $ (reverse $ init justTerms) !! idInLstFromOld)) [targetArg]
                                  substLst = 
                                    M.maybe [] (\parentIds -> map (\refNum -> SubstSet [] (convertToSubstableTerm (A.aVar refNum,idInLstFromOld) 0) ) (filter (< idInLstFromOld) $ L.sort parentIds)) (lookup idInLstFromOld parentLsts)
                                  clueWithResult = case head parentLsts of (resultIdFromOld,parentLst) -> if any (==resultIdFromOld-idInLstFromOld-1) parentLst then M.Just (convertToSubstableTerm (result,resultIdFromOld) idInLstFromOld,arrowType) else M.Nothing
                                  clueWithArg =  -- y0-clueAboutArg | [(var 0 (var -1),var -2=-1+0-1)] / y1-clueAboutArg | [(var 1 (var -1),var -3=-1+1-3)]
                                      M.mapMaybe (\(argIdFromOld,parentLst) -> if any (==argIdFromOld-idInLstFromOld-1) parentLst && ( argIdFromOld < argNum ) then M.Just (convertToSubstableTerm ((reverse args) !! argIdFromOld,argIdFromOld) idInLstFromOld,A.aVar (-1+idInLstFromOld-argIdFromOld)) else M.Nothing) parentLsts
                              in SubGoal goal substLst (clueWithArg,clueWithResult)
                        subgoals = {-- D.trace ("parentLst for " ++ (show unformattedFunctionType) ++ " is " ++ (show parentLsts)) $--}  map subgoalForArg [0..(argNum-1)] 
                    in SubGoalSet QT.PiE (M.Just functionTree) subgoals dSide
              in map subgoalsetForFunction argNumAndFunctions
      in (subgoalsetForFunctions,"")


piIntro goal =
  case acceptableType QT.PiI goal False [] of
  (Nothing,message) -> ([],message)
  (Just arrowType,_) -> 
    case arrowType of
      A.Arrow l t ->
        let (sig,var) = conFromGoal goal
            subgoalsets = let
                canBeConclusionTerm = maybe M.Nothing (\term -> if A.canBeSame 0 dSideterm term then M.Just (A.rmLam (length l) term) else M.Nothing) (termFromGoal goal)
                dSideterm = maybe (A.betaReduce $ A.addLam (length l) (A.aVar (-2))) id canBeConclusionTerm
                dSide = A.AJudgment sig var dSideterm arrowType
                subgoal1 = let
                    goal = Goal sig var (M.Just arrowType) [A.aType,A.Conclusion UDdB.Kind]
                  in SubGoal goal [] ([],M.Nothing)
                subgoal2 = let
                    goal = Goal sig (l ++ var) canBeConclusionTerm [t]
                  in SubGoal goal [] ([],M.Nothing)
              in [SubGoalSet QT.PiI M.Nothing [subgoal1,subgoal2] dSide]
        in (subgoalsets,"")
      _ -> ([],exitMessage (TypeMisMatch arrowType) QT.PiI)


piForm goal = undefined

sigmaIntro goal = undefined

sigmaForm goal = undefined

ax goal = undefined

dne goal = undefined

-- | dediuce
deduce :: Goal -> WB.Depth -> WB.Setting -> WB.Result
deduce goal depth setting = undefined