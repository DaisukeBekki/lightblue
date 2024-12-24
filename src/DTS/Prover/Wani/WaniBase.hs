{-# LANGUAGE OverloadedStrings #-}
{-|
  Module      : DTS.Wani.WaniBase
  Definitions for wani
-}
module DTS.Prover.Wani.WaniBase (
   -- * Parameters
    ATerm,
    AType,
    Depth,
    ProofMode(..),
    Status(..),
    Setting(..),
    Result(..),
    -- * Defaults
    statusDef,
    settingDef,
    resultDef,
    -- * Rules
    DeduceRule,
    TypecheckRule,
    -- * Functions
    mergeResult,
    mergeStatus,
    -- ** Debug functions
    debugLogWithTerm,
    debugLog,
    -- ** Backward Inference Term
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
    generatedTempTerm,
    Clue(..),
    SubGoalSet(..),
    ProofType(..),
    ProofTerm(..),
    convertToSubstableTerm,
    acceptableType,
    exitMessage,
    ExitReason(..)
) where

import qualified DTS.DTTdeBruijn as DdB  -- UDTT
import qualified DTS.Prover.Wani.Arrowterm as A
import qualified Interface.Tree as UDT
import qualified DTS.QueryTypes as QT

import qualified Data.Text.Lazy as T 
import qualified Data.List as L 
import qualified Data.Maybe as M
import qualified Debug.Trace as D

type ATerm = A.Arrowterm
type AType = A.Arrowterm
type Depth = Int

type DeduceRule = A.SAEnv -> A.AEnv -> AType -> Depth -> Setting -> Result
type TypecheckRule = A.SAEnv -> A.AEnv -> ATerm -> AType -> Depth -> Setting -> Result

data ProofMode = Plain | WithDNE | WithEFQ deriving (Show,Eq)

data Status = Status 
  {failedlst :: [(A.Context,ATerm,AType)], -- ^ Once wani failed to @typecheck@, wani add the tuple to this list
   deduceNgLst :: [(A.Context,AType)], -- ^ Once wani failed to @deduce@, wani add the pair to this list
   usedMaxDepth ::Depth, 
   allProof :: Bool -- ^ In the bottom of the tree, one proof is enough to judge whether the hypo is true or not.
  }deriving (Show,Eq)

data Setting = Setting 
  {mode :: ProofMode,
   falsum :: Bool,
   maxdepth :: Depth,
   maxtime :: Int,
   debug :: Bool,
   sStatus :: Status,
   ruleConHojo :: String} deriving (Show,Eq)

data Result = Result
  {trees :: [UDT.Tree A.Arrowrule A.AJudgment],
   errMsg :: T.Text,
   rStatus :: Status} deriving (Show,Eq)

mergeResult :: Result -> Result -> Result
mergeResult rs1 rs2 =
  Result{trees = (trees rs1) ++ (trees rs2),errMsg =  (T.append (errMsg rs1)  (errMsg rs2)),rStatus = mergeStatus (rStatus rs1) (rStatus rs2)}

mergeStatus :: Status -> Status -> Status
mergeStatus st1 st2 =
  Status {failedlst = L.nub(concatMap failedlst [st1,st2]),usedMaxDepth = maximum (map usedMaxDepth [st1,st2]),deduceNgLst=L.nub(concatMap deduceNgLst [st1,st2]),allProof = (allProof st1) || (allProof st2)}

statusDef :: Status
statusDef = Status{failedlst=[],usedMaxDepth = 0,deduceNgLst=[],allProof = False}

settingDef :: Setting
settingDef = Setting{mode = Plain,falsum = True,maxdepth = 9,maxtime = 100000,debug = False,sStatus = statusDef,ruleConHojo = "sub"}

resultDef :: Result
resultDef = Result{trees = [],errMsg = "",rStatus = statusDef}

debugLog :: A.Context -> AType -> Depth -> Setting -> T.Text -> a -> a
debugLog con = debugLogWithTerm con (A.Conclusion $ DdB.Con $T.pack "?")

debugLogWithTerm :: A.Context -> ATerm -> AType -> Depth -> Setting -> T.Text -> a -> a
debugLogWithTerm (sig,var) term target depth setting label answer=
  if debug setting
    then
      D.trace
        (L.replicate (2*depth) ' ' ++ show depth ++ " " ++(if allProof (sStatus setting) then "all " else "")++   T.unpack label ++ " " ++ show (A.AJudgment sig var term target)++ " ")
        answer
    else answer

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
      let wrappedTerm = maybe (A.Conclusion $ DdB.Con "?") id maybeTerm
      in L.intercalate " / " $ map (\(num,proofType) -> "GoalOption-" ++ (show num) ++ "/" ++ (show $ length proofTypes) ++ (show $ A.AJudgment sig var wrappedTerm proofType)) $ zip [1..] proofTypes

conFromGoal :: Goal -> (A.SAEnv,A.AEnv)
conFromGoal (Goal sig var _ _ ) = (sig,var)

termFromGoal :: Goal -> Maybe ProofTerm
termFromGoal (Goal _ _ maybeProofTerm _ ) = maybeProofTerm

typesFromGoal :: Goal -> [ProofType]
typesFromGoal (Goal _ _ _ proofTypes) = proofTypes

type Rule = Goal -> Setting -> ([SubGoalSet],T.Text)

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
    ProofTerm -- ^ The target is notated as `A.aVar` number. The way the numbers are assigned follow De Bruijn. That is, 0 for itself and 1+ for the left-minigoal-proofterm.
    Int -- ^ distance; The value to the variable adjacent to the left is 0 
  deriving (Eq)


instance Show SubstSet where
  show term = case term of
    SubstSet substTerms target num -> 
      let termText = show substTerms
      in "[ "++ termText ++ " / (" ++ (show num) ++"= "++(show target)++")]"

type SubstLst = [SubstSet]

substPrefix :: Char
substPrefix = 's'

generatedTempTerm :: A.Arrowterm -> T.Text -> A.Arrowterm
generatedTempTerm origin id =
  let gen =  T.concat [(T.singleton substPrefix),id]
  in 
    if origin == A.arrowSubst origin (A.aCon gen) (A.aCon "dummy")
    then A.aCon gen
    else generatedTempTerm origin gen

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

exitMessage :: ExitReason -> QT.DTTrule ->T.Text
exitMessage reasonCode label = 
  let message = case reasonCode of
        MultipleTypes -> "Goal has multiple types"
        TypeMisMatch aterm -> T.concat [T.pack $ show aterm, " is not acceptable type"]
        TermMisMatch aterm -> T.concat [T.pack $ show aterm, " is not acceptable term"]
        Other output -> output
  in T.concat [message," in ",T.pack (show label),"\n"]


data ExitReason = MultipleTypes | TypeMisMatch ProofType | TermMisMatch (M.Maybe ProofTerm) | Other T.Text

-- | If isAllow is true, this function returns l only when lst includes goaltype. If isAllow is false, this function returns l only when lst excludes goaltype.
acceptableType :: QT.DTTrule -> Goal -> Bool -> [ProofType] -> (M.Maybe ProofType,T.Text) 
acceptableType label goal isAllow lst =
  case typesFromGoal goal of
    [l] -> if isAllow ==  (any (l==) lst) then (Just l,"") else (Nothing,exitMessage (TypeMisMatch l) label)
    _ -> (Nothing,exitMessage MultipleTypes label)

-- e1:entity -> type , [u0:entity,u1:entity,u2:entity->type,u3:(var 0)(var 2),u4:(var 4)(var 2)]=>x
-- convertToSubstableTerm ((var 0)(var 2),3) 0 -> u3:(var -3)(var -1) s2 s0
-- convertToSubstableTerm ((var 4)(var 2),4) 0 -> u4:(var 0)(var -2) (var0) s1
-- convertToSubstableTerm ((var 0)(var 2),3) 1 -> u3:(var -2)(var 0) 
-- convertToSubstableTerm ((var 4)(var 2),3) 1 -> u4:(var 1)(var -1)

-- convertToSubstableTerm ((var 0)(var 2),3) 0 -> u3:(var -4)(var -2)
-- convertToSubstableTerm ((var 4)(var 2),4) 0 -> u4:(var 0)(var -3)
-- convertToSubstableTerm ((var 0)(var 2),3) 1 -> u3:(var -3)(var 0)
-- convertToSubstableTerm ((var 4)(var 2),3) 1 -> u4:(var 1)(var -2)
convertToSubstableTerm :: (ProofType,Int) -> Int -> ProofType
convertToSubstableTerm (childTerm,idAboutChild) idAboutTarget = A.shiftIndices childTerm (idAboutTarget-idAboutChild) 0