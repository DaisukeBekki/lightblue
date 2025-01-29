{-# LANGUAGE OverloadedStrings #-}
module DTS.Prover.Wani.BackwardRules
(
  -- * Rules
  piIntro,
  piElim,
  piForm,
  sigmaIntro,
  sigmaForm,
  eqForm,
  membership,
  dne,
  efq
) where

import qualified DTS.DTTdeBruijn as DdB   -- DTT
import qualified DTS.UDTTdeBruijn as UDdB
import qualified DTS.Prover.Wani.Arrowterm as A -- Aterm
import qualified Interface.Tree as UDT
import qualified DTS.QueryTypes as QT

import qualified DTS.Prover.Wani.WaniBase as WB 
import qualified DTS.Prover.Wani.Forward as F

import qualified Data.Text.Lazy as T 
import qualified Data.List as L 
import qualified Data.Maybe as M
import qualified Debug.Trace as D


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
piIntro :: WB.Rule
piIntro goal setting = 
  case WB.acceptableType QT.PiI goal False [] of
  (Nothing,message) -> ([],message)
  (Just arrowType,_) -> 
    case arrowType of
      A.Arrow l t ->
        let (sig,var) = WB.conFromGoal goal
            subgoalsets = let
                canBeConclusionTerm = A.rmLam (length l) (WB.termFromGoal goal)-- maybe M.Nothing (\term -> (A.rmLam (length l) term)) (WB.termFromGoal goal)
                dSideterm = maybe (A.betaReduce $ A.addLam (length l) (A.aVar (-1))) id  canBeConclusionTerm
                dSide = A.AJudgment sig var dSideterm arrowType
                subgoal1 = let
                    goal1 = WB.Goal sig (l ++ var) canBeConclusionTerm [t]
                  in WB.SubGoal goal1 [] ([],M.Nothing)
                subgoal2 = let
                    goal2 = WB.Goal sig var (M.Just arrowType) [A.aType,A.Conclusion DdB.Kind]
                  in WB.SubGoal goal2 [] ([],M.Nothing)
              in [WB.SubGoalSet QT.PiI M.Nothing [subgoal1,subgoal2] dSide]
        in (subgoalsets,"")
      _ -> ([],WB.exitMessage (WB.TypeMisMatch arrowType) QT.PiI)

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
piElim :: WB.Rule
piElim goal setting =
  case WB.acceptableType QT.PiE goal False [(A.Conclusion DdB.Kind)] of
    (Nothing,message) -> -- point1 : typeMisMatch
      ([],message)
    (Just arrowType,_) -> 
      let (sig,var) = WB.conFromGoal goal
          maybeTerm = WB.termFromGoal goal
          termsInProofTerm = let -- [b,a,f] for (f(a))(b)
            maybeTermsInAppTerm appTerm =
                case appTerm of 
                  A.ArrowApp f t ->
                    t : (maybeTermsInAppTerm f)
                  f -> [f]
            in maybe [] maybeTermsInAppTerm maybeTerm
          termIsNotAppType = (length termsInProofTerm == 1) -- When termsInProofTerm is a list with one element, the term is not appType
          isDeduce = null termsInProofTerm
          (env',b') = case arrowType of A.Arrow env b -> (env,b) ; _ -> ([],arrowType)
      in if termIsNotAppType -- point2 : termMisMatch
        then ([],WB.exitMessage (WB.TermMisMatch maybeTerm) QT.PiE)
        else
          let
            argNumAndFunctions = -- return [(the num of args,functionTree) pair]
              let forwarded = F.forwardContext sig var
                  argNumAndFunction functionTree = -- return Maybe (the num of args,functionTree) pair "the num of args" is the num `function` need to gain `r`
                    let canBeFunctionJudgment = A.downSide' functionTree
                        canBeFunctionTerm = A.termfromAJudgment canBeFunctionJudgment
                    in
                      case A.typefromAJudgment canBeFunctionJudgment of
                        A.Arrow env b -> 
                          let d = (length env) - (length env')
                          in 
                            if  -- checkif deduce or Typecheck
                              ( isDeduce
                                || -- if typecheck, we have to use only function specified in the term
                              (((length termsInProofTerm) == (d+1)) && canBeFunctionTerm == last termsInProofTerm)
                              )
                              && -- If target is Arrow type such as A->B, we can use only functions with more arguments than arrowType such as C->A->B which has 2 args.
                              d >= 0
                              && -- exclude A -> C for proofsearch about B
                              A.canBeSame d (A.betaReduce $ A.arrowNotat $ A.Arrow (take (length env') env) b) (A.betaReduce $ A.arrowNotat $ A.shiftIndices (A.Arrow env' b') d 0)
                            then M.Just (d,functionTree) 
                            else M.Nothing
                        _ -> M.Nothing
              in
                M.mapMaybe argNumAndFunction (WB.trees forwarded)
            subgoalsetForFunctions =
              let subgoalsetForFunction (argNum,functionTree) = 
                    let functionJudgment = A.downSide' functionTree
                        A.Arrow args result = -- format the function ; if the (function,result) is ([a,b]->c,b->c), the term will be ([a]->([b]->c))
                              let A.Arrow formerLst latter = A.typefromAJudgment functionJudgment
                                  notArgFormerNum = length formerLst - argNum
                              in A.Arrow (drop notArgFormerNum formerLst) (A.arrowNotat $ A.Arrow (take notArgFormerNum formerLst) latter)  
                        dSide =
                          let
                            arrowTerm = M.maybe (A.betaReduce $ foldl A.ArrowApp (A.termfromAJudgment functionJudgment) (reverse $ map A.aVar [(-argNum)..(-1)])) id maybeTerm
                            arrowType' = foldl (\r (old,newNegate) -> A.arrowSubst r (A.aVar $ negate newNegate) (A.aVar old)) (A.shiftIndices result (negate $ length args) 0) (zip (reverse [0..(argNum-1)]) [1..])
                          in A.AJudgment sig var arrowTerm arrowType'
                        subgoals =
                          let
                            parentLsts = -- ex : [(4,[1,3]),(3,[1,3]),(2,[]),(1,[0,1]),(0,[])] for [ y0:entity, y1:var 1(var 0),y2:entity ->type,y3:var3(var 1)] => var 1(var 3) 
                              reverse $ zipWith (\term num -> (num,A.varsInaTerm term)) (reverse (result:args)) [0..]
                            subgoalForArg idInLstFromOld = 
                              let origin = (reverse args) !! idInLstFromOld
                                  parentLst' = M.maybe [] (\parentIds -> filter (< idInLstFromOld) $ L.sort parentIds) (lookup idInLstFromOld parentLsts)
                                  substLst = map (\num -> (WB.SubstSet [] (WB.generatedTempTerm origin (T.pack $ show num)) num)) $ filter (0<=) parentLst'
                                  targetArg = A.shiftIndices (foldl (\target (WB.SubstSet lst term num) -> A.arrowSubst target term (A.aVar num)) origin substLst) (-idInLstFromOld) 0
                                  goal' = WB.Goal sig var (if isDeduce then M.Nothing else (M.Just $ (reverse $ init termsInProofTerm) !! idInLstFromOld)) [targetArg]
                                  (substLstForResult,clueWithResult) = -- Maybe (ProofTerm,ProofTerm)
                                    let (resultIdFromOld,parentLst) = (head parentLsts) in 
                                      if any (==resultIdFromOld-idInLstFromOld-1) parentLst 
                                        then ([],M.Just (WB.convertToSubstableTerm (result,resultIdFromOld) idInLstFromOld,arrowType))
                                        else ([],M.Nothing)
                                  clueWithArg =  -- [(ProofTerm,ProofType)] y0-clueAboutArg | [(var 0 (var -1),var -2=-1+0-1)] / y1-clueAboutArg | [(var 1 (var -1),var -3=-1+1-3)]
                                    M.mapMaybe (\(argIdFromOld,parentLst) -> if any (==argIdFromOld-idInLstFromOld-1) parentLst && ( argIdFromOld < argNum ) then M.Just (WB.convertToSubstableTerm ((reverse args) !! argIdFromOld,argIdFromOld) idInLstFromOld,A.aVar (-1+idInLstFromOld-argIdFromOld)) else M.Nothing) parentLsts
                                in WB.SubGoal goal' (L.nub $ substLstForResult ++ substLst) (clueWithArg,clueWithResult)
                          in map subgoalForArg [0..(argNum-1)] 
                    in WB.SubGoalSet QT.PiE (M.Just functionTree) subgoals dSide
              in map subgoalsetForFunction argNumAndFunctions
      in (subgoalsetForFunctions,"")

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
-- |          |                |           | clue   | \( ([],\text{Nothing}) \)                                                                   |
-- |          |                +-----------+--------+---------------------------------------------------------------------------------------------+
-- |          |                | subgoal2  | goal   | \( \Gamma , A1 \vdash \text{Just} man(var 0) :  [ type , kind ]  \)                         |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           |substLst| \( [] \)                                                                                    |
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
piForm :: WB.Rule
piForm goal setting = 
  case WB.acceptableType QT.PiF goal True [(A.Conclusion DdB.Type),(A.Conclusion DdB.Kind)] of
    (Nothing,message) -> -- point1 : typeMisMatch
      ([],message)
    (Just arrowType,_) -> 
      case WB.termFromGoal goal of
        M.Just (A.Arrow con res) ->
          let (sig,var) = WB.conFromGoal goal
              subgoalset = 
                let dside = A.AJudgment sig var (A.Arrow con res) arrowType
                    subgoals = 
                      let subgoalForMem idInLstFromOld = 
                            let
                              goal = 
                                WB.Goal 
                                  sig 
                                  ((drop ((length con) +1 - idInLstFromOld) (res:con)) ++ var) 
                                  (M.Just ((reverse $res:con) !! idInLstFromOld))
                                  [A.aType,A.Conclusion DdB.Kind]
                            in WB.SubGoal goal [] ([],M.Nothing)
                      in map subgoalForMem [0..(length con)]
                in WB.SubGoalSet QT.PiF M.Nothing subgoals dside
          in ([subgoalset],"")
        term ->  -- if term is M.Nothing or M.Just `not Arrow type`, return WB.TermMisMatch
          ([],WB.exitMessage (WB.TermMisMatch term) QT.PiF)

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
-- |          |                |             | substLst | \( [([],var(-1))] \)                                                                    |
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
-- |          |                |             | substLst | \( [([],var(-1))] \)                                                                    |
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

sigmaIntro :: WB.Rule
-- targetSigma = A.ArrowSigma' [A.Arrow [A.aCon $ T.pack "A"] (A.ArrowApp (A.aCon $ T.pack "B") (A.aVar 0))] (A.Arrow [A.aCon $ T.pack "A"] (A.ArrowApp (A.ArrowApp (A.aCon $ T.pack "C") (A.aVar 0)) (A.ArrowApp (A.aVar 1) (A.aVar 0)))) 
-- について、shiftIndices term -1 0 が A -> C 0 0 0 になっちゃう問題。shiftIndices  -1 を使うのが向いていない説がある。
sigmaIntro goal setting = 
  case WB.acceptableType QT.SigmaI goal False [] of
    (Nothing,message) -> -- point1 : typeMisMatch
      ([],message)
    (Just (A.ArrowSigma' for lat),_) -> 
      let (sig,var) = WB.conFromGoal goal
          maybeTerm = WB.termFromGoal goal
          termsInProofTerm = let -- [a,b,c] for (a,(b,c))
            maybeTermsInPairTerm pairTerm =
                case pairTerm of 
                  A.ArrowPair f t ->
                    f : (maybeTermsInPairTerm t)
                  f -> [f]
            in maybe [] maybeTermsInPairTerm maybeTerm
          termIsNotPairType = (length termsInProofTerm == 1) -- When termsInProofTerm is a list with one element, the term is not appType
          isDeduce = null termsInProofTerm
      in if termIsNotPairType -- point2 : termMisMatch
        then ([],WB.exitMessage (WB.TermMisMatch maybeTerm) QT.SigmaI)
        else
          let subgoalset =  let 
                dSide = let
                  memNum = length $ lat:for
                  arrowTerm = maybe (foldr A.ArrowPair (A.aVar (- memNum)) (map A.aVar (reverse $ [(-(memNum-1))..(-1)]))) id maybeTerm
                  in A.AJudgment sig var arrowTerm (A.ArrowSigma' for lat)
                subgoals =
                  let parentLsts = -- ex : [(4,[1,3]),(3,[1,3]),(2,[]),(1,[0,1]),(0,[])] for [ y0:entity, y1:var 1(var 0),y2:entity ->type,y3:var3(var 1),var 1(var 3)] 
                          reverse $ zipWith (\term num -> (num,A.varsInaTerm term)) (reverse (lat:for)) [0..]
                      subgoalForMem idInLstFromOld = 
                        let origin = (reverse $ lat:for) !! idInLstFromOld
                            parentLst' = M.maybe [] (\parentIds -> filter (\num -> 0 <= num && num < idInLstFromOld) $ L.sort parentIds) (lookup idInLstFromOld parentLsts)
                            substLst = map (\num ->  (WB.SubstSet [] (WB.generatedTempTerm origin (T.pack $ show num)) num)) parentLst'
                            targetMem = A.shiftIndices (foldl (\target (WB.SubstSet lst term num) -> A.arrowSubst target term (A.aVar num)) origin substLst) (-idInLstFromOld) 0
                            goal' = WB.Goal sig var (if isDeduce then M.Nothing else M.Just$ termsInProofTerm !! idInLstFromOld) [targetMem]
-- ここ
                            clues = --
                              M.mapMaybe 
                                (\(argIdFromOld,parentLst) -> 
                                  if any (==argIdFromOld-idInLstFromOld-1) parentLst 
                                    then M.Just (
                                      WB.convertToSubstableTerm ((reverse (lat:for)) !! argIdFromOld,argIdFromOld) idInLstFromOld,
                                      A.aVar (-1+idInLstFromOld-argIdFromOld)) 
                                    else M.Nothing)
                                parentLsts
                        in WB.SubGoal goal' substLst (clues,M.Nothing)
                  in map subgoalForMem [0..(length for)]
                in WB.SubGoalSet QT.SigmaI M.Nothing subgoals dSide
            in ([subgoalset],"")
    (Just a,message) -> -- point3 : typeMisMatch
      ([],WB.exitMessage (WB.TypeMisMatch a) QT.SigmaI)


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
sigmaForm :: WB.Rule

sigmaForm goal setting = 
  case WB.acceptableType QT.SigmaF goal True [(A.Conclusion DdB.Type)] of
    (Nothing,message) -> -- point1 : typeMisMatch
      ([],message)
    (Just arrowType,_) ->
      case WB.termFromGoal goal of
        M.Just (A.ArrowSigma' con res) -> 
          let (sig,var) = WB.conFromGoal goal
              subgoalset = 
                let dside = A.AJudgment sig var (A.ArrowSigma' con res) arrowType
                    subgoals = 
                      let subgoalForMem idInLstFromOld = 
                            let 
                              goal = 
                                WB.Goal 
                                  sig 
                                  ((drop ((length con) - idInLstFromOld) con) ++ var) 
                                  (M.Just ((reverse con) !! idInLstFromOld))
                                  [A.aType,A.Conclusion DdB.Kind]
                            in WB.SubGoal goal [] ([],M.Nothing)
                      in (WB.SubGoal ( WB.Goal sig (con ++ var) (M.Just res) [A.aType]) [] ([],M.Nothing)):map subgoalForMem [0..((length con)-1)]
                in WB.SubGoalSet QT.SigmaF M.Nothing subgoals dside
          in ([subgoalset],"")
        term -> -- if term is M.Nothing or M.Just `not Arrow type`, return WB.TermMisMatch
          ([],WB.exitMessage (WB.TermMisMatch term) QT.SigmaF)

-- | eqForm
-- 
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
-- | typecheck| input goal     | \( \Gamma \vdash  t (\equiv \text{Just } A {=}_{c} B : s \)                                                      |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | assertion      | \( t \text{ is Just } = \text{ type and (s is a list contains type) } \)                                         |
-- |          +----------------+-----------+--------+---------------------------------------------------------------------------------------------+
-- |          | subgoals       | subgoal1  | goal   | \( \Gamma \vdash \text{Just } c : [type] \)                                                 |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           |substLst| \( [] \)                                                                                    |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           | clue   | \( ([],\text{Nothing}) \)                                                                   |
-- |          |                +-----------+--------+---------------------------------------------------------------------------------------------+
-- |          |                | subgoal2  | goal   | \( \Gamma \vdash \text{Just} A : [ c ] \)                                                   |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           |substLst| \( [] \)                                                                                    |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           | clue   | \( ([],\text{Nothing}) \)                                                                   |
-- |          |                +-----------+--------+---------------------------------------------------------------------------------------------+
-- |          |                | subgoaln  | goal   | \( \Gamma \vdash \text{Just } B : [ c ] \)                                                  |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           |substLst| \( [] \)                                                                                    |
-- |          |                |           +--------+---------------------------------------------------------------------------------------------+
-- |          |                |           | clue   | \( ([],\text{Nothing}) \)                                                                   |
-- |          +----------------+-----------+--------+---------------------------------------------------------------------------------------------+
-- |          | dSide          | \( \Gamma \vdash t (\equiv \text{Just } A {=}_{c} B : s \)                                                       |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | subgoalSet     | \( \text{"eqForm" (Nothing) subgoals dSide} \)                                                                   |
-- |          +----------------+------------------------------------------------------------------------------------------------------------------+
-- |          | output         | \( [subgoalSet] \)                                                                                               |
-- +----------+----------------+------------------------------------------------------------------------------------------------------------------+
eqForm :: WB.Rule
eqForm goal setting= 
  case WB.acceptableType QT.IqF goal True [(A.Conclusion DdB.Type),(A.Conclusion DdB.Kind)] of
    (Nothing,message) -> -- point1 : typeMisMatch
      ([],message)
    (Just arrowType,_) -> 
      case WB.termFromGoal goal of
        M.Just (A.ArrowEq t a b) ->
          let (sig,var) = WB.conFromGoal goal
              subgoalset = 
                let dside = A.AJudgment sig var (A.ArrowEq t a b) arrowType
                    subgoalForT =
                      let goal = WB.Goal sig var (M.Just t) [arrowType]
                      in WB.SubGoal goal [] ([],M.Nothing)
                    subgoalForA =
                      let goal = WB.Goal sig var (M.Just a) [t]
                      in WB.SubGoal goal [] ([],M.Nothing)
                    subgoalForB =
                      let goal = WB.Goal sig var (M.Just b) [t]
                      in WB.SubGoal goal [] ([],M.Nothing)
                in WB.SubGoalSet QT.IqF M.Nothing [subgoalForT,subgoalForA,subgoalForB] dside
          in ([subgoalset],"")
        term -> -- if term is M.Nothing or M.Just `not Arrow type`, return WB.TermMisMatch
          ([],WB.exitMessage (WB.TermMisMatch term) QT.IqF)

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
membership :: WB.Rule
membership goal setting = 
  case WB.acceptableType QT.Var goal False [] of
    (Nothing,message) -> ([],message)
    (Just arrowType,_) -> 
      let (sig,var) = WB.conFromGoal goal
          forwardedTrees = WB.trees $ F.forwardContext sig var
          trees = L.nub $
            filter 
              (\tree ->
                  let
                    x = A.downSide' tree
                    xCon = A.envfromAJudgment x
                    xType = A.typefromAJudgment x
                    xTerm = A.termfromAJudgment x
                    aboutTerm = maybe True (\term -> A.sameTerm ((sig,var),term) (xCon,xTerm)) (WB.termFromGoal goal)
                  in
                    aboutTerm && A.sameTerm ((sig,var),arrowType) (xCon,xType))
              forwardedTrees
          subgoalsets =
            map
              (\tree ->
                let
                  dSide = A.downSide' tree
                in WB.SubGoalSet QT.Var (M.Just tree) [] dSide
              )
              trees
      in (subgoalsets,"")


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
dne :: WB.Rule

dne goal setting = 
  case WB.acceptableType QT.PiE goal False [(A.Conclusion DdB.Kind),A.aType ] of
    (M.Nothing,message) -> -- point1 : typeMisMatch
      ([],T.append "dne- " message)
    (M.Just (A.Arrow [A.Arrow _ (A.Conclusion DdB.Bot)] (A.Conclusion DdB.Bot)),_) -> 
      ([],"duplicate DNE")
    (M.Just arrowType,_) ->
      let (sig,var) = WB.conFromGoal goal
          subgoalsets = let
              dSideTerm = A.ArrowApp (A.aCon $T.pack " dne ") (A.aVar (-2))
              dSide = A.AJudgment sig var dSideTerm arrowType
              subgoal1 = -- arrowType の型が Type であるかを確認する
                WB.SubGoal
                  ( WB.Goal sig var (M.Just arrowType) [A.aType])
                  []
                  ([],M.Nothing)
              subgoal2 = let
                  goal = WB.Goal sig var M.Nothing [A.Arrow [A.Arrow [arrowType] (A.Conclusion DdB.Bot)] (A.Conclusion DdB.Bot)]
                in WB.SubGoal goal [] ([],M.Nothing)
            in [WB.SubGoalSet QT.PiI M.Nothing [subgoal1,subgoal2] dSide]
        in (subgoalsets,"")

efq goal setting = 
   case WB.acceptableType QT.PiE goal False [(A.Conclusion DdB.Bot)] of
    (M.Nothing,message) -> -- point1 : typeMisMatch
      ([],T.append "efq- " message)
    (M.Just arrowType,_) ->
      let (sig,var) = WB.conFromGoal goal
          subgoalsets = let
              dSideTerm = A.ArrowApp (A.aCon $T.pack " efq ") (A.aVar (-2))
              dSide = A.AJudgment sig var dSideTerm arrowType
              subgoal1 = -- arrowType の型が Type であるかを確認する
                WB.SubGoal
                  ( WB.Goal sig var (M.Just arrowType) [A.aType])
                  []
                  ([],M.Nothing)
              subgoal2 = let
                  goal = WB.Goal sig var M.Nothing [A.Conclusion DdB.Bot]
                in WB.SubGoal goal [] ([],M.Nothing)
            in [WB.SubGoalSet QT.PiI M.Nothing [subgoal1,subgoal2] dSide]
        in (subgoalsets,"")