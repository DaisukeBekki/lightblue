{-# LANGUAGE TupleSections, RecordWildCards #-}

module DTS.Prover.Wani.Prove
(
  display,
  prove'
) where

import qualified Interface.Tree as UDT
import qualified DTS.DTTdeBruijn as DdB
import qualified DTS.QueryTypes as QT
import qualified DTS.Prover.Wani.Arrowterm as A
import qualified DTS.Prover.Wani.BackwardWithRules as BR
import qualified DTS.Prover.Wani.WaniBase as WB 
import qualified Interface.Tree as UDT
import qualified DTS.QueryTypes as QT

import qualified Data.Text.Lazy as T 
import qualified Data.List as L 
import qualified Data.Maybe as M
import qualified Debug.Trace as D
import qualified ListT as ListT
import qualified Control.Monad.Trans.Class as ListT

import qualified Data.Time.Clock as Time

import qualified Interface.HTML as HTML
import qualified Data.Bifunctor

display :: UDT.Tree QT.DTTrule DdB.Judgment -> IO T.Text
display jtree  = do
  return 
    $ T.append (T.pack HTML.htmlHeader4MathML) $
      T.append HTML.startMathML $
      T.append (HTML.toMathML jtree) $
      T.append HTML.endMathML
       (T.pack HTML.htmlFooter4MathML)

-- | return prooftrees and some info(max depth, etc.)
hojo ::  DdB.Context
  -> DdB.Signature
  -> DdB.Preterm
  -> WB.Setting
  -> M.Maybe Time.NominalDiffTime
  -> IO WB.Result
hojo varEnv sigEnv pre_type setting timeLimitDiff = 
  let sigEnv' = map (Data.Bifunctor.second A.fromDT2A) sigEnv
      varEnv' = map A.fromDT2A varEnv
      arrowType = A.fromDT2A  pre_type
      result = searchProofWithIncrementalDepth sigEnv' varEnv' arrowType 1 setting timeLimitDiff 1 (let num = WB.maxdepth setting in if num < 0 then M.Nothing else M.Just num)
  in WB.debugLog (sigEnv',varEnv') arrowType 0 setting "goal" result

searchProof :: WB.DeduceRule
searchProof a b c d setting= 
  BR.deduce a b c d setting
    >>= \result -> return result{WB.trees = L.nub (WB.trees result)}

searchProofWithIncrementalDepth :: A.SAEnv -> A.AEnv -> WB.AType -> WB.Depth -> WB.Setting -> M.Maybe Time.NominalDiffTime ->  Int -> M.Maybe Int-> IO WB.Result
searchProofWithIncrementalDepth a b c d setting timeLimitDiff currentDepth maybeLim =
  Time.getCurrentTime >>= \currentTime ->
    let timeLimit = M.maybe M.Nothing (\t -> let timeLimit = Time.addUTCTime t currentTime in (M.Just timeLimit) ) timeLimitDiff in
      searchProof a b c d setting{WB.maxdepth = currentDepth}{WB.timeLimit = timeLimit }
        >>= \result ->
          D.trace ("d=" ++ (show currentDepth) ++ {--("timeLimitDiff : "++(show timeLimitDiff)++" currentTime : "++(show currentTime)) ++--} (" / timeLimit : "++(show timeLimit))) $ 
          if (null (WB.trees result) && maybe True (currentDepth <) maybeLim)
            then searchProofWithIncrementalDepth a b c d setting timeLimitDiff (currentDepth+1) maybeLim 
            else return result

-- | Prover for lightblue:
prove' :: QT.ProverBuilder
prove' QT.ProofSearchSetting{..} (DdB.ProofSearchQuery sig ctx typ) =  -- LiftT IO (Tree (U.Judgment U.DTT) UDTTrule)
  let setting = WB.Setting {
        WB.mode = case logicSystem of
                    Nothing -> WB.Plain
                    Just QT.Intuitionistic -> WB.WithEFQ
                    Just QT.Classical -> WB.WithDNE,
        WB.falsum = True,
        WB.maxdepth = case maxDepth of
                        Just n -> n
                        Nothing -> 9,
        WB.maxtime = case maxTime of
                        Just t -> t
                        Nothing -> 100000,
        WB.debug = -1,
        WB.sStatus = WB.statusDef,
        WB.enableneuralDTS = neuralDTS
        };
      ioResult = hojo ctx ((A.aEntityName,DdB.Type):sig) typ setting (M.maybe Nothing (\t -> M.Just $ toEnum (t * (10^9))) maxTime)
  in ListT.lift ioResult >>= \result ->
    ListT.fromFoldable (map A.aTreeTojTree' (WB.trees result))
