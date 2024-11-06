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
import qualified DTS.Prover.Wani.Backward as B
import qualified DTS.Prover.Wani.WaniBase as WB 
import qualified Interface.Tree as UDT
import qualified DTS.QueryTypes as QT

import qualified Data.Text.Lazy as T 
import qualified Data.List as L 
import qualified Debug.Trace as D
import qualified ListT as ListT

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
  -> WB.Result
hojo varEnv sigEnv pre_type setting = 
  let sigEnv' = map (Data.Bifunctor.second A.fromDT2A) sigEnv
      varEnv' = map A.fromDT2A varEnv
      arrowType = A.fromDT2A  pre_type
      result = searchProof' sigEnv' varEnv' arrowType 1 setting
  in WB.debugLog (sigEnv',varEnv') arrowType 0 setting "goal" result

searchProof' :: WB.DeduceRule
searchProof' a b c d setting= 
  let result =  B.deduce a b c d setting
  in result{WB.trees = L.nub (WB.trees result)}

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
        WB.debug = False,
        WB.sStatus = WB.statusDef
        };
      result = hojo ctx sig typ setting
  in ListT.fromFoldable $ map A.aTreeTojTree' $ WB.trees result
