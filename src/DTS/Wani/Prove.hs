{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module DTS.Wani.Prove
(
  settingDNE,
  settingEFQ,
  announce,
  announce',
  execute,
  prove
) where

import qualified DTS.DTT as DT            -- DTT
import qualified DTS.UDTT as UD            -- DTT
import qualified DTS.Wani.Arrowterm as A
import qualified DTS.Prover_daido.Judgement  as J
import qualified DTS.Wani.Backward as B
import qualified DTS.Wani.WaniBase as WB 

import qualified Data.Text.Lazy as T 
import qualified Data.List as L 
import qualified Debug.Trace as D

import qualified Interface.HTML as HTML
import qualified Data.Bifunctor

settingDNE :: WB.Setting
settingDNE = WB.settingDef{WB.mode = WB.WithDNE}
settingEFQ :: WB.Setting
settingEFQ = WB.settingDef{WB.mode = WB.WithEFQ}


announce' :: WB.Result -> IO T.Text  
announce' result = 
  if null (WB.trees result)
    then
      return $ T.pack "Nothing to announce"
    else
      announce $A.aTreeTojTree $ head $WB.trees result

announce :: J.Tree J.Judgement -> IO T.Text  
announce jtree = do
  return 
    $ T.append (T.pack HTML.htmlHeader4MathML) $
      T.append HTML.startMathML $
      T.append (J.treeToMathML jtree) $
      T.append HTML.endMathML
       (T.pack HTML.htmlFooter4MathML)

execute ::  J.TEnv -- ^ var_context ex)[(DT.Con (T.pack "prop")),(DT.Con (T.pack "set")),(DT.Con (T.pack "prop"))]
  -> A.SUEnv -- ^ sig_context ex)[((T.pack "prop"),DT.Type),((T.pack "set"),DT.Type)] , classic
  -> DT.Preterm -- ^ type ex) (DT.Pi (DT.Var 0) (DT.Sigma (DT.Var 0,DT.Var 3)))
  -> WB.Setting -- ^ limitations
  -> [J.Tree J.Judgement] -- ^ prooftrees
execute a b c d= map A.aTreeTojTree  $ WB.trees (prove a b c d) 

-- | return prooftrees and some info(max depth, etc.)
prove ::  J.TEnv
  -> A.SUEnv 
  -> DT.Preterm 
  -> WB.Setting
  -> WB.Result
prove varEnv sigEnv pre_type setting = 
  let sigEnv' = map (Data.Bifunctor.second A.fromDT2A) sigEnv
      varEnv' = map A.fromDT2A varEnv
      con = (sigEnv',varEnv')
      arrowType = A.fromDT2A  pre_type
      result = searchProof con arrowType 1 setting
      -- ここで typecheck して正当性確認」
  in WB.debugLog con arrowType 0 setting "goal" result

searchProof :: WB.DeduceRule
searchProof a b c setting= 
  let result =  B.deduce a b c setting
  in result{WB.trees = L.nub (WB.trees result)}