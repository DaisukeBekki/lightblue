{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Prover
Description : A Prover(-Interface) for DTS
Copyright   : Daisuke Bekki
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module DTS.Prover
( defaultTypeCheck,
  defaultProofSearch,
  checkFelicity,
  checkEntailment,
  choice
) where

import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.List as L           --base
import qualified System.IO as S           --base
import qualified Parser.ChartParser as CP
import qualified Interface.HTML as HTML
import qualified DTS.UDTT as UD
import qualified DTS.Prover.TypeChecker as Ty
import qualified DTS.Prover.Judgement as Ty

-- | type check with the default signature = entity:type, evt:type
defaultTypeCheck :: [UD.Signature] -> UD.Context -> UD.Preterm -> UD.Preterm -> [Ty.UTree Ty.UJudgement]
defaultTypeCheck sig cont term typ = Ty.typeCheckU cont (("evt",UD.Type):("entity",UD.Type):sig) term typ

-- | proof search with the default signature = entity:type, evt:type
defaultProofSearch :: [UD.Signature] -> UD.Context -> UD.Preterm -> [Ty.UTree Ty.UJudgement]
defaultProofSearch sig cont typ = Ty.proofSearch cont (("evt",UD.Type):("entity",UD.Type):sig) typ

-- | checks felicity condition
checkFelicity :: [UD.Signature] -> [UD.Preterm] -> UD.Preterm -> [Ty.UTree Ty.UJudgement]
checkFelicity sig cont term = defaultTypeCheck sig cont term (UD.Type)

-- | executes type check to a context
sequentialTypeCheck :: [UD.Signature] -> [UD.Preterm] -> [UD.Preterm]
sequentialTypeCheck sig = foldr (\sr cont -> let result = do
                                                          t1 <- checkFelicity sig cont sr;
                                                          t2 <- Ty.aspElim t1;
                                                          t3 <- Ty.getTerm t2
                                                          return $ Ty.repositP t3 in
                                             if null result
                                                then (UD.Con "Typecheck or aspElim failed"):cont
                                                else (head result):cont
                                ) []

checkEntailment :: Int -> Int -> [T.Text] -> T.Text -> IO()
checkEntailment beam nbest premises hypothesis = do
  let hline = "<hr size='15' />";
  --
  -- Show premises and hypothesis
  --
  --mapM_ T.putStr ["[", jsem_id, "]"]
  mapM_ (\p -> mapM_ T.putStr ["<p>P: ", p, "</p>"]) premises
  mapM_ T.putStr ["<p>H: ", hypothesis, "</p>"]
  T.putStrLn hline
  --
  -- Parse sentences
  --
  let sentences = hypothesis:(reverse premises)     -- reverse the order of sentences (hypothesis first, the first premise last)
  nodeslist <- mapM (CP.simpleParse beam) sentences -- parse sentences
  let pairslist = map ((map (\node -> (node, UD.betaReduce $ UD.sigmaElimination $ CP.sem node))).(take nbest)) nodeslist;
      -- Example: [[(nodeA1,srA1),(nodeA2,srA2)],[(nodeB1,srB1),(nodeB2,srB2)],[(nodeC1,srC1),(nodeC2,srC2)]]
      --          where sentences = A,B,C (where A is the hypothesis), nbest = 2_
      chosenlist = choice pairslist;
      -- Example: [[(nodeA1,srA1),(nodeB1,srB1),(nodeC1,srC1)],[(nodeA1,srA1),(nodeB1,srB1),(nodeC2,srC2)],...]
      nodeSRlist = map unzip chosenlist;
      -- Example: [([nodeA1,nodeB1,nodeC1],[srA1,srB1,srC1]),([nodeA1,nodeB1,nodeC2],[srA1,srB1,srC2]),...]
  tripledNodes <- mapM (\(nds,srs) -> do
                         let newsig = foldl L.union [] $ map CP.sig nds;
                             typecheckedSRs = sequentialTypeCheck newsig srs;
                             -- Example: u0:srA1, u1:srB1, u2:srC1 (where A1 is the hyp.)
                             -- この時点で一文目はtypecheck of aspElim failed
                             proofdiagrams = case typecheckedSRs of
                                               [] -> []
                                               (hype:prems) -> defaultProofSearch newsig prems hype;
                         return (nds,typecheckedSRs,proofdiagrams)
                       ) nodeSRlist;
  S.hPutStrLn S.stderr $ show tripledNodes
  let nodeSrPrList = dropWhile (\(_,_,p) -> null p) tripledNodes;
      (nds,srs,pds) = if null nodeSrPrList
                        then head tripledNodes
                        else head nodeSrPrList
  --
  -- Show parse trees
  --
  T.putStrLn HTML.startMathML
  mapM_ (T.putStrLn . HTML.toMathML) $ reverse nds
  T.putStrLn HTML.endMathML
  T.putStrLn hline
  --
  -- Show proof diagrams
  --
  if null pds
     then mapM_ T.putStrLn [
           "No proof diagrams for: ", 
           HTML.startMathML, 
           UD.printProofSearchQuery (tail srs) (head srs),
           HTML.endMathML
           ]
      else do
           T.putStrLn "Proved: "
           T.putStrLn HTML.startMathML
           mapM_ (T.putStrLn . Ty.utreeToMathML) pds
           T.putStrLn HTML.endMathML
  T.putStrLn hline

choice :: [[a]] -> [[a]]
choice [] = [[]]
choice (a:as) = [x:xs | x <- a, xs <- choice as]
