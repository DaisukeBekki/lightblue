{-# LANGUAGE RecordWildCards #-}

{-|
Module      : DTS.Inference
Copyright   : Daisuke Bekki
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

A Prover-Interfaces for DTS.
-}

module DTS.Inference (
  InferenceSetting(..)
  , InferencePair(..)
  , InferenceResult(..)
  , checkEntailment
  ) where

import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.List as L           --base
import qualified Parser.ChartParser as CP
import qualified Interface.HTML as HTML
import qualified DTS.UDTT as UD
--import qualified DTS.Prover.Diag.Prover as Ty
import qualified DTS.Prover.Diag.TypeChecker as Ty
import qualified DTS.Prover.Diag.Judgement as Ty

data InferenceSetting = InferenceSetting {
  beam :: Int     -- ^ beam width
  , nbest :: Int  -- ^ n-best
  , typeChecker :: UD.Signatuer -> UD.Context -> UD.Preterm -> UD.Preterm -> [
  , prover :: UD.Signature -> UD.Context -> UD.Preterm -> [Ty.
  } deriving (Eq, Show)

data InferencePair = InferencePair {
  premises :: [T.Text]   -- ^ premises
  , hypothesis :: T.Text -- ^ a hypothesis
  } deriving (Eq, Show)

data InferenceResult = InferenceResult {
  inferencePair :: InferencePair
  , nodes :: [CP.Node]
  , maxDepth :: Maybe Int
  } deriving (Eq, Show)

-- | Checks if the premise texts entails the hypothesis text.
-- | The specification of this function reflects a view about what are entailments between texts, that is an interface problem between natural language semantics and logic
checkEntailment :: InferenceSetting 
                   -> InferencePair 
                   -> IO(InferenceResult)
checkEntailment InferenceSetting{..} InferencePair{..} = do
  -- | Parse sentences
  let sentences = hypothesis:(reverse premises)     -- reverse the order of sentences (hypothesis first, the first premise last)
  nodeslist <- mapM (CP.simpleParse beam) sentences -- parse sentences
  let pairslist = map ((map (\node -> (node, UD.betaReduce $ UD.sigmaElimination $ CP.sem node))).(take nbest)) nodeslist;
      -- | Example: [[(nodeA1,srA1),(nodeA2,srA2)],[(nodeB1,srB1),(nodeB2,srB2)],[(nodeC1,srC1),(nodeC2,srC2)]]
      -- |          where sentences = A,B,C (where A is the hypothesis), nbest = 2_
      chosenlist = choice pairslist;
      -- | Example: [[(nodeA1,srA1),(nodeB1,srB1),(nodeC1,srC1)],[(nodeA1,srA1),(nodeB1,srB1),(nodeC2,srC2)],...]
      nodeSRlist = map unzip chosenlist;
      -- | Example: [([nodeA1,nodeB1,nodeC1],[srA1,srB1,srC1]),([nodeA1,nodeB1,nodeC2],[srA1,srB1,srC2]),...]
  tripledNodes <- mapM (\(nds,srs) -> do
                         let newsig = foldl L.union [] $ map CP.sig nds;
                             typecheckedSRs = sequentialTypeCheck newsig srs;
                             -- | Example: u0:srA1, u1:srB1, u2:srC1 (where A1 is the hyp.)
                             -- | この時点で一文目はtypecheck of aspElim failed
                             proofdiagrams = case typecheckedSRs of
                                               [] -> []
                                               (hype:prems) -> defaultProofSearch newsig prems hype;
                         return (nds,typecheckedSRs,proofdiagrams)
                       ) nodeSRlist;
  --S.hPutStrLn S.stderr $ show tripledNodes
  let nodeSrPrList = dropWhile (\(_,_,p) -> null p) tripledNodes;
      (nds,srs,pds) = if null nodeSrPrList
                        then head tripledNodes
                        else head nodeSrPrList

choice :: [[a]] -> [[a]]
choice [] = [[]]
choice (a:as) = [x:xs | x <- a, xs <- choice as]

instance MathML InferenceResult where
  toMathml InferenceResult {..} = 
    let hline = "<hr size='15' />" in T.concat [
      -- | Show premises and hypothesis
      --mapM_ T.putStr ["[", jsem_id, "]"]
      mapM (\p -> mapM_ T.putStr ["<p>P: ", p, "</p>"]) premises
      mapM_ T.putStr ["<p>H: ", hypothesis, "</p>"]
      , hline
      -- | Show parse trees
      , HTML.startMathML
    mapM_ (T.putStrLn . HTML.toMathML) $ reverse nds
    T.putStrLn HTML.endMathML
    T.putStrLn hline
    -- | Show proof diagrams
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

