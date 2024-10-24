{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}

{-|
Module      : DTS.NaturalLanguageInference
Copyright   : Daisuke Bekki
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

A module for Natural Language Inference 
-}

module DTS.NaturalLanguageInference (
  InferenceSetting(..)
  , InferencePair(..)
  , InferenceResult(..)
  , ProverName(..)
  , checkInference
  ) where

import Control.Monad (forM)               --base
import qualified Data.Char as C           --base
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.List as L           --base
import qualified Parser.ChartParser as CP
import qualified Interface.HTML as HTML
import qualified Interface.TeX as TEX
import qualified Interface.Tree as I
import qualified DTS.UDTTdeBruijn as UDTT
import qualified DTS.DTTdeBruijn as DTT
import qualified DTS.QueryTypes as QT
import qualified DTS.TypeChecker as TY
import qualified DTS.Prover.Wani.Prove as Wani

data InferenceSetting = InferenceSetting {
  beam :: Int     -- ^ beam width
  , nbest :: Int  -- ^ n-best
  , maxDepth :: Maybe Int -- ^ max depth for prover
  , maxTime :: Maybe Int  -- ^ max time for prover
  , typeChecker :: QT.TypeChecker
  , proverName :: ProverName
  } 

data InferenceLabel = YES | NO | UNK deriving (Eq, Show, Read)

data InferencePair = InferencePair {
  premises :: [T.Text]   -- ^ premises
  , hypothesis :: T.Text -- ^ a hypothesis
  } deriving (Eq, Show)

data InferenceResult = InferenceResult (InferencePair, [CP.Node], [UDTT.Preterm], DTT.Signature, [I.Tree QT.DTTrule DTT.Judgment]) --, QT.ProofSearchQuery, QT.ProofSearchResult)) 

data ProverName = Wani | Null deriving (Eq,Show)

instance Read ProverName where
  readsPrec _ r =
    [(Wani,s) | (x,s) <- lex r, map C.toLower x == "wani"]
    ++ [(Diag,s) | (x,s) <- lex r, map C.toLower x == "diag"]
    ++ [(Coq,s) | (x,s) <- lex r, map C.toLower x == "coq"]

getProver :: ProverName -> QT.Prover
getProver pn = case pn of
  Wani -> Wani.prove'
  Null -> TY.nullProver

-- | Checks if the premise texts entails the hypothesis text.
-- | The specification of this function reflects a view about what are entailments between texts,
-- | that is an interface problem between natural language semantics and logic
checkInference :: InferenceSetting 
                   -> InferencePair 
                   -> IO InferenceResult
checkInference InferenceSetting{..} infPair = do
  return $ InferenceResult ((InferencePair [] T.empty), [], [], [], [])
{-
  -- | Parse sentences
  let sentences = (hypothesis infPair):(reverse $ premises infPair)     -- | reverse the order of sentences (hypothesis first, the first premise last)
  nodeslist <- mapM (CP.simpleParse beam) sentences -- | [[CCG.Node]] parse sentences
  let pairslist = map ((map (\node -> (node, UDTT.betaReduce $ UDTT.sigmaElimination $ CP.sem node))).(take nbest)) nodeslist
      -- | Example: [[(nodeA1,srA1),(nodeA2,srA2)],[(nodeB1,srB1),(nodeB2,srB2)],[(nodeC1,srC1),(nodeC2,srC2)]]
      -- |          where sentences = A,B,C (where A is the hypothesis), nbest = 2_
      chosenlist = choice pairslist
      -- | Example: [[(nodeA1,srA1),(nodeB1,srB1),(nodeC1,srC1)],[(nodeA1,srA1),(nodeB1,srB1),(nodeC2,srC2)],...]
      nodeSRlist = map unzip chosenlist
      -- | Example: [([nodeA1,nodeB1,nodeC1],[srA1,srB1,srC1]),([nodeA1,nodeB1,nodeC2],[srA1,srB1,srC2]),...]
      prover = getProver proverName
  return $ InferenceResult $ do
    (ccgnds,srs) <- QT.ListEx (nodeSRlist, "")
    let allsigs = foldl L.union [] $ map CP.sig ccgnds;
    typeCheckTrees <- mapM (\sr -> typeChecker prover $ QT.TypeCheckQuery allsigs [] sr UDTT.Type) $ drop 1 srs
    return (infPair, ccgnds, srs, allsigs, typeCheckTrees)
    {-
    (hype:prems) <- TY.sequentialTypeCheck TY.typeCheck prover allsigs srs; -- [DTTpreterms]
    let query = QT.ProofSearchQuery allsig prems hype
    -- | Example: u0:srA1, u1:srB1, u2:srC1 (where A1 is the hyp.)
    return (infPair, ccgnds, query, prover (QT.ProofSearchSetting maxDepth maxTime (Just QT.Intuitionistic)) query)
    -}
-}

-- type ProofSearchResult = [Tree (U.Judgment U.DTT) UDTTrule]
-- data InferenceResult = InferenceResult [([CP.Node], QT.ProofSearchResult)] deriving (Eq)

-- | (x:xs) :: [[a]], x :: [a], xs :: [[a]], y :: a, choice xs :: [[a]], ys :: [a], (y:ys) :: [a] 
choice :: [[a]] -> [[a]]
choice [] = [[]]
choice (x:xs) = [(y:ys) | y <- x, ys <- choice xs]

{-
instance HTML.MathML InferenceResult where
  toMathML (InferenceResult (QT.ListEx (results,errMsg))) =
    T.concat $ (flip map) results $ \(infPairs,ccgNodes,srs,allsigs,checkedSrs) -> --,proofSearchQuery,proofSearchResults) ->
      let wrapMathML = \x -> T.concat [HTML.startMathML, HTML.toMathML x, HTML.endMathML] in
      T.concat [
        T.intercalate ", " (premises infPairs)
        , "==>"
        , (hypothesis infPairs)
        , "<hr />"
        , errMsg
        , "<hr />"
        --, T.concat $ map wrapMathML ccgNodes 
        --, "<hr />"
        , T.concat $ map wrapMathML srs 
        , "<hr />"
        , HTML.toMathML allsigs
        , "<hr />"
        --, T.concat $ map wrapMathML checkedSrs 
        ]
-}

  {-
    T.concat [
      -- | Show premises and hypothesis
      --mapM_ T.putStr ["[", jsem_id, "]"]
      T.concat $ map (\p -> T.concat ["<p>P: ", p, "</p>"]) premises
      , T.concat ["<p>H: ", hypothesis, "</p>"]
      , hline
      -- | Show parse trees
      , HTML.startMathML
      , T.concat $ map HTML.toMathML $ reverse [] ---直す！
      , HTML.endMathML
      , hline
      -- | Show proof diagrams
      , if null [] --pds 直す！
          then T.concat [
             "No proof diagrams for: "
             , HTML.startMathML
             --, UDTT.printProofSearchQuery (tail srs) (head srs)
             , HTML.endMathML
             ]
          else T.concat [
             "Proved: "
             , HTML.startMathML
             --, T.concat $ map Ty.utreeToMathML pds
             , HTML.endMathML
             ]
      , hline
      ]
  -}

--instance TEX.TypeSet InferenceResult where
--  toTeX InferenceResult{..} =

