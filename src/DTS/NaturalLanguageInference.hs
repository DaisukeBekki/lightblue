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
  , getProver
  , SentenceAndParseTrees(..)
  , ParseTreesAndFelicityCheck(..)
  , FelicityCheckAndMore(..)
  , MoreSentencesOrInference(..)
  , InferenceAndResults(..)
  , singleParseWithTypeCheck
  , sequentialParseWithTypeCheck
  , checkInference
  , printParseResults
  ) where

import Control.Monad (forM,forM_,liftM,join) --base
import Control.Monad.State (lift)         --mtl
import qualified System.IO as S           --base
import qualified Data.Char as C           --base
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.List as L           --base
import ListT (ListT(..),fromFoldable,toList,cons)      --list-t
import qualified Parser.ChartParser as CCG
import qualified Parser.CCG as CCG
import Interface 
import Interface.Text
import Interface.HTML as HTML
import Interface.TeX
import Interface.Tree as Tree
import Parser.Language (LangOptions(..),jpOptions)
import qualified DTS.UDTTdeBruijn as UDTT
import qualified DTS.UDTTwithName as UDTTwN
import qualified DTS.DTTdeBruijn as DTT
import qualified DTS.DTTwithName as DTTwN
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

data InferenceResult = InferenceResult (InferencePair, [CCG.Node], [UDTT.Preterm], DTT.Signature, [Tree QT.DTTrule DTT.Judgment]) --, QT.ProofSearchQuery, QT.ProofSearchResult)) 

data ProverName = Wani | Null deriving (Eq,Show)

instance Read ProverName where
  readsPrec _ r =
    [(Wani,s) | (x,s) <- lex r, map C.toLower x == "wani"]
    ++ [(Null,s) | (x,s) <- lex r, map C.toLower x == "null"]
    -- ++ [(Diag,s) | (x,s) <- lex r, map C.toLower x == "diag"]
    -- ++ [(Coq,s) | (x,s) <- lex r, map C.toLower x == "coq"]

getProver :: ProverName -> QT.Prover
getProver pn = case pn of
  Wani -> Wani.prove'
  Null -> TY.nullProver

{-- Data structure for sequential parsing and the inference --} 
data SentenceAndParseTrees = SentenceAndParseTrees T.Text (ListT IO ParseTreesAndFelicityCheck) -- ^ A next sentence and its parse results
data ParseTreesAndFelicityCheck = ParseTreesAndFelicityCheck CCG.Node UDTT.TypeCheckQuery (ListT IO FelicityCheckAndMore) -- ^ A parse result, type check query for its felicity condition, and its results
data FelicityCheckAndMore = FelicityCheckAndMore QT.DTTProofDiagram MoreSentencesOrInference -- ^ A type check diagram and the next sentence if this is not the last sentence, or an inference query otherwise.
data MoreSentencesOrInference = MoreSentences SentenceAndParseTrees | AndInference InferenceAndResults | NoSentence 
data InferenceAndResults = InferenceAndResults DTT.ProofSearchQuery (ListT IO QT.DTTProofDiagram) -- ^ A proof search query for the inference and its results.

-- | Parse single text, and check its semantic felicity condition.
singleParseWithTypeCheck :: CCG.ParseSetting -> DTT.Signature -> DTT.Context -> T.Text -> SentenceAndParseTrees
singleParseWithTypeCheck parseSetting signtr contxt text = 
  SentenceAndParseTrees text $ do
    -- | IO [CCG.node] =lift=>           ListT IO [CCG.node] 
    -- |               =fmap(foldable)=> ListT IO (ListT IO CCG.Node)
    -- |               =join=>           ListT IO CCG.Node
    node <- join $ fmap fromFoldable $ lift $ CCG.simpleParse parseSetting text 
    let signtr' = L.nub $ (CCG.sig node) ++ signtr
        tcQuery = UDTT.Judgment signtr' contxt (CCG.sem node) DTT.Type
    tcDiagram <- TY.typeCheck (getProver Wani) tcQuery
    return $ ParseTreesAndFelicityCheck node tcQuery $ do
               return $ FelicityCheckAndMore tcDiagram NoSentence

-- | Parse sequential texts, and check their semantic felicity condition.
sequentialParseWithTypeCheck :: CCG.ParseSetting -> DTT.Signature -> DTT.Context -> [T.Text] -> MoreSentencesOrInference
sequentialParseWithTypeCheck _ _ [] [] = NoSentence
sequentialParseWithTypeCheck _ signtr (typ:contxt) [] = 
  let psq = DTT.ProofSearchQuery signtr contxt typ 
      pss = QT.ProofSearchSetting Nothing Nothing (Just QT.Intuitionistic)
  in AndInference $ InferenceAndResults psq $ (getProver Wani) pss psq
sequentialParseWithTypeCheck parseSetting signtr contxt (text:texts) = 
  MoreSentences $ SentenceAndParseTrees text $ do
    node <- join $ fmap fromFoldable $ lift $ CCG.simpleParse parseSetting text 
    let signtr' = L.nub $ (CCG.sig node) ++ signtr
        tcQuery = UDTT.Judgment signtr' contxt (CCG.sem node) DTT.Type
    tcDiagram <- TY.typeCheck (getProver Wani) tcQuery
    let contxt' = (DTT.trm $ Tree.node tcDiagram):contxt
    return $ ParseTreesAndFelicityCheck node tcQuery $ do
               return $ FelicityCheckAndMore tcDiagram $ sequentialParseWithTypeCheck parseSetting signtr' contxt' texts

-- | Flatten SequentialParseResults
-- enumerateSequentialParseResult :: SentenceAndParseResult -> ListT IO (ListT IO (CCG.Node, UDTT.TypeCheckQuery, QT.DTTProofDiagram))
-- enumerateSequentialParseResult parseResults = do
--   pr <- parseResults
--   tcr <- subsequentTypeCheck pr
--   spr <- enumerateSequentialParseResult $ subsequentParse tcr
--   return $ cons (parseTree pr, typeCheckQuery pr, typeCheckDiagram tcr) spr

-- flatten :: ListT IO (ListT IO a) -> IO [[a]]
-- -- | ListT m (ListT m a) => m [ListT m a] => m [m [a]] => m (m [[a]])
-- flatten = join . liftM (sequence . map toList) . toList

printTriple :: (CCG.Node, UDTT.TypeCheckQuery, QT.DTTProofDiagram) -> IO ()
printTriple (node, tcQuery, diagram) = T.putStrLn $ T.concat [
    startMathML
    , toMathML node
    , T.pack $ interimOf HTML ""
    , toMathML $ UDTTwN.fromDeBruijnJudgment tcQuery
    , T.pack $ interimOf HTML ""
    , toMathML $ fmap DTTwN.fromDeBruijnJudgment diagram
    , endMathML
    ]

-- | Checks if the premise texts entails the hypothesis text.
-- | The specification of this function reflects a view about what are entailments between texts,
-- | that is an interface problem between natural language semantics and logic
checkInference :: InferenceSetting 
                   -> InferencePair 
                   -> IO ()
checkInference InferenceSetting{..} infPair = do
  -- | Parse sentences
  let sentences = reverse $ (hypothesis infPair):(reverse $ premises infPair) 
      parseResult = sequentialParseWithTypeCheck CCG.defaultParseSetting [("dummy",DTT.Entity)] [] sentences 
  -- results <- flatten $ enumerateSequentialParseResult parseResult
  -- mapM_ printTriple $ head results
  print "ToDo: print and analyze results of sequential parse"

-- | prints a CCG node (=i-th parsing result for a sid-th sentence) in a specified style (=HTML|text|XML|TeX)
printParseResults :: S.Handle -> Style -> Int -> Bool -> SentenceAndParseTrees -> IO ()
printParseResults handle _ sid ifTypeCheck (SentenceAndParseTrees sentence parseTrees) = do
  S.hPutStr handle $ "<p>[s" ++ (show sid) ++ "] "
  T.hPutStr handle sentence
  S.hPutStr handle "</p>"
  parseTrees' <- toList parseTrees 
  -- | [ParseTreesAndFelicityCheck CCG.Node UDTT.TypeCheckQuery (ListT IO FelicityCheckAndMore) ]
  forM_ (zip parseTrees' ([0..]::[Int])) $ \((ParseTreesAndFelicityCheck node tcQuery tcResults),ith) -> do
          S.hPutStr handle $ "<p>[parse " ++ show ith ++ ": score="
          T.hPutStr handle $ CCG.showScore node 
          S.hPutStr handle "] "
          T.hPutStr handle $ CCG.pf node
          S.hPutStr handle "</p>"
          mapM_ (T.hPutStr handle) [HTML.startMathML, HTML.toMathML node, HTML.endMathML]
          mapM_ (T.hPutStr handle) [
            HTML.startMathML
            , toMathML $ UDTTwN.fromDeBruijnJudgment tcQuery
            , HTML.endMathML
            ]
          tcResults' <- toList tcResults
          forM_ tcResults' $ \(FelicityCheckAndMore tcDiagram moreResult) -> do
            S.hPutStr handle $ interimOf HTML ""
            mapM_ (T.hPutStr handle) [
              HTML.startMathML
              , toMathML $ fmap DTTwN.fromDeBruijnJudgment tcDiagram
              , HTML.endMathML
              ]
            case moreResult of
              MoreSentences sentenceAndParseTrees -> do
                printParseResults handle HTML sid ifTypeCheck sentenceAndParseTrees
              AndInference (InferenceAndResults psq proofDiagrams) -> do
                mapM_ (T.hPutStr handle) [
                  HTML.startMathML
                  , toMathML $ DTTwN.fromDeBruijnProofSearchQuery psq
                  , HTML.endMathML
                  ]
                proofDiagrams' <- toList proofDiagrams
                forM_ proofDiagrams' $ \proofDiagram -> do
                  S.hPutStr handle $ interimOf HTML ""
                  mapM_ (T.hPutStr handle) [
                    HTML.startMathML
                    , toMathML $ fmap DTTwN.fromDeBruijnJudgment proofDiagram
                    , HTML.endMathML
                    ]
              NoSentence -> S.hPutStr handle $ interimOf HTML "No more sentence" 

{-
  -- | Parse sentences
  let sentences = (hypothesis infPair):(reverse $ premises infPair)     -- | reverse the order of sentences (hypothesis first, the first premise last)
  nodeslist <- mapM (CCG.simpleParse beam) sentences -- | [[CCG.Node]] parse sentences
  let pairslist = map ((map (\node -> (node, UDTT.betaReduce $ UDTT.sigmaElimination $ CCG.sem node))).(take nbest)) nodeslist
      -- | Example: [[(nodeA1,srA1),(nodeA2,srA2)],[(nodeB1,srB1),(nodeB2,srB2)],[(nodeC1,srC1),(nodeC2,srC2)]]
      -- |          where sentences = A,B,C (where A is the hypothesis), nbest = 2_
      chosenlist = choice pairslist
      -- | Example: [[(nodeA1,srA1),(nodeB1,srB1),(nodeC1,srC1)],[(nodeA1,srA1),(nodeB1,srB1),(nodeC2,srC2)],...]
      nodeSRlist = map unzip chosenlist
      -- | Example: [([nodeA1,nodeB1,nodeC1],[srA1,srB1,srC1]),([nodeA1,nodeB1,nodeC2],[srA1,srB1,srC2]),...]
      prover = getProver proverName
  return $ InferenceResult $ do
    (ccgnds,srs) <- QT.ListEx (nodeSRlist, "")
    let allsigs = foldl L.union [] $ map CCG.sig ccgnds;
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
-- data InferenceResult = InferenceResult [([CCG.Node], QT.ProofSearchResult)] deriving (Eq)

-- | (x:xs) :: [[a]], x :: [a], xs :: [[a]], y :: a, choice xs :: [[a]], ys :: [a], (y:ys) :: [a] 
-- choice :: [[a]] -> [[a]]
-- choice [] = [[]]
-- choice (x:xs) = [(y:ys) | y <- x, ys <- choice xs]

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

