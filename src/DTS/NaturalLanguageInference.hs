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
  --, InferencePair(..)
  --, InferenceResult(..)
  , ProverName(..)
  , getProver
  , ParseResult(..)
  , ParseTreeAndFelicityChecks(..)
  , QueryAndDiagrams(..)
  , parseWithTypeCheck
  , printParseResult
  , trawlParseResult
  ) where

import Control.Monad (when,forM_,join)    --base
import Control.Monad.State (lift)         --mtl
import Control.Monad.IO.Class (liftIO)    --base
import Control.Applicative ((<|>))          --base
import qualified System.IO as S           --base
import qualified Data.Char as C           --base
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.List as L           --base
import ListT (ListT(..),fromFoldable,toList,take,null) --list-t
import qualified Parser.ChartParser as CP      --lightblue
import qualified Parser.CCG as CCG             --lightblue
import Interface                               --lightblue
import Interface.Text                          --lightblue
import Interface.HTML as HTML                  --lightblue
import Interface.TeX                           --lightblue
import Interface.Tree as Tree                  --lightblue
--import Parser.Language (LangOptions(..),jpOptions)
import qualified DTS.UDTTdeBruijn as UDTT      --lightblue
import qualified DTS.UDTTwithName as UDTTwN    --lightblue
import qualified DTS.DTTdeBruijn as DTT        --lightblue
import qualified DTS.DTTwithName as DTTwN      --lightblue
import qualified DTS.QueryTypes as QT          --lightblue
import qualified DTS.TypeChecker as TY         --lightblue
import qualified DTS.Prover.Wani.Prove as Wani --lightblue
import qualified JSeM as JSeM                  --jsem

data InferenceSetting = InferenceSetting {
  beam :: Int     -- ^ beam width
  , maxDepth :: Maybe Int -- ^ max depth for prover
  , maxTime :: Maybe Int  -- ^ max time for prover
  , parseSetting :: CP.ParseSetting
  , typeChecker :: QT.TypeChecker
  , proverName :: ProverName
  } 

type InferenceLabel = JSeM.YesNo
--data InferenceLabel = YES | NO | UNK deriving (Eq, Show, Read)

-- data InferencePair = InferencePair {
--   premises :: [T.Text]   -- ^ premises
--   , hypothesis :: T.Text -- ^ a hypothesis
--   } deriving (Eq, Show)

--data InferenceResult = InferenceResult (InferencePair, [CCG.Node], [UDTT.Preterm], DTT.Signature, [Tree QT.DTTrule DTT.Judgment]) --, QT.ProofSearchQuery, QT.ProofSearchResult)) 

data ProverName = Wani | Null deriving (Eq,Show)

instance Read ProverName where
  readsPrec _ r =
    [(Wani,s) | (x,s) <- lex r, map C.toLower x == "wani"]
    ++ [(Null,s) | (x,s) <- lex r, map C.toLower x == "null"]
    -- ++ [(Diag,s) | (x,s) <- lex r, map C.toLower x == "diag"]
    -- ++ [(Coq,s) | (x,s) <- lex r, map C.toLower x == "coq"]

getProver :: ProverName -> QT.ProverBuilder
getProver pn = case pn of
  Wani -> Wani.prove'
  Null -> TY.nullProver

{-- Data structure for sequential parsing and the inference --} 

data ParseResult = 
  SentenceAndParseTrees T.Text (ListT IO ParseTreeAndFelicityChecks) -- ^ A next sentence and its parse results
  | InferenceResults QueryAndDiagrams QueryAndDiagrams 
  | NoSentence 
data ParseTreeAndFelicityChecks = 
  ParseTreeAndFelicityChecks CCG.Node DTT.Signature UDTT.TypeCheckQuery (ListT IO (QT.DTTProofDiagram, ParseResult)) 
  -- ^ A parse result, type check query for its felicity condition, and its results
  -- ^ A type check diagram and the next sentence if this is not the last sentence, or an inference query otherwise.
data QueryAndDiagrams = 
  QueryAndDiagrams DTT.ProofSearchQuery (ListT IO QT.DTTProofDiagram) 
  -- ^ A proof search query for the inference and its results.

type Discourse = [T.Text]

-- | Parse sequential texts, and check their semantic felicity condition.
-- | If noInference = True, it does not execute inference.
-- | The specification of this function reflects a view about what are entailments between texts,          
-- | that is an interface problem between natural language semantics and logic
parseWithTypeCheck :: CP.ParseSetting -> QT.Prover -> DTT.Signature -> DTT.Context -> Discourse -> ParseResult
parseWithTypeCheck _ _ _ [] [] = NoSentence     -- ^ Context is empty and no sentece is given 
parseWithTypeCheck ps prover signtr (typ:contxt) [] = -- ^ Context is given and no more sentence (= All parse done)
  if CP.noInference ps
    then NoSentence
    else let psqPos = DTT.ProofSearchQuery signtr contxt $ typ 
             resultPos = takeNbest (CP.nProof ps) $ prover psqPos
             psqNeg = DTT.ProofSearchQuery signtr contxt $ DTT.Pi typ DTT.Bot
             resultNeg = takeNbest (CP.nProof ps) $ prover psqNeg
         in InferenceResults (QueryAndDiagrams psqPos resultPos) (QueryAndDiagrams psqNeg resultNeg)
parseWithTypeCheck ps prover signtr contxt (text:texts) = 
  SentenceAndParseTrees text $ do
    --lift $ S.putStrLn $ "nParse = " ++ (show $ CP.nParse ps)
    -- | IO [CCG.node] =lift=>           ListT IO [CCG.node] 
    -- |               =fmap(foldable)=> ListT IO (ListT IO CCG.Node)
    -- |               =join=>           ListT IO CCG.Node
    -- |               =take n=>         ListT IO CCG.Node
    node <- takeNbest (CP.nParse ps) $ join $ fmap fromFoldable $ lift $ CP.simpleParse ps text 
    let signtr' = L.nub $ (CCG.sig node) ++ signtr
        tcQueryType = UDTT.Judgment signtr' contxt (CCG.sem node) DTT.Type
        tcQueryKind = UDTT.Judgment signtr' contxt (CCG.sem node) DTT.Kind
    return $ ParseTreeAndFelicityChecks node signtr' tcQueryType $ do
               tcDiagram <- takeNbest (CP.nTypeCheck ps) $ (TY.typeCheck prover (CP.verbose ps) tcQueryType)
                                                           <|> (TY.typeCheck prover (CP.verbose ps) tcQueryKind)
               let contxt' = (DTT.trm $ Tree.node tcDiagram):contxt
               return (tcDiagram, parseWithTypeCheck ps prover signtr' contxt' texts)

-- | Take n element from the top of the list.
-- | If n < 0, it returns all the elements.
takeNbest :: Int -> ListT IO a -> ListT IO a
takeNbest n l
  | n >= 0 = ListT.take n l
  | otherwise = l
 
-- | prints a CCG node (=i-th parsing result for a given sentence) in a specified style (=HTML|text|XML|TeX)
printParseResult :: S.Handle -> Style -> Int -> Bool -> Bool -> String -> ParseResult -> IO ()
printParseResult h style sid noTypeCheck posTagOnly title (SentenceAndParseTrees sentence parseTrees) = do
    let title' = "Sentence " ++ (show sid)
    T.hPutStrLn h $ T.concat["[", T.pack title', " of ", T.pack title, ": ", sentence, "]\n"]
    parseTrees' <- toList parseTrees 
    -- | [ParseTreeAndFelicityChecks CCG.Node UDTT.TypeCheckQuery (ListT IO FelicityCheckAndMore) ]
    forM_ (zip parseTrees' ([1..]::[Int])) $ \((ParseTreeAndFelicityChecks node signtr tcQuery tcResults),ith) -> do
      let title'' = "Parse tree " ++ (show ith) ++ " of " ++ title'
      S.hPutStrLn h $ interimOf style $ "[" ++ title'' ++ ": score=" ++ (T.unpack $ CCG.showScore node) ++ "]"
      T.hPutStrLn h $ T.concat ["PF = ", CCG.pf node]
      if posTagOnly
        then do
          posTagger h style node
        else do
          T.hPutStrLn h $ printer style node
          S.hPutStrLn h $ interimOf style $ "[Signature for " ++ title'' ++ "]"
          T.hPutStrLn h $ printer style $ DTTwN.fromDeBruijnSignature signtr
          S.hPutStrLn h "\n"
          S.hPutStrLn h $ interimOf style $ "[Type check query for " ++ title'' ++ "]"
          T.hPutStrLn h $ printer style $ UDTTwN.fromDeBruijnJudgment tcQuery
      tcResults' <- toList tcResults
      --S.putStrLn $ (show $ length tcResults') ++ " results."
      forM_ (zip tcResults' ([1..]::[Int])) $ \((tcDiagram, moreResult),jth) -> do
        when (not (noTypeCheck || posTagOnly)) $ do
          let title''' = "Type check diagram " ++ (show jth) ++ " of " ++ title''
          S.hPutStrLn h "\n"
          S.hPutStrLn h $ interimOf style $ "[" ++ title''' ++ "]"
          T.hPutStrLn h $ printer style $ fmap DTTwN.fromDeBruijnJudgment tcDiagram
        printParseResult h style (sid+1) noTypeCheck posTagOnly title moreResult
printParseResult h style _ _ _ title (InferenceResults (QueryAndDiagrams psqPos proofDiagramsPos) (QueryAndDiagrams psqNeg proofDiagramsNeg)) = do
  S.hPutStrLn h $ interimOf style $ "[Positive proof search query for " ++ title ++ "]"
  T.hPutStrLn h $ printer style $ DTTwN.fromDeBruijnProofSearchQuery psqPos
  proofDiagramsPos' <- toList proofDiagramsPos
  S.hPutStrLn h $ (show $ length proofDiagramsPos') ++ " proof diagrams found\n"
  forM_ (zip proofDiagramsPos' ([1..]::[Int])) $ \(proofDiagram,kth) -> do
    let title' = "Proof diagram " ++ (show kth) ++ " for " ++ title
    S.hPutStrLn h $ interimOf style $ "[" ++ title' ++ "]"
    T.hPutStrLn h $ printer style $ fmap DTTwN.fromDeBruijnJudgment proofDiagram
  S.hPutStrLn h $ interimOf style $ "[Negative proof search query for " ++ title ++ "]"
  T.hPutStrLn h $ printer style $ DTTwN.fromDeBruijnProofSearchQuery psqNeg
  proofDiagramsNeg' <- toList proofDiagramsNeg
  S.hPutStrLn h $ (show $ length proofDiagramsNeg') ++ " proof diagrams found"
  forM_ (zip proofDiagramsNeg' ([1..]::[Int])) $ \(proofDiagram,kth) -> do
    let title' = "Proof diagram " ++ (show kth) ++ " for the negation of " ++ title
    S.hPutStrLn h $ interimOf style $ "[" ++ title' ++ "]"
    T.hPutStrLn h $ printer style $ fmap DTTwN.fromDeBruijnJudgment proofDiagram
printParseResult _ _ _ _ _ _ NoSentence = return () -- S.hPutStrLn h $ interimOf style "[End of discourse]" 

printer :: (SimpleText a, Typeset a, MathML a) => Style -> a -> T.Text
printer TEXT = toText
printer TEX  = toTeX
printer HTML = \obj -> T.concat [HTML.startMathML, toMathML obj, HTML.endMathML]
printer _    = toText

{-- Trawling functions --}

trawlParseResult :: ParseResult -> ListT IO InferenceLabel
trawlParseResult (SentenceAndParseTrees _ parseTreeAndFelicityChecks) = do
  (ParseTreeAndFelicityChecks _ _ _ felicityCheckAndMores) <- parseTreeAndFelicityChecks 
  (_, parseResult) <- felicityCheckAndMores
  label <- trawlParseResult parseResult
  return label
trawlParseResult (InferenceResults (QueryAndDiagrams _ resultPos) (QueryAndDiagrams _ resultNeg)) = do
  ifYes <- liftIO $ ListT.null resultPos
  ifNo  <- liftIO $ ListT.null resultNeg
  return $ case () of
             _ | not ifYes -> JSeM.Yes
               | not ifNo  -> JSeM.No
               | otherwise -> JSeM.Unk
trawlParseResult NoSentence = fromFoldable []

 

