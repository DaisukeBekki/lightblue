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
  , parseWithTypeCheck
  , printSentenceAndParseTrees
  , printMoreSentenceOrInference
  ) where

import Control.Monad (when,forM_,join,liftM) --base
import Control.Monad.State (lift)         --mtl
import qualified System.IO as S           --base
import qualified Data.Char as C           --base
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.List as L           --base
import ListT (ListT(..),fromFoldable,toList,take) --list-t
import qualified Parser.ChartParser as CP
import qualified Parser.CCG as CCG
import Interface 
import Interface.Text
import Interface.HTML as HTML
import Interface.TeX
import Interface.Tree as Tree
--import Parser.Language (LangOptions(..),jpOptions)
import qualified DTS.UDTTdeBruijn as UDTT
import qualified DTS.UDTTwithName as UDTTwN
import qualified DTS.DTTdeBruijn as DTT
import qualified DTS.DTTwithName as DTTwN
import qualified DTS.QueryTypes as QT
import qualified DTS.TypeChecker as TY
import qualified DTS.Prover.Wani.Prove as Wani

data InferenceSetting = InferenceSetting {
  beam :: Int     -- ^ beam width
  , maxDepth :: Maybe Int -- ^ max depth for prover
  , maxTime :: Maybe Int  -- ^ max time for prover
  , parseSetting :: CP.ParseSetting
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

getProver :: ProverName -> QT.ProverBuilder
getProver pn = case pn of
  Wani -> Wani.prove'
  Null -> TY.nullProver

{-- Data structure for sequential parsing and the inference --} 
data SentenceAndParseTrees = SentenceAndParseTrees T.Text (ListT IO ParseTreesAndFelicityCheck) -- ^ A next sentence and its parse results
data ParseTreesAndFelicityCheck = ParseTreesAndFelicityCheck CCG.Node DTT.Signature UDTT.TypeCheckQuery (ListT IO FelicityCheckAndMore) -- ^ A parse result, type check query for its felicity condition, and its results
data FelicityCheckAndMore = FelicityCheckAndMore QT.DTTProofDiagram MoreSentencesOrInference -- ^ A type check diagram and the next sentence if this is not the last sentence, or an inference query otherwise.
data MoreSentencesOrInference = MoreSentences SentenceAndParseTrees | AndInference InferenceAndResults | NoSentence 
data InferenceAndResults = InferenceAndResults DTT.ProofSearchQuery (ListT IO QT.DTTProofDiagram) -- ^ A proof search query for the inference and its results.

type Discourse = [T.Text]

-- | Parse sequential texts, and check their semantic felicity condition.
-- | If isInference = True, check whether the premise texts entails the hypothesis text.
-- | The specification of this function reflects a view about what are entailments between texts,          
-- | that is an interface problem between natural language semantics and logic
parseWithTypeCheck :: CP.ParseSetting -> QT.Prover -> DTT.Signature -> DTT.Context -> Discourse -> MoreSentencesOrInference
parseWithTypeCheck _ _ _ [] [] = NoSentence     -- ^ Context is empty and no sentece is given 
parseWithTypeCheck ps prover signtr (typ:contxt) [] = -- ^ Context is given and no more sentence (= All parse done)
  if CP.isInference ps
    then let psq = DTT.ProofSearchQuery signtr contxt typ 
         in AndInference $ InferenceAndResults psq $ takeNbest (CP.nProof ps) $ prover psq
    else NoSentence
parseWithTypeCheck ps prover signtr contxt (text:texts) = 
  MoreSentences $ SentenceAndParseTrees text $ do
    --lift $ S.putStrLn $ "nParse = " ++ (show $ CP.nParse ps)
    -- | IO [CCG.node] =lift=>           ListT IO [CCG.node] 
    -- |               =fmap(foldable)=> ListT IO (ListT IO CCG.Node)
    -- |               =join=>           ListT IO CCG.Node
    -- |               =take n=>         ListT IO CCG.Node
    node <- takeNbest (CP.nParse ps) $ join $ fmap fromFoldable $ lift $ CP.simpleParse ps text 
    let signtr' = L.nub $ (CCG.sig node) ++ signtr
        tcQuery = UDTT.Judgment signtr' contxt (CCG.sem node) DTT.Type
    return $ ParseTreesAndFelicityCheck node signtr' tcQuery $ do
               tcDiagram <- takeNbest (CP.nTypeCheck ps) $ TY.typeCheck prover (CP.verbose ps) tcQuery
               let contxt' = (DTT.trm $ Tree.node tcDiagram):contxt
               return $ FelicityCheckAndMore tcDiagram $ parseWithTypeCheck ps prover signtr' contxt' texts

-- | Take n element from the top of the list.
-- | If n < 0, it returns all the elements.
takeNbest :: Int -> ListT IO a -> ListT IO a
takeNbest n l
  | n >= 0 = ListT.take n l
  | otherwise = l
 
-- | prints a CCG node (=i-th parsing result for a given sentence) in a specified style (=HTML|text|XML|TeX)
printSentenceAndParseTrees :: S.Handle -> Style -> Bool -> Bool -> SentenceAndParseTrees -> IO ()
printSentenceAndParseTrees h style noTypeCheck posTagOnly (SentenceAndParseTrees sentence parseTrees) = do
    T.hPutStrLn h sentence
    parseTrees' <- toList parseTrees 
    -- | [ParseTreesAndFelicityCheck CCG.Node UDTT.TypeCheckQuery (ListT IO FelicityCheckAndMore) ]
    forM_ (zip parseTrees' ([1..]::[Int])) $ \((ParseTreesAndFelicityCheck node signtr tcQuery tcResults),ith) -> do
      S.hPutStrLn h $ interimOf style $ "[parse " ++ (show ith) ++ ": score=" ++ (T.unpack $ CCG.showScore node) ++ "]"
      T.hPutStrLn h $ T.concat ["PF = ", CCG.pf node]
      if posTagOnly
        then do
          posTagger h style node
        else do
          T.hPutStrLn h $ printer style node
          S.hPutStrLn h $ interimOf style $ "[Signature]"
          T.hPutStrLn h $ printer style $ DTTwN.fromDeBruijnSignature signtr
          S.hPutStrLn h $ interimOf style $ "[Type check query]"
          T.hPutStrLn h $ printer style $ UDTTwN.fromDeBruijnJudgment tcQuery
      tcResults' <- toList tcResults
      S.putStrLn $ (show $ length tcResults') ++ " results."
      forM_ (zip tcResults' ([1..]::[Int])) $ \((FelicityCheckAndMore tcDiagram moreResult),jth) -> do
        when (not (noTypeCheck || posTagOnly)) $ do
          S.hPutStrLn h $ interimOf style $ "[Type check diagram " ++ (show jth) ++ "]"
          T.hPutStrLn h $ printer style $ fmap DTTwN.fromDeBruijnJudgment tcDiagram
        printMoreSentenceOrInference h style noTypeCheck posTagOnly moreResult

printMoreSentenceOrInference :: S.Handle -> Style -> Bool -> Bool -> MoreSentencesOrInference -> IO ()
printMoreSentenceOrInference h style ntc pto (MoreSentences s) = printSentenceAndParseTrees h style ntc pto s
printMoreSentenceOrInference h style _ _ (AndInference (InferenceAndResults psq proofDiagrams)) = do
  T.hPutStrLn h $ printer style $ DTTwN.fromDeBruijnProofSearchQuery psq
  proofDiagrams' <- toList proofDiagrams
  forM_ (zip proofDiagrams' ([1..]::[Int])) $ \(proofDiagram,kth) -> do
    S.hPutStrLn h $ interimOf style $ "[Proof diagram " ++ (show kth) ++ "]"
    T.hPutStrLn h $ printer style $ fmap DTTwN.fromDeBruijnJudgment proofDiagram
printMoreSentenceOrInference h style _ _ NoSentence = return () -- S.hPutStrLn h $ interimOf style "[End of discourse]" 

printer :: (SimpleText a, Typeset a, MathML a) => Style -> a -> T.Text
printer TEXT = toText
printer TEX  = toTeX
printer HTML = \obj -> T.concat [HTML.startMathML, toMathML obj, HTML.endMathML]
printer _    = toText
