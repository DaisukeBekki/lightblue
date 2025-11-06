{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface.Express.Lightblue (
  parseSentence',
  parse,
  parseWithTypeCheck,
  getTypeCheckDiagram,
  parseWithTypeCheck3,
  getProofSearchDiagram,
  parseSentenceForQuery,
  parseSentenceForDiagram
  ) where

import Control.Monad (forM_,forM)
import qualified Data.Text.Lazy as T
import qualified Parser.ChartParser as CP
import qualified Parser.CCG as CCG
import qualified Parser.LangOptions as PL (defaultJpOptions)
import qualified DTS.NaturalLanguageInference as NLI
import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as DTT
import qualified DTS.UDTTdeBruijn as UDTT
import  System.IO.Unsafe (unsafePerformIO)
import System.IO (hPutStrLn, stderr)
import ListT (ListT(..),fromFoldable,toList,take,null) --list-t
import Debug.Trace 

parseSentence' :: NLI.ParseResult -> IO ([CCG.Node])
parseSentence' parseResult = do
  -- ParseResult から Nodeを取り出す
  -- getNodes :: NLI.ParseResult -> [CCG.Node]
  nodes <- getNodes parseResult
  System.IO.hPutStrLn stderr $ "Number of nodes in parseSentence': " ++ show (length nodes) -- 1となる
  return nodes

parseSentenceForQuery :: NLI.ParseResult -> IO ([UDTT.TypeCheckQuery])
parseSentenceForQuery parseResult = do
  tcqs <- getTypeCheckQuery parseResult
  return tcqs

parseSentenceForDiagram :: NLI.ParseResult -> IO ([[QT.DTTProofDiagram]])
parseSentenceForDiagram parseResult = do
  let tcdsList = getTypeCheckDiagram2 parseResult
  return tcdsList

getNodes :: NLI.ParseResult -> IO [CCG.Node]
getNodes NLI.NoSentence = do
  return []
getNodes (NLI.SentenceAndParseTrees _ parseTrees) = do
  -- toList :: Monad m => ListT m a -> m [a]
  -- parseTrees' :: [ParseTreeAndFelicityChecks]
  parseTrees' <-  toList parseTrees
  let nodeList = map (\(NLI.ParseTreeAndFelicityChecks node _ _ _) -> node) parseTrees'
  return nodeList
getNodes (NLI.InferenceResults _ _) = do
  return []

getTypeCheckQuery :: NLI.ParseResult -> IO [UDTT.TypeCheckQuery]
getTypeCheckQuery NLI.NoSentence = do
  return []
getTypeCheckQuery (NLI.SentenceAndParseTrees _ parseTrees) = do
  parseTrees' <-  toList parseTrees
  let tcqList = map (\(NLI.ParseTreeAndFelicityChecks _ _ tcq _) -> tcq) parseTrees'
  return tcqList
getTypeCheckQuery (NLI.InferenceResults _ _) = do
  return []

-- ParseSetting, beamだけ指定
defaultParseSetting' :: Int -> IO CP.ParseSetting
defaultParseSetting' beam = do
  jpOptions <- PL.defaultJpOptions
  return $ CP.ParseSetting jpOptions beam (-1) (-1) (-1) True Nothing True False


defaultParseSettingForInference :: IO CP.ParseSetting
defaultParseSettingForInference = do
  jpOptions <- PL.defaultJpOptions
  return $ CP.ParseSetting jpOptions 24 1 1 5 True Nothing False False


-- beamとsentenceのみ用いてchartを得る
parse :: Int           -- ^ The beam width
          -> T.Text        -- ^ A sentence to be parsed
          -> IO CP.Chart   -- ^ The resulting CYK-chart
parse beam sentence = do
    useOnlybeamParseSetting <- trace ("useOnlybeamParseSetting") defaultParseSetting' beam
    chart <- trace ("chart") CP.parse useOnlybeamParseSetting sentence 
    return chart

-- beamとsentenceを用いてParseResult取り出す
-- parseWithTypeCheck :: CP.ParseSetting -> QT.Prover -> DTT.Signature -> DTT.Context -> Discourse -> ParseResult
parseWithTypeCheck :: Int -> T.Text -> IO NLI.ParseResult
parseWithTypeCheck beam sentence = do
  useOnlybeamParseSetting <- defaultParseSetting' beam -- useOnlybeamParseSetting :: CP.ParseSetting
  let prover = NLI.getProver NLI.Wani $ QT.ProofSearchSetting Nothing Nothing (Just QT.Classical)
  let parseResult = NLI.parseWithTypeCheck useOnlybeamParseSetting prover [("dummy",DTT.Entity)] [] $ T.lines sentence
  return parseResult

-- for seeing proof search diagram
-- discourceを用いてParseResultを取り出す
parseWithTypeCheck3 :: [T.Text] -> IO NLI.ParseResult
parseWithTypeCheck3 discource = do
  parseSetting <- defaultParseSettingForInference
  let prover = NLI.getProver NLI.Wani $ QT.ProofSearchSetting Nothing Nothing (Just QT.Classical)
  let parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity)] [] discource
  return parseResult
  

getTypeCheckDiagram :: NLI.ParseResult -> [QT.DTTProofDiagram]
getTypeCheckDiagram NLI.NoSentence = []
-- parseTrees :: ListT IO ParseTreeAndFelicityChecks
getTypeCheckDiagram (NLI.SentenceAndParseTrees _ parseTrees) = 
    -- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
    -- toList :: Monad m => ListT m a -> m [a]
    -- (unsafePerformIO $ toList parseTrees) :: [ParseTreeAndFelicityChecks]
    concatMap (\(NLI.ParseTreeAndFelicityChecks _ _ _ tcResults) -> 
        -- concatMap (\(tcDiagram, _) -> [tcDiagram]) $ (unsafePerformIO $ toList tcResults) :: [QT.DTTProofDiagram]
        concatMap (\(tcDiagram, _) -> [tcDiagram]) $ (unsafePerformIO $ toList tcResults)) 
    (unsafePerformIO $ toList parseTrees)
getTypeCheckDiagram (NLI.InferenceResults _ _) = []

getTypeCheckDiagram2 :: NLI.ParseResult -> [[QT.DTTProofDiagram]]
getTypeCheckDiagram2 NLI.NoSentence = []
-- parseTrees :: ListT IO ParseTreeAndFelicityChecks
getTypeCheckDiagram2 (NLI.SentenceAndParseTrees _ parseTrees) = 
    -- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
    -- toList :: Monad m => ListT m a -> m [a]
    -- (unsafePerformIO $ toList parseTrees) :: [ParseTreeAndFelicityChecks]
    map (\(NLI.ParseTreeAndFelicityChecks _ _ _ tcResults) -> 
        concatMap (\(tcDiagram, _) -> [tcDiagram]) $ (unsafePerformIO $ toList tcResults)) 
    (unsafePerformIO $ toList parseTrees) -- :: [(QT.DTTProofDiagram, ParseResult)]
getTypeCheckDiagram2 (NLI.InferenceResults _ _) = []


getProofSearchDiagram :: NLI.ParseResult -> ListT IO QT.DTTProofDiagram
getProofSearchDiagram (NLI.SentenceAndParseTrees _ parseTreeAndFelicityChecks) = do
  (NLI.ParseTreeAndFelicityChecks _ _ _ felicityCheckAndMores) <- trace ("parseTreeAndFelicityChecks") parseTreeAndFelicityChecks
  (_, parseResult) <- trace ("felicityCheckAndMores") felicityCheckAndMores
  diagrams <- trace ("再帰") getProofSearchDiagram parseResult
  return diagrams
getProofSearchDiagram (NLI.InferenceResults (NLI.QueryAndDiagrams _ resultPos) _) = trace ("InferenceResults") resultPos
getProofSearchDiagram NLI.NoSentence = trace ("No Sentence") fromFoldable []