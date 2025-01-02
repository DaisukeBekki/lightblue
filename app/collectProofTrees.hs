{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Options.Applicative hiding (style) --optparse-applicative
import Control.Applicative (optional)     --base
import Control.Monad (forM)               --base
import ListT (ListT(..),toList,fromFoldable)                     --list-t
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.Text as StrictT     --text
import qualified Data.Text.IO as StrictT  --text
import Data.Ratio ((%))                   --base
import qualified Data.Char as C           --base
import qualified Data.List as L           --base
import qualified Data.Fixed as F          --base
import qualified System.IO as S           --base
import qualified System.Environment as E -- base
import qualified Data.Map as M            --container
import qualified Data.Time as Time        --time
import qualified Parser.ChartParser as CP
import qualified Parser.PartialParsing as CP
import qualified Parser.Language.Japanese.Lexicon as LEX
import qualified Parser.Language.Japanese.MyLexicon as LEX
import qualified Parser.Language.Japanese.Juman.CallJuman as Juman
import qualified Parser.Language.Japanese.Filter.KNPFilter as Filter
import qualified Parser.Language.Japanese.Filter.KWJAFilter as Filter
import Parser.Language (jpOptions)
import qualified Interface as I
import qualified Interface.Text as I
import qualified JSeM as J
import qualified JSeM.XML as J
import qualified DTS.UDTTdeBruijn as UDTT
import qualified DTS.DTTdeBruijn as DTT
import DTS.TypeChecker (typeInfer,nullProver)
import qualified DTS.QueryTypes as QT
import qualified DTS.NaturalLanguageInference as NLI
import qualified JSeM as JSeM                         --jsem

main :: IO()
main = do
    contents <- T.readFile "../jsem/data/v1.0/Verbs.xml"
    lexicalResource <- LEX.lexicalResourceBuilder Juman.KWJA
    let style = I.TEXT
        beamW = 32
        nParse = 1
        nTypeCheck = 1
        nProof = 1
        nodeFilter = Nothing
        noInference = False
        noTypeCheck = False
        nSample = 10
        verbose = False
        maxDepth = Just 6
        maxTime = Nothing
        handle = S.stdout
        parseSetting = CP.ParseSetting jpOptions lexicalResource beamW nParse nTypeCheck nProof True Nothing nodeFilter noInference verbose
        prover = NLI.getProver NLI.Wani $ QT.ProofSearchSetting maxDepth maxTime (Just QT.Classical)
    parsedJSeM <- J.xml2jsemData $ T.toStrict contents
    parseResults <- forM parsedJSeM $ \j -> do
        let title = "JSeM-ID " ++ (StrictT.unpack $ J.jsem_id j)
        S.putStr $ "[" ++ title ++ "] "
        mapM_ StrictT.putStr $ J.premises j
        S.putStr " ==> "
        StrictT.putStrLn $ J.hypothesis j
        let sentences = reverse $ (T.fromStrict $ J.hypothesis j):(map T.fromStrict $ J.premises j)
        return $ toList $ trawlParseResult $ NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity)] [] sentences
    proofDiagrams <- mconcat parseResults
    mapM_ (T.putStrLn . I.toText) $ take nSample proofDiagrams

{-- Trawling functions --}

trawlParseResult :: NLI.ParseResult -> ListT IO QT.DTTProofDiagram
trawlParseResult (NLI.SentenceAndParseTrees _ parseTreeAndFelicityChecks) = do
  (NLI.ParseTreeAndFelicityChecks _ _ _ felicityCheckAndMores) <- parseTreeAndFelicityChecks 
  (_, parseResult) <- felicityCheckAndMores
  trawlParseResult parseResult
trawlParseResult (NLI.InferenceResults (NLI.QueryAndDiagrams _ resultPos) (NLI.QueryAndDiagrams _ resultNeg)) = mappend resultPos resultNeg
trawlParseResult NLI.NoSentence = fromFoldable []
  