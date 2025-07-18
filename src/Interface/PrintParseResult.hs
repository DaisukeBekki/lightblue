{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Interface.PrintParseResult
Copyright   : Koharu Saeki
Licence     : All right reserved
Maintainer  : Koharu Saeki 
Stability   : beta

A module for Natural Language Inference 
-}

module Interface.PrintParseResult (
  printParseResult
  ) where

import Control.Monad (when,forM_)    --base
import qualified System.IO as S           --base
import ListT (toList) --list-t
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import Interface                               --lightblue
import Interface.HTML as HTML                  --lightblue
import Interface.Text                          --lightblue
import Interface.TeX                           --lightblue
import qualified Parser.CCG as CCG             --lightblue
import DTS.NaturalLanguageInference            --lightblue
import qualified DTS.UDTTwithName as UDTTwN    --lightblue
import qualified DTS.DTTwithName as DTTwN      --lightblue

-- | prints a CCG node (=i-th parsing result for a given sentence) in a specified style (=HTML|text|XML|TeX)
printParseResult :: S.Handle -> Style -> Int -> Bool -> Bool -> String -> ParseResult -> IO ()
printParseResult h style sid noTypeCheck posTagOnly title (SentenceAndParseTrees sentence parseTrees) = do
    let title' = "Sentence " ++ (show sid)
    T.hPutStrLn h $ T.concat["[", T.pack title', " of ", T.pack title, ": ", sentence, "]\n"]
    parseTrees' <- toList parseTrees 
    -- | [ParseTreeAndFelicityChecks CCG.Node UDTT.TypeCheckQuery (ListT IO FelicityCheckAndMore) ]
    forM_ (zip parseTrees' ([1..]::[Int])) $ \((ParseTreeAndFelicityChecks node signtr tcQuery tcResults),ith) -> do
      let title'' = "Parse tree " ++ (show ith) ++ " of " ++ title'
      S.hPutStrLn h $ interimOf style $ "[" ++ title'' ++ "]"
      T.hPutStrLn h $ T.concat ["PF = ", CCG.pf node, " / Score = ", CCG.showScore node]
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