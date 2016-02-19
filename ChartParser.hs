{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module ChartParser (
  parse,
  printChart,
  topBox,
  printNodes,
  printChartInSimpleText
  ) where

import qualified System.IO as S
import Data.List
import qualified Data.Text.Lazy as T
import qualified Data.Map as M
--import qualified Data.Time as Time
--import qualified System.IO as S
import qualified CombinatoryCategorialGrammar as G --(Node, unaryRules, binaryRules, trinaryRules, isCONJ, cat, SimpleText)
import qualified JumanLexicon as L (Lexicon, lookupLexicon, setupLexicon, emptyCategories)
import Data.Char

-- | The type 'Chart' is a type for CYK-charts. 
type Chart = M.Map (Int,Int) [G.Node]

-- | The 'printChart' function displays the CYK chart
printChart :: S.Handle -> Chart -> IO()
printChart handle chart = mapM_ printList $ M.toList $ M.filter (/= []) chart
  where printList (key,nodes) = do -- list化したChartを画面表示する。
          S.hPutStr handle $ "\\subsubsection*{" ++ show key ++ ": ノード数 " ++ (show $ length nodes) ++ "}"
          mapM_ (\node -> S.hPutStrLn handle $ "\\noindent\\kern-4em\\scalebox{.3}{" ++ T.unpack (G.toTeX node) ++ "\\\\}\\par\\medskip") nodes
-- | 
printNodes :: S.Handle -> Int -> [G.Node] -> IO()
printNodes handle n nodes = mapM_ (\node -> S.hPutStrLn handle $ "\\noindent\\kern-2em\\scalebox{.3}{" ++ T.unpack (G.toTeX node) ++ "\\\\}\\par\\medskip") $ take n $ nodes

-- |
printChartInSimpleText :: S.Handle -> [G.Node] -> IO()
printChartInSimpleText handle nodes = mapM_ (\node -> S.hPutStr handle $ (T.unpack $ G.toText node) ++ "\n") $ reverse nodes

-- | Main parsing function
parse :: Int -> T.Text -> IO(Chart)
parse beam sentence = do
  --start <- Time.getCurrentTime
  lexicon <- L.setupLexicon sentence
  --stop <- Time.getCurrentTime    
  --S.hPutStrLn S.stderr $ "Setting up numeration: " ++ show (Time.diffUTCTime stop start)
  return $ parseMain beam lexicon sentence

-- | The 'parseMain' function parses a (Japanees) text and generates a CYK-chart
--   The Int is the beam width.
parseMain :: Int -> L.Lexicon -> T.Text -> Chart
parseMain beam lexicon text
  | text == T.empty = M.empty -- foldl returns a runtime error when t is empty
  | otherwise       = let (chart,_,_) = T.foldl' (chartAccumulator beam lexicon) (M.empty, 0, T.empty) (purifyText text) 
                      in chart
  where --purifyText :: T.Text -> T.Text
    purifyText = T.filter (\c -> not (isSpace c) || c /= '、' || c /= '。' || c /= '，' || c /= '．') 

-- | The 'lookupChart' function looks up a chart with the key (i,j) 
--   and returns the value of type [Node]
lookupChart :: Int -> Int -> Chart -> [G.Node]
lookupChart i j chart = 
  case (M.lookup (i,j) chart) of Just list -> list
                                 Nothing   -> []

type PartialChart = (Chart,Int,T.Text)

-- | The 'chartAccumulator' function
--
chartAccumulator :: Int -> L.Lexicon -> PartialChart -> Char -> PartialChart
chartAccumulator beam lexicon partialchart c = 
  let (chart,i,stack) = partialchart in
  let newstack = (T.cons c stack) in
  let (newchart,_,_,to) = T.foldl' (boxAccumulator beam lexicon) (chart,T.empty,i,i+1) newstack in
  (newchart,to,newstack)

type PartialBox = (Chart,T.Text,Int,Int)

-- | The 'boxAccumulator' function
boxAccumulator :: Int -> L.Lexicon -> PartialBox -> Char -> PartialBox
boxAccumulator beam lexicon partialbox c = 
  let (chart,word,i,j) = partialbox in
  let newword = T.cons c word in 
  let list0 = L.lookupLexicon newword lexicon in
  let list1 = checkUnaryRules list0 in
  let list2 = checkBinaryRules i j chart list1 in
  let list3 = checkTrinaryRules i j chart list2 in
  let list4 = checkEmptyCategories list3 in 
--  let list5 = checkEmptyCategories list4 in 
  ((M.insert (i,j) (take beam $ reverse $ sort list4) chart), newword, i-1, j)

checkUnaryRules :: [G.Node] -> [G.Node]
checkUnaryRules prevlist = 
  foldl' (\acc node -> G.unaryRules node acc) prevlist prevlist
                      
checkBinaryRules :: Int -> Int -> Chart -> [G.Node] -> [G.Node]
checkBinaryRules i j chart prevlist = 
  foldl' (\acck k -> foldl' (\accl lnode -> foldl' (\accr rnode -> G.binaryRules lnode rnode accr) 
                                                   accl  
                                                   (lookupChart k j chart)) 
                            acck 
                            (lookupChart i k chart)) 
         prevlist
         (take (j-i-1) [i+1..]) -- [k | i<k<j]

checkTrinaryRules :: Int -> Int -> Chart -> [G.Node] -> [G.Node]
checkTrinaryRules i j chart prevlist =
  foldl' (\acck k -> foldl' (\accc cnode -> foldl' (\accl lnode -> foldl' (\accr rnode -> G.trinaryRules lnode cnode rnode accr)
                                                                          accl
                                                                          (lookupChart (k+1) j chart))
                                                   accc
                                                   (lookupChart i k chart))
                            acck
                            (filter (\n -> G.isCONJ (G.cat n)) (lookupChart k (k+1) chart)))
         prevlist
         (take (j-i-2) [i+1..]) -- [k | i<k<j-1]  i-k k-k+1 k+1-j

checkEmptyCategories :: [G.Node] -> [G.Node]
checkEmptyCategories prevlist =
  foldr (\p -> (G.binaryRules (fst p) (snd p)) . (G.binaryRules (snd p) (fst p))) prevlist [(x,y) | x <- prevlist, y <- L.emptyCategories]

-- | The function "topBox" picks up the nodes in the "top" box in the chart.
--
topBox :: Chart -> [G.Node]
topBox chart = let ((_,k),_) = M.findMax chart in
                 case M.lookup (0,k) chart of 
                   Just nodes -> reverse $ sort nodes
                   Nothing    -> []

