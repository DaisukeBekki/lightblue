{-# LANGUAGE RecordWildCards #-}

{-|
Module      : PartialParsing
Copyright   : (c) Daisuke Bekki, 2016, 2024
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

-}

module Parser.PartialParsing (
  -- * Partial parsing function(s)
  ParseResult(..)
  , extractParseResult
  , simpleParse
  , simpleParse'
  ) where

import qualified Data.List as L
import qualified Data.Text.Lazy as T                  --text
import qualified Data.Map as M              --container
import qualified Parser.ChartParser as CP   --lightblue
import qualified Parser.CCG as CCG --(Node, unaryRules, binaryRules, trinaryRules, isCONJ, cat, SimpleText)
--import Parser.Language (jpOptions)
import Parser.Language (defaultJpOptions)
import qualified Parser.Language.Japanese.Juman.CallJuman as Juman

{- Partial Parsing -}

simpleParse :: CP.ParseSetting 
           -> T.Text -- ^ an input text
           -> IO [CCG.Node]
simpleParse ps@CP.ParseSetting{..} sentence = do 
  --let parseSetting = defaultParseSetting {beamWidth = beamW}
  chart <- CP.parse ps sentence
  return $ case extractParseResult beamWidth chart of
             Full nodes -> nodes
             Partial nodes -> nodes
             Failed -> []

-- | For the compatibility with ABCbanKParser
simpleParse' :: Maybe (Int,Int)   -- ^ Debug mode: If Just (i,j), dump parse result of (i,j). 
            -> Int    -- ^ beam 
            -> Bool   -- ^ If purify (deplicated)
            -> (T.Text -> IO (Int -> Int -> [CCG.Node] -> [CCG.Node])) -- ^ filter for CCG nodes
            -> T.Text -- ^ an input text
            -> IO ([CCG.Node],CP.Chart)
simpleParse' ifDebug beamW ifPurify filterNodes sentence = do
  let ps = CP.ParseSetting defaultJpOptions beamW 1 1 1 ifPurify ifDebug True False
  chart <- CP.parse ps sentence
  case extractParseResult beamW chart of
    Full nodes -> return (nodes,chart)
    Partial nodes -> return (nodes,chart)
    Failed -> return ([],chart)

-- | A data type for the parsing result.
data ParseResult = 
  Full [CCG.Node]      -- ^ when there are at least one node in the topmost box in the chart, returning the nodes.
  | Partial [CCG.Node] -- ^ when the parser did not obtain the full result, it collects partially parsed segments from the chart, returning their conjunctions.
  | Failed             -- ^ when no box in the chart contains any node.
  deriving (Eq,Show)

-- | takes a (parse result) chart and returns a list consisting of nodes, partially parsed substrings.
extractParseResult :: Int -> CP.Chart -> ParseResult
extractParseResult beam chart = 
  f $ L.sortBy isLessPrivilegedThan $ filter (\((_,_),nodes) -> not (L.null nodes)) $ M.toList $ chart
  where f [] = Failed
        f c@(((i,_),nodes):_) | i == 0 = Full $ map CCG.wrapNode (sortByNumberOfArgs nodes)
                              | otherwise = Partial $ g (map CCG.wrapNode (sortByNumberOfArgs nodes)) (filter (\((_,j),_) -> j <= i) c)
        g results [] = results
        g results (((i,_),nodes):cs) = g (take beam [CCG.conjoinNodes x y | x <- map CCG.wrapNode nodes, y <- results]) $ filter (\((_,j),_) -> j <= i) cs

-- | a `isLessPriviledgedThan` b means that b is more important parse result than a.
isLessPrivilegedThan :: ((Int,Int),a) -> ((Int,Int),a) -> Ordering
isLessPrivilegedThan ((i1,j1),_) ((i2,j2),_) | i1 == i2 && j1 == j2 = EQ
                                             | j2 > j1 = GT
                                             | j1 == j2 && i2 < i1 = GT
                                             | otherwise = LT

sortByNumberOfArgs :: [CCG.Node] -> [CCG.Node]
sortByNumberOfArgs = L.sortOn (\node -> (numberOfArgs $ CCG.cat node, node))

-- | receives a category and returns an integer based on the number of arguments of the category, which is used for sorting nodes with respect to which node is considered to be a better result of the parsing.  Lesser is better, but non-propositional categories (such as NP, CONJ, LPAREN and RPAREN) are the worst (=10) even if they take no arguments.
numberOfArgs :: CCG.Cat -> Int
numberOfArgs node = case node of
  CCG.SL x _   -> (numberOfArgs x) + 1
  CCG.BS x _   -> (numberOfArgs x) + 1
  CCG.T _ _ c  -> numberOfArgs c
  CCG.S _      -> 1
  CCG.NP _     -> 10
  CCG.Sbar _   -> 0
  CCG.N        -> 2
  CCG.CONJ     -> 100
  CCG.LPAREN   -> 100
  CCG.RPAREN   -> 100
  CCG.PUNCT    -> 100
  CCG.PERIOD   -> 100
