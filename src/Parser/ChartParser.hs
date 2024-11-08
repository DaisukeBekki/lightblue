{-# LANGUAGE RecordWildCards #-}

{-|
Module      : ChartParser
Copyright   : (c) Daisuke Bekki, 2016, 2024
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

A left-corner CKY-parser for lexical grammars.
-}

module Parser.ChartParser (
  -- * Data structures for CCG derivations
  Chart
  , CCG.Node(..)
  -- * Main parsing functions
  , ParseSetting(..)
  , defaultParseSetting
  , parse
  , simpleParse
  , simpleParse'
  -- * Partial parsing function(s)
  , ParseResult(..)
  , extractParseResult
  ) where

import Data.List as L
import Data.Char                          --base
import System.IO.Unsafe (unsafePerformIO) --base
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.Map as M         --container
import qualified Parser.CCG as CCG --(Node, unaryRules, binaryRules, trinaryRules, isCONJ, cat, SimpleText)
import Parser.Language (LangOptions(..),jpOptions)
import qualified Parser.Language.Japanese.Juman.CallJuman as Juman
import qualified Parser.Language.Japanese.Lexicon as L (LexicalResource(..), lexicalResourceBuilder, LexicalItems, lookupLexicon, setupLexicon, emptyCategories, myLexicon)
import qualified Parser.Language.Japanese.Templates as LT
import qualified DTS.QueryTypes as QT

{- Main functions -}

-- | The type for CYK-charts.
data ParseSetting = ParseSetting {
  langOptions :: LangOptions   -- ^ Language options
  , lexicalResource :: L.LexicalResource
  -- , morphaName :: Juman.MorphAnalyzerName -- ^ Morphological analyzer
  , beamWidth :: Int           -- ^ The beam width
  , nParse :: Int              -- ^ Show N-best parse trees for each sentence
  , nTypeCheck :: Int          -- ^ Show N-best type check diagram for each logical form
  , nProof :: Int              -- ^ Show N-best proof diagram for each proof search
  , ifPurify :: Bool           -- ^ If True, apply purifyText to the input text before parsing
  , ifDebug :: Maybe (Int,Int) -- ^ Debug mode: If Just (i,j), then debug mode and dump parse result of (i,j). If Nothing, then non-debug mode
  , ifFilterNode :: Maybe (Int -> Int -> [CCG.Node] -> [CCG.Node]) -- ^ filter for CCG nodes.  Nothing: no filtering
  , noInference :: Bool        -- ^ If True, it is an inference and execute proof search
  , verbose :: Bool            -- ^ If True, type checker and inferer dump logs
  } 

defaultParseSetting :: IO ParseSetting
defaultParseSetting = do
  lr <- L.lexicalResourceBuilder Juman.KWJA
  return $ ParseSetting jpOptions lr 32 (-1) (-1) (-1) True Nothing Nothing True False

type Chart = M.Map (Int,Int) [CCG.Node]

-- | Main parsing function to parse a Japanees sentence that generates a CYK-chart.
parse :: ParseSetting 
         -> T.Text    -- ^ A sentence to be parsed
         -> IO Chart  -- ^ A pair of the resulting CYK-chart and a list of CYK-charts for segments
parse ParseSetting{..} sentence 
  | sentence == T.empty = return M.empty -- returns an empty chart, otherwise foldl returns a runtime error when text is empty
  | otherwise = do
      let sentenceToParse = if ifPurify
                              then purifyText langOptions sentence
                              else sentence
          nodeFilter = case ifFilterNode of
                         Just filter -> filter
                         Nothing -> (\_ _ -> id)
      lexicon <- L.setupLexicon lexicalResource sentenceToParse
      let (chart,_,_,_) = T.foldl' (chartAccumulator ifDebug beamWidth lexicon nodeFilter) 
                                   (M.empty,[0],0,T.empty)
                                   sentenceToParse
      return chart

simpleParse :: ParseSetting 
           -> T.Text -- ^ an input text
           -> IO [CCG.Node]
simpleParse ps@ParseSetting{..} sentence = do 
  --let parseSetting = defaultParseSetting {beamWidth = beamW}
  chart <- parse ps sentence
  return $ case extractParseResult beamWidth chart of
             Full nodes -> nodes
             Partial nodes -> nodes
             Failed -> []

-- | For the compatibility with ABCbanKParser
simpleParse' :: Maybe (Int,Int)   -- ^ Debug mode: If Just (i,j), dump parse result of (i,j). 
            -> Int    -- ^ beam 
            -> Bool   -- ^ If purify
            -> (Int -> Int -> [CCG.Node] -> [CCG.Node]) -- ^ filter for CCG nodes
            -> T.Text -- ^ an input text
            -> IO([CCG.Node],Chart)
simpleParse' ifDebug beamW ifPurify filterNodes sentence = do
  lexicalResource <- L.lexicalResourceBuilder Juman.KWJA
  chart <- parse (ParseSetting jpOptions lexicalResource beamW 1 1 1 ifPurify ifDebug (Just filterNodes) True False) sentence
  case extractParseResult beamW chart of
    Full nodes -> return (nodes,chart)
    Partial nodes -> return (nodes,chart)
    Failed -> return ([],chart)

-- parse :: Int           -- ^ The beam width
--          -> Bool       -- ^ If True, use purifyText
--          -> (Int -> Int -> [CCG.Node] -> [CCG.Node]) -- ^ filter for CCG nodes
--          -> T.Text     -- ^ A sentence to be parsed
--          -> IO (Chart) -- ^ A pair of the resulting CYK-chart and a list of CYK-charts for segments
-- parse = parse' Nothing
-- | The main routine of 'parse' function
-- parse' :: Maybe (Int,Int) -- ^ Debug mode: If Just (i,j), then debug mode and dump parse result of (i,j). If Nothing, then non-debug mode
--           -> Int          -- ^ The beam width
--           -> Bool         -- ^ If True, use purifyText
--           -> (Int -> Int -> [CCG.Node] -> [CCG.Node]) -- ^ filter for CCG nodes
--           -> T.Text       -- ^ A sentence to be parsed
--           -> IO (Chart)   -- ^ A pair of the resulting CYK-chart and a list of CYK-charts for segments
-- parse' ifDebug beam ifPurify filterNodes sentence 
--   | sentence == T.empty = return M.empty -- returns an empty chart, otherwise foldl returns a runtime error when text is empty
--   | otherwise = do
--       lexicon <- L.setupLexicon (T.replace "―" "。" sentence)
--       let (chart,_,_,_) = T.foldl' (chartAccumulator ifDebug beam lexicon filterNodes) 
--                                    (M.empty,[0],0,T.empty)
--                                    (if ifPurify
--                                       then purifyText sentence
--                                       else sentence)
--       return chart

-- | Simple parsing function to return just the best node for a given sentence
-- simpleParse :: Int    -- ^ beam 
--             -> T.Text -- ^ an input text
--             -> IO [CCG.Node]
-- simpleParse beam sentence = do 
--   (nodes,_) <- simpleParse' Nothing beam True (\_ _ -> id) sentence
--   return nodes

-- simpleParse' :: Maybe (Int,Int)   -- ^ Debug mode: If Just (i,j), dump parse result of (i,j). 
--             -> Int    -- ^ beam 
--             -> Bool   -- ^ If purify
--             -> (Int -> Int -> [CCG.Node] -> [CCG.Node]) -- ^ filter for CCG nodes
--             -> T.Text -- ^ an input text
--             -> IO([CCG.Node],Chart)
-- simpleParse' ifDebug beam ifPurify filterNodes sentence = do
--   chart <- parse' ifDebug beam ifPurify filterNodes sentence
--   case extractParseResult beam chart of
--     Full nodes -> return (nodes,chart)
--     Partial nodes -> return (nodes,chart)
--     Failed -> return ([],chart)

-- | removes occurrences of non-letters from an input text.
purifyText :: LangOptions -> T.Text -> T.Text
purifyText langOptions text = 
  case T.uncons text of -- remove a non-literal symbol at the beginning of a sentence (if any)
    Nothing -> T.empty
    Just (c,t) | isSpace c                                 -> purifyText langOptions t               -- ignore white spaces
               | T.any (==c) (symbolsToIgnore langOptions) -> purifyText langOptions t               -- ignore meaningless symbols
               | T.any (==c) (punctuations langOptions)    -> T.cons '、' $ purifyText langOptions t -- punctuations
               | otherwise                                 -> T.cons c $ purifyText langOptions t

-- | quadruples representing a state during parsing:
-- the parsed result (Chart) of the left of the pivot,
-- the stack of ending positions of the previous 'separators' (i.e. '、','，',etc), 
-- the pivot (=the current parsing position), and
-- the revsersed list of chars that has been parsed
type PartialChart = (Chart,[Int],Int,T.Text)

-- | The 'chartAccumulator' function is the accumulator of the 'parse' function
chartAccumulator :: Maybe (Int,Int)   -- ^ Debug mode: If Just (i,j), dump parse result of (i,j). 
                    -> Int            -- ^ The beam width as the first parameter
                    -> L.LexicalItems -- ^ my lexicon as the second parameter
                    -> (Int -> Int -> [CCG.Node] -> [CCG.Node]) -- ^ filter for CCG nodes
                    -> PartialChart   -- ^ The accumulated result, given
                    -> Char           -- ^ The next char of a unparsed text
                    -> PartialChart   -- ^ The accumulated result, updated
chartAccumulator ifDebug beam lexicon filterNodes (chart,seplist@(sep:seps),i,stack) c 
  -- The case where the next Char is a punctuation. Recall that each seperator is an end of a phase
  | c == '、' = let newchart = M.fromList $ ((i,i+1),[andCONJ (T.singleton c), emptyCM (T.singleton c)]):(foldl' (punctFilter sep i) [] $ M.toList chart);
                    newstack = T.cons c stack
               in (newchart, ((i+1):seplist), (i+1), newstack) --, (take 1 (sort (lookupChart sep (i+1) newchart)):parsed))
  | c == '。' = let newchart = M.fromList $ foldl' (punctFilter sep i) [] $ M.toList chart;
                    newstack = T.cons c stack
               in (newchart, ((i+1):seplist), (i+1), newstack) --, (take 1 (sort (lookupChart sep (i+1) newchart)):parsed))
  | otherwise 
     = let newstack = (T.cons c stack);
           (newchart,_,_,_) = T.foldl' (boxAccumulator ifDebug beam filterNodes lexicon) (chart,T.empty,i,i+1) newstack;
           newseps | c `elem` ['「','『'] = (i+1:seplist)
                   | c `elem` ['」','』'] = seps
                   | otherwise = seplist 
       in (newchart,newseps,(i+1),newstack)
-- chartAccumulator _ _ (_,[],_,_) _ = ?

-- | 
punctFilter :: Int    -- ^ Previous pivot
               -> Int -- ^ Current pivot
               -> [((Int,Int),[CCG.Node])] -- ^ The list of nodes that has been endorced
               -> ((Int,Int),[CCG.Node])   -- ^ With respect to a given entry (=e@((from,to),nodes)), 
               -> [((Int,Int),[CCG.Node])]
punctFilter sep i charList e@((from,to),nodes) 
  | to == i = ((from,to+1),filter (CCG.isBunsetsu . CCG.cat) nodes):(e:charList) 
  | otherwise = e:charList
                -- if from <= sep
                --    then e:charList
                --    else charList

andCONJ :: T.Text -> CCG.Node
andCONJ c = LT.lexicalitem c "punct" 100 CCG.CONJ LT.andSR

emptyCM :: T.Text -> CCG.Node
emptyCM c = LT.lexicalitem c "punct" 99 (((CCG.T True 1 LT.modifiableS) `CCG.SL` ((CCG.T True 1 LT.modifiableS) `CCG.BS` (CCG.NP [CCG.F[CCG.Ga,CCG.O]]))) `CCG.BS` (CCG.NP [CCG.F[CCG.Nc]])) LT.argumentCM

type PartialBox = (Chart,T.Text,Int,Int)

-- | The 'boxAccumulator' function
boxAccumulator :: Maybe (Int,Int)   -- ^ Debug mode: If Just (i,j), dump parse result of (i,j). 
                  -> Int            -- ^ beam width
                  -> (Int -> Int -> [CCG.Node] -> [CCG.Node]) -- ^ filter for appropriate CCG nodes
                  -> L.LexicalItems -- ^ my lexicon
                  -> PartialBox     -- ^ accumulated result (Chart, Text, Int, Int)
                  -> Char           -- ^ 
                  -> PartialBox
boxAccumulator ifDebug beam filterNodes lexicon (chart,word,i,j) c = unsafePerformIO $ do
  let newword = T.cons c word;
      list0 = if (T.compareLength newword 23) == LT 
                -- Does not execute lookup for a long word. Run "LongestWord" to check that the length of the longest word (=23).
                then L.lookupLexicon newword lexicon
                else [];
      list1 = checkBinaryRules i j chart $ checkUnaryRules list0 
      beforeFiltering = list1
      afterFiltering = take beam $ L.sort $ checkEmptyCategories $ checkParenthesisRule i j chart $ checkCoordinationRule i j chart $ filterNodes i j $ beforeFiltering
  case ifDebug of
    Just (x,y) ->
      if i >= x && j <= y
        then do
          putStr $ "\n------" ++ (show (i,j)) ++ "------"  
          putStrLn "\nBefore filtering: "
          print beforeFiltering
          putStrLn "\nAfter filtering: "
          print afterFiltering
        else return ()
    Nothing -> return ()
  return $ ((M.insert (i,j) afterFiltering chart), newword, i-1, j)
  --((M.insert (i,j) (cutoff (max (beam+i-j) 24) list1) chart), newword, i-1, j)

-- | take `beam` nodes from the top of `ndoes`.
--cutoff :: Int -> [CCG.Node] -> [CCG.Node]
--cutoff beam nodes = if length nodes <= beam then nodes else take beam $ sort nodes

-- | looks up a chart with the key (i,j) and returns the value of type [Node]
lookupChart :: Int -> Int -> Chart -> [CCG.Node]
lookupChart i j chart = 
  case (M.lookup (i,j) chart) of Just list -> list
                                 Nothing   -> []

checkUnaryRules :: [CCG.Node] -> [CCG.Node]
checkUnaryRules prevlist = 
  foldl' (\acc node -> CCG.unaryRules node acc) prevlist prevlist

checkBinaryRules :: Int -> Int -> Chart -> [CCG.Node] -> [CCG.Node]
checkBinaryRules i j chart prevlist = 
  foldl' (\acck k -> foldl' (\accl lnode -> foldl' (\accr rnode -> CCG.binaryRules lnode rnode accr) 
                                                   accl  
                                                   (lookupChart k j chart)) 
                            acck 
                            (lookupChart i k chart)) 
         prevlist
         (take (j-i-1) [i+1..]) -- [k | i<k<j]

checkCoordinationRule :: Int -> Int -> Chart -> [CCG.Node] -> [CCG.Node]
checkCoordinationRule i j chart prevlist =
  foldl' (\acck k -> foldl' (\accc cnode -> foldl' (\accl lnode -> foldl' (\accr rnode -> CCG.coordinationRule lnode cnode rnode accr)
                                                                          accl
                                                                          (lookupChart (k+1) j chart))
                                                   accc
                                                   (lookupChart i k chart))
                            acck
                            (filter (\n -> (CCG.cat n)==CCG.CONJ) (lookupChart k (k+1) chart)))
         prevlist
         (take (j-i-2) [i+1..]) -- [k | i<k<j-1]  i-k k-k+1 k+1-j

checkParenthesisRule :: Int -> Int -> Chart -> [CCG.Node] -> [CCG.Node]
checkParenthesisRule i j chart prevlist 
  | i+3 <= j = foldl' (\accl lnode -> foldl' (\accr rnode -> foldl' (\accc cnode -> CCG.parenthesisRule lnode cnode rnode accc)
                                                                    accr 
                                                                    (lookupChart (i+1) (j-1) chart))
                                             accl
                                             (filter (\n -> (CCG.cat n)==CCG.RPAREN) (lookupChart (j-1) j chart)))
                      prevlist
                      (filter (\n -> (CCG.cat n)==CCG.LPAREN) (lookupChart i (i+1) chart))
  | otherwise = prevlist

checkEmptyCategories :: [CCG.Node] -> [CCG.Node]
checkEmptyCategories prevlist =
  foldl' (\p ec -> foldl' (\list node -> (CCG.binaryRules node ec) $ (CCG.binaryRules ec node) list) p p) prevlist L.emptyCategories

{- Partial Parsing -}

-- | A data type for the parsing result.
data ParseResult = 
  Full [CCG.Node]      -- ^ when there are at least one node in the topmost box in the chart, returning the nodes.
  | Partial [CCG.Node] -- ^ when the parser did not obtain the full result, it collects partially parsed segments from the chart, returning their conjunctions.
  | Failed             -- ^ when no box in the chart contains any node.
  deriving (Eq,Show)

-- | takes a (parse result) chart and returns a list consisting of nodes, partially parsed substrings.
extractParseResult :: Int -> Chart -> ParseResult
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

{-
sortByNumberOfArgs = sortByNumberOfArgsLoop 20 []

sortByNumberOfArgsLoop :: Int           -- ^ If a node whose number of args exceed this threshold will be discarded.
                          -> [CCG.Node] -- ^ Nodes selected so far.
                          -> [CCG.Node] -- ^ Given set of nodes to filter.
                          -> [CCG.Node]
sortByNumberOfArgsLoop threshold selected nodes = case nodes of
  [] -> L.sort selected
  (n:ns) -> let numOfArg = (CCG.numberOfArgs . CCG.cat) n in
            case () of  
              _ | numOfArg < threshold -> sortByNumberOfArgsLoop numOfArg [n] ns          -- discard the selected ones and update threshold
                | threshold < numOfArg -> sortByNumberOfArgsLoop threshold selected ns    -- ignore n and proceed
                | otherwise            -> sortByNumberOfArgsLoop numOfArg (n:selected) ns -- noa = threshold.  Add n to the selected ones and proceed.
-}

{--
-- | takes only the nodes with the best score.
-- 'nodes' needs to be sorted before applying 'bestOnly' (e.g. bestOnly $ L.sort nodes)
bestOnly :: [CCG.Node] -> [CCG.Node]
bestOnly nodes = case nodes of
  [] -> []
  (firstnode:ns) -> firstnode:(takeWhile (\node -> CCG.score(node) >= CCG.score(firstnode)) ns)
--}

--orCONJ :: T.Text -> CCG.Node
--orCONJ c = LT.lexicalitem c "new" 100 CCG.CONJ LT.orSR

{-
punctFilter i chartList e@((from,to),nodes)
  | to == i = let filterednodes = Maybe.catMaybes $ map (\n -> case CCG.unifyWithHead [] [] LT.anySExStem (CCG.cat n) of 
                                                                 Nothing -> CCG.unifyWithHead [] [] N (CCG.cat n) of
                                                                              Nothing -> Nothing
                                                                              _ -> Just n
                                                                 _ -> Just n) nodes
              in ((from,to+1),filterednodes):(e:chartList)
  | otherwise = e:chartList
-}

{-
punctFilter :: Int -> Int -> [((Int,Int),[CCG.Node])] -> ((Int,Int),[CCG.Node]) -> [((Int,Int),[CCG.Node])]
punctFilter sep i chartList e@((from,to),nodes)
  | to == i = if from <= sep 
                 then ((from,to+1),nodes):(e:chartList)
                 else chartList
  | otherwise = if from < sep
                  then e:chartList
                  else chartList
-}

