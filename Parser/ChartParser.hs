{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : ChartParser
Description : A left-corner CKY-parser for lexical grammars
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module Parser.ChartParser (
  -- * Data structures for CCG derivations
  Chart,
  CCG.Node(..),
  -- * Main parsing functions
  parse,
  simpleParse,
  posTagger,
  -- * Text and TeX interfaces
  printChartInTeX,
  printNodesInText,
  printNNodesInTeX,
  printNodesInTeX,
  -- * Partial parsing function(s)
  ParseResult(..),
  extractBestParse
  ) where

import Data.List as L
import Data.Char                       --base
import Data.Fixed                      --base
import qualified Data.Text.Lazy as T   --text
import qualified Data.Map as M         --container
import qualified System.IO as S        --base
import qualified Parser.CCG as CCG --(Node, unaryRules, binaryRules, trinaryRules, isCONJ, cat, SimpleText)
import qualified Parser.Japanese.Lexicon as L (LexicalItems, lookupLexicon, setupLexicon, emptyCategories)
import qualified Parser.Japanese.Templates as LT
import qualified Interface.Text as T
import qualified Interface.TeX as TEX

-- | The type for CYK-charts.
type Chart = M.Map (Int,Int) [CCG.Node]

{- Come functions for pretty printing Chart/Nodes -}

-- | prints every box in the (parsed) CYK chart as a TeX source code.
printChartInTeX :: S.Handle -> Chart -> IO()
printChartInTeX handle chart = mapM_ printList $ M.toList $ M.filter (/= []) chart
  where printList (key,nodes) = do -- list化したChartを画面表示する。
          S.hPutStr handle $ "\\subsubsection*{" ++ show key ++ ": ノード数 " ++ (show $ length nodes) ++ "}"
          printNodesInTeX handle nodes

-- | prints CCG nodes (=a parsing result) as a plain text.
printNodesInText :: S.Handle -> [CCG.Node] -> IO()
printNodesInText handle nodes = 
  do
  S.hPutStrLn handle (take 100 $ repeat '-')
  mapM_ (\node -> do S.hPutStr handle $ T.unpack $ T.toText node
                     S.hPutStrLn handle $ take 100 $ repeat '-'
        ) nodes
  S.hPutStrLn handle $ "Number of nodes: " ++ show (length nodes)

-- | prints n-nodes (n is a natural number) from given list of CCG nodes (=a parsing result) as a TeX source code.
printNNodesInTeX :: S.Handle -> Int -> [CCG.Node] -> IO()
printNNodesInTeX handle n nodes = mapM_ (\node -> S.hPutStrLn handle $ "\\noindent\\kern-2em\\scalebox{.2}{" ++ T.unpack (TEX.toTeX node) ++ "\\\\}\\par\\medskip") $ take n nodes

-- | prints CCG nodes (=a parsing result) as a TeX source code.
printNodesInTeX :: S.Handle -> [CCG.Node] -> IO()
printNodesInTeX handle = mapM_ (\node -> S.hPutStrLn handle $ "\\noindent\\kern-2em\\scalebox{.2}{" ++ T.unpack (TEX.toTeX node) ++ "\\\\}\\par\\medskip")

-- | prints CCG nodes (=a parsing result) in a \"part-of-speech tagger\" style
posTagger :: S.Handle -> [CCG.Node] -> IO()
posTagger handle nodes = 
  case nodes of
    [] -> S.hPutStrLn handle "No results."
    (n:_) -> mapM_ ((S.hPutStrLn handle) . T.unpack) $ node2PosTags n

-- | A subroutine for `posTagger` function
node2PosTags :: CCG.Node -> [T.Text]
node2PosTags node@(CCG.Node _ _ _ _ _ _ _ _) =
  case CCG.daughters node of
    [] -> [T.concat [CCG.pf node, "\t", T.toText (CCG.cat node), " \t", T.toText (CCG.sem node), "\t", CCG.source node, "\t[", T.pack (show ((fromRational $ CCG.score node)::Fixed E2)), "]"]]
    dtrs -> [t | dtr <- dtrs, t <- node2PosTags dtr]

{-
-- | picks up the nodes in the "top" box in the chart.
topBox :: Chart -> [CCG.Node]
topBox chart = let ((_,k),_) = M.findMax chart in
                 case M.lookup (0,k) chart of
                   Just nodes -> sort nodes
                   Nothing    -> []

-- | checks if a given node's category is `S` or `Sbar`.
isS :: CCG.Node -> Bool
isS node = case CCG.cat node of
             CCG.S _ -> True
             CCG.Sbar _ -> True
             _ -> False
-}

{- Main functions -}

-- | Main parsing function to parse a Japanees sentence and generates a CYK-chart.
parse :: Int           -- ^ The beam width
         -> T.Text     -- ^ A sentence to be parsed
         -> IO(Chart) -- ^ A pair of the resulting CYK-chart and a list of CYK-charts for segments
parse beam sentence 
  | sentence == T.empty = return M.empty -- foldl returns a runtime error when text is empty
  | otherwise =
      do
      lexicon <- L.setupLexicon (T.replace "―" "。" sentence)
      let (chart,_,_,_) = T.foldl' (chartAccumulator beam lexicon) (M.empty,[0],0,T.empty) (purifyText sentence)
      return chart

-- | removes occurrences of non-letters from an input text.
purifyText :: T.Text -> T.Text
purifyText text = case T.uncons text of -- remove a non-literal symbol at the beginning of a sentence (if any)
                    Nothing -> T.empty
                    Just (c,t) | isSpace c -> purifyText t                                                                   -- ignore white spaces
                               | c `elem` ['！','？','!','?','…','「','」','◎','○','●','▲','△','▼','▽','■','□','◆','◇','★','☆','※','†','‡'] -> purifyText t -- ignore some symbols as meaningless
                               | c `elem` ['，',',','-','―','?','。','．','／','＼'] -> T.cons '、' $ purifyText t          -- punctuations
                               | otherwise -> T.cons c $ purifyText t

-- | Simple parsing function to return just the best node for a given sentence
simpleParse :: Int -> T.Text -> IO([CCG.Node])
simpleParse beam sentence = do
  chart <- parse beam sentence
  case extractBestParse chart of
    Full nodes -> return nodes
    Partial nodes -> return nodes
    Failed -> return []

-- | triples representing a state during parsing:
-- the parsed result (=chart) of the left of the pivot,
-- the stack of positions of the previous 'separators' (i.e. '、','，',etc)
-- the pivot (=the current parsing position),
-- the revsersed list of chars that has been parsed
-- the list of partial parse results
type PartialChart = (Chart,[Int],Int,T.Text)

-- | The 'chartAccumulator' function
chartAccumulator :: Int             -- ^ beam width
                    -> L.LexicalItems    -- ^ my lexicon
                    -> PartialChart -- ^ accumulated result (Chart, Int, Text)
                    -> Char         -- ^ next char of a unparsed text
                    -> PartialChart
chartAccumulator beam lexicon (chart,seplist,i,stack) c =
  let newstack = (T.cons c stack);
      (sep:seps) = seplist in
  if c == '、' -- Each seperator is an end of a phase
    then let newchart = M.fromList $ ((i,i+1),[andCONJ (T.singleton c), emptyCM (T.singleton c)]):(foldl' (punctFilter sep i) [] $ M.toList chart)
         in (newchart, ((i+1):seps), (i+1), newstack) --, (take 1 (sort (lookupChart sep (i+1) newchart)):parsed))
    else let (newchart,_,_,_) = T.foldl' (boxAccumulator beam lexicon) (chart,T.empty,i,i+1) newstack;
             newseps | c `elem` ['「','『'] = (i+1:seplist)
                     | c `elem` ['」','』'] = seps
                     | otherwise = seplist 
         in (newchart,newseps,(i+1),newstack)

punctFilter :: Int -> Int -> [((Int,Int),[CCG.Node])] -> ((Int,Int),[CCG.Node]) -> [((Int,Int),[CCG.Node])]
punctFilter sep i charList e@((from,to),nodes) 
  | to == i = ((from,to+1),filter (\n -> CCG.isBunsetsu $ CCG.cat n) nodes):(e:charList)
  | otherwise = if from <= sep
                   then e:charList
                   else charList

andCONJ :: T.Text -> CCG.Node
andCONJ c = LT.lexicalitem c "punct" 100 CCG.CONJ LT.andSR

emptyCM :: T.Text -> CCG.Node
emptyCM c = LT.lexicalitem c "punct" 99 (((CCG.T True 1 LT.modifiableS) `CCG.SL` ((CCG.T True 1 LT.modifiableS) `CCG.BS` (CCG.NP [CCG.F[CCG.Ga,CCG.O]]))) `CCG.BS` (CCG.NP [CCG.F[CCG.Nc]])) LT.argumentCM

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

type PartialBox = (Chart,T.Text,Int,Int)

-- | The 'boxAccumulator' function
boxAccumulator :: Int               -- ^ beam width
                  -> L.LexicalItems -- ^ my lexicon
                  -> PartialBox     -- ^ accumulated result (Chart, Text, Int, Int)
                  -> Char           -- ^ 
                  -> PartialBox
boxAccumulator beam lexicon (chart,word,i,j) c =
  let newword = T.cons c word;
      list0 = if (T.compareLength newword 23) == LT -- Does not execute lookup for a long word (run "LongestWord" to check it (=23))
                then L.lookupLexicon newword lexicon
                else [];
      list1 = checkEmptyCategories $ checkParenthesisRule i j chart $ checkCoordinationRule i j chart $ checkBinaryRules i j chart $ checkUnaryRules list0 in
  ((M.insert (i,j) (cutoff beam list1) chart), newword, i-1, j)
  --((M.insert (i,j) (cutoff (max (beam+i-j) 24) list1) chart), newword, i-1, j)

-- | take `beam` nodes from the top of `ndoes`.
cutoff :: Int -> [CCG.Node] -> [CCG.Node]
cutoff beam nodes = if length nodes <= beam then nodes else take beam $ sort nodes

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

data ParseResult = Full [CCG.Node] | Partial [CCG.Node] | Failed deriving (Eq,Show)

-- | takes a (parse result) chart and returns a list consisting of nodes, partially parsed substrings.
extractBestParse :: Chart -> ParseResult
extractBestParse chart = 
  f $ L.sortBy isLessPrivilegedThan $ filter (\((_,_),nodes) -> not (L.null nodes)) $ M.toList $ chart
  where f [] = Failed
        f (((i,_),nodes):cs) | i == 0 = Full $ map CCG.wrapNode (sortByNumberOfArgs $ bestOnly $ L.sort nodes)
                             | otherwise = Partial $ g (map CCG.wrapNode (sortByNumberOfArgs $ bestOnly $ L.sort nodes)) (filter (\((_,j),_) -> j < i) cs)
        g results [] = map CCG.drel results
        g results (((i,_),nodes):cs) = g [CCG.conjoinNodes (CCG.wrapNode $ head $ sortByNumberOfArgs $ bestOnly $ L.sort nodes) (head results)] $ filter (\((_,j),_) -> j < i) cs

-- | a `isLessPriviledgedThan` b means that b is more important parse result than a.
isLessPrivilegedThan :: ((Int,Int),a) -> ((Int,Int),a) -> Ordering
isLessPrivilegedThan ((i1,j1),_) ((i2,j2),_) | i1 == i2 && j1 == j2 = EQ
                                             | j2 > j1 = GT
                                             | j1 == j2 && i2 < i1 = GT
                                             | otherwise = LT

-- | takes only the nodes with the best score.
bestOnly :: [CCG.Node] -> [CCG.Node]
bestOnly nodes = case sort nodes of
  [] -> []
  (firstnode:ns) -> firstnode:(takeWhile (\node -> CCG.score(node) >= CCG.score(firstnode)) ns)

sortByNumberOfArgs :: [CCG.Node] -> [CCG.Node]
sortByNumberOfArgs = sortByNumberOfArgs2 20 []

sortByNumberOfArgs2 :: Int -> [CCG.Node] -> [CCG.Node] -> [CCG.Node]
sortByNumberOfArgs2 i selected nodes = case nodes of
  [] -> selected
  (n:ns) -> let j = (CCG.numberOfArgs . CCG.cat) n in
            case () of
              _ | j < i  -> sortByNumberOfArgs2 j [n] ns
                | i < j  -> sortByNumberOfArgs2 i selected ns
                | otherwise -> sortByNumberOfArgs2 j (n:selected) ns



