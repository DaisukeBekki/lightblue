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
  --, defaultParseSetting
  , parse
  ) where

import Data.List as L
import Data.Char                          --base
import System.IO.Unsafe (unsafePerformIO) --base
import Data.Function    ((&))             --base
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.Map as M         --container
import qualified Parser.CCG as CCG --(Node, unaryRules, binaryRules, trinaryRules, isCONJ, cat, SimpleText)
--import qualified Parser.PartialParsing as Partial --lightblue
import Parser.Language (LangOptions(..)) --lightblue
import qualified Parser.Language.Japanese.Juman.CallJuman as Juman
import qualified Parser.Language.Japanese.Lexicon as JP (LexicalItems, setupLexicon, emptyCategories, myLexicon)
import qualified Parser.Language.Japanese.Templates as LT
import qualified Parser.Language.English.Lexicon as EN (setupLexicon)
import qualified DTS.QueryTypes as QT

-- | Backwards function application.
(.->) :: a -> (a -> b) -> b
(.->) = (&)

{- Main functions -}

-- | The type for CYK-charts.
data ParseSetting = ParseSetting {
  langOptions :: LangOptions   -- ^ Language options
  , beamWidth :: Int           -- ^ The beam width
  , nParse :: Int              -- ^ Show N-best parse trees for each sentence
  , nTypeCheck :: Int          -- ^ Show N-best type check diagram for each logical form
  , nProof :: Int              -- ^ Show N-best proof diagram for each proof search
  , ifPurify :: Bool           -- ^ If True, apply purifyText to the input text before parsing
  , ifDebug :: Maybe (Int,Int) -- ^ Debug mode: If Just (i,j), dump parse result of (i,j). 
  --, nodeFilterBuilder :: T.Text -> IO (Int -> Int -> [CCG.Node] -> [CCG.Node]) -- ^ node filter
  , noInference :: Bool        -- ^ If True, it is an inference and execute proof search
  , verbose :: Bool            -- ^ If True, type checker and inferer dump logs
  } 

-- defaultParseSetting :: IO ParseSetting
-- defaultParseSetting = do
--   jpOptions <- JP.lexicalResourceBuilder Juman.KWJA
--   return $ ParseSetting jpOptions 32 (-1) (-1) (-1) Nothing Nothing True False (\_ _ -> id)

type Chart = M.Map (Int,Int) [CCG.Node]
type Token = T.Text

-- -- | removes occurrences of non-letters from an input text.
-- purifyText :: T.Text -> T.Text -> T.Text
-- purifyText symbolsToIgnore text = 
--   case T.uncons text of -- remove a non-literal symbol at the beginning of a sentence (if any)
--     Nothing -> T.empty
--     Just (c,t) | isSpace c                   -> purifyText symbolsToIgnore t -- ignore white spaces
--                | T.any (==c) symbolsToIgnore -> purifyText symbolsToIgnore t -- ignore meaningless symbols
--                | otherwise                   -> T.cons c $ purifyText symbolsToIgnore t

-- | Main parsing function to parse a Japanees sentence that generates a CYK-chart.
parse :: ParseSetting 
         -> T.Text    -- ^ A sentence to be parsed
         -> IO Chart  -- ^ A pair of the resulting CYK-chart and a list of CYK-charts for segments
parse parseSetting@ParseSetting{..} sentence 
  | sentence == T.empty = return M.empty -- returns an empty chart, otherwise foldl returns a runtime error when text is empty
  | otherwise = do
      (tokens,lexicon) <- case langOptions of
        JpOptions _ _ _ _ _ _ _ _ _ -> JP.setupLexicon langOptions sentence
        EnOptions _ _ _ _ _         -> EN.setupLexicon sentence
      nodeFilter <- nodeFilterBuilder langOptions $ T.concat tokens
      -- mapM_ T.putStrLn tokens
      let (chart,_,_) = foldl' (chartAccumulator parseSetting lexicon nodeFilter) 
                               (M.empty,0,[])
                               tokens
      return chart


-- | quadruples representing a state during parsing:
-- the parsed result (Chart) of the left of the pivot,
-- the pivot (=the current parsing position), and
-- the revsersed list of chars that has been parsed
type PartialChart = (Chart,Int,[Token])

-- | The 'chartAccumulator' function is the accumulator of the 'parse' function
chartAccumulator :: ParseSetting 
                    -> JP.LexicalItems -- ^ my lexicon as the second parameter
                    -> (Int -> Int -> [CCG.Node] -> [CCG.Node]) -- ^ node filter
                    -> PartialChart    -- ^ The accumulated result, given
                    -> Token           -- ^ The next char of a unparsed text
                    -> PartialChart    -- ^ The accumulated result, updated
chartAccumulator parseSetting@ParseSetting{..} lexicon nodeFilter (chart,i,stack) token = unsafePerformIO $ do
  let newstack = token:stack -- | :: [Token]
      (newchart,_,_,_) = foldl' (boxAccumulator parseSetting lexicon nodeFilter) 
                                (chart,T.empty,i,i+1)
                                newstack
  return (newchart,i+1,newstack)
-- chartAccumulator _ _ (_,[],_,_) _ = ?

type PartialBox = (Chart,T.Text,Int,Int) -- (chard, word, i j)

-- | The 'boxAccumulator' function
boxAccumulator :: ParseSetting
                  -> JP.LexicalItems -- ^ lexicon
                  -> (Int -> Int -> [CCG.Node] -> [CCG.Node]) -- ^ node filter
                  -> PartialBox      -- ^ accumulated result (Chart, Text, Int, Int)
                  -> Token           -- ^ 
                  -> PartialBox
boxAccumulator ParseSetting{..} lexicon nodeFilter (chart,word,i,j) token = unsafePerformIO $ do
  let newword = case existDelimiter langOptions of
                    True -> if T.null word 
                              then token
                              else T.concat [token, " ", word]
                    False -> T.append token word
  let list0 = if (T.compareLength newword (longestWordLength langOptions)) == LT 
                -- Does not execute lookup for a long word. Run "LongestWord" to check that the length of the longest word (=23).
                then lookupLexicon newword lexicon
                else [];
  let nodesBeforeFiltering = checkUnaryRules list0 
        .-> checkBinaryRules i j chart  
      nodesAfterFiltering = nodesBeforeFiltering
        .-> nodeFilter i j
        .-> checkCoordinationRule i j chart 
        .-> checkParenthesisRule i j chart
        .-> checkEmptyCategories
        .-> L.sort 
        .-> take beamWidth
  case ifDebug of
    Just (x,y) ->
      if i >= x && j <= y
        then do
          putStr $ "\n------" ++ (show (i,j)) ++ "------"  
          putStrLn "\nBefore filtering: "
          print nodesBeforeFiltering
          putStrLn "\nAfter filtering: "
          print nodesAfterFiltering
        else return ()
    Nothing -> return ()
  return $ ((M.insert (i,j) nodesAfterFiltering chart), newword, i-1, j)

-- | This function takes a word and a lexicon and returns a set of CCG lexical entries whose PF is that word.
lookupLexicon :: T.Text -> [CCG.Node] -> [CCG.Node]
lookupLexicon word lexicon = filter (\l -> (CCG.pf l) == word) lexicon

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
  foldl' (\p ec -> foldl' (\list node -> (CCG.binaryRules node ec) $ (CCG.binaryRules ec node) list) p p) prevlist JP.emptyCategories

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

-- -- | 
-- punctFilter :: Int    -- ^ Previous pivot
--                -> Int -- ^ Current pivot
--                -> [((Int,Int),[CCG.Node])] -- ^ The list of nodes that has been endorced
--                -> ((Int,Int),[CCG.Node])   -- ^ With respect to a given entry (=e@((from,to),nodes)), 
--                -> [((Int,Int),[CCG.Node])]
-- punctFilter sep i charList e@((from,to),nodes) 
--   | to == i = ((from,to+1),filter (CCG.isBunsetsu . CCG.cat) nodes):(e:charList) 
--   | otherwise = e:charList
--                 -- if from <= sep
--                 --    then e:charList
--                 --    else charList

-- andCONJ :: T.Text -> CCG.Node
-- andCONJ c = LT.lexicalitem c "punct" 100 CCG.CONJ LT.andSR

-- emptyCM :: T.Text -> CCG.Node
-- emptyCM c = LT.lexicalitem c "punct" 99 (((CCG.T True 1 LT.modifiableS) `CCG.SL` ((CCG.T True 1 LT.modifiableS) `CCG.BS` (CCG.NP [CCG.F[CCG.Ga,CCG.O]]))) `CCG.BS` (CCG.NP [CCG.F[CCG.Nc]])) LT.argumentCM


