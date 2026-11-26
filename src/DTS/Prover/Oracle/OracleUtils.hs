{-# LANGUAGE OverloadedStrings #-}

module DTS.Prover.Oracle.OracleUtils
    ( loadWordEmbeddings
    , calcPScore
    ) where

import Data.List (zipWith)
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy.Read as TR
import Data.Maybe (mapMaybe)

eps :: Float
eps = 1e-6

norm :: [Float] -> Float
norm xs = sqrt $ sum $ map (\x -> x*x) xs

dot :: [Float] -> [Float] -> Float
dot xs ys = sum $ zipWith (*) xs ys

clip :: Float -> Float
clip x = max (-1 + eps) (min (1 - eps) x)

sigmoid :: Float -> Float
sigmoid x = 1 / (1 + exp x)

angleChild :: Float -> [Float] -> [Float] -> Float
angleChild k parent child =
    let
        normParent = norm parent
        normParentSq = normParent * normParent
        normChild = norm child
        normChildSq = normChild * normChild
        euclidDist = max (norm $ zipWith (-) parent child) eps
        dotProd = dot parent child
        g = 1 + normParentSq * normChildSq - 2 * dotProd
        gSqrt = sqrt g
        childNumerator = dotProd * (1 + normParentSq) - normParentSq * (1 + normChildSq)
        childDenominator = euclidDist * normParent * gSqrt
        cosAngleChild = clip (childNumerator / childDenominator)
    in acos cosAngleChild

angleParent :: Float -> [Float] -> [Float] -> Float
angleParent k parent _child =
    let
        normParent = norm parent
    in asin (k * (1 - normParent * normParent) / normParent)

loadWordEmbeddings :: FilePath -> IO (M.Map T.Text [[Float]])
loadWordEmbeddings path = do
    contents <- TIO.readFile path
    let ls = drop 1 $ T.lines contents
    let parseFloat t = case TR.double t of
              Right (d, _) -> Just (realToFrac d :: Float)
              _ -> Nothing
    let parseLine line = case T.splitOn "," line of
              (wordTxt : dimsTxt) -> Just (wordTxt, mapMaybe parseFloat dimsTxt)
              _ -> Nothing
    let parsedLines = mapMaybe parseLine ls
    pure $ M.fromListWith (++) [(word, [dims]) | (word, dims) <- parsedLines]

calcPScore :: Float -> [Float] -> [Float] -> Float
calcPScore alpha pEmb cEmb =
    let a1 = angleChild alpha pEmb cEmb
        a2 = angleParent alpha pEmb cEmb
        score = a1 - a2
    in sigmoid score
