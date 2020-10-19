{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module DTS.Alligator.AlexHappy.Classification (
  --Enum(..),
  ClassificationCounts,
  ClassificationReport(..),
  showClassificationReport,
  showConfusionMatrix,
  average,
  variance,
  standartDeviation,
  main,
  getAssesmentAndConfusionMatrix,
  resultClasssification
  ) where

import Data.List (foldl')          --base
import Text.Printf (printf)        --base
import qualified Data.Text as T    --text
import qualified Data.Text.IO as T --text
import qualified DTS.Alligator.AlexHappy.TPTPInfo as TI
import qualified Data.List.Split as S
import Debug.Trace as D

type ClassificationCounts = (Int,Int,Int,Int)

data ClassificationReport = ClassificationReport {
  title :: T.Text,
  precision :: Double,
  recall :: Double,
  f1 :: Double,
  support :: Int
  } deriving (Eq)

data Assesment = Assesment{
  basica :: [ClassificationReport],
  microa :: ClassificationReport,
  macroa :: ClassificationReport,
  weighteda :: ClassificationReport
}deriving (Eq)

instance Show ClassificationReport where
  show ClassificationReport{..} =
    "precision=" ++ (show precision) ++ ", " ++
    "recall=" ++ (show recall) ++ ", " ++
    "f1=" ++ (show f1) ++ ", " ++
    "support=" ++ (show support)

instance Show Assesment where
  show Assesment{..} =
    unlines ["Scores:",
      "\t Prec \t Rec \t F1 \t Supp \t",
      unlines $ for basica formatReport,
      formatReport $ microa,
      formatReport $ macroa,
      formatReport $ weighteda,
      ""]
    where
      formatReport :: ClassificationReport -> String
      formatReport repo = T.unpack $ T.concat [
        " ",
        title repo,
        "\t ",
        T.intercalate "\t " $ map (\action -> T.pack $ printf "%3.3f" $ action repo) [precision, recall, f1],
        "\t ",
        T.pack $ show $ support repo,
        "\t"
        ]

-- | ラベルの型はBounded (Prelude)であると仮定
-- 例：data Fruit = Orange | Apple | Grape deriving (Eq,Show,Bounded)
--     [minBound..maxBound::Fruit]で[Orange,Apple,Grape]を作れる

classificationCountsFor :: (Show label, Eq label, Enum label, Bounded label) => label -> [(label,label)] -> ClassificationCounts
classificationCountsFor pivot =
  foldl' (\(tp,fp,fn,tn) (prediction,answer) ->
                               if | (prediction == pivot) && (answer == pivot) -> (tp+1,fp,fn,tn)
                                  | prediction == pivot -> (tp,fp+1,fn,tn)
                                  | answer == pivot -> (tp,fp,fn+1,tn)
                                  | otherwise -> (tp,fp,fn,tn+1)
         ) (0,0,0,0)

--ここで計算
count2report :: (T.Text, ClassificationCounts) -> ClassificationReport
count2report (title, counts@(tp,_,fn,_)) =
  let precision = calc_precision counts
      recall = calc_recall counts
      f1 = calc_f1 precision recall
      support = tp + fn
  in ClassificationReport{..}

overall :: [ClassificationCounts] -> ClassificationCounts
overall = foldl' (\(tp,fp,fn,tn) (tp',fp',fn',tn') -> (tp+tp',fp+fp',fn+fn',tn+tn')) (0,0,0,0)
--  ClassificationCounts (sum $ map tp counts) (sum $ map fp counts) (sum $ map fn counts) (sum $ map tn counts)

micro :: [ClassificationCounts] -> ClassificationReport
micro counts =
  let title = "micro"
      overall_counts = overall counts
      precision = calc_precision overall_counts
      recall    = calc_recall overall_counts
      f1        = calc_f1 precision recall
      support    = sum $ map (\(tp,_,fn,_) -> tp + fn) counts
   in ClassificationReport{..}

macro :: (Eq label) => [label] -> [ClassificationCounts] -> ClassificationReport
macro labels counts =
  let title = "macro"
      numOfLabels = length labels
      normalizer = (1::Double)/(fromIntegral numOfLabels)
      precision = normalizer * (sum $ map calc_precision counts)
      recall    = normalizer * (sum $ map calc_recall counts)
      f1        = normalizer * (sum $ map (\c -> calc_f1 (calc_precision c) (calc_recall c)) counts)
--      f1        = calc_f1 precision recall
      support    = sum $ map (\(tp,_,fn,_) -> tp + fn) counts
   in ClassificationReport{..}

weighted :: [ClassificationReport] -> ClassificationReport
weighted reports =
  let denominator = sum $ map support reports in
  if denominator == 0
    then ClassificationReport "w-avg" 1 0 0 denominator
    else
      let prec = (sum $ map (\r -> (fromIntegral $ support r) * (precision r)) reports)/(fromIntegral denominator)
          recc = (sum $ map (\r -> (fromIntegral $ support r) * (recall r)) reports)/(fromIntegral denominator)
          f_1 = (sum $ map (\r -> (fromIntegral $ support r) * (f1 r)) reports)/(fromIntegral denominator)
          supp = denominator
          in ClassificationReport "w-avg" prec recc f_1 supp

{-
overall_accuracy :: (Eq label) => [(label,label)] -> Float
overall_accuracy results =
  let denominator = length results
      tp = length $ filter (\(pre,ans) -> pre == ans) results in
  if denominator == 0
    then 0 :: Float
    else (fromIntegral tp)/(fromIntegral denominator)

average_accuracy :: (Eq label, Enum label, Bounded label) => [label] -> [(label,label)] -> Float
average_accuracy labels results =
  let numOfLabels = length labels
      normalizer = (fromIntegral 1)/(fromIntegral numOfLabels)
  in 0

calc_accuracy :: ClassificationCounts -> Float
calc_accuracy ClassificationCounts{..} =
  let denominator = tp+fp+fn+tn in
  if denominator == 0
     then 0 :: Float
     else (fromIntegral (tp+tn))/(fromIntegral denominator)
-}

calc_precision :: ClassificationCounts -> Double
calc_precision (tp,fp,_,_) =
  let denominator = tp+fp in
  if denominator == 0
     then 1 :: Double
     else (fromIntegral tp)/(fromIntegral denominator)

calc_recall :: ClassificationCounts -> Double
calc_recall (tp,_,fn,_) =
  let denominator = tp+fn in
  if denominator == 0
     then 0 :: Double
     else (fromIntegral tp)/(fromIntegral denominator)

calc_f1 :: Double -> Double -> Double
calc_f1 precision recall =
  let denominator = precision + recall in
  if denominator == 0
     then 0 :: Double
     else (2*precision*recall)/denominator

for :: [a] -> (a -> b) -> [b]
for = flip map

showConfusionMatrix :: (Show label, Eq label, Enum label, Bounded label) => [(label,label)] -> T.Text
showConfusionMatrix results =
  let labels = [minBound..maxBound]
  in T.concat [
    "\t",
    T.concat $ for labels $ \answer -> T.concat [
       " ",
       T.pack $ show answer,
       "\t"
       ],
    "\n",
    T.unlines $ for labels $ \prediction -> T.concat [
       " ",
       T.pack $ show prediction,
       "\t",
       T.concat $ for labels $ \answer -> T.concat [
           " ",
           T.pack $ show $ length $ filter (\(p,a) -> p==prediction && a==answer) results,
           "\t"
           ],
       "\t"
       ]
     ]


getAssesmentAndConfusionMatrix :: (Show label, Eq label, Enum label, Bounded label) => [(label,label)] -> (Assesment,T.Text)
getAssesmentAndConfusionMatrix results =
  let labels = [minBound..maxBound]
      counts = map (flip classificationCountsFor results) labels
      basica= map count2report $ zip (map (T.pack . show) labels) counts
      microa = micro counts
      macroa = macro labels counts
      weighteda = weighted basica
  in (Assesment{..},showConfusionMatrix results)

showClassificationReport :: (Show label, Eq label, Enum label, Bounded label) => [(label,label)] -> T.Text
showClassificationReport results =
  --let dat = zip predictions answers
  let labels = [minBound..maxBound]
      counts = map (flip classificationCountsFor results) labels  -- ::[ClassificationCounts]
      reports = map count2report $ zip (map (T.pack . show) labels) counts
  in T.unlines [
    "Scores:",
    "\t Prec \t Rec \t F1 \t Supp \t",
    T.unlines $ for reports formatReport,
    formatReport $ micro counts,
    formatReport $ macro labels counts,
    formatReport $ weighted reports,
    "",
    "Confusion matrix: ",
    showConfusionMatrix results
    ]
  where
    formatReport :: ClassificationReport -> T.Text
    formatReport repo = T.concat [
      " ",
      title repo,
      "\t ",
      T.intercalate "\t " $ map (\action -> T.pack $ printf "%3.3f" $ action repo) [precision, recall, f1],
      "\t ",
      T.pack $ show $ support repo,
      "\t"
      ]

average :: (Real a) => [a] -> Double
average xs = (realToFrac $ sum xs) / fromIntegral (length xs)

variance :: (Real a) => [a] -> Double
variance xs = let avgxs = average xs in
              average $ for xs $ \x -> (realToFrac x - avgxs)^(2::Integer)

standartDeviation :: (Real a) => [a] -> Double
standartDeviation xs = sqrt $ variance xs

--test code

data Animal = Cat | Fish | Hen deriving (Eq, Show, Enum, Bounded)



-- *Torch.Util.Classification> data Answer = Yes | No | Unknown deriving(Eq,Show,Enum,Bounded)
-- *Torch.Util.Classification> test = [(Yes,Yes),(No,No),(Yes,No),(No,Yes)]
-- *Torch.Util.Classification> getAssesmentAndConfusionMatrix test
--

-- compareCsv TI.resultfname 3 2
compareCsv :: String -> Int -> Int -> IO (Assesment,T.Text)
compareCsv fname l1num l2num =do
  csvstr <- readFile fname
  let csv = map (S.splitOn "\t") $S.splitOn "\n"  csvstr
      label = map
              (
                \lst ->
                    let f = case lst !! l1num of
                              "" -> D.trace ("null in l"++(show l1num)++"@"++(head lst)) TI.UNKNOWN
                              fw -> read fw :: TI.Result
                        s = case lst !! l2num of
                              "" -> D.trace ("null in l"++(show l2num)++"@"++(head lst))TI.UNKNOWN
                              sw -> TI.statusToResult (read sw :: TI.Status)
                    in (f,s)
              )
              $filter (\lst -> length lst > l1num) $tail csv
  return $getAssesmentAndConfusionMatrix label


htmlhead = "<!DOCTYPE html><html lang=\"ja\"><head><meta charset =\"UTF-8\"><title>compare</title></head><body>"
htmllast = "</body></html>"

resultClasssification :: IO()
resultClasssification = do
  writeFile (TI.outputdir++"compare.csv") ""--htmlhead
  (dneassesment,dneconMat) <- compareCsv TI.resultfname 3 2
  appendFile (TI.outputdir++"compare.csv")("dne\n"++(T.unpack dneconMat) ++ "\n" ++show dneassesment)
  (efqassesment,efqconMat) <- compareCsv TI.resultfname 5 2
  appendFile (TI.outputdir++"compare.csv")("efq\n"++(T.unpack efqconMat) ++ "\n" ++show efqassesment)
  -- appendFile (TI.outputdir++"compare.html") htmllast

main :: IO()
main = T.putStrLn $ showClassificationReport [
         (Cat,Cat),(Cat,Cat),(Cat,Cat),(Cat,Cat),
         (Fish,Cat),
         (Hen,Cat),
         (Cat,Fish),(Cat,Fish),(Cat,Fish),(Cat,Fish),(Cat,Fish),(Cat,Fish),
         (Fish,Fish),(Fish,Fish),
         (Hen,Fish),(Hen,Fish),
         (Cat,Hen),(Cat,Hen),(Cat,Hen),
         (Hen,Hen),(Hen,Hen),(Hen,Hen),(Hen,Hen),(Hen,Hen),(Hen,Hen)
         ]
