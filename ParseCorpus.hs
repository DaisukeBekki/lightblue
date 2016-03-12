{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | usage: ./ParseFile <text file> 
-- | 
--import Prelude hiding (readFile, writeFile)
import qualified Data.Text.Lazy as T                 --text
import qualified Data.Text.Lazy.IO as T              --text
import qualified Data.List as L                      --base
import qualified Data.Time as Time                   --time
import Data.Ratio
import Data.Fixed
import qualified System.IO as S                      --base
import qualified System.Environment as S             --base
import qualified CombinatoryCategorialGrammar as CCG
import qualified DependentTypes as DTS
import qualified ChartParser as CP

main :: IO()
main = do
    start <- Time.getCurrentTime
    args <- S.getArgs
    sentences <- T.readFile $ head args
    let mylexicon = CP.myLexicon
    (i,j) <- L.foldl' (f mylexicon) (return (0,0)) (T.lines sentences)
    stop <- Time.getCurrentTime
    let totaltime = Time.diffUTCTime stop start
    --CP.printChartInSimpleText $ topbox
    S.hPutStrLn S.stderr $ "Results: " ++ (show i) ++ "/" ++ (show j) ++ " (" ++ (show $ ((fromRational ((toEnum i % toEnum j)*100))::Fixed E3)) ++ "%)"
    S.hPutStrLn S.stderr $ "Execution Time: " ++ show totaltime ++ " (average: " ++ (show $ ((fromRational ((toEnum (fromEnum totaltime)) % toEnum (j*1000000000000)))::Fixed E3)) ++ "s/sentence)"

f :: [CCG.Node] -> IO(Int,Int) -> T.Text -> IO(Int,Int)
f mylexicon score s =
  do
  (i,j) <- score
  chart <- CP.parse 20 mylexicon s
  let nodes = CP.sOnly $ CP.topBox chart
  if (nodes == [])
    then do
         T.putStrLn $ "Re-parsing with the beam width 100..."
         chart100 <- CP.parse 100 mylexicon s
         let nodes100 = CP.sOnly $ CP.topBox chart100
         if (nodes100 == [])   
           then do
                T.putStrLn $ T.concat ["no\t", s, "\n"]
                return (i,j+1)
           else do
                T.putStrLn $ T.concat ["yes\t", s, "\n\t", DTS.toTextWithVN [] $ CCG.sem $ head nodes100, "\n"]
                return (i+1,j+1)
    else do
         T.putStrLn $ T.concat ["yes\t", s, "\n\t", DTS.toTextWithVN [] $ CCG.sem $ head nodes, "\n"]
         return (i+1,j+1)

{-
main :: IO()
main = do
    start <- Time.getCurrentTime
    args <- S.getArgs
    sentences <- T.readFile $ head args
    let mylexicon = CP.myLexicon
    mapM_ (\s -> do
                 chart <- CP.parse 20 mylexicon s
                 let nodes = CP.sOnly $ CP.topBox chart
                 if (nodes == [])
                   then do
                        T.putStrLn $ "Re-parsing with the beam width 100..."
                        chart100 <- CP.parse 100 mylexicon s
                        let nodes100 = CP.sOnly $ CP.topBox chart100
                        if (nodes100 == [])   
                          then T.putStrLn $ T.concat ["no\t", s, "\n"]
                          else T.putStrLn $ T.concat ["yes\t", s, "\n\t", DTS.toTextWithVN [] $ CCG.sem $ head nodes100, "\n"]
                   else T.putStrLn $ T.concat ["yes\t", s, "\n\t", DTS.toTextWithVN [] $ CCG.sem $ head nodes, "\n"]
                 ) $ T.lines sentences
    stop <- Time.getCurrentTime
    --CP.printChartInSimpleText $ topbox
    S.hPutStrLn S.stderr $ "\nExecution Time: " ++ show (Time.diffUTCTime stop start)
-}