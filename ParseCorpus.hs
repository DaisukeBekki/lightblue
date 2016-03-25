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
import qualified ChartParser as CP

main :: IO()
main = do
    start <- Time.getCurrentTime
    args <- S.getArgs
    sentences <- T.readFile $ head args
    let mylexicon = CP.myLexicon
    (i,j) <- L.foldl' (f mylexicon) (return (0,0)) $ filter (/= T.empty) $ concat $ map (T.split (`elem` ['、','―','，','。','．','「','」'])) $ T.lines sentences
    stop <- Time.getCurrentTime
    let totaltime = Time.diffUTCTime stop start
    --CP.printChartInSimpleText $ topbox
    S.hPutStrLn S.stderr $ "Results: " ++ (show i) ++ "/" ++ (show j) ++ " (" ++ (show $ ((fromRational ((toEnum i % toEnum j)*100))::Fixed E3)) ++ "%)"
    S.hPutStrLn S.stderr $ "Execution Time: " ++ show totaltime ++ " (average: " ++ (show $ ((fromRational ((toEnum (fromEnum totaltime)) % toEnum (j*1000000000000)))::Fixed E3)) ++ "s/sentence)"

f :: [CCG.Node] 
     -> IO(Int,Int) -- ^ (The number of succeeded parses, the number of processed sentences)
     -> T.Text      -- ^ The next sentence to parse
     -> IO(Int,Int)
f mylexicon score s =
  do
  (i,j) <- score
  lexicon <- CP.setupLexicon mylexicon s
  let chart64 = CP.parseMain 64 lexicon s
  S.putStr $ "[" ++ show (j+1) ++ "]\t"
  T.putStrLn s
  let nodes = CP.bestOnly $ CP.topBox chart64
  if (nodes == [])
    then do
         T.putStrLn $ "Re-parsing with the beam width being 256..."
         let chart100 = CP.parseMain 256 lexicon s
         let nodes256 = CP.bestOnly $ CP.topBox chart100
         if (nodes256 == [])
           then do
                T.putStrLn $ T.concat ["no\n"]
                return (i,j+1)
           else do
                let h = head nodes256
                T.putStrLn $ T.concat ["yes\t", CCG.toText $ CCG.cat h, "\n\t", CCG.toText $ CCG.sem h, "\n"]
                return (i+1,j+1)
    else do
         let h = head nodes
         T.putStrLn $ T.concat ["yes\t", CCG.toText $ CCG.cat h, "\n\t", CCG.toText $ CCG.sem h, "\n"]
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