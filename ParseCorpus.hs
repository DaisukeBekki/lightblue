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
import qualified Parser.CombinatoryCategorialGrammar as CCG
import qualified Parser.ChartParser as CP
--import qualified XMLmodule as X

main :: IO()
main = do
    start <- Time.getCurrentTime
    args <- S.getArgs
    sentences <- T.readFile $ head args
    let mylexicon = CP.myLexicon
    (i,j) <- L.foldl' (f mylexicon) (return (0,0)) $ filter (/= T.empty) $ T.lines sentences
    stop <- Time.getCurrentTime
    let totaltime = Time.diffUTCTime stop start
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
  S.putStr $ "[" ++ show (j+1) ++ "] "
  T.putStrLn s
  T.putStrLn $ "Beam width = 16:"
  let (chart0,_) = CP.parseMain 16 lexicon s
  if CP.topBox chart0 /= []
    then
      do
      T.putStr $ T.concat ["Succeeded (", T.pack (show $ i+1), "/", T.pack (show $ j+1)," = "] 
      S.putStrLn $ percent (i+1,j+1) ++ "%)\n"
      T.putStrLn $ CCG.toText $ head $ CP.bestOnly $ CP.topBox chart0
      -- T.putStrLn $ T.concat ["yes\t", CCG.toText $ CCG.cat h, "\n\t", CCG.toText $ CCG.sem h, "\n"]
      return (i+1,j+1)
    else
      do
      T.putStrLn $ "Beam width = 64:"
      let (chart1,nodes1) = CP.parseMain 64 lexicon s
      if CP.topBox chart1 /= []
        then
          do
          T.putStr $ T.concat ["Succeeded (", T.pack (show $ i+1), "/", T.pack (show $ j+1)," = "] 
          S.putStrLn $ percent (i+1,j+1) ++ "%)\n"
          T.putStrLn $ CCG.toText $ head $ CP.bestOnly $ CP.topBox chart1
          -- T.putStrLn $ T.concat ["yes\t", CCG.toText $ CCG.cat h, "\n\t", CCG.toText $ CCG.sem h, "\n"]
          return (i+1,j+1)
        else
          do
          T.putStr $ T.concat ["Failed (", T.pack (show $ i), "/", T.pack (show $ j+1), " = "]
          S.putStrLn $ percent (i,j+1) ++ "%), showing parsed segments:\n"
          mapM_ (T.putStrLn . (\l -> if l==[] then T.empty else CCG.toText $ head l) . CP.bestOnly) (reverse nodes1)
          return (i,j+1)

percent :: (Int,Int) -> String
percent (i,j) = if j == 0
                   then show (0::Fixed E2)
                   else show ((fromRational (toEnum i % toEnum j)::Fixed E2) * 100)

{-
Results: 27/51 (52.941%)
Execution Time: 1087.37509s (average: 21.321s/sentence)
-}

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