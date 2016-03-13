{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

--import Prelude hiding (readFile, writeFile)
import qualified Data.Text.Lazy.IO as T  --text
import qualified Data.Time as Time       --time
import qualified System.IO as S          --base
import qualified System.Environment as S --base
import qualified ChartParser as CP
import qualified XMLmodule as XML

main :: IO()
main = do
  start    <- Time.getCurrentTime
  args     <- S.getArgs
  sentence <- T.getLine
  let mylexicon = CP.myLexicon
  chart    <- CP.parse 64 mylexicon sentence
  let topbox = CP.topBox chart
  if (topbox /= [])
    then
      do
      stop     <- Time.getCurrentTime
      let time = Time.diffUTCTime stop start
      mapM_ (action chart topbox time) args
    else
      do
      T.putStrLn $ "Re-parsing with the beam-width 256..."
      chart2 <- CP.parse 512 mylexicon sentence
      let topbox2 = CP.topBox chart2
      stop     <- Time.getCurrentTime
      let time = Time.diffUTCTime stop start
      mapM_ (action chart2 topbox2 time) args
  where action chart ns time op
          | op == "-tex" = CP.printChartInTeX S.stdout $ CP.bestOnly $ ns
          | op == "-text" = CP.printChartInSimpleText S.stderr $ CP.bestOnly $ ns          
          | op == "-xml"  = XML.render S.stderr $ CP.bestOnly $ ns
          | op == "-postag"  = CP.posTagger S.stdout $ CP.bestOnly $ ns          
          | op == "-debug" = CP.printChart S.stdout chart -- do; CP.printNodes S.stdout 30 $ ns; CP.printChart S.stdout chart
          | op == "-time" = S.hPutStrLn S.stderr $ "Total Execution Time: " ++ show time
          | otherwise = return ()