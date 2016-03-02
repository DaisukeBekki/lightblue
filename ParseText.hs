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
  let beam = 100
  start    <- Time.getCurrentTime
  args     <- S.getArgs
  sentence <- T.getLine
  chart    <- CP.parse beam sentence
  let topbox = CP.topBox chart
  stop     <- Time.getCurrentTime
  let time = Time.diffUTCTime stop start
  mapM_ (action chart topbox time) args
  where action chart topbox time op
          | op == "-tex" = CP.printChartInTeX S.stdout $ CP.bestOnly $ topbox
          | op == "-text" = CP.printChartInSimpleText S.stderr $ CP.bestOnly $ topbox
          | op == "-xml"  = XML.render S.stderr $ CP.bestOnly $ topbox
          | op == "-postag"  = CP.posTagger S.stdout $ CP.bestOnly $ topbox
          | op == "-debug" = do; CP.printNodes S.stdout 30 $ topbox; CP.printChart S.stdout chart
          | op == "-time" = S.hPutStrLn S.stderr $ "Total Execution Time: " ++ show time
          | otherwise = return ()