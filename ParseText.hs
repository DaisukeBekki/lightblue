{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

--import Prelude hiding (readFile, writeFile)
import qualified Data.Text.Lazy.IO as T
import qualified ChartParser as CP
import qualified Data.Time as Time
import qualified System.IO as S
import qualified System.Environment as S
import qualified XMLmodule as XML

main :: IO()
main = do
  let beam = 100
  start    <- Time.getCurrentTime
  args     <- S.getArgs
  sentence <- T.getLine
  chart    <- CP.parse beam sentence
  let topbox = CP.topBox chart
  stop <- Time.getCurrentTime
  let time = Time.diffUTCTime stop start
  mapM_ (action chart topbox time) args
  where action chart topbox time op
          | op == "-tex"  = CP.printChart S.stdout chart
          | op == "-text" = CP.printChartInSimpleText S.stderr $ topbox
          | op == "-xml"  = XML.render S.stderr topbox 
          | op == "-debug" = CP.printNodes S.stdout 30 $ topbox
          | op == "-time" = S.hPutStrLn S.stderr $ "Total Execution Time: " ++ show time
          | otherwise = return ()