{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

--import Prelude hiding (readFile, writeFile)
import qualified Data.Text.Lazy as T     --text
import qualified Data.Text.Lazy.IO as T  --text
import qualified Data.Time as Time       --time
import qualified System.IO as S          --base
import qualified System.Environment as S --base
import qualified Parser.ChartParser as CP
import qualified Parser.XMLmodule as XML        --conduit

main :: IO()
main = do
  start    <- Time.getCurrentTime
  args     <- S.getArgs
  sentence <- T.getLine
  lexicon <- CP.setupLexicon CP.myLexicon (T.replace "―" "。" sentence)
  let (chart1,_) = CP.parseMain 32 lexicon sentence
  let topbox1 = CP.topBox chart1
  if topbox1 /= []
    then
      do
      stop     <- Time.getCurrentTime
      let time = Time.diffUTCTime stop start
      mapM_ (action chart1 topbox1 time) args
    else
      do
      S.hPutStrLn S.stderr "Re-parsing with the beam-width 128..."
      let (chart2,_) = CP.parseMain 128 lexicon sentence
      let topbox2 = CP.topBox chart2
      stop     <- Time.getCurrentTime
      let time = Time.diffUTCTime stop start
      mapM_ (action chart2 topbox2 time) args
  where action chart topbox time op
          | op == "-tex" = CP.printNodesInTeX S.stdout $ CP.bestOnly $ topbox
          | op == "-text" = CP.printChartInSimpleText S.stderr $ CP.bestOnly $ topbox
          | op == "-xml"  = XML.render S.stderr $ CP.bestOnly $ topbox
          | op == "-postag"  = CP.posTagger S.stdout $ CP.bestOnly $ topbox
          | op == "-debug" = CP.printChart S.stdout chart -- do; CP.printNodes S.stdout 30 $ ns; CP.printChart S.stdout chart
          | op == "-time" = S.hPutStrLn S.stderr $ "Total Execution Time: " ++ show time
          | otherwise = return ()
