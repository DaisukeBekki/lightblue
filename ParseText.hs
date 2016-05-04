{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.Time as Time        --time
import qualified System.IO as S           --base
import qualified System.Environment as S  --base
import qualified Parser.ChartParser as CP
import qualified Parser.Japanese.Lexicon as LEX
import qualified Parser.XMLmodule as XML

main :: IO()
main = do
  start    <- Time.getCurrentTime
  args     <- S.getArgs
  sentence <- T.getLine
  (chart,_) <- CP.parse 24 sentence
  let top = CP.topBox chart;
      sonly = filter CP.isS top;
      top' = if sonly == [] then top else sonly
  stop     <- Time.getCurrentTime
  let time = Time.diffUTCTime stop start
  mapM_ (action sentence chart (CP.bestOnly top') time) args
  where action sentence chart topbox time op
          | op == "-tex" = CP.printNodesInTeX S.stdout $ topbox
          | op == "-text" = CP.printChartInSimpleText S.stderr $ topbox
          | op == "-xml"  = XML.render S.stderr $ topbox
          | op == "-postag"  = CP.posTagger S.stdout $ topbox
          | op == "-numeration" = do {numeration <- LEX.setupLexicon sentence; mapM_ (T.putStrLn . CP.toText) numeration}
          | op == "-debug" = CP.printChart S.stdout chart
          | op == "-time" = S.hPutStrLn S.stderr $ "Total Execution Time: " ++ show time
          | otherwise = return ()
