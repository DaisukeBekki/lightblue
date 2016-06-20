{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.Time as Time        --time
import qualified System.IO as S           --base
import qualified System.Environment as S  --base
import qualified Parser.ChartParser as CP
import qualified Parser.Japanese.Lexicon as LEX
import qualified Interface.Text as T
import qualified Interface.XML as XML
import qualified Interface.TeX as TeX

main :: IO()
main = do
  start    <- Time.getCurrentTime
  args     <- S.getArgs
  sentence <- T.getLine
  chart    <- CP.parse 24 sentence
  let top = CP.topBox chart;
      sonly = filter CP.isS top;
      top' = if sonly == [] then top else sonly
  stop     <- Time.getCurrentTime
  let time = Time.diffUTCTime stop start
  mapM_ (action sentence chart (CP.bestOnly top') time) args
  where action sentence chart topbox time op
          | op == "-tex" = TeX.printNodesInTeX S.stdout $ topbox
          | op == "-text" = CP.printChartInText S.stderr $ topbox
          | op == "-xml"  = XML.render S.stderr $ topbox
          | op == "-postag"  = CP.posTagger S.stdout $ topbox
          | op == "-numeration" = do {numeration <- LEX.setupLexicon sentence; mapM_ (T.putStrLn . T.toText) numeration}
          | op == "-debug" = TeX.printChartInTeX S.stdout chart
          | op == "-time" = S.hPutStrLn S.stderr $ "Total Execution Time: " ++ show time
          | op == "-partial" = case CP.extractBestParse chart of
                                 Just node -> T.putStrLn $ T.toText $ node
                                 Nothing -> T.putStrLn "Parse error"
          | otherwise = return ()
