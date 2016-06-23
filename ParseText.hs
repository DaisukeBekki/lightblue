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
  chart <- CP.parse 24 sentence
  let nodes = case CP.extractBestParse chart of
                CP.Full ns -> ns
                CP.Partial ns -> ns
  stop     <- Time.getCurrentTime
  let time = Time.diffUTCTime stop start
  mapM_ (action sentence chart nodes time) args
  where action sentence chart nodes time op
          | op == "-tex" = TeX.printNodesInTeX S.stdout $ nodes
          | op == "-text" = CP.printNodesInText S.stderr $ nodes
          | op == "-xml"  = XML.render S.stderr $ nodes
          | op == "-postag"  = CP.posTagger S.stdout $ nodes
          | op == "-numeration" = do {numeration <- LEX.setupLexicon sentence; mapM_ (T.putStrLn . T.toText) numeration}
          | op == "-debug" = TeX.printChartInTeX S.stdout chart
          | op == "-time" = S.hPutStrLn S.stderr $ "Total Execution Time: " ++ show time
          | otherwise = return ()
