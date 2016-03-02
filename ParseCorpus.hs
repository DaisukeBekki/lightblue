{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | usage: ./ParseFile <text file> 
-- | 
--import Prelude hiding (readFile, writeFile)
import qualified Data.Text.Lazy as T                 --text
import qualified Data.Text.Lazy.IO as T              --text
import qualified CombinatoryCategorialGrammar as CCG
import qualified DependentTypes as DTS
import qualified ChartParser as CP
import qualified Data.Time as Time                   --time
import qualified System.IO as S                      --base
import qualified System.Environment as S             --base

main :: IO()
main = do
    start <- Time.getCurrentTime
    args <- S.getArgs
    sentences <- T.readFile $ head args
    mapM_ (\s -> do
                 chart <- CP.parse 100 s
                 let nodes = CP.topBox chart
                 if (nodes == [])
                   then T.putStr $ T.concat ["no\t", s, "\n\n"]
                   else T.putStr $ T.concat ["yes\t", s, "\n\t", DTS.toTextWithVN [] $ CCG.sem $ head nodes, "\n\n"]
                 ) $ T.lines sentences
    stop <- Time.getCurrentTime
    --CP.printChartInSimpleText $ topbox
    S.hPutStrLn S.stderr $ "\\n Execution Time: " ++ show (Time.diffUTCTime stop start)
