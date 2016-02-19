{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | usage: ./ParseFile <text file> 
-- | 
--import Prelude hiding (readFile, writeFile)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified ChartParser as CP
import qualified Data.Time as Time
import qualified System.IO as S
import qualified System.Environment as S

main :: IO()
main = do
    start <- Time.getCurrentTime
    args <- S.getArgs
    sentences <- T.readFile $ head args
    mapM_ (\s -> do 
                 chart <- CP.parse 100 s
                 let chk = if (CP.topBox chart == [])
                           then "no"   
                           else "yes"     
                 T.putStr chk
                 T.putStr "\t"
                 T.putStrLn s
                 ) $ T.lines sentences
    stop <- Time.getCurrentTime    
    --CP.printChartInSimpleText $ topbox
    S.hPutStrLn S.stderr $ "\\n Execution Time: " ++ show (Time.diffUTCTime stop start)
