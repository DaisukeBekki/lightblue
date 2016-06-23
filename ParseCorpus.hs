{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | usage: ./ParseFile <text file>
import qualified Data.Text.Lazy as T                 --text
import qualified Data.Text.Lazy.IO as T              --text
import qualified Data.List as L                      --base
import qualified Data.Time as Time                   --time
import qualified Data.Ratio as R
import qualified Data.Fixed as F
import qualified System.IO as S                      --base
import qualified System.Environment as S             --base
import qualified Parser.ChartParser as CP
import qualified Interface.Text as T

main :: IO()
main = do
    start <- Time.getCurrentTime
    args <- S.getArgs
    sentences <- T.readFile $ head args
    (i,j) <- L.foldl' f (return (0,0)) $ filter isSentence $ T.lines sentences
    stop <- Time.getCurrentTime
    let totaltime = Time.diffUTCTime stop start
    S.hPutStrLn S.stdout $ "Results: " ++ (show i) ++ "/" ++ (show j) ++ " (" ++ (show $ ((fromRational ((toEnum i R.% toEnum j)*100))::F.Fixed F.E3)) ++ "%)"
    S.hPutStrLn S.stdout $ "Execution Time: " ++ show totaltime ++ " (average: " ++ (show $ ((fromRational ((toEnum (fromEnum totaltime)) R.% toEnum (j*1000000000000)))::F.Fixed F.E3)) ++ "s/sentence)"
    where isSentence t = not (t == T.empty || "（" `T.isSuffixOf` t)

f :: IO(Int,Int) -- ^ (The number of succeeded parses, the number of processed sentences)
     -> T.Text      -- ^ A next sentence to parse
     -> IO(Int,Int)
f score sentence = do
  (i,j) <- score
  S.putStr $ "[" ++ show (j+1) ++ "] "
  T.putStrLn sentence
  chart <- CP.parse 24 sentence
  case CP.extractBestParse chart of
    CP.Full nodes -> 
       do
       T.putStr $ T.concat ["Fully parsed (", T.pack (show $ i+1), "/", T.pack (show $ j+1)," = "] 
       S.putStrLn $ percent (i+1,j+1) ++ "%)\n"
       T.putStrLn $ T.toText $ head $ nodes
       return (i+1,j+1)
    CP.Partial nodes -> 
       do
       T.putStr $ T.concat ["Partially parsed (", T.pack (show $ i), "/", T.pack (show $ j+1), " = "]
       S.putStrLn $ percent (i,j+1) ++ "%)\n"
       T.putStrLn $ T.toText $ head $ nodes
       return (i,j+1)

percent :: (Int,Int) -> String
percent (i,j) = if j == 0
                   then show (0::F.Fixed F.E2)
                   else show ((fromRational (toEnum i R.% toEnum j)::F.Fixed F.E2) * 100)

