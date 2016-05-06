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
import qualified Parser.CombinatoryCategorialGrammar as CCG
import qualified Parser.ChartParser as CP

main :: IO()
main = do
    start <- Time.getCurrentTime
    args <- S.getArgs
    sentences <- T.readFile $ head args
    (i,j) <- L.foldl' f (return (0,0)) $ filter isSentence $ T.lines sentences
    stop <- Time.getCurrentTime
    let totaltime = Time.diffUTCTime stop start
    S.hPutStrLn S.stderr $ "Results: " ++ (show i) ++ "/" ++ (show j) ++ " (" ++ (show $ ((fromRational ((toEnum i R.% toEnum j)*100))::F.Fixed F.E3)) ++ "%)"
    S.hPutStrLn S.stderr $ "Execution Time: " ++ show totaltime ++ " (average: " ++ (show $ ((fromRational ((toEnum (fromEnum totaltime)) R.% toEnum (j*1000000000000)))::F.Fixed F.E3)) ++ "s/sentence)"

isSentence :: T.Text -> Bool
isSentence sentence = not (t == T.empty || "☎" `T.isPrefixOf` t || "（" `T.isSuffixOf` t)

f :: IO(Int,Int) -- ^ (The number of succeeded parses, the number of processed sentences)
     -> T.Text      -- ^ A next sentence to parse
     -> IO(Int,Int)
f score s =
  do
  (i,j) <- score
  S.putStr $ "[" ++ show (j+1) ++ "] "
  T.putStrLn s
  (chart0,nodes0) <- CP.parse 32 s
  if CP.topBox chart0 /= []
    then
      do
      T.putStr $ T.concat ["Succeeded (", T.pack (show $ i+1), "/", T.pack (show $ j+1)," = "] 
      S.putStrLn $ percent (i+1,j+1) ++ "%)\n"
      let topbox = CP.topBox chart0;
          sonly = filter CP.isS topbox
      T.putStrLn $ CCG.toText $ head $ CP.bestOnly $ if sonly == [] then topbox else sonly
      return (i+1,j+1)
    else
      do
      T.putStr $ T.concat ["Failed (", T.pack (show $ i), "/", T.pack (show $ j+1), " = "]
      S.putStrLn $ percent (i,j+1) ++ "%), showing parsed segments:\n"
      mapM_ (T.putStrLn . (\l -> if l==[] then T.empty else CCG.toText $ head l) . CP.bestOnly) (reverse nodes0)
      return (i,j+1)

percent :: (Int,Int) -> String
percent (i,j) = if j == 0
                   then show (0::F.Fixed F.E2)
                   else show ((fromRational (toEnum i R.% toEnum j)::F.Fixed F.E2) * 100)

