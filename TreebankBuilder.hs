{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as T                 --text
import qualified Data.Text.Lazy.IO as T              --text
--import qualified System.IO as S                      --base
import qualified System.Environment as S             --base
import qualified Data.Ratio as R
import qualified Data.Fixed as F
import qualified Parser.ChartParser as CP
import qualified DTS.DependentTypes as DTS
--import qualified Interface.Text as T
import qualified Interface.TeX as TEX

main :: IO()
main = do
  args <- S.getArgs
  text <- T.readFile $ head args
  let sentences = filter (/= T.empty) $ T.lines text
  nodes <- mapM (CP.simpleParse 32) sentences
  mapM_ (\(i,sentence,(vn,sr)) -> 
          T.putStrLn $ T.concat [
            "[", 
            T.pack (show i), 
            "] ", 
            sentence, 
            "\n\\ \\begin{center}\\scalebox{", 
            scaleboxsize (fromIntegral $ T.length sentence), 
            "}{\\ensuremath{", 
            TEX.toTeX vn, 
            " : ", 
            TEX.toTeX sr, 
            "}}\\end{center}\\newpage"
            ]
            ) $ zip3 ([1..]::[Int]) sentences (DTS.fromDeBruijnContext $ map (CP.sem . head) nodes)

scaleboxsize :: Int -> T.Text
scaleboxsize i = T.pack $ show (fromRational (toEnum (max (100-(i*0.8)) 30) R.% toEnum 100)::F.Fixed F.E2)
