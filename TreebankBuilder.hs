{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as T                 --text
import qualified Data.Text.Lazy.IO as T              --text
--import qualified System.IO as S                      --base
import qualified System.Environment as S             --base
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
  mapM_ (\(i,sentence,sr) -> do
                   T.putStrLn $ T.concat ["[", T.pack (show i), "] ", sentence]
                   T.putStrLn "\\ \\begin{center}\\scalebox{.8}{$"
                   T.putStrLn $ T.concat ["s_{", T.pack (show i), "}: ", TEX.toTeX sr]
                   T.putStrLn "$}\\end{center}\\newpage"
                   ) $ zip3 [1..] sentences (DTS.initializeIndex $ mapM (DTS.fromDeBruijn . CP.sem . head) nodes)
