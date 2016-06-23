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
  srs <- mapM sentence2DTS sentences
  let (srs',_,_) = (DTS.runRenumber $ sequence srs) 1 1
  mapM_ (\(sentence,sr) -> do
                   T.putStrLn sentence
                   T.putStrLn "\\ \\begin{center}\\scalebox{.8}{$"
                   T.putStrLn $ TEX.toTeX sr
                   T.putStrLn "$}\\end{center}\\newpage"
                   ) $ zip sentences srs'

sentence2DTS :: T.Text -> IO(DTS.Renumber DTS.Preterm)
sentence2DTS sentence = do
  chart <- CP.parse 32 sentence
  let nodes = case CP.extractBestParse chart of
                CP.Full ns -> ns
                CP.Partial ns -> ns
  return $ DTS.renumber2 $ CP.sem $ head nodes
