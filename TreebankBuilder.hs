{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as T                 --text
import qualified Data.Text.Lazy.IO as T              --text
import qualified System.IO as S                      --base
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
  let (sr,_,_) = (DTS.runRenumber (mapM DTS.renumber2 $ srs)) 1 1
  mapM_ (\(s,r) -> do
                   T.hPutStrLn S.stderr s
                   T.putStrLn s
                   T.putStrLn "\\ \\begin{center}\\scalebox{.8}{$"
                   T.putStrLn $ TEX.toTeX r
                   T.putStrLn "$}\\end{center}\\newpage"
                   ) $ zip sentences sr

sentence2DTS :: T.Text -> IO(DTS.Preterm)
sentence2DTS sentence = do
  (chart0,_) <- CP.parse 24 sentence
  let topbox = CP.topBox chart0
      sonly = filter CP.isS topbox
  if topbox /= []
     then return $ CP.sem $ head $ if sonly == [] then topbox else sonly
     else return DTS.Top
