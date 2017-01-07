{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as T     -- text
import qualified Data.Text.Lazy.IO as T  -- text
import qualified Data.List as L          -- base
--import qualified System.Process as S     -- process
--import qualified System.Environment as E -- base
--import qualified Control.Monad as M      -- base
import qualified DTS.UDTT as DTS
import qualified Parser.ChartParser as CP
import qualified Interface.HTML as HTML
import qualified DTS.Prover.TypeChecker as Ty
import qualified DTS.Prover.Judgement as Ty

-- | Usage: cat test.txt | ./checkEntailment > output.html
-- where 'test.txt' is a text file, consisting of premises and a coclusion.
-- (each line contains one sentence)
main :: IO()
main = do
  sentences <- T.getContents
  nodes <- mapM ((fmap head) . (CP.simpleParse 24)) (T.lines sentences)
  let premises = map CP.sem $ L.init $ nodes;
      conclusion = CP.sem $ L.last $ nodes;
      siglists = map CP.sig nodes;
  T.putStrLn HTML.htmlHeader4MathML
  mapM_ (\node -> do
                  T.putStrLn HTML.startMathML
                  T.putStrLn $ HTML.toMathML node
                  T.putStrLn HTML.endMathML
                  T.putStrLn "<hr size='10' />"
                  ) nodes
  T.putStrLn HTML.startMathML
  mapM_ (T.putStrLn . Ty.utreeToMathML) $ Ty.proofSearch (reverse premises) (L.concat $ [("evt",DTS.Type),("entity",DTS.Type)]:siglists) conclusion
  T.putStrLn HTML.endMathML
  T.putStrLn HTML.htmlFooter4MathML
