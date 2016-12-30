{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as T                 --text
import qualified Data.Text.Lazy.IO as T              --text
import qualified System.Environment as S             --base
import qualified Parser.ChartParser as CP
import qualified DTS.UDTT as DTS
import qualified Interface.HTML as HTML

main :: IO()
main = do
  args <- S.getArgs
  text <- T.readFile $ head args
  let sentences = filter (/= T.empty) $ T.lines text
  T.putStrLn HTML.htmlHeader4MathML
  nodes <- mapM ((CP.simpleParse 32)) sentences
  let vsrs = DTS.initializeIndex $ DTS.fromDeBruijnContextLoop [] $ map (CP.sem . head) nodes
  mapM_ (\(sentence,(varname,sr)) -> 
          T.putStrLn $ T.concat [
            "<p>", sentence, "</p>",
            "<math xmlns='http://www.w3.org/1998/Math/MathML'><mrow>", HTML.toMathML varname, "<mo>:</mo>", HTML.toMathML sr, "</mrow></math>"
            ]
        ) $ zip sentences vsrs
  T.putStrLn HTML.htmlFooter4MathML

{-
  mapM_ (\(i,sentence,(vn,sr)) -> 
          T.putStrLn $ T.concat [
            "[", 
            T.pack (show i), 
            "] ", 
            sentence, 
            "\n\\ \\begin{center}\\scalebox{", 
            TEX.scaleboxsize sentence, 
            "}{\\ensuremath{", 
            TEX.toTeX vn, 
            " : ", 
            TEX.toTeX sr, 
            "}}\\end{center}\\newpage"
            ]
            ) $ zip3 ([1..]::[Int]) sentences (DTS.initializeIndex $ DTS.fromDeBruijnContextLoop [] $ map (CP.sem . head) nodes)
-}
