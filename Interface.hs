{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Interface
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module Interface (
  printNodesInHTML,
  printNodesInText,
  printNodesInXML,
  printNodesInTeX,
  printChartInTeX,
  printNNodesInTeX,
  posTagger,
  printNumeration
  ) where

import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.Map as M            --container
import qualified System.IO as S           --base
import Data.Fixed                         --base
import qualified Parser.ChartParser as CP
import qualified Parser.CCG as CCG
import qualified Parser.Japanese.Lexicon as LEX
import qualified Interface.Text as T
import qualified Interface.TeX as TEX
import qualified Interface.HTML as HTML
import qualified Interface.OpenNLP as NLP
import qualified DTS.UDTT as U
import qualified DTS.Prover.TypeChecker as Ty
import qualified DTS.Prover.Judgement as Ty

{- Some functions for pretty printing Chart/Nodes -}

-- | prints CCG nodes (=a parsing result) as a TeX source code.
printNodesInHTML :: S.Handle -> Bool -> [CCG.Node] -> IO()
printNodesInHTML handle typeCheck nodes = 
  do
  T.hPutStrLn handle HTML.htmlHeader4MathML
  mapM_ (\node -> 
          do
          mapM_ (T.hPutStrLn handle) ["<p>", 
                                      CCG.pf node,
                                      " [",
                                      T.pack $ show $ ((fromRational $ CCG.score node)::Fixed E2),
                                      "]</p>",
                                      HTML.startMathML,
                                      HTML.toMathML node,
                                      HTML.endMathML
                                      ]
          if typeCheck 
             then do
                  T.hPutStrLn handle $ HTML.startMathML;
                  mapM_ ((T.hPutStrLn handle) . Ty.utreeToMathML) $ Ty.typeCheckU [] ((CCG.sig node)++[("evt",U.Type),("entity",U.Type)]) (CCG.sem node) (U.Type);
                  T.hPutStrLn handle $ HTML.endMathML 
             else return ()
          ) nodes
  T.hPutStrLn handle HTML.htmlFooter4MathML

-- | prints CCG nodes (=a parsing result) as a plain text.
printNodesInText :: S.Handle -> [CCG.Node] -> IO()
printNodesInText handle nodes = 
  do
  S.hPutStrLn handle (take 100 $ repeat '-')
  mapM_ (\node -> do S.hPutStr handle $ T.unpack $ T.toText node
                     S.hPutStrLn handle $ take 100 $ repeat '-'
        ) nodes
  S.hPutStrLn handle $ "Number of nodes: " ++ show (length nodes)

printNodesInXML :: S.Handle -> T.Text -> [CCG.Node] -> IO()
printNodesInXML handle sentence nodes = T.hPutStrLn handle $ NLP.node2NLP 0 sentence $ head nodes

-- | prints every box in the (parsed) CYK chart as a TeX source code.
printChartInTeX :: S.Handle -> CP.Chart -> IO()
printChartInTeX handle chart = mapM_ printList $ M.toList $ M.filter (/= []) chart
  where printList (key,nodes) = do -- list化したChartを画面表示する。
          S.hPutStr handle $ "\\subsubsection*{" ++ show key ++ ": ノード数 " ++ (show $ length nodes) ++ "}"
          printNodesInTeX handle nodes

-- | prints n-nodes (n is a natural number) from given list of CCG nodes (=a parsing result) as a TeX source code.
printNNodesInTeX :: S.Handle -> Int -> [CCG.Node] -> IO()
printNNodesInTeX handle n nodes = mapM_ (\node -> S.hPutStrLn handle $ "\\noindent\\kern-2em\\scalebox{.2}{" ++ T.unpack (TEX.toTeX node) ++ "\\\\}\\par\\medskip") $ take n nodes

-- | prints CCG nodes (=a parsing result) as a TeX source code.
printNodesInTeX :: S.Handle -> [CCG.Node] -> IO()
printNodesInTeX handle nodes = 
  mapM_ (\node -> T.hPutStrLn handle $ T.concat [
            "\\noindent\\kern-2em\\scalebox{.2",
            --TEX.scaleboxsize $ CCG.pf node,
            "}{\\ensuremath{", 
            -- TEX.toTeX $ CCG.sem node,
            TEX.toTeX node,
            "}}\\medskip"
            ]
            ) $ take 1 nodes

-- | prints CCG nodes (=a parsing result) in a \"part-of-speech tagger\" style
posTagger :: S.Handle -> [CCG.Node] -> IO()
posTagger handle nodes = 
  case nodes of
    [] -> S.hPutStrLn handle "No results."
    (n:_) -> mapM_ ((S.hPutStrLn handle) . T.unpack) $ node2PosTags n

-- | A subroutine for `posTagger` function
node2PosTags :: CCG.Node -> [T.Text]
node2PosTags node@(CCG.Node _ _ _ _ _ _ _ _) =
  case CCG.daughters node of
    [] -> [T.concat [CCG.pf node, "\t", T.toText (CCG.cat node), " \t", T.toText (CCG.sem node), "\t", CCG.source node, "\t[", T.pack (show ((fromRational $ CCG.score node)::Fixed E2)), "]"]]
    dtrs -> [t | dtr <- dtrs, t <- node2PosTags dtr]

printNumeration :: S.Handle -> T.Text -> IO()
printNumeration handle sentence = do 
  numeration <- LEX.setupLexicon sentence
  mapM_ ((T.hPutStrLn handle). T.toText) numeration
