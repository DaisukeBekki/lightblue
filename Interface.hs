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
  Style(..),
  headerOf,
  footerOf,
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
import qualified Parser.ChartParser as CP
import qualified Parser.CCG as CCG
import qualified Parser.Japanese.Lexicon as LEX
import qualified Interface.Text as T
import qualified Interface.TeX as TEX
import qualified Interface.HTML as HTML
import qualified Interface.OpenNLP as NLP
import qualified DTS.Prover as Prover
import qualified DTS.Prover.Judgement as Ty

{- Some functions for pretty printing Chart/Nodes -}

{-
-- | prints CCG nodes (=a parsing result) in a specified format.
printNodes :: S.Handle  -- ^ handle for output
              -> Int    -- ^ N-best
              -> Bool   -- ^ if executing type-check
              -> String -- ^ format={text|html|xml|tex}
              -> [CCG.Node] -- ^ CCG nodes (=parsing results) to print
              -> IO()
printNodes handle nBest typeCheck format nodes = do
  T.hPutStrLn handle $ case () of 
                         _ | format == "text" -> T.pack $ take 100 $ repeat '-'
                           | format == "html" -> HTML.htmlHeader4MathML
                           | format == "xml"  -> T.pack "<?xml version='1.0' encoding='UTF-8'?><root><document id='d0'><sentences>"
  mapM_ (\node -> mapM_ (T.hPutStrLn handle) $ case () of
                                                 _ | format == "text" -> [T.toText node, take 100 $ repeat '-']
                                                   | format == "html" -> ["<p>",CCG.pf node," [",CCG.showScore node,"]</p>",HTML.startMathML,HTML.toMathML node,HTML.endMathML]
                                                   | format == "xml"  -> [NLP.node2NLP 0 sentence node]
         ) $ take nBest nodes
  T.hPutStrLn handle $ case () of 
                         _ | format == "text" -> 
                           | format == "html" -> HTML.htmlFooter4MathML
                           | format == "xml"  -> "</sentences></document></root>"
-}

data Style = HTML | TEXT | XML | TEX deriving (Eq,Show,Read)

{-
instance Read Style where
  readsPrec _ input = case input of
    "html" -> HTML
    "text" -> TEXT
    "tex"  -> TEX
    "xml"  -> XML
-}

headerOf :: Style -> String
headerOf style = case style of
  HTML -> HTML.htmlHeader4MathML
  TEXT -> take 100 $ repeat '-'
  XML  -> "<?xml version='1.0' encoding='UTF-8'?><root><document id='d0'><sentences>"
  TEX  -> ""

footerOf :: Style -> String
footerOf style = case style of
  HTML -> HTML.htmlFooter4MathML
  TEXT -> take 100 $ repeat '-'
  XML  -> "</sentences></document></root>"
  TEX  -> ""

-- | prints CCG nodes (=a parsing result) as a TeX source code.
printNodesInHTML :: S.Handle -> Bool -> [CCG.Node] -> IO()
printNodesInHTML handle typeCheck nodes = do
  mapM_ (\node -> do
                  mapM_ (T.hPutStrLn handle) ["<p>", 
                                             CCG.pf node,
                                             " [",
                                             CCG.showScore node,
                                             "]</p>",
                                             HTML.startMathML,
                                             HTML.toMathML node,
                                             HTML.endMathML
                                             ]
                  if typeCheck 
                     then do
                          T.hPutStrLn handle $ HTML.startMathML;
                          mapM_ ((T.hPutStrLn handle) . Ty.utreeToMathML) $ Prover.checkFelicity (CCG.sig node) [] (CCG.sem node);
                          -- T.hPutStrLn handle $ DTS.toVerticalMathML $ do
                          --   t1 <- Ty.checkFelicity (CCG.sig node) [] (CCG.sem node);
                          --   t2 <- Ty.aspElim t1
                          --   t3 <- Ty.getTerm t2
                          --   return $ DTS.betaReduce $ Ty.repositP t3
                          T.hPutStrLn handle $ HTML.endMathML 
                     else return ()
          ) nodes

-- | prints CCG nodes (=a parsing result) as a plain text.
printNodesInText :: S.Handle -> Bool -> [CCG.Node] -> IO()
printNodesInText handle _ nodes = do
  mapM_ (\node -> do 
                  T.hPutStr handle $ T.toText node
                  S.hPutStrLn handle $ take 100 $ repeat '-'
        ) $ nodes

printNodesInXML :: S.Handle -> T.Text -> [CCG.Node] -> IO()
printNodesInXML handle sentence nodes = do
  mapM_ (\node -> do 
                  T.hPutStrLn handle $ NLP.node2NLP 0 False sentence node
        ) nodes

-- | prints CCG nodes (=a parsing result) as a TeX source code.
printNodesInTeX :: S.Handle -> Bool -> [CCG.Node] -> IO()
printNodesInTeX handle _ nodes = 
  mapM_ (\node -> T.hPutStrLn handle $ T.concat [
            "\\noindent\\kern-2em\\scalebox{.2",
            TEX.scaleboxsize $ CCG.pf node,
            "}{\\ensuremath{", 
            -- TEX.toTeX $ CCG.sem node,
            TEX.toTeX node,
            "}}\\medskip"
            ]
            ) nodes

-- | prints every box in the (parsed) CYK chart as a TeX source code.
printChartInTeX :: S.Handle -> Bool -> CP.Chart -> IO()
printChartInTeX handle typeCheck chart = mapM_ printList $ M.toList $ M.filter (/= []) chart
  where printList (key,nodes) = do -- list化したChartを画面表示する。
          S.hPutStr handle $ "\\subsubsection*{" ++ show key ++ ": ノード数 " ++ (show $ length nodes) ++ "}"
          printNodesInTeX handle typeCheck nodes

-- | prints n-nodes (n is a natural number) from given list of CCG nodes (=a parsing result) as a TeX source code.
printNNodesInTeX :: S.Handle -> Bool -> [CCG.Node] -> IO()
printNNodesInTeX handle _ nodes = mapM_ (\node -> S.hPutStrLn handle $ "\\noindent\\kern-2em\\scalebox{.2}{" ++ T.unpack (TEX.toTeX node) ++ "\\\\}\\par\\medskip") nodes

-- | prints CCG nodes (=a parsing result) in a \"part-of-speech tagger\" style
posTagger :: S.Handle -> Style -> [CCG.Node] -> IO()
posTagger handle style nodes = case style of
  XML -> mapM_ ((T.hPutStrLn handle) . (NLP.node2NLP 0 True T.empty)) nodes
  _ -> mapM_ (\node -> mapM_ (T.hPutStrLn handle) $ node2PosTags style node) nodes

-- | A subroutine for `posTagger` function
node2PosTags :: Style -> CCG.Node -> [T.Text]
node2PosTags style node =
  case CCG.daughters node of
    [] -> [printLexicalItem style node]
    dtrs -> [t | dtr <- dtrs, t <- node2PosTags style dtr]

printLexicalItem :: Style -> CCG.Node -> T.Text
printLexicalItem style node = case style of
  TEXT -> T.concat [CCG.pf node, "\t", T.toText (CCG.cat node), " \t", T.toText (CCG.sem node), "\t", CCG.source node, "\t[", CCG.showScore node, "]"]
  TEX  -> TEX.toTeX node
  HTML -> T.concat $ [HTML.startMathML, HTML.toMathML node, HTML.endMathML]
  XML  -> NLP.node2NLP 0 True (CCG.pf node) node

printNumeration :: S.Handle -> Style -> T.Text -> IO()
printNumeration handle style sentence = do 
  numeration <- LEX.setupLexicon sentence
  mapM_ ((T.hPutStrLn handle) . (printLexicalItem style)) numeration
