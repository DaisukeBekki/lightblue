{-# LANGUAGE RecordWildCards #-}

{-|
-- Module      : Interface
-- Copyright   : (c) Daisuke Bekki, 2016
-- Licence     : All right reserved
-- Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
-- Stability   : beta
-- 
-- Interface programs
-}

module Interface (
  Style(..)
  , ParseOutput(..)
  , headerOf
  , interimOf
  , footerOf
  , printNodes
  , posTagger
  , printNumeration
  --, treebankBuilder
  ) where

import qualified Data.Char as C           --base
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified System.IO as S           --base
import qualified Parser.ChartParser as CP
import qualified Parser.CCG as CCG
import qualified Parser.Language.Japanese.Lexicon as LEX
import qualified Parser.Language.Japanese.Juman.CallJuman as Juman
import qualified Interface.Text as T
import qualified Interface.TeX as TEX
import qualified Interface.HTML as HTML
import qualified Interface.XML as X
import qualified DTS.UDTTdeBruijn as UDTTdB
import qualified DTS.UDTTwithName as UDTTwN
import qualified DTS.DTTdeBruijn as DTTdB
import qualified DTS.DTTwithName as DTTwN
--import qualified UDTTdB.Prover.Diag.Prover as Ty
--import qualified UDTTdB.Prover.Diag.Judgement as Ty
--import qualified Classifier.DiscourseRelation as DR
import qualified Interface.SVG as SVG

{- Some functions for pretty printing Chart/Nodes -}

readerBuilder :: [(a,String)] -> Int -> ReadS a
readerBuilder list _ r = concat $ map (\(constructor,string) -> [(constructor,s) | (x,s) <- lex r, map C.toLower x == string]) list

-- | values of lightblue -s option
data Style = HTML | TEXT | XML | TEX | SVG deriving (Eq,Show)

instance Read Style where
  readsPrec = readerBuilder [(HTML,"html"),(TEXT,"text"),(TEX,"tex"),(XML,"xml"),(SVG,"svg")]
  --readsPrec _ r = 
    -- [(HTML,s) | (x,s) <- lex r, map C.toLower x == "html"]
    -- ++ [(TEXT,s) | (x,s) <- lex r, map C.toLower x == "text"]
    -- ++ [(TEX,s) | (x,s) <- lex r, map C.toLower x == "tex"]
    -- ++ [(XML,s) | (x,s) <- lex r, map C.toLower x == "xml"]

data ParseOutput = TREE | POSTAG deriving (Eq,Show)

instance Read ParseOutput where
  readsPrec _ r =
    [(TREE,s) | (x,s) <- lex r, map C.toLower x == "tree"]
    ++ [(POSTAG,s) | (x,s) <- lex r, map C.toLower x == "postag"]
    -- ++ [(NUMERATION,s) | (x,s) <- lex r, map C.toLower x == "numeration"]

-- | header in style
headerOf :: Style -> String
headerOf style = case style of
  HTML -> HTML.htmlHeader4MathML
  TEXT -> replicate 100 '-'
  XML  -> "<?xml version='1.0' encoding='UTF-8'?><root><document id='d0'><sentences>"
  TEX  -> ""
  SVG  -> SVG.svgHeader

-- | interim in style
interimOf :: Style -> String -> String
interimOf style text = case style of
  HTML -> "<hr size='15' />"
  TEXT -> "------" ++ text ++ (replicate (94-(length text)) '-')
  XML  -> ""
  TEX  -> ""
  SVG  -> ""

-- | footer in style
footerOf :: Style -> String
footerOf style = case style of
  HTML -> HTML.htmlFooter4MathML
  TEXT -> "□"
  XML  -> "</sentences></document></root>"
  TEX  -> ""
  SVG  -> SVG.svgFooter

-- | prints a CCG node (=i-th parsing result for a sid-th sentence) in a specified style (=HTML|text|XML|TeX)
printNodes :: S.Handle -> Style -> Int -> T.Text -> Bool -> [CCG.Node] -> IO()
printNodes handle HTML sid sentence typecheck nodes = do
  S.hPutStr handle $ "<p>[s" ++ (show sid) ++ "] "
  T.hPutStr handle sentence
  S.hPutStr handle "</p>"
  mapM_ (\(node,ith) -> do
          S.hPutStr handle $ "<p>[parse " ++ show ith ++ ": score="
          T.hPutStr handle $ CCG.showScore node 
          S.hPutStr handle "] "
          T.hPutStr handle $ CCG.pf node
          S.hPutStr handle "</p>"
          mapM_ (T.hPutStr handle) [HTML.startMathML,HTML.toMathML node,HTML.endMathML]
          if typecheck 
             then do
                  T.hPutStrLn handle $ HTML.startMathML;
                  let trees = [] -- map Ty.utreeToMathML $ Ty.checkFelicity (CCG.sig node) [] (CCG.sem node);
                      -- T.hPutStrLn handle $ UDTTdB.toVerticalMathML $ do
                      --   t1 <- Ty.checkFelicity (CCG.sig node) [] (CCG.sem node);
                      --   t2 <- Ty.aspElim t1
                      --   t3 <- Ty.getTerm t2
                      --   return $ UDTTdB.betaReduce $ Ty.repositP t3
                  if null trees 
                    then return ()
                    else T.hPutStrLn handle $ head trees
                  T.hPutStrLn handle $ HTML.endMathML
             else return ()
        ) $ zip nodes ([0..]::[Int])

printNodes handle TEXT sid sentence _ nodes = do
  S.hPutStr handle $ "[s" ++ (show sid) ++ "] "
  T.hPutStrLn handle sentence
  mapM_ (\(node,ith) -> do
           S.hPutStrLn handle $ interimOf TEXT $ "[parse tree " ++ (show $ ith+1) ++ "]" --" for s" ++ (show sid) ++ "]"
           T.hPutStr handle $ T.toText node
        ) $ zip nodes ([0..]::[Int])

printNodes handle XML sid sentence _ nodes = do
  S.hPutStr handle $ "<sentence id='s" ++ (show sid) ++ "'>"
  T.hPutStr handle sentence
  mapM_ (\(node,ith) ->
          T.hPutStr handle $ X.node2XML sid ith False node
        ) $ zip nodes ([0..]::[Int])
  S.hPutStr handle "</sentence>"

printNodes handle TEX sid sentence _ nodes = do
  S.hPutStr handle $ "\\noindent\\kern-2em[s" ++ (show sid) ++ "] "
  T.hPutStr handle sentence
  S.hPutStr handle "\\\\"
  mapM_ (\(node,ith) -> do
          mapM_ (T.hPutStr handle) [
            "\\scalebox{",
            TEX.scaleboxsize sentence,
            "}{\\ensuremath{[parse ", 
            T.pack $ show ith,
            "]",
            TEX.toTeX node,
            "}}\\medskip"
            ]
        ) $ zip nodes ([0..]::[Int])

printNodes handle SVG _ _ _ nodes = do
  mapM_ (\(node,ith) ->
          T.hPutStr handle $ SVG.node2svg node
        ) $ zip nodes ([0..]::[Int])

-- | prints CCG node (=a parsing result) in a \"part-of-speech tagger\" style
posTagger :: S.Handle -> Style -> CCG.Node -> IO()
posTagger handle XML = (T.hPutStrLn handle) . (X.node2XML 0 0 True)
posTagger handle style = mapM_ (T.hPutStrLn handle) . (node2PosTags style)

-- | A subroutine for `posTagger` function
node2PosTags :: Style -> CCG.Node -> [T.Text]
node2PosTags style node =
  case CCG.daughters node of
    [] -> [printLexicalItem style node]
    dtrs -> [t | dtr <- dtrs, t <- node2PosTags style dtr]

printLexicalItem :: Style -> CCG.Node -> T.Text
printLexicalItem style node = case style of
  TEXT -> T.concat [CCG.pf node, "\t", T.toText (CCG.cat node), " \t", T.toText $ UDTTwN.fromDeBruijn [] $ CCG.sem node, "\t", CCG.source node, "\t[", CCG.showScore node, "]"]
  TEX  -> TEX.toTeX node
  HTML -> T.concat $ [HTML.startMathML, HTML.toMathML node, HTML.endMathML]
  XML  -> X.node2XML 0 0 True node
  SVG  -> SVG.node2svg node

-- | prints the numeration
printNumeration :: S.Handle -> Style -> LEX.LexicalResource -> T.Text -> IO()
printNumeration handle style lexicalResource sentence = do
  numeration <- LEX.setupLexicon lexicalResource sentence
  mapM_ ((T.hPutStrLn handle) . (printLexicalItem style)) numeration

-- -- | Deprecated:
-- -- | parses sentences in the given corpus and yields a list of SRs in HTML format.
-- treebankBuilder :: Int -> [T.Text] -> IO()
-- treebankBuilder beam sentences = do
--   S.putStrLn HTML.htmlHeader4MathML
--   T.putStrLn HTML.startMathML
--   nodes <- mapM (CP.simpleParse beam) sentences
--   let srs = map (UDTTwN.fromDeBruijn [] . CP.sem . head) nodes
--   S.putStrLn "<mtable columnalign='left'>"
--   mapM_ (\(sentence,(var,term)) -> do
--             T.hPutStrLn S.stderr sentence
--             mapM_ T.putStr ["<mtr><mtd><mtext color='Purple'>",
--                             sentence,
--                             "</mtext></mtd></mtr><mtr><mtd>",
--                             HTML.toMathML var,
--                             "<mo>:</mo>",
--                             HTML.toMathML term,
--                             "</mtd></mtr>"
--                             ]
--         ) $ zip sentences srs
--   S.putStrLn "</mtable>"
--   mapM_ (\(_,preterm) -> sr2drelTSV preterm) srs
--   T.putStrLn HTML.endMathML
--   S.putStrLn HTML.htmlFooter4MathML

-- -- | Deprecated:
-- -- | traverses a DTS preterm and output a TSV line when finding a DRel (used in the `lightblue treebank` command)
-- sr2drelTSV :: UDTTwN.Preterm -> IO()
-- sr2drelTSV preterm = case preterm of
--   UDTTwN.Pi _ a b -> do {sr2drelTSV a; sr2drelTSV b}
--   UDTTwN.Not a    -> sr2drelTSV a
--   UDTTwN.Lam _ m  -> sr2drelTSV m
--   UDTTwN.App m n  -> do {sr2drelTSV m; sr2drelTSV n}
--   UDTTwN.Sigma _ a b -> do {sr2drelTSV a; sr2drelTSV b}
--   UDTTwN.Pair m n -> do {sr2drelTSV m; sr2drelTSV n}
--   UDTTwN.Proj _ m -> sr2drelTSV m
--   UDTTwN.Lamvec _ m -> sr2drelTSV m
--   UDTTwN.Appvec _ m -> sr2drelTSV m
--   UDTTwN.Asp m -> sr2drelTSV m
--   UDTTwN.Succ n -> sr2drelTSV n
--   UDTTwN.Natrec e g n -> do{sr2drelTSV e; sr2drelTSV g; sr2drelTSV n}
--   UDTTwN.Eq a m n -> do{sr2drelTSV a; sr2drelTSV m; sr2drelTSV n}
--   UDTTwN.Refl a m -> do{sr2drelTSV a; sr2drelTSV m}
--   UDTTwN.Idpeel m n -> do{sr2drelTSV m; sr2drelTSV n}
--   --UDTTwN.DRel i t a b -> do
--   --                   DR.outputTSV i t a b
--   --                   sr2drelTSV a
--   --                   sr2drelTSV b
--   _ -> return ()

{-
-- | prints every box in the (parsed) CYK chart as a TeX source code.
printChartInTeX :: S.Handle -> Bool -> CP.Chart -> IO()
printChartInTeX handle typeCheck chart = mapM_ printList $ M.toList $ M.filter (/= []) chart
  where printList (key,nodes) = do -- list化したChartを画面表示する。
          S.hPutStr handle $ "\\subsubsection*{" ++ show key ++ ": ノード数 " ++ (show $ length nodes) ++ "}"
          printNodesInTeX handle typeCheck nodes

-- | prints n-nodes (n is a natural number) from given list of CCG nodes (=a parsing result) as a TeX source code.
printNNodesInTeX :: S.Handle -> Bool -> [CCG.Node] -> IO()
printNNodesInTeX handle _ nodes = mapM_ (\node -> S.hPutStrLn handle $ "\\noindent\\kern-2em\\scalebox{.2}{" ++ T.unpack (TEX.toTeX node) ++ "\\\\}\\par\\medskip") nodes
-}

