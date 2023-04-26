{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface.SVG (
  node2svg,
  svgHeader,
  svgFooter
  ) where

import qualified Data.Text.Lazy    as T hiding (map,reverse) --text
--import qualified Data.Text.Lazy.IO as T        --text
import qualified Control.Applicative as M --base
import qualified Control.Monad as M       --base
import Parser.CCG
import Interface.Text

newtype SVG a = SVG { getSVG :: [SVGlink] -> [SVGnode] -> [SVGnode] -> Int -> (a,[SVGlink],[SVGnode],[SVGnode],Int) }

instance Monad SVG where
  return m = SVG (\links nodes leaves index -> (m,links,nodes,leaves,index))
  (SVG m) >>= f = SVG (\links nodes leaves index -> let (m',links',nodes',leaves',index') = m links nodes leaves index;
                                                        (SVG n) = f m';
                                                    in
                                                    n links' nodes' leaves' index')

instance Functor SVG where
  fmap = M.liftM

instance M.Applicative SVG where
  pure = return
  (<*>) = M.ap

getIndex :: SVG Int
getIndex = SVG (\links nodes leaves i -> (i,links,nodes,leaves,i+1))

pushLink :: SVGlink -> SVG ()
pushLink link = SVG (\links nodes leaves i -> ((),(link:links),nodes,leaves,i))

pushNode :: SVGnode -> SVG ()
pushNode node = SVG (\links nodes leaves i -> ((),links,(node:nodes),leaves,i+1))

pushLeaf :: SVGnode -> SVG ()
pushLeaf leaf = SVG (\links nodes leaves i -> ((),links,nodes,(leaf:leaves),i+1))

node2svg :: Node -> T.Text
node2svg node = 
  let (_, links, nodes, leaves, _) = (getSVG $ node2svgloop 0 node) [] [] [] 1;
      rev = reverse leaves;
  in
  T.concat $ (map printSVGlink $ tail $ reverse links)
             ++ (map printSVGnode $ reverse nodes)
             ++ (map printSVGnode rev)
             ++ (printSameRank rev)

node2svgloop :: Int -> Node -> SVG ()
node2svgloop parent node =
  case daughters node of
    [] -> do
          i <- getIndex
          pushLink (SVGlink parent i)
          pushNode (SVGnode i $ cat2text $ cat node)
          j <- getIndex
          pushLink (SVGlink i j)
          pushLeaf (SVGleaf j $ pf node)
    dtrs -> do
            i <- getIndex
            pushNode (SVGnode i $ cat2text $ cat node)
            pushLink (SVGlink parent i)
            mapM_ (node2svgloop i) dtrs

printSameRank :: [SVGnode] -> [T.Text]
printSameRank nodes =
  ["{rank = same; "]
  ++ map (\(SVGleaf i _) -> T.concat ["e", T.pack $ show i, ";"]) nodes
  ++ ["}\n"]

data SVGlink = SVGlink Int Int deriving (Eq)

printSVGlink :: SVGlink -> T.Text
printSVGlink (SVGlink i j) = T.concat ["e", T.pack $ show i, " -> e", T.pack $ show j, "\n"]

data SVGnode = SVGnode Int T.Text | SVGleaf Int T.Text deriving (Eq)

printSVGnode :: SVGnode -> T.Text
printSVGnode (SVGnode i label) = T.concat [
  "e",
  T.pack $ show i,
  "[fontname=\"Helvetica,sans-Serif\", fontsize=12, shape=plaintext, label=\"",
  label,
  "\"]\n"
  ]
printSVGnode (SVGleaf i label) = T.concat [
  "e",
  T.pack $ show i,
  "[margin=0.001, height=0.2, color = \"#d3d3d3\", fillcolor = \"#d3d3d3\", shape = rect, style=\"filled\", label = \"",
  label,
  "\"]\n"
  ]

cat2text :: Cat -> T.Text
cat2text category = case category of
    SL x y      -> T.concat [cat2text x, "/", cat2text' y]
    BS x y      -> T.concat [cat2text x, "\\\\", cat2text' y]
    T True i _     -> T.concat ["T", T.pack $ show i]
    T False i c     -> T.concat [cat2text c, "<", (T.pack $ show i), ">"]
    S (pos:(conj:_)) -> 
              T.concat [
                       "S[",
                       toText pos,
                       "][",
                       toText conj,
                       "]"
                       ]
    NP [cas]    -> T.concat ["NP[", toText cas, "]"]
    Sbar [sf]   -> T.concat ["Sbar[", toText sf, "]"]
    N           -> "N"
    CONJ        -> "CONJ"
    LPAREN      -> "LPAREN"
    RPAREN      -> "RPAREN"
    _ -> "Error in Simpletext Cat"
    where -- A bracketed version of `toText'` function
    cat2text' c = if isBaseCategory c
                  then cat2text c
                  else T.concat ["(", cat2text c, ")"]

svgHeader :: String
svgHeader = "digraph G {\n\
  \rankdir = TB;\n\
  \splines = polyline;\n\
  \subgraph {\n\
  \    edge [\n\
  \        dir = none;\n\
  \        sametail = h1\n\
  \    ]"

svgFooter :: String
svgFooter = "    } /* closing subgraph */\n}"

{-
main :: IO()
main = do
  T.putStrLn svgHeader
  T.putStrLn svgFooter
-}
