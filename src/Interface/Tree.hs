{-# LANGUAGE RecordWildCards #-}

module Interface.Tree (
  Tree(..)
  , isLeaf
  ) where

import Data.Bifunctor 
import qualified Data.Text.Lazy as T
import Interface.Text
import Interface.TeX
import Interface.HTML
--import Interface.XML
--import Interface.SVG

-- | Tree of a, where r is a type for rule name
data Tree r a = Tree {
  ruleName :: r
  , node :: a
  , daughters :: [Tree r a]
  } deriving (Eq, Show)

instance Bifunctor Tree where
  first f (Tree rn n dtrs) = Tree (f rn) n (map (first f) dtrs)
  second g (Tree rn n dtrs) = Tree rn (g n) (map (second g) dtrs)

isLeaf :: (Tree r a) -> Bool
isLeaf (Tree _ _ dtrs) = case dtrs of
  [] -> True
  _  -> False

-- | ゆくゆくはCCG.Nodeも Tree CCG.Node として定義しなおすことで、
-- | Treeに対する可視化の関数を共有するようにする（が、CCG.Nodeは
-- | - leafのときのみPFを表示する
-- | - top-levelでのみsignatureを表示する
-- | いったクセがあるので要検討  MathMLtree（メソッドはprintAsLeafを定義）のようなclassを作ろう
instance (SimpleText r, SimpleText a) => SimpleText (Tree r a) where
  toText = toTextLoop 0 

toTextLoop :: (SimpleText r, SimpleText a) => Int -> (Tree r a) -> T.Text
toTextLoop indent Tree{..} =
  let t = [T.pack (replicate indent ' '), "(", toText ruleName, ") ", toText node, "\n"] in
  case daughters of
    [] ->   T.concat t
    dtrs -> T.concat $ t ++ (map (toTextLoop $ indent+2) dtrs)

instance (Typeset r, Typeset a) => Typeset (Tree r a) where
  toTeX Tree{..} = T.concat 
    ["\\nd[", toTeX ruleName, "]{", toTeX node, "}{", T.intercalate "&" $ map toTeX daughters, "}"]

instance (MathML r, MathML a) => MathML (Tree r a) where
  toMathML Tree{..} = T.concat [
    "<mrow><mstyle displaystyle='true'><mfrac linethickness='medium'><mrow>"
    , T.intercalate "<mo>&nbsp;</mo>" $ map toMathML daughters
    , "</mrow>"
    , toMathML node
    , "</mfrac></mstyle><mstyle fontsize='0.4' color='Black'>("
    , toMathML ruleName
    , ")</mstyle></mrow>"
    ]

instance Functor (Tree r) where
  fmap f (Tree ruleName node daughters) =
    Tree ruleName (f node) (map (fmap f) daughters)

