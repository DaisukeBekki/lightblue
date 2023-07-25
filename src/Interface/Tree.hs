{-# LANGUAGE RecordWildCards #-}

module Interface.Tree (
  Tree(..)
  ) where

import qualified Data.Text.Lazy as T
import Interface.Text
import Interface.TeX
import Interface.HTML
--import Interface.XML
--import Interface.SVG

-- | Tree of a, where b is a type for rule name
data Tree a b = Tree {
  ruleName :: b
  , node :: a
  , daughters :: [Tree a b]
  } deriving (Eq, Show)

-- | ゆくゆくはCCG.Nodeも Tree CCG.Node として定義しなおすことで、
-- | Treeに対する可視化の関数を共有するようにする（が、CCG.Nodeは
-- | - leafのときのみPFを表示する
-- | - top-levelでのみsignatureを表示する
-- | いったクセがあるので要検討
instance (SimpleText a, SimpleText b) => SimpleText (Tree a b) where
  toText = toTextLoop 0 

toTextLoop :: (SimpleText a, SimpleText b) => Int -> (Tree a b) -> T.Text
toTextLoop indent Tree{..} =
  let t = [T.pack (replicate indent ' '), toText ruleName, " ", toText node, "\n"] in
  case daughters of
    [] ->   T.concat t
    dtrs -> T.concat $ t ++ (map (toTextLoop $ indent+2) dtrs)

instance (MathML a, MathML b) => MathML (Tree a b) where
  toMathML Tree{..} = T.concat $
    ["<mrow><mfrac linethickness='2px'><mrow>"]
    ++ (map toMathML daughters)
    ++ ["</mrow></mfrac><mtext fontsize='0.8' color='Black'>", toMathML ruleName, "</mtext></mrow>"]

instance (Typeset a, Typeset b) => Typeset (Tree a b) where
  toTeX Tree{..} = T.concat 
    ["\\nd[", toTeX ruleName, "]{", toTeX node, "}{", T.intercalate "&" $ map toTeX daughters, "}"]

