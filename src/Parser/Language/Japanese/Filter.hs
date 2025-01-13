{-# LANGUAGE DeriveGeneric #-}

-- {-|
-- Module      : LightblueFilter
-- Licence     : LGPL
-- Copyright   : Asa Tomita
-- Stability   : beta
-- An interface module with lightblue parser
-- -}

module Parser.Language.Japanese.Filter (
  Filter
  , FilterName(..)
  ) where

import qualified Data.Char as C      --base
import qualified Data.Text.Lazy as T --text
import qualified Parser.CCG as CCG   --lightblue

-- The type for node filters
type Filter = Int -> Int -> [CCG.Node] -> [CCG.Node]

-- Filter names
data FilterName = KNP | KWJA | NONE deriving (Eq,Show)

instance Read FilterName where
  readsPrec _ r =
    [(KNP,s) | (x,s) <- lex r, map C.toLower x == "knp"]
    ++ [(KWJA,s) | (x,s) <- lex r, map C.toLower x == "kwja"]
    ++ [(NONE,s) | (x,s) <- lex r, map C.toLower x == "none"]  


