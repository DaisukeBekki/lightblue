module DTS.Alligator.AlexHappy.GetDepth.Syntax where

import Data.Default (Default(..))

data Expr
  = Sout String
  | File String String
  | NumOf String Int
  | Status String
  | PreNum Int
  | ClaNum Int
  | Formula String String String String
  | Include String
  deriving (Eq,Show)

data Nums
  = Nums{
      filename :: String,
      predicates :: Int,
      variables :: Int,
      atoms :: Int,
      clauses :: Int,
      clauseSize :: Int,
      functors :: Int,
      maximalDepth :: Int
    } deriving (Show,Eq)
instance Default Nums where
  def = Nums{filename="",predicates=0,variables=0,atoms=0,clauses=0,clauseSize=0,functors=0,maximalDepth=0}
