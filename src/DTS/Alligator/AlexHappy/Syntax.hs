module DTS.Alligator.AlexHappy.Syntax where

data Expr
  = Sout String
  | File String String
  | Status String
  | PreNum Int
  | ClaNum Int
  | Formula String String String String
  deriving (Eq,Show)
