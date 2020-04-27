module DTS.Alligator.AlexHappy.Syntaxf where

newtype Tvar = Tvar String deriving (Eq,Show)
data Tbop = Tand | Tor | Timp | Tequiv deriving (Eq,Show)

data Expr =
  Tletter String
  | Ttrue
  | Tfalse
  | Tneg Expr
  | Tbinary Tbop Expr Expr
  | Tall [Tvar] Expr
  | Texist [Tvar] Expr
  | TApp Expr [Expr]
  deriving (Eq, Show)
