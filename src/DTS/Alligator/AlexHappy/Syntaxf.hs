module DTS.Alligator.AlexHappy.Syntaxf where

data Tvar =
  Tvar String
  | TDef Expr Expr
  deriving (Eq,Show)

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
