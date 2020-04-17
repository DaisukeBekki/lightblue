{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DTS.Alligator.AlexHappy.Parserf (
  parseExpr,
) where

import DTS.Alligator.AlexHappy.Lexerf
import DTS.Alligator.AlexHappy.Syntaxf
import qualified Data.List as L           -- base

import Control.Monad.Except
}

%name expr
%tokentype { Token }
%monad { Except String } { (>>=) } { return }
%error { parseError }

%token
    neg        { TokenNeg}
    word       { TokenWord $$ }
    biOp       { TokenBiop $$ }
    and        { TokenAnd }
    or         { TokenOr }
    imp        { TokenImp }
    equiv      { TokenEquiv }
    rbracket   { TokenRBracket }
    lbracket   { TokenLBracket }

%%

formula
    : word
      { Tletter $1 }
    | neg formula
      { Tneg $2 }
    | lbracket formula rbracket
      { $2 }
    | formula biOp formula
      { Tbinary Tequiv $1 $3}
    | formula and formula
      { Tbinary Tand $1 $3}
    | formula or formula
      { Tbinary Tor $1 $3}
    | formula imp formula
      { Tbinary Timp $1 $3}
    | formula equiv formula
      { Tbinary Tequiv $1 $3}

{



parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String Expr
parseExpr input =
  let tokenStream = scanTokens input in
  runExcept (expr tokenStream)
}
