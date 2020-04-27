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
    top        { TokenTop }
    bot        { TokenBot }
    rbracket   { TokenRBracket }
    lbracket   { TokenLBracket }
    rrbracket  { TokenRRBracket }
    rlbracket  { TokenRLBracket }
    coron      { TokenCoron }
    comma      { TokenComma }
    all        { TokenAll }
    exists     { TokenExists }

%%


formula
    : word
      { Tletter $1 }
    | top
      { Ttrue }
    | bot
      { Tfalse }
    | formula and formula
      { Tbinary Tand $1 $3}
    | formula or formula
      { Tbinary Tor $1 $3}
    | formula imp formula
      { Tbinary Timp $1 $3}
    | formula equiv formula
      { Tbinary Tequiv $1 $3}
    | formula biOp formula
      { Tbinary Tequiv $1 $3}
    | neg formula
      { Tneg $2 }
    | lbracket formula rbracket
      { $2 }
    | all rlbracket vars rrbracket coron formula
      { Tall $3 $6 }
    | exists rlbracket vars rrbracket coron formula
      { Texist $3 $6 }
    | formula lbracket formulae rbracket
      { TApp $1 (L.reverse $3) }
vars
    : word
      { [Tvar $1] }
    | word comma vars
      { (Tvar $1) : $3 }

formulae
    : formula
      { [$1] }
    | formula comma formulae
      { $1 : $3 }

{



parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String Expr
parseExpr input =
  let tokenStream = scanTokens input in
  runExcept (expr tokenStream)
}
