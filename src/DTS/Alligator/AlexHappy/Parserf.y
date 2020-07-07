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
    biOp       { TokenBiop $$ }
    neg        { TokenNeg}
    word       { TokenWord $$ }
    and        { TokenAnd }
    or         { TokenOr }
    imp        { TokenImp }
    equiv      { TokenEquiv }
    eq         { TokenEq }
    noteq      { TokenNotEq }
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
    | lbracket formula imp formula rbracket
      { Tbinary Timp $2 $4}
    | top
      { Ttrue }
    | bot
      { Tfalse }
    | lbracket formula and trueSubFormula rbracket
      { Tbinary Tand $2 $4}
    | lbracket formula or trueSubFormula rbracket
      { Tbinary Tor $2 $4}
    | lbracket formula equiv formula rbracket
      { Tbinary Tequiv $2 $4}
    | lbracket formula eq formula rbracket
      { Tbinary Tequiv $2 $4}
    | lbracket formula noteq formula rbracket
      { Tneg (Tbinary Tequiv $2 $4)}
    | lbracket formula biOp formula rbracket
      { Tbinary Tequiv $2 $4}
    | lbracket formula rbracket
      { $2 }
    | all rlbracket vars rrbracket coron formula
      { Tall $3 $6 }
    | exists rlbracket vars rrbracket coron formula
      { Texist $3 $6 }
    | formula lbracket vars rbracket
      { TApp $1 $3 }
    | neg formula
      { Tneg $2 }



trueSubFormula
    : formula and trueSubFormula
      { Tbinary Tand $1 $3}
    | formula or trueSubFormula
      { Tbinary Tor $1 $3}
    | formula
      { $1 }
var
    : formula coron eq formula
      { [TDef $1 $4] }
    | formula
      { [TFormula $1] }
vars
    : var comma vars
      { $1 ++ $3 }
    | var
      { $1 }

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
