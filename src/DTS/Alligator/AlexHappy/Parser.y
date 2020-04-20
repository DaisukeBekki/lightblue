{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DTS.Alligator.AlexHappy.Parser (
  parseExpr,
) where

import DTS.Alligator.AlexHappy.Lexer
import DTS.Alligator.AlexHappy.Syntax
import qualified Data.List as L           -- base

import Control.Monad.Except
}

%name expr
%tokentype { Token }
%monad { Except String } { (>>=) } { return }
%error { parseError }

%token
    int        { TokenNum $$ }
    coron      { TokenCoron }
    file       { TokenFile }
    per        { TokenHead }
    word       { TokenWord $$ }
    connective { TokenConne $$ }
    predicates { TokenPreNum }
    and        { TokenAnd }
    comma      { TokenComma }
    period     { TokenPeriod }
    rbracket   { TokenRBracket }
    lbracket   { TokenLBracket }
    fof        { TokenFOF }

%%
terms
    : term                   { [$1] }
    | term terms             { $1 : $2 }

words
    : word                   { [$1] }
    | int                    { [(show $1)] }
    | coron                  { [":"] }
    | comma                  { [] }
    | period                 { [] }
    | connective             { [$1] }
    | and                    { ["&"] }
    | and words              {  "&" : $2 }
    | coron words            { $2 }
    | period words           { $2 }
    | comma words            { $2 }
    | word words             { $1 : $2 }
    | connective words       { $1 : $2 }
    | int words              { (show $1) : $2 }

formula
    : word                   { [$1] }
    | int                    { [(show $1)] }
    | connective             { [$1] }
    | and                    { ["&"] }
    | and formula            {  "&" : $2 }
    | word formula           { $1 : $2 }
    | connective formula     { $1 : $2 }
    | coron formula          { ":" : $2 }
    | int formula            { (show $1) : $2 }
    | lbracket               { ["("] }
    | rbracket               { [")"] }
    | lbracket formula       { "(" : $2 }
    | rbracket formula       { ")" : $2 }

others
    : formula                { [$1] }
    | words                  { [$1] }
    | formula others         { $1 : $2}
    | words others         { $1 : $2}

term
   : file coron word coron words
      { File $3 (L.concat $5) }
   | predicates coron int lbracket words rbracket
      { PreNum $3 }
   | per others
      { Sout "" }
   | fof lbracket word comma word comma formula period
      { Formula "fof"  $3 $5 (L.init $ L.concat $7) }
   | per
      { Sout "" }


{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String [Expr]
parseExpr input =
  let tokenStream = scanTokens input in
  runExcept (expr tokenStream)
}
