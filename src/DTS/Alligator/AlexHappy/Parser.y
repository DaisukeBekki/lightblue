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
    status     { TokenStatus }
    per        { TokenHead }
    word       { TokenWord $$ }
    connective { TokenConne $$ }
    predicates { TokenPreNum }
    clausenum  { TokenClause }
    and        { TokenAnd }
    comma      { TokenComma }
    period     { TokenPeriod }
    rbracket   { TokenRBracket }
    lbracket   { TokenLBracket }
    fof        { TokenFOF }
    cnf        { TokenCNF }
    include    { TokenInclude}

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
    | include                { ["include"] }
    | include words          { "include" : $2 }
    | and words              {  "&" : $2 }
    | coron words            { $2 }
    | period words           { $2 }
    | comma words            { $2 }
    | word words             { $1 : $2 }
    | connective words       { $1 : $2 }
    | int words              { (show $1) : $2 }

name
    : word                   { $1 }
    | word name              { $1 ++ $2 }
    | int                    { (show $1) }
    | int name               { (show $1) ++ $2 }

formula
    : word                   { [$1] }
    | and formula            {  "&" : $2 }
    | word formula           { $1 : $2 }
    | coron                  { [":"]}
    | comma                  { [","] }
    | connective formula     { $1 : $2 }
    | coron formula          { ":" : $2 }
    | comma formula          { "," : $2 }
    | int formula            { (show $1) : $2 }
    | lbracket               { ["("] }
    | rbracket               { [")"] }
    | lbracket formula       { "(" : $2 }
    | rbracket formula       { ")" : $2 }

others
    : words                  {}
    | lbracket others        {}
    | formula others         {}
    | words others           {}
    | rbracket others        {}
    | rbracket               {}
    | formula                {}

term
   : file coron word coron words
      { File $3 (L.concat $5) }
   | include lbracket word rbracket period
      { Include $3 }
   | status coron word
      { Status $3 --statusはTheorem/ContradictoryAxioms/CounterSatisfiable/Satisfiable/Unsatisfiable/Unknown/Openのどれか(どれも一語)}
   | predicates coron int lbracket words rbracket
      { PreNum $3 }
   | clausenum coron int lbracket words rbracket
      { ClaNum $3 }
   | per others
      { Sout "" }
   | fof lbracket name comma word comma formula period
      { Formula "fof"  $3 $5 (L.init $ L.concat $7) }
   | cnf lbracket name comma word comma formula period
      { Formula "cnf"  $3 $5 (L.init $ L.concat $7) }
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
