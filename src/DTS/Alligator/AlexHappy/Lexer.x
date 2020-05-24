{
module DTS.Alligator.AlexHappy.Lexer (
  Token(..),
  scanTokens
) where

import DTS.Alligator.AlexHappy.Syntax
import DTS.Alligator.Arrowterm
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]
$space = [\ ]
$hyphen = [\-]
$word = [A-Z a-z $digit \_ \' \- $hyphen \+ \[ \] \! \? \; \= \$ \" \/ \{ \} \* \< \> \^ \\]


tokens :-
  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;
  \&
    { \s -> TokenAnd }
  \)
    { \s -> TokenRBracket }
  \(
    { \s -> TokenLBracket }
  \]
    { \s -> TokenRRBracket }
  \[
    { \s -> TokenRLBracket }
  \~
    { \s -> TokenConne s}
  \|
    { \s -> TokenConne s}
  \&
    { \s -> TokenConne s}
  \< \= \>
    { \s -> TokenConne s}
  \< \=
    { \s -> TokenConne s}
  \= \>
    { \s -> TokenConne s}
  \< \~ \>
    { \s -> TokenConne s}
  \~ \|
    { \s -> TokenConne s}
  \~ \&
    { \s -> TokenConne s}
  \,
    { \s -> TokenComma }
  \.
    { \s -> TokenPeriod }
  fof
    { \s -> TokenFOF }
  cnf
    { \s -> TokenCNF}
  [$space]* $digit+
    { \s -> TokenNum (read s) }
  include
    { \s -> TokenInclude }
  \% [$space]* Number [$space]+ of [$space]+ predicates
    { \s -> TokenPreNum}
  \% [$space]* Number [$space]+ of [$space]+ atoms
    { \s -> TokenPreNum}
  \% [$space]* Number [$space]+ of [$space]+ variables
    { \s -> TokenPreNum}
  \% [$space]*Number [$space]+ of [$space]+ clauses
    { \s -> TokenClause}

Syntax   :
  \% [$space]*Syntax [$space]* :[$space]*Number [$space]+ of [$space]+ clauses
    { \s -> TokenClause}
  \% [$space]* File
    { \s -> TokenFile }
  \% [$space]* Status
    { \s -> TokenStatus}
  [$space]* \%
    { \s -> TokenHead }
  [$space]* \:
    { \s -> TokenCoron }
  [$word]+
    { \s -> TokenWord s }
  [$word]+ \. [$word]+
    { \s -> TokenWord s }
  \< \- \>
    { \s -> TokenWord "<->"}
  \- \>
    { \s -> TokenWord "->"}
  \. \. \.
    { \s -> TokenWord "..."}
  \. \.
    { \s -> TokenWord "..."}

{

data Token
  = TokenNum Int
  | TokenPreNum
  | TokenClause
  | TokenEOF
  | TokenFOF
  | TokenCNF
  | TokenHead
  | TokenCoron
  | TokenComma
  | TokenInclude
  | TokenConne String
  | TokenWord String
  | TokenFile
  | TokenStatus
  | TokenAnd
  | TokenRBracket
  | TokenLBracket
  | TokenRRBracket
  | TokenRLBracket
  | TokenPeriod
  deriving (Eq,Show)

scanTokens = alexScanTokens

}
