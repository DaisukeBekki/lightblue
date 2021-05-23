{
module DTS.Alligator.AlexHappy.GetDepth.Lexer (
  Token(..),
  scanTokens
) where

import DTS.Alligator.AlexHappy.GetDepth.Syntax
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
    { \s -> TokenNumOf "predicates"}
  \% [$space]* Number [$space]+ of [$space]+ atoms
    { \s -> TokenNumOf "atoms"}
  \% [$space]* Number [$space]+ of [$space]+ variables
    { \s -> TokenNumOf "variables"}
  \% [$space]* Syntax   : Number [$space]+ of [$space]+ clauses
    { \s -> TokenNumOf "clauses"}
  \% [$space]*Number [$space]+ of [$space]+ clause size
    { \s -> TokenNumOf "clause size"}
  \% [$space]*Maximal [$space]+ clause [$space]+ size
    { \s -> TokenNumOf "clause size"}
  \% [$space]*Number [$space]+ of [$space]+ functors
    { \s -> TokenNumOf "functors"}
  \% [$space]*Maximal [$space]+ term [$space]+ depth
    { \s -> TokenNumOf "Maximal term depth"}

Syntax   :
  \% [$space]*Syntax [$space]* :[$space]*Number [$space]+ of [$space]+ clauses
    { \s -> TokenNumOf "clauses"}
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
  = TokenNumOf String
  | TokenNum Int
  | TokenEOF
  | TokenFOF
  | TokenCNF
  | TokenHead
  | TokenCoron
  | TokenComma
  | TokenInclude
  | TokenConne String
  | TokenWord String
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
