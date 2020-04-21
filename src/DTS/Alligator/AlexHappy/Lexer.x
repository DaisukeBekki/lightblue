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
$word = [A-Z a-z $digit \_ \' \- $hyphen \+ \[ \] \! \? \; \= \$ \" \/ \{ \} \* \< \>]


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
  [$space]* $digit+
    { \s -> TokenNum (read s) }
  \% [$space]* Number [$space]+ of [$space]+ predicates
    { \s -> TokenPreNum}
  \% [$space]* File
    { \s -> TokenFile }
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
  | TokenEOF
  | TokenFOF
  | TokenHead
  | TokenCoron
  | TokenComma
  | TokenConne String
  | TokenWord String
  | TokenFile
  | TokenAnd
  | TokenRBracket
  | TokenLBracket
  | TokenRRBracket
  | TokenRLBracket
  | TokenPeriod
  deriving (Eq,Show)

scanTokens = alexScanTokens

}
