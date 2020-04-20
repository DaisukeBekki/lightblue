{
module DTS.Alligator.AlexHappy.Lexerf (
  Token(..),
  scanTokens
) where

import DTS.Alligator.AlexHappy.Syntaxf
import DTS.Alligator.Arrowterm
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]
$space = [\ ]
$hyphen = [\-]
$word = [A-Z a-z $digit \_ \' \- $hyphen]


tokens :-
  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;
  \)
    { \s -> TokenRBracket }
  \(
    { \s -> TokenLBracket }
  \]
    { \s -> TokenRRBracket }
  \[
    { \s -> TokenRLBracket }
  \~
    { \s -> TokenNeg}
  \,
    { \s -> TokenComma }
  \|
    { \s -> TokenOr}
  \&
    { \s -> TokenAnd}
  \:
    { \s -> TokenCoron}
  \!
    { \s -> TokenAll }
  \?
    { \s -> TokenExists}
  \< \= \>
    { \s -> TokenEquiv}
  \< \=
    { \s -> TokenBiop s}
  \= \>
    { \s -> TokenImp}
  \< \~ \>
    { \s -> TokenBiop s}
  \~ \|
    { \s -> TokenBiop s}
  \~ \&
    { \s -> TokenBiop s}
  \$ [$space]* true
    { \s -> TokenTop}
  \$ [$space]* false
    { \s -> TokenBot}
  \~ \&
    { \s -> TokenBiop s}
  [$word]+
    { \s -> TokenWord s }

{

data Token
  = TokenNeg
  | TokenBiop String
  | TokenWord String
  | TokenAnd
  | TokenCoron
  | TokenComma
  | TokenOr
  | TokenAll
  | TokenExists
  | TokenImp
  | TokenTop
  | TokenBot
  | TokenEquiv
  | TokenRBracket
  | TokenLBracket
  | TokenRRBracket
  | TokenRLBracket
  deriving (Eq,Show)

scanTokens = alexScanTokens

}
