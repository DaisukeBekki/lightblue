{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module DTS.Alligator.TPTP.Parser (
  Tvar(..),
  Tbop(..),
  Tformula(..),
  cleanse,
  parseFormula
  ) where

import qualified System.IO as S
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as Te
import qualified DTS.DTT as DT
import Text.Parsec
import Text.Parsec.Text

data Tvar = Tvar Char deriving (Eq,Show)
data Tbop = Tand | Tor | Timp | Tequiv deriving (Eq,Show)

data Tformula =
  Tletter Char
  | Ttrue
  | Tfalse
  | Tneg Tformula
  | Tbinary Tbop Tformula Tformula
  | Tall [Tvar] Tformula
  | Texist [Tvar] Tformula
  deriving (Eq, Show)


cleanse :: T.Text -> T.Text
cleanse tptp_text =
   T.filter (not . C.isSpace) $ T.concat $ filter (\ln -> case T.uncons ln of
                                              Just ('%',_) -> False
                                              Just _ -> True
                                              Nothing -> False
                                              ) $ T.lines tptp_text


parseFormula :: T.Text -> IO(Tformula)
parseFormula text =
  case parse tptpParser "" text of
    Left err -> do
                T.hPutStrLn S.stderr text
                fail $ show err
    Right f -> return f


tptpParser :: Parser Tformula
tptpParser = do
  string "fof("
  many1 alphaNum
  char ','
  string "conjecture"
  char ','
  f <- formulaParser
  string ")."
  return f

bopParser :: Parser Tbop
bopParser = do
  (char '&' *> return Tand)
  <|>
  (char '|' *> return Tor)
  <|>
  (string "==>" *> return Timp)
  <|>
  (string "<=>" *> return Tequiv)

formulaParser :: Parser Tformula
formulaParser = do
  ( Tletter <$> oneOf "pqr" )
  <|>
  (do
   char '~'
   Tneg <$> formulaParser
   )
  <|>
  (do
   char '('
   p1 <- formulaParser
   op <- bopParser
   p2 <- formulaParser
   char ')'
   return $ Tbinary op p1 p2
   )



main :: IO()
main = (putStrLn . show) =<< parseFormula =<< cleanse <$> T.readFile "DTS/Alligator/TPTP/SYN045+1.p"
