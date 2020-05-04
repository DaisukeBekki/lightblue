module DTS.Alligator.AlexHappy.FileParser (fileparseInfo) where

import DTS.Alligator.AlexHappy.Eval (evalInfo)
import DTS.Alligator.AlexHappy.Parser (parseExpr)
import System.Environment

import DTS.Alligator.AlexHappy.TPTPInfo
import Data.Default (Default(..))

processInfo :: String -> IO Info
processInfo input = do
  let ast' = parseExpr input
  case ast' of
    Right ast -> do
      evalInfo ast
    Left err ->
      return $ def {note = "Parser Error" ++ show err}


fileparseInfo :: String -> IO Info
fileparseInfo fname  = do
  input <- readFile fname
  processInfo input
