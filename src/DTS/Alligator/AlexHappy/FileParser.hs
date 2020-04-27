module DTS.Alligator.AlexHappy.FileParser (fileparse) where

import DTS.Alligator.AlexHappy.Eval (eval,eval1)
import DTS.Alligator.AlexHappy.Parser (parseExpr)
import System.Environment

process :: String -> IO (Either String (Bool,String,String,String))
process input = do
  let ast' = parseExpr input
  case ast' of
    Right ast -> do
      result <- eval ast
      case result of
        Just result' -> return $Right result'
        Nothing ->  return $ Left ("Process Error")
    Left err ->
      return $ Left ("Parser Error" ++ show err)
-- ^
fileparse :: String -- ^ filename(relative path from "src" directory)
  -> IO (Either String (Bool,String,String,String)) -- ^ Parser Error / Right (isProved,axioms,conjectures,processed)
fileparse fname  = do
  input <- readFile fname
  process input

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> putStrLn "Usage: assign <input file>"
    [fname] -> do
      result <- fileparse fname
      print result
