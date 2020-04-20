-- module DTS.Alligator.AlexHappy.Main () where

import DTS.Alligator.AlexHappy.Eval (eval)
import DTS.Alligator.AlexHappy.Parser (parseExpr)
import System.Environment

process :: String -> IO ()
process input = do
  let ast = parseExpr input
  case ast of
    Right ast ->  eval ast
      -- print ast
    Left err -> do
      putStrLn "Parser Error:"
      print err

-- ^ srcで ¥>sh ./make.sh
-- ^ ¥>./DTS/Alligator/AlexHappy/Main "./DTS/Alligator/TPTP/test.test"
main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> putStrLn "Usage: assign <input file>"
    [fname] -> do
      contents <- readFile fname
      process contents
