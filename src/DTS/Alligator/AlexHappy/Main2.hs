module DTS.Alligator.AlexHappy.Main2 () where

import DTS.Alligator.AlexHappy.Evalf (t2dt)
import DTS.Alligator.AlexHappy.Parserf (parseExpr)
import System.Environment
import qualified DTS.DTT as DT

processf :: String -> Either String DT.Preterm
processf input = do
  let ast = parseExpr input
  case ast of
    Right ast ->Right $ snd $ t2dt ast []
    Left err -> Left $ "Error in processf @" ++ input

-- ^ srcで ¥>sh ./make.sh
-- ^ ¥>./DTS/Alligator/AlexHappy/Main2 "./DTS/Alligator/TPTP/test2.test"
main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> putStrLn "Usage: assign <input file>"
    [fname] -> do
      contents <- readFile fname
      print $ processf contents
