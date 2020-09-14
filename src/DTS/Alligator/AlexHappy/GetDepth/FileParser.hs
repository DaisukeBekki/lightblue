module DTS.Alligator.AlexHappy.GetDepth.FileParser (fileparseInfo) where

import DTS.Alligator.AlexHappy.GetDepth.Eval (evalNum)
import DTS.Alligator.AlexHappy.GetDepth.Parser (parseExpr)
import DTS.Alligator.AlexHappy.GetDepth.Syntax
import System.Environment

import DTS.Alligator.AlexHappy.TPTPInfo
import Data.Default (Default(..))
import Debug.Trace as D

processInfo :: String -> String -> Nums
processInfo input fname =
  let ast' = parseExpr input in
  case ast' of
    Right ast ->
      let astNum = filter (\x -> case x of ; (NumOf a b) -> True ; _ -> False) ast in
      evalNum ast fname
    Left err ->
      def


fileparseInfo :: String -> IO Nums
fileparseInfo fname  = do
  input <- readFile fname
  return $ processInfo input fname
