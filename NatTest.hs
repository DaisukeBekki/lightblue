{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy.IO as T
import DependentTypes

main :: IO()
main = do
  T.putStrLn $ toText $ betaReduce $ (Succ Zero) `add` (Succ (Succ Zero))
  T.putStrLn $ toText $ betaReduce $ (Succ $ Succ $ Succ Zero) `multiply` (Succ (Succ Zero))