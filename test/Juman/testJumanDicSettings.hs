module Main where

import Parser.Language.Japanese.Juman.Config (testConfig) --lightblue
import Parser.Language.Japanese.Juman.ParseJumanDic (dryRun) --lightblue

main :: IO()
main = do
  testConfig
  dryRun
