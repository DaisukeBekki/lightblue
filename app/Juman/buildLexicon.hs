module Main where

import Parser.Language.Japanese.Juman.Config (testConfig) --lightblue
import Parser.Language.Japanese.Juman.ParseCaseFrame (buildCaseFrame) --lightblue
import Parser.Language.Japanese.Juman.ConvertJumanDic (buildJumanLexicon) --lightblue

main :: IO()
main = do
  buildCaseFrame -- parse Kyodai Case Frame xml file
  --buildJumanLexicon -- parse Juman dictionary files and merge them with case frame data

