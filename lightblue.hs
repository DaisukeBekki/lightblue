{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.Time as Time        --time
import qualified System.IO as S           --base
import Options.Applicative                -- optparse-applicative
import Options.Applicative.Help.Core (parserUsage) -- optparse-applicative
import Data.Semigroup ((<>))              -- semigroup
import qualified Parser.ChartParser as CP
import qualified Interface as I
import qualified Interface.JSeM as J

data Options = Options
  { outputContent :: String
  , nBest  :: Int
  , showTypeCheck :: Bool
  , showExecutionTime :: Bool
  } | Version | JSEM

main :: IO()
main = execParser opts >>= lightblueMain 
  where opts = info (helper <*> optionParser)
                 ( fullDesc
                 <> progDesc "echo <sentence> | ./lightblue -o CONTENT"
                 <> header "lightblue - a Japanese CCG parser with DTS representations (c) Daisuke Bekki" )

{-
  <$> :: (a -> b) -> f a -> f b
  <*> :: f(a -> b) -> f a -> f b
-}

optionParser :: Parser Options
optionParser = 
  flag' Version ( long "version" 
                <> short 'v' 
                <> help "Show the version of the software" 
                <> hidden )
  <|> 
  flag' JSEM ( long "jsem" 
             <> help "Parse JSeM data"  
             <> hidden )
  <|> 
  Options 
    <$> strOption 
      ( long "output-content"
      <> short 'o'
      <> metavar "CONTENT"
      <> help "Print the CONTENT={text|tex|xml|html|postag|numeration|jsem}" 
      <> showDefault
      <> value "html" )
    <*> option auto 
      ( long "n-best"
      <> short 'n'
      <> help "Show N-best derivations"
      <> showDefault
      <> value 1
      <> metavar "INT" )
    <*> switch 
      ( long "show-type-check"
      <> short 'c'
      <> help "Execute typechecking for the SR" )
    <*> switch 
      ( long "show-excecution-time"
      <> short 't'
      <> help "Show the execution time" )

lightblueMain :: Options -> IO()
lightblueMain Version = putStrLn "lightblue version 1.6.0"
lightblueMain JSEM = do
  contents <- T.getContents
  mapM_ (T.putStrLn . T.pack . show) $ J.parseJSeM contents
lightblueMain options = do
  start    <- Time.getCurrentTime
  sentence <- T.getLine
  chart <- CP.parse 24 sentence
  let nodes = case CP.extractBestParse chart of
                CP.Full ns -> ns
                CP.Partial ns -> ns
                CP.Failed -> []
  stop     <- Time.getCurrentTime
  let time = Time.diffUTCTime stop start
  case () of
    _ | outputContent options == "html"       -> I.printNodesInHTML S.stdout (showTypeCheck options) nodes
      | outputContent options == "text"       -> I.printNodesInText S.stdout nodes
      | outputContent options == "tex"        -> I.printNodesInTeX  S.stdout nodes
      | outputContent options == "xml"        -> I.printNodesInXML  S.stdout sentence nodes
      | outputContent options == "postag"     -> I.posTagger S.stdout nodes
      | outputContent options == "numeration" -> I.printNumeration S.stdout sentence
      | otherwise -> putStrLn $ show $ parserUsage defaultPrefs optionParser "hoge"
  if showExecutionTime options 
     then S.hPutStrLn S.stderr $ "Total Execution Time: " ++ show time
     else return ()

