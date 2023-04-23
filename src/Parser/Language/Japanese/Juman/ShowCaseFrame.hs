{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
河原・林部らの格フレーム辞書(XML)をパーズして、指定した語の格フレームデータを表示する
Usage: echo 思う | ./ShowCaseFrame
-}

module Parser.Language.Japanese.Juman.ShowCaseFrame (
  showCaseFrame
  ) where

import Prelude as P
import qualified Data.ByteString.Lazy as BSL
import Text.XML.Expat.SAX
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Parser.Language.Japanese.Juman.Config (Config(..),fetchConfig)

showCaseFrame :: IO()
showCaseFrame = do
  config <- fetchConfig
  line <- T.getLine
  contents <- BSL.readFile $ kyodaiCaseFrameData config
  let entries = parse defaultParseOptions contents
  searchEntry line entries

type Event = SAXEvent T.Text T.Text

searchEntry :: T.Text -> [Event] -> IO()
searchEntry word events = case events of
  [] -> return ()
  (e@(StartElement "entry" attribs):es) ->
    do
    let attrMap = M.fromList attribs
    case M.lookup "headword" attrMap of
      Just headword -> 
        if word `T.isPrefixOf` headword
          then do
               printEvent e
               printEvents word es
        else searchEntry word es
      Nothing -> searchEntry word es
  (_:es) -> searchEntry word es

printEvents :: T.Text -> [Event] -> IO()
printEvents word events = case events of
  [] -> return ()
  (e@(EndElement "entry"):es) -> do
                                 printEvent e
                                 searchEntry word es
  (e:es) -> do
            printEvent e
            printEvents word es

printEvent :: Event -> IO()
printEvent event = case event of
  StartElement tag attribs -> 
    do
    T.putStr $ T.concat ["<",tag]
    mapM_ (\(t1,t2) -> T.putStr $ T.concat [" ",t1,"=\"",t2,"\""]) attribs
    T.putStr ">"
  EndElement tag -> T.putStr $ T.concat ["</",tag,">"]
  CharacterData text ->  T.putStr text
  --StartCData ->  
  --EndCData ->  
  _ -> return ()
