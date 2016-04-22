{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-
module ParseJSeM (
  JSeMData(..)
  ) where
-}

import Prelude hiding (readFile)
import qualified Data.Text as T -- this is required in order to substitute "Data.Text.Internal"
                      -- that Text.XML(.Cursor) uses.
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Lazy
import qualified System.Process as S  -- process
import qualified System.Environment as E -- base
import qualified Text.XML as X        -- Need 'xml-conduit' package
import qualified Text.XML.Cursor as X -- Need 'xml-conduit' package
import qualified Parser.ChartParser as CP  -- lightblue

main :: IO()
main = do
  args <- E.getArgs
  parseJSeM $ head args
  --"../JSeM_beta/JSeM_beta_150415.xml"

data JSeMData = JSeMData {
  jsem_id :: T.Text,
  answer :: T.Text,
  phenomena :: [T.Text],
  inference_type :: [T.Text],
  premise :: [T.Text],
  hypothesis :: T.Text
  } deriving (Eq, Show)

parseJSeM :: FilePath -> IO()
parseJSeM xml = do
  callCoq "hoge"
  doc <- X.readFile X.def xml
  let cursor = X.fromDocument doc
  let problemNodes = X.child cursor >>= X.element "problem"
  mapM_ (processData . problem2Data) problemNodes

problem2Data :: X.Cursor -> JSeMData
problem2Data problem =
  let children = [problem] >>= X.child in
  JSeMData {
    jsem_id = T.concat $ [problem] >>= X.laxAttribute "jsem_id",
    answer = T.concat $ [problem] >>= X.laxAttribute "answer",
    phenomena = map T.strip $ [problem] >>= X.laxAttribute "phenomena" >>= T.split (==','),
    inference_type = map T.strip $ [problem] >>= X.laxAttribute "inference_type" >>= T.split (==','),
    premise = map (T.strip . T.replace "\r\n" "") $ children >>= X.element "p" >>= X.child >>= X.element "script" >>= X.child >>= X.content,
    hypothesis = T.concat $ map (T.strip . T.replace "\r\n" "") $ children >>= X.element "h" >>= X.child >>= X.element "script" >>= X.child >>= X.content
    }

processData :: JSeMData -> IO()
processData jsemdata = do
  T.putStrLn $ T.concat ["id [", jsem_id (jsemdata), "]"]
  mapM_ (\p -> do {T.putStr $ T.concat ["P: ", p, "\n\t"]; psem <- parseStrictText p; Lazy.putStrLn psem}) $ premise jsemdata
  T.putStr $ T.concat ["H: ", hypothesis jsemdata, "\n\t"]
  hsem <- parseStrictText $ hypothesis $ jsemdata
  Lazy.putStrLn hsem
  T.putStrLn ""

parseStrictText :: T.Text -> IO(Lazy.Text)
parseStrictText t = do
  (chart,_) <- CP.parse 32 $ Lazy.fromStrict t
  let nodes = CP.topBox chart
  if nodes == []
     then return "No parse result."
     else return $ CP.toText $ CP.sem $ head $ nodes

callCoq :: T.Text -> IO()
callCoq _ = do
  let coqcommand = T.concat ["echo -e \"Extraction Language Scheme.\nParameter A:Prop.\nParameter B:Prop.\nTheorem id: A -> B -> A.\nExtraction id.\n\" | coqtop 2> /dev/null | awk '{if($0 != \"\") {print $0}}' | tail -n 2"]
  (_, stdout, _, _) <- S.runInteractiveCommand $ T.unpack coqcommand
  t <- Lazy.hGetContents stdout
  Lazy.putStrLn $ Lazy.replace "\n" "" $ Lazy.strip t