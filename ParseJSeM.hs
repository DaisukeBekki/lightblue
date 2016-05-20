{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-
module ParseJSeM (
  JSeMData(..)
  ) where
-}

import Prelude hiding (readFile)
import qualified Data.Text as Strict -- this is required in order to substitute "Data.Text.Internal"
                      -- that Text.XML(.Cursor) uses.
--import qualified Data.Text.IO as Strict
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Lazy
import qualified System.Process as S  -- process
import qualified System.Environment as E -- base
import qualified Text.XML as X        -- Need 'xml-conduit' package
import qualified Text.XML.Cursor as X -- Need 'xml-conduit' package
import qualified Parser.ChartParser as CP  -- lightblue
import qualified DTS.DependentTypes as DTS   -- lightblue

main :: IO()
main = do
  args <- E.getArgs
  parseJSeM $ head args
  --"../JSeM_beta/JSeM_beta_150415.xml"

data JSeMData = JSeMData {
  jsem_id :: Lazy.Text,
  answer :: Lazy.Text,
  phenomena :: [Lazy.Text],
  inference_type :: [Lazy.Text],
  premise :: [Lazy.Text],
  hypothesis :: Lazy.Text
  } deriving (Eq, Show)

parseJSeM :: FilePath -> IO()
parseJSeM xml = do
  --callCoq "hoge"
  doc <- X.readFile X.def xml
  let cursor = X.fromDocument doc
  let problemNodes = X.child cursor >>= X.element "problem"
  mapM_ (processData . problem2Data) problemNodes

problem2Data :: X.Cursor -> JSeMData
problem2Data problem =
  let children = [problem] >>= X.child in
  JSeMData {
    jsem_id = Lazy.fromStrict $ Strict.concat $ [problem] >>= X.laxAttribute "jsem_id",
    answer = Lazy.fromStrict $ Strict.concat $ [problem] >>= X.laxAttribute "answer",
    phenomena = map (Lazy.fromStrict . Strict.strip) $ [problem] >>= X.laxAttribute "phenomena" >>= Strict.split (==','),
    inference_type = map (Lazy.fromStrict . Strict.strip) $ [problem] >>= X.laxAttribute "inference_type" >>= Strict.split (==','),
    premise = map (Lazy.fromStrict . Strict.strip . Strict.replace "\r\n" "") $ children >>= X.element "p" >>= X.child >>= X.element "script" >>= X.child >>= X.content,
    hypothesis = Lazy.fromStrict $ Strict.concat $ map (Strict.strip . Strict.replace "\r\n" "") $ children >>= X.element "h" >>= X.child >>= X.element "script" >>= X.child >>= X.content
    }

processData :: JSeMData -> IO()
processData jsemdata = do
  Lazy.putStrLn $ Lazy.concat ["id [", jsem_id (jsemdata), "]"]
  mapM_ (\p -> do {Lazy.putStr $ Lazy.concat ["P: ", p, "\n"]}) $ premise jsemdata
  Lazy.putStr $ Lazy.concat ["H: ", hypothesis jsemdata, "\n"]
  psems <- mapM parseText $ premise jsemdata
  hsem <- parseText $ hypothesis jsemdata
  let sem = DTS.betaReduce $ DTS.currying psems hsem
  Lazy.putStrLn $ CP.toText sem
  Lazy.putStrLn ""

parseText :: Lazy.Text -> IO(DTS.Preterm)
parseText text = do
  (chart,_) <- CP.parse 16 text
  let nodes = CP.topBox chart
  if nodes == []
     then return $ DTS.Con "No parse"
     else return $ CP.sem $ head $ nodes

callCoq :: Lazy.Text -> IO()
callCoq _ = do
  let coqcommand = Lazy.concat ["echo -e \"Extraction Language Scheme.\nParameter A:Prop.\nParameter B:Prop.\nTheorem id: A -> B -> A.\nExtraction id.\n\" | coqtop 2> /dev/null | awk '{if($0 != \"\") {print $0}}' | tail -n 2"]
  (_, stdout, _, _) <- S.runInteractiveCommand $ Lazy.unpack coqcommand
  t <- Lazy.hGetContents stdout
  Lazy.putStrLn $ Lazy.replace "\n" "" $ Lazy.strip t