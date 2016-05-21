{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : JSeM interface
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}
module Interface.JSeM (
  JSeMData(..),
  parseJSeM
  ) where

import qualified Data.Text as StrictT -- this is required in order to substitute "Data.Text.Internal" that Text.XML(.Cursor) uses.
import qualified Data.Text.Lazy as LazyT    -- text
import qualified Text.XML as X          -- xml-conduit
import qualified Text.XML.Cursor as X   -- xml-conduit

-- | A data type for each JSeM entry.
data JSeMData = JSeMData {
  jsem_id :: LazyT.Text,
  answer :: LazyT.Text,
  phenomena :: [LazyT.Text],
  inference_type :: [LazyT.Text],
  premise :: [LazyT.Text],
  hypothesis :: LazyT.Text
  } deriving (Eq, Show)

-- | takes a file path of a JSeM file (XML format) and returns a list of 'JSeMData'.
parseJSeM :: FilePath -> IO([JSeMData])
parseJSeM xmlfile = do
  doc <- X.readFile X.def xmlfile
  let cursor = X.fromDocument doc
  let problemNodes = X.child cursor >>= X.element "problem"
  return $ map problem2JSeMData problemNodes

-- | takes a "problem" node in a JSeM file and translates it to a 'JSeMData'.  Note that the xml-conduit package uses Data.Text (=strict texts) as internal format of text data, and `problem2JSeMData` function converts them to Data.Text.Lazy (=lazy texts), which is a standard format of text data in lightblue.
problem2JSeMData :: X.Cursor -> JSeMData
problem2JSeMData problem =
  let children = [problem] >>= X.child in
  JSeMData {
    jsem_id = LazyT.fromStrict $ StrictT.concat $ [problem] >>= X.laxAttribute "jsem_id",
    answer = LazyT.fromStrict $ StrictT.concat $ [problem] >>= X.laxAttribute "answer",
    phenomena = map (LazyT.fromStrict . StrictT.strip) $ [problem] >>= X.laxAttribute "phenomena" >>= StrictT.split (==','),
    inference_type = map (LazyT.fromStrict . StrictT.strip) $ [problem] >>= X.laxAttribute "inference_type" >>= StrictT.split (==','),
    premise = map (LazyT.fromStrict . StrictT.strip . StrictT.replace "\r\n" "") $ children >>= X.element "p" >>= X.child >>= X.element "script" >>= X.child >>= X.content,
    hypothesis = LazyT.fromStrict $ StrictT.concat $ map (StrictT.strip . StrictT.replace "\r\n" "") $ children >>= X.element "h" >>= X.child >>= X.element "script" >>= X.child >>= X.content
    }


