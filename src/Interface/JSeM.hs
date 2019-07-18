{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Interface.JSeM
Copyright   : (c) Daisuke Bekki, 2017
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

JSeM interface
-}
module Interface.JSeM (
  JSeMData(..),
  JSeMlabel(..),
  parseJSeM
  ) where

import qualified Data.Text as StrictT     -- this is required in order to substitute "Data.Text.Internal" that Text.XML(.Cursor) uses.
import qualified Data.Text.Lazy as LazyT  --text
import qualified Text.XML as X            --xml-conduit
import qualified Text.XML.Cursor as X     --xml-conduit

-- | A data type for each JSeM entry.
data JSeMData = JSeMData {
  jsem_id :: LazyT.Text,
  answer :: JSeMlabel,
  phenomena :: [LazyT.Text],
  inference_type :: [LazyT.Text],
  premise :: [LazyT.Text],
  hypothesis :: LazyT.Text
  } deriving (Eq, Show)

-- | Three answers: yes, no, unknown
data JSeMlabel = YES | NO | UNKNOWN | UNDEF deriving (Eq,Show)

-- | takes a file path of a JSeM file (XML format) and returns a list of 'JSeMData'.
parseJSeM :: LazyT.Text -> IO([JSeMData])
parseJSeM filepath = 
  let cursor = X.fromDocument $ X.parseText_ X.def filepath
      problemNodes = X.child cursor >>= X.element "problem"
  in mapM problem2JSeMData problemNodes

-- | takes a "problem" node in a JSeM file and translates it to a 'JSeMData'.  
-- Note that the xml-conduit package uses Data.Text (=strict texts) as internal format of text data, 
-- and `problem2JSeMData` function converts them to Data.Text.Lazy (=lazy texts), 
-- which is a standard format of text data in lightblue.
problem2JSeMData :: X.Cursor -> IO(JSeMData)
problem2JSeMData problem = do
  let children = [problem] >>= X.child
      answertext = LazyT.fromStrict $ StrictT.concat $ [problem] >>= X.laxAttribute "answer"
      idtext = LazyT.fromStrict $ StrictT.concat $ [problem] >>= X.laxAttribute "jsem_id"
  ans <- case answertext of
           "yes" -> return YES
           "no" -> return NO
           "unknown" -> return UNKNOWN
           "undef" -> return UNDEF
           _ -> ioError $ userError $ LazyT.unpack $ LazyT.concat ["#", idtext, " has an undefined answer: ", answertext]
  return JSeMData {
    jsem_id = idtext,
    answer = ans,
    phenomena = map (LazyT.fromStrict . StrictT.strip) $ [problem] >>= X.laxAttribute "phenomena" >>= StrictT.split (==','),
    inference_type = map (LazyT.fromStrict . StrictT.strip) $ [problem] >>= X.laxAttribute "inference_type" >>= StrictT.split (==','),
    premise = map (LazyT.fromStrict . StrictT.strip . StrictT.replace "\r\n" "") $ children >>= X.element "p" >>= X.child >>= X.element "script" >>= X.child >>= X.content,
    hypothesis = LazyT.fromStrict $ StrictT.concat $ map (StrictT.strip . StrictT.replace "\r\n" "") $ children >>= X.element "h" >>= X.child >>= X.element "script" >>= X.child >>= X.content
    }
