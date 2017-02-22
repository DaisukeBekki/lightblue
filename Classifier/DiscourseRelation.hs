{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Interface
Copyright   : (c) Kimi Kaneko, 2017
Licence     : All right reserved
Maintainer  : Kimi Kaneko <kaneko.kimi@is.ocha.ac.jp>
Stability   : beta
-}

module Classifier.DiscourseRelation (
  outputTSV
  ) where

import qualified Data.Text.Lazy as T    -- text
import qualified Data.Text.Lazy.IO as T -- text
import qualified System.IO as S         -- base
import DTS.UDTTwithName
import Interface.Text

-- | korewo kaizo subesi
outputTSV :: Int -> T.Text -> Preterm -> Preterm -> IO()
outputTSV i t a b =
  T.hPutStrLn S.stderr $ T.concat [
    T.pack $ show i,
    "\t",
    t,
    "\t",
    toText a,
    "\t",
    toText b,
    "\t",
    "subj",
    "\t",
    "obj"
    ]
