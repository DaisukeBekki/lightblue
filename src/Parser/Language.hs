{-|
Module      : Parser.Language
Copyright   : (c) Daisuke Bekki, 2023
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Language settings
-}
module Parser.Language (
  Language(..),
  LangOptions(..),
  jpOptions
  ) where

import qualified Data.Text.Lazy as T    --text
-- import qualified Data.Text.Lazy.IO as T --text

-- | Languages
data Language = Japanese | English deriving (Eq,Show,Read)

-- | Language options
data LangOptions = LangOptions {
  name :: Language
  , existDelimiter :: Bool
  , meaninglessSymbols :: T.Text
  , punctuations :: T.Text
  } deriving (Eq,Show)

jpOptions :: LangOptions
jpOptions = LangOptions {
  name = Japanese
  , existDelimiter = False
  , meaninglessSymbols = "！？!?…「」◎○●▲△▼▽■□◆◇★☆※†‡."
  , punctuations = "，,-―?／＼"
  } 

enOptions :: LangOptions
enOptions = LangOptions {
  name = English
  , existDelimiter = True
  , meaninglessSymbols = ""
  , punctuations = ","
  }

