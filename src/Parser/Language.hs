{-|
Module      : Parser.Language
Copyright   : (c) Daisuke Bekki, 2023
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Language settings
-}
module Parser.Language (
  Language(..)
  , LangOptions(..)
  , jpOptions
  , enOptions
  ) where

import qualified Data.Char as C         --base
import qualified Data.Text.Lazy as T    --text

-- | Languages
data Language = Japanese | English deriving (Eq,Show)
instance Read Language where
  readsPrec _ r =
    [(Japanese,s) | (x,s) <- lex r, map C.toLower x == "japanese"]
    ++ [(English,s) | (x,s) <- lex r, map C.toLower x == "english"]

-- | Language options
data LangOptions = LangOptions {
  name :: Language
  , existDelimiter :: Bool
  , symbolsToIgnore :: T.Text
  , punctuations :: T.Text
  } deriving (Eq,Show)

jpOptions :: LangOptions
jpOptions = LangOptions {
  name = Japanese
  , existDelimiter = False
  , symbolsToIgnore = "！？!?…「」◎○●▲△▼▽■□◆◇★☆※†‡."
  , punctuations = "，,-――?／＼"
  } 

enOptions :: LangOptions
enOptions = LangOptions {
  name = English
  , existDelimiter = True
  , symbolsToIgnore = ""
  , punctuations = ","
  }

