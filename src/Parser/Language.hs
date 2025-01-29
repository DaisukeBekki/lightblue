{-|
Module      : Parser.Language
Copyright   : (c) Daisuke Bekki, 2023
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Language settings
-}
module Parser.Language (
  --Language(..)
  LangOptions(..)
  , defaultJpOptions
  , defaultEnOptions
  ) where

import GHC.Int                          --base
import qualified Data.Char as C         --base
import qualified Data.Text.Lazy as T    --text
import qualified Parser.CCG as CCG      --lightblue
import qualified Parser.Language.Japanese.Juman.CallJuman as JU --lightblue
import qualified Parser.Language.Japanese.Filter as JFilter     --lightblue

-- | Languages
-- data Language = Japanese | English deriving (Eq,Show)
-- instance Read Language where
--   readsPrec _ r =
--     [(Japanese,s) | (x,s) <- lex r, map C.toLower x == "japanese"]
--     ++ [(English,s) | (x,s) <- lex r, map C.toLower x == "english"]

-- | Language options
data LangOptions = JpOptions {
  --name :: Language
  existDelimiter :: Bool
  , symbolsToIgnore :: T.Text
  , punctuations :: T.Text
  , periods :: T.Text
  , longestWordLength :: Int64
  , baseLexicon :: [CCG.Node]
  , jumanDic :: [[T.Text]]
  , morphaName :: JU.MorphAnalyzerName
  , nodeFilterBuilder :: T.Text -> IO JFilter.Filter -- ^ filter for CCG nodes
  } | EnOptions { 
  --name :: Language
  existDelimiter :: Bool
  , punctuations :: T.Text
  , periods :: T.Text
  , longestWordLength :: Int64
  , nodeFilterBuilder :: T.Text -> IO JFilter.Filter -- ^ filter for CCG nodes
  } 

defaultJpOptions :: LangOptions
defaultJpOptions = JpOptions {
  --name = Japanese
  existDelimiter = False
  , symbolsToIgnore = "！？!?…◎○●▲△▼▽■□◆◇★☆※†‡"
  , punctuations = "、，,-――／＼"
  , periods = "。．."
  , longestWordLength = 23
  , baseLexicon = []
  , jumanDic = []
  , morphaName = JU.JUMANPP
  , nodeFilterBuilder = \_ -> return (\_ _ -> id)
  } 

defaultEnOptions :: LangOptions
defaultEnOptions = EnOptions {
  --name = English
  existDelimiter = True
  , punctuations = ","
  , periods = "."
  , longestWordLength = 23
  , nodeFilterBuilder = \_ -> return (\_ _ -> id)
  }
