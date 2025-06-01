{-|
Module      : Parser.LangOptions
Copyright   : (c) Daisuke Bekki, 2025
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Language settings
-}
module Parser.LangOptions (
  defaultJpOptions
  , defaultEnOptions
  ) where

import GHC.Int                          --base
import qualified System.Environment as E --base
import qualified Data.Char as C         --base
import qualified Data.Text.Lazy as T    --text
import qualified Data.Text.Lazy.IO as T --text
import qualified Parser.CCG as CCG      --lightblue
import Parser.Language (LangOptions(..)) --lightblue
import qualified Parser.Language.Japanese.Juman.CallJuman as JU --lightblue
import qualified Parser.Language.Japanese.Filter as JFilter     --lightblue
import qualified Parser.Language.Japanese.Lexicon as JLEX
import qualified Parser.Language.Japanese.MyLexicon as JLEX

defaultJpOptions :: IO LangOptions
defaultJpOptions = do
  lightbluepath <- E.getEnv "LIGHTBLUE"
  jumandicData <- T.readFile $ lightbluepath ++ "src/Parser/Language/Japanese/Juman/Juman.dic"
  return $ JpOptions {
    existDelimiter = False
    , symbolsToIgnore = "！？!?…◎○●▲△▼▽■□◆◇★☆※†‡"
    , punctuations = "、，,-――／＼"
    , periods = "。．."
    , longestWordLength = 23
    , baseLexicon = JLEX.myLexicon
    , jumanDic = map (T.split (=='\t')) $ T.lines jumandicData
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
