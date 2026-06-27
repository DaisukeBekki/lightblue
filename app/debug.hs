{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Prelude hiding (id)
import qualified Data.Text.Lazy as T --text
import qualified Data.List as L      --base
import qualified Data.Maybe as Maybe --base
import Data.Fixed                    --base
import Data.Ratio                    --base
import qualified DTS.DTTdeBruijn as DTTdB  --lightblue
import DTS.UDTTdeBruijn as DTS hiding (sig)--lightblue
import qualified DTS.DTTwithName as DTTwN  --lightblue
import qualified DTS.UDTTwithName as VN    --lightblue
import qualified Interface.Tree as Tree    --lightblue
import Interface.Text
import Interface.TeX
import Interface.HTML
import Parser.CCG
import Parser.Language.Japanese.Templates  --lightblue
import Parser.Language.English.Lexicon 

main :: IO ()
main = do
  print $ S [] == S []
