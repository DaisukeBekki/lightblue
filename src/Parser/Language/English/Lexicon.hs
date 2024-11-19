{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{-|
Module      : Parser.Language.Japanese.Lexicon
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

A Japanese CCG lexicon.
-}

module Parser.Language.English.Lexicon (
  setupLexicon
) where

import qualified GHC.Generics as G       --base
import qualified System.Environment as E --base
import Data.String (fromString) --base
import qualified Data.Text as StrictT    --text
import qualified Data.Text.Lazy as T     --text
import qualified Data.Text.Lazy.IO as T  --text
import qualified Shelly as S             --shelly
import qualified Data.Aeson            as A --aeson
import qualified Data.ByteString.Char8 as B --bytestring 
import qualified Data.Yaml             as Y --yaml
import Parser.CCG                --lightblue
import Parser.Language.Japanese.Templates
import qualified DTS.DTTdeBruijn as DTT --lightblue
import DTS.UDTTdeBruijn as UDTT hiding (sig) --lightblue

type DTTpreterm = DTT.Preterm
type UDTTpreterm = UDTT.Preterm
type Signature = DTT.Signature

setupLexicon :: T.Text -> IO ()
setupLexicon sentence = do
  lightbluepath <- E.getEnv "LIGHTBLUE"
  let nltkScript = lightbluepath ++ "src/Parser/Language/English/nltk_pos_tagger.py"
      command = T.concat ["echo \"", sentence, "\" | python ", T.pack nltkScript]
  output <- S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ T.toStrict command 
  let nltkWords = (A.decode $ fromString $ StrictT.unpack output) :: Maybe [NLTKword]
  case nltkWords of
    Just nltkWords' -> putStrLn $ show $ concat $ map fromNLTKtoCCG nltkWords'
    Nothing -> T.putStrLn $ T.concat ["Error in NLTK pos tagging: ", sentence]

data NLTKword = NLTKword {
  word :: T.Text
  , pos :: T.Text
  } deriving (Eq, Show, G.Generic)

instance A.FromJSON NLTKword

mylex :: [T.Text] -> T.Text -> Cat -> (UDTT.Preterm, Signature) -> [Node]
mylex wds num cat' (sem',sig') = [(lexicalitem wd num 100 cat' (sem',sig')) | wd <- wds ]

fromNLTKtoCCG :: NLTKword -> [Node]
fromNLTKtoCCG (NLTKword word pos) = case (word,pos) of
  --(w,"NNP") -> NP [] -- john
  --(w,"DT") -> ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `SL` N)   -- every, a
  (w,"NN") -> 
    mylex [w] "NN" N (commonNounSR w)
  --(w,"WP") -> (N `BS` N) `SL` (S [] `BS` NP [] ) -- who
  --(w,"VBZ") -> (S [] `BS` NP []) `SL` NP []  -- own
  --(w,"PRP") -> NP [] -- it
  --(w,".") -> NP []
  (w,_) -> 
    mylex [w] "Error" N (commonNounSR w) 