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

import Prelude hiding (id)
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
import Parser.CCG                       --lightblue
--import Parser.Language 
import Parser.Language.Japanese.Templates          --lightblue
import qualified DTS.DTTdeBruijn as DTT            --lightblue
import DTS.UDTTdeBruijn as UDTT hiding (sig)       --lightblue

type DTTpreterm = DTT.Preterm
type UDTTpreterm = UDTT.Preterm
type Signature = DTT.Signature
type LexicalItems = [Node]
type Token = T.Text
type Filter = Int -> Int -> [Node] -> [Node]

setupLexicon :: T.Text -> IO ([Token], LexicalItems)
setupLexicon sentence = do
  lightbluepath <- E.getEnv "LIGHTBLUE"
  let nltkScript = lightbluepath ++ "src/Parser/Language/English/nltk_pos_tagger.py"
      command = T.concat ["echo \"", sentence, "\" | python ", T.pack nltkScript]
  output <- S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ T.toStrict command 
  let nltkWords = (A.decode $ fromString $ StrictT.unpack output) :: Maybe [NLTKword]
  case nltkWords of
    Just nltkWords' -> do
      putStrLn $ show nltkWords'
      let tokens = map word nltkWords'
          lexicon = concat $ map fromNLTKtoCCG nltkWords'
      -- putStrLn $ "Lexicon: " ++ (show lexicon)
      return (tokens, lexicon)
    Nothing -> do
      T.putStrLn $ T.concat ["Error in NLTK pos tagging: ", sentence]
      return ([], [])

data NLTKword = NLTKword {
  word :: T.Text
  , pos :: T.Text
  } deriving (Eq, Show, G.Generic)

instance A.FromJSON NLTKword

mylex :: [T.Text] -> T.Text -> Cat -> (UDTT.Preterm, Signature) -> [Node]
mylex wds num cat' (sem',sig') = [(lexicalitem wd num 100 cat' (sem',sig')) | wd <- wds ]

sid :: UDTT.Preterm
sid = UDTT.Lam (UDTT.Var 0)

terminator :: UDTT.Preterm
terminator = UDTT.Ann (UDTT.Lam UDTT.Top) (DTT.Pi DTT.Entity DTT.Type)

fromNLTKtoCCG :: NLTKword -> [Node]
fromNLTKtoCCG (NLTKword word pos) = case (word,pos) of
  (w,"NNP") -> mylex [w] "NNP" (NP []) (properNameSR w)
  ("she","PRP") -> mylex ["she"] "PRP" (NP []) ((Proj Fst (Asp (Sigma Entity (Sigma Entity (App (App (Con "woman") (Var 1)) (Var 0)))))), [("woman", DTT.Pi DTT.Entity DTT.Type)])
  --NP [] -- john
  ("A","DT") -> mylex ["a"] "DT" ((S [] `SL` (S [] `BS` NP [])) `SL` N) ((Lam (Lam (Lamvec (Sigma (Sigma Entity (App (App (Var 3) (Var 0)) terminator)) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),[])
  ("a","DT") -> mylex ["a"] "DT" ((S [] `SL` (S [] `BS` NP [])) `SL` N) ((Lam (Lam (Lamvec (Sigma (Sigma Entity (App (App (Var 3) (Var 0)) terminator)) (Appvec 1 (App (Var 2) (Proj Fst (Var 0)))))))),[])
  (w,"DT") -> mylex [w] "DT" (NP [] `SL` N) (nominalModifier w)
  -- ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]])) `SL` N)   -- every, a
  (w,"NN") -> mylex [w] "NN" (N) (commonNounSR w)             -- Common noun? Prenominal adjective??
  (w,"WP") -> mylex [w] "WP" (N `SL` N) (nominalModifier w)
  -- (N `BS` N) `SL` (S [] `BS` NP [] ) -- who
  (w,"NNS") -> mylex [w] "NNS" (S [] `BS` NP []) (predSR 1 w)
  (w,"VBN") -> mylex [w] "VBN" (S [] `BS` NP []) (predSR 1 w) -- ex. walked
  (w,"VBD") -> mylex [w] "VBD" (S [] `BS` NP []) (predSR 1 w) -- ex. sat
  (w,"VBZ") -> mylex [w] "VBZ" (S [] `BS` NP [] `SL` NP []) (predSR 2 w)
  -- (S [] `BS` NP []) `SL` NP []  -- own
  (w,"IN") -> mylex [w] "IN" ((S [] `BS` NP []) `BS` (S [] `BS` NP [])) (sid,[])
  (w,"RB") -> mylex [w] "RB" ((S [] `BS` NP []) `BS` (S [] `BS` NP [])) (sid,[])
  ("and","CC") -> mylex ["and"] "CC" CONJ andSR
  ("or","CC") -> mylex ["or"] "CC" CONJ orSR
  ("Either","CC") -> mylex ["Either"] "CC" ((S []) `SL` (S []) `SL` CONJ `SL` (S [])) ((UDTT.Lam (UDTT.Lam (UDTT.Lam (UDTT.Lam (UDTT.App (UDTT.App (Var 2) (UDTT.App (Var 3) (Var 0))) (UDTT.App (Var 1) (Var 0))))))),[])
  (w,"PRP") -> mylex [w] "PRP" (NP []) (properNameSR w)
  (w,".") -> mylex [w] "." (PERIOD) (id,[])
  (w,_) -> mylex [w] "_" (PUNCT) (id,[])
  -- mylex [w] "Error" N (commonNounSR w) 