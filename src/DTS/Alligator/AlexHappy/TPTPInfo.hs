module DTS.Alligator.AlexHappy.TPTPInfo where

import Data.Default (Default(..))
import qualified DTS.DTT as DT

timelimit :: Int
timelimit =  1000000

data Info
  = Info{
      language :: String,
      status :: String,
      filename :: String,
      result :: Bool,
      strcontext :: String,
      strtarget :: String,
      strprocessed :: String,
      note :: String,
      context :: [DT.Preterm],
      prelst :: [(String,Int)],
      negated_conjecture :: [String],
      target :: Maybe DT.Preterm
    } deriving (Show,Eq)

instance Default Info where
  def = Info{negated_conjecture = [],language = "",status="",prelst=[],filename = "",result = False,strcontext = "",strtarget = "",strprocessed = "",note = "",context=[],target=Nothing}

tptpdir :: String
tptpdir = "../../TPTP-v7.3.0/"
