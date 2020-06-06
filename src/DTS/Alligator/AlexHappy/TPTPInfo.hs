module DTS.Alligator.AlexHappy.TPTPInfo where

import Data.Default (Default(..))
import qualified DTS.DTT as DT

timelimit :: Int
timelimit =  60000000

data Language =
 THF -- ^ formulae in typed higher-order form
 | TFF -- ^ formulae in typed first-order form
 | FOF -- ^ formulae in first order form
 | CNF -- ^ formulae in clause normal form.
  deriving(Eq)

instance Show Language where
  show role =
    case role of
     THF -> "thf"
     TFF ->"tff"
     FOF -> "fof"
     CNF -> "cnf"

instance Read Language where
  readsPrec _  str =
    let lans = [THF,TFF,FOF,CNF]
    in
      map (\(ro,(s,r))-> (ro,r))
        $filter (\(ro,(s,r)) -> show ro == s)
          $ zip lans $ map (\lan -> splitAt (length (show lan)) str) lans


data Role =
   Axiom
 | Hypothesis
 | Definition
 | Assumption
 | Lemma
 | RTheorem
 | Corollary
 | Conjecture
 | NegatedConjecture
 | Plain
 | Type
 | RUnknown
 deriving(Eq)

instance Show Role where
  show role =
    case role of
     Axiom -> "axiom"
     Hypothesis ->"hypothesis"
     Assumption -> "assumption"
     Lemma -> "lemma"
     RTheorem -> "theorem"
     Corollary -> "corollary"
     Conjecture -> "conjecture"
     NegatedConjecture -> "negated_conjecture"
     Plain -> "plain"
     Type -> "type"
     RUnknown -> "unknown"

instance Read Role where
  readsPrec _  str =
    let roles = [Axiom,Hypothesis,Assumption,Lemma,RTheorem,Corollary,Conjecture,NegatedConjecture,Plain,Type,RUnknown]
    in
      map (\(ro,(s,r))-> (ro,r))
        $filter (\(ro,(s,r)) -> show ro == s)
          $ zip roles $ map (\role -> splitAt (length (show role)) str) roles

-- | They are accepted, without proof, as a basis for proving conjectures in THF, TFF, and FOF problems. In CNF problems the axiom-like formulae are accepted as part of the set whose satisfiability has to be established.
isAxiomLike :: Role -> Bool
isAxiomLike role =
  role `elem` [Axiom, Hypothesis, Definition, Assumption, Lemma, RTheorem, Corollary]

data Status =
   Theorem
 | ContradictoryAxioms
 | Satisfiable
 | Unsatisfiable
 | CounterSatisfiable
 | Unknown
 | Open
 deriving(Show,Read,Eq)


shouldBeTrue :: Status -> Bool
shouldBeTrue status =
  status `elem` [Theorem,ContradictoryAxioms]

shouldBeFalse :: Status -> Bool
shouldBeFalse status =
  status `elem` [CounterSatisfiable,Unsatisfiable]

shouldBeUnknown :: Status -> Bool
shouldBeUnknown status =
  status `elem` [Satisfiable,Unknown,Open]

data Result = YES | NO | UNKNOWN deriving (Show,Eq)

data Info
  = Info{
      language :: Maybe Language,
      status :: Maybe Status,
      filename :: String,
      dneResult :: Result,
      efqResult :: Result,
      dneUrl :: String,
      efqUrl :: String,
      strcontext :: String,
      strtarget :: String,
      strprocessed :: String,
      strnegated :: String,
      note :: String,
      context :: [DT.Preterm],
      prelst :: [(String,Int)],
      negated_conjecture :: Maybe DT.Preterm,
      target :: Maybe DT.Preterm
    } deriving (Show,Eq)

instance Default Info where
  def = Info{dneUrl = "",efqUrl ="",strnegated = "",negated_conjecture = Nothing,language = Nothing,status=Nothing,prelst=[],filename = "",dneResult = UNKNOWN,efqResult = UNKNOWN,strcontext = "",strtarget = "",strprocessed = "",note = "",context=[],target=Nothing}

tptpdir :: String
tptpdir = "../../TPTP-v7.3.0/"

resultfname :: String
resultfname = "DTS/Alligator/AlexHappy/output/result.csv"

outputdir :: String
outputdir = "DTS/Alligator/AlexHappy/output/"
