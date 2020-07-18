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

statusToResult :: Status -> Result
statusToResult status
  | shouldBeTrue status = YES
  | shouldBeFalse status = NO
  | shouldBeUnknown status = UNKNOWN

data Result = YES | NO | UNKNOWN deriving (Show,Read,Eq,Enum,Bounded)

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

testFileExtentions :: [String]
testFileExtentions = ["test","p"]

dirs :: [String]
-- dirs = ["DTS/Alligator/Test/"]
dirs = ["DTS/Alligator/Test/","../../TPTP-v7.3.0/Problems/SYN/"]

exceptList :: [String]
exceptList =
  [
    "../../TPTP-v7.3.0/Problems/SYN/SYN000_4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000=2.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000+1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN014-1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN072-1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN013-1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN077-1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN078-1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN552-1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN076+1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN075+1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN078+1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN551+1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN407^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN732^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN055^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000^3.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN071+1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN989^2.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN057^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN731^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN056^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN377^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN988^2.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN551+3.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN367^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN001^4.002.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000+2.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN007^4.014.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN393^4.002.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN416^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN051^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN355^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN382^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN417+1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN036^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN356^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN381^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN388^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN988^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN989^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN389^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN357^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN999^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN998^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN001^4.001.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN978^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN416^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN386^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN374^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN360^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN996^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN983^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN997^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN375^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN361^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN977^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN377^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN995^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN994^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN390^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN984^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN990^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN052^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN991^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN391^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN985^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN365^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000-1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN387^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN993^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN987^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN001^4.004.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN393^4.004.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000_1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN045^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN392^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN992^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN358^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN364^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN915^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN040^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN397^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN049^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN036^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN916^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000^2.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN041^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN357^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN989^3.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN046^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN001^4.003.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000-2.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN393^4.003.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN047^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN045^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN058^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN064^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN741^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN059^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN987^2.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000_2.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN387^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN044^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN551+2.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000_3.p"
  ]
