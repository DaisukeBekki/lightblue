{-|
Module      : Parser.Japanese.Template
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

A set of lexical templates for building a lexicon.
-}

module Parser.Language.Japanese.Templates (
  -- * Templates for lexical items
  lexicalitem,
  -- * Templates for CCG syntactic features
  defS,
  ---
  verb,
  adjective,
  nomPred,
  nonStem,
  modifiableS,
  anyPos,
  m5,
  pmmmm,
  mpmmm,
  mmpmm,
  mmmpm,
  mppmm,
  -- * Templates for DTS representations
  id,
  entity,
  event,
  state,
  --
  verbCat,
  verbSR,
  predSR,
  nPlaceVerbType,
  nPlaceEventType,
  nPlaceStateType,
  nPlacePredType,
  properNameSR,
  commonNounSR,
  generalizedQuantifierSR,
  intensionalEvent,
  intensionalState,
  modalSR,
  --modifierSR,
  ---
  mannerAdverb,
  eventModifier,
  nominalModifier,
  negOperator,
  negOperator2,
  argumentCM,
  adjunctCM,
  adjunctNM,
  andSR,
  orSR,
  conjunctionSR,
  ) where

import Prelude hiding (id,not)
import qualified Data.Text.Lazy as T -- text
import Data.Ratio
import Parser.CCG
import qualified DTS.DTTdeBruijn as DTT
import DTS.UDTTdeBruijn as UDTT hiding (sig)

type UDTTpreterm = UDTT.Preterm
type DTTpreterm = DTT.Preterm
type Signature = DTT.Signature

not :: UDTT.Preterm -> UDTT.Preterm
not t = UDTT.Pi t UDTT.Bot

{- Some Macros for defining lexical items -}

-- | defines a lexical item.
lexicalitem :: T.Text                    -- ^ A phonetic form
               -> T.Text                 -- ^ A source of a lexical item, such as a number in the CCG textbook.
               -> Integer                -- ^ A score (0 to 100)
               -> Cat                    -- ^ A syntactic category
               -> (UDTTpreterm, Signature) -- ^ A semantic representation (in DTS) and a list of signatures
               -> Node
lexicalitem pf' source' score' cat' (sem',sig') = Node {rs=LEX, pf=pf', cat=cat', sem=sem', daughters=[], score=(score' % 100), source=source', sig=sig'}

{- Some Marcos for CCG categories/features -}

-- | Category S with the default feature setting (mainly for stems).
defS :: [FeatureValue] -> [FeatureValue] -> Cat
defS p c = S [F p,F c,F[M],F[M],F[M],F[M],F[M]]

entity :: DTT.Preterm
entity = DTT.Entity

event :: DTT.Preterm
event = DTT.Entity

state :: DTT.Preterm
state = DTT.Entity

--catS :: [FeatureValue] -> [FeatureValue] -> Feature -> Feature -> Feature -> Feature -> Feature -> Cat
--catS pos conj pm1 pm2 pm3 pm4 pm5 = S [F pos, F conj, pm1, pm2, pm3, pm4, pm5]

-- | A set of conjugation forms of JP verbs.
verb :: [FeatureValue]
verb = [V5k, V5s, V5t, V5n, V5m, V5r, V5w, V5g, V5z, V5b, V5IKU, V5YUK, V5ARU, V5NAS, V5TOW, V1, VK, VS, VSN, VZ, VURU]

-- | A set of conjugation forms of JP adjectives.
adjective :: [FeatureValue]
adjective = [Aauo, Ai, ANAS, ATII, ABES]

-- | A set of conjugation forms of JP nominal predicates.
nomPred :: [FeatureValue]
nomPred = [Nda, Nna, Nno, Nni, Nemp, Ntar]

-- | All conjugation forms, i.e. `verb` ++ `adjective` ++ `nomPred`
anyPos :: [FeatureValue]
anyPos = verb ++ adjective ++ nomPred

nonStem :: [FeatureValue]
nonStem = [Neg, Cont, Term, Attr, Hyp, Imper, Pre, NStem, VoR, VoS, VoE, NegL, TeForm]

modifiableS :: Cat
modifiableS = S [SF 2 anyPos, SF 3 nonStem, SF 4 [P,M],SF 5 [P,M],SF 6 [P,M],F[M],F[M]]

-- [F[M],F[M],F[M],F[M],F[M]]
m5 :: [Feature]
m5 = [F[M],F[M],F[M],F[M],F[M]]

pmmmm :: [Feature]
pmmmm = [F[P],F[M],F[M],F[M],F[M]]

mpmmm :: [Feature]
mpmmm = [F[M],F[P],F[M],F[M],F[M]]

mmpmm :: [Feature]
mmpmm = [F[M],F[M],F[P],F[M],F[M]]

mmmpm :: [Feature]
mmmpm = [F[M],F[M],F[M],F[P],F[M]]

mppmm :: [Feature]
mppmm = [F[M],F[P],F[P],F[M],F[M]]

--anyConj :: [ConjFeature]
--anyConj = [Stem, UStem, Neg, Cont, Term, Attr, Hyp, Imper, Pre, EuphT, EuphD, ModU, ModS, VoR, VoS, VoE, TeForm, NiForm, Yooni]

--anyCase :: [CaseFeature] 
--anyCase = [Nc, Ga, O, Ni, To, Niyotte, No]

{- Templates for Semantic Representation -}
-- | Lam x.x
id :: UDTT.Preterm
id = Lam (Var 0)

verbCat :: T.Text            -- ^ a case frame (e.g. "ガヲニ")
           -> [FeatureValue] -- ^ a part-of-speech feature value
           -> [FeatureValue] -- ^ a conjugational feature value
           -> Cat
verbCat caseframe posF conjF = verbCat' caseframe (defS posF conjF)

verbCat' :: T.Text         -- ^ a case frame (e.g. "ガヲニ")
           -> Cat          -- ^ a category to return (accumulated)
           -> Cat
verbCat' caseframe ct = case T.uncons caseframe of
  Just (c,cs) | c == 'ガ' -> verbCat' cs $ ct `BS` NP [F[Ga]]
              | c == 'ヲ' -> verbCat' cs $ ct `BS` NP [F[O]]
              | c == 'ニ' -> verbCat' cs $ ct `BS` NP [F[Ni]]
              | c == 'ト' -> verbCat' cs $ ct `BS` Sbar [F[ToCL]]
              | c == 'ヨ' -> verbCat' cs $ ct `BS` NP [F[Niyotte]]
              | otherwise -> verbCat' cs ct
  Nothing-> ct

-- | verbSR i op
-- i==1 -> S\NP:             \x.\c.(e:event)Xop(e,x)X(ce)
-- i==2 -> S\NP\NP:       \y.\x.\c.(e:event)X(op(e,x,y)X(ce)
-- i==3 -> S\NP\NP\NP: \z.\y.\x.\c.(e:event)X(op(e,x,y,z)X(ce)
-- ...
verbSR :: T.Text -> DTTpreterm -> T.Text -> (UDTTpreterm, Signature)
verbSR daihyo eventuality caseframe = 
  let predname = T.concat [daihyo, "/", caseframe] in
  (verbSR' predname caseframe caseframe, [(predname, nPlaceVerbType eventuality caseframe)])

verbSR' :: T.Text      -- ^ daihyo
          -> T.Text -- ^ A case frame
          -> T.Text -- ^ A case frame (save)
          -> UDTTpreterm
verbSR' daihyo caseframe caseframe2 = case T.uncons caseframe of
  --let n = ((fromIntegral $ T.length caseframe)::Int) + 1 in
  Just (_,cs) -> Lam (verbSR' daihyo cs caseframe2)
  Nothing -> (Lam (Sigma Entity (Sigma (App (argstcore 2 (Con daihyo) caseframe2) (Var 0)) (App (Var 2) (Var 1)))))

argstcore :: Int -> UDTT.Preterm -> T.Text -> UDTT.Preterm
argstcore n tm caseframe = 
  case T.uncons caseframe of
    Nothing -> tm
    Just (c,cs) | c == 'ト' -> App (argstcore (n+1) tm cs) (App (Var n) (Lam Top))
                | otherwise -> App (argstcore (n+1) tm cs) (Var n)

nPlaceVerbType :: DTT.Preterm    -- ^ Eventuarlity
                  -> T.Text        -- ^ A case frame
                  -> DTT.Preterm
nPlaceVerbType eventuality caseframe = case T.uncons caseframe of
  Nothing -> DTT.Pi eventuality DTT.Type
  Just (c,cs) | c == 'ガ' -> DTT.Pi DTT.Entity (nPlaceVerbType eventuality cs)
              | c == 'ヲ' -> DTT.Pi DTT.Entity (nPlaceVerbType eventuality cs)
              | c == 'ニ' -> DTT.Pi DTT.Entity (nPlaceVerbType eventuality cs)
              --  c == 'ト' -> Pi entity (nPlaceVerbType eventuality cs)
              | c == 'ト' -> DTT.Pi DTT.Type (nPlaceVerbType eventuality cs)
              | c == 'ヨ' -> DTT.Pi DTT.Entity (nPlaceVerbType eventuality cs)
              | otherwise -> nPlaceVerbType eventuality cs

nPlaceEventType :: Int -> DTT.Preterm
nPlaceEventType i
  | i < 0 = DTT.Con $ T.concat ["nPlaceEvent with: ", T.pack (show i)]
  | i == 0 = DTT.Pi DTT.Entity DTT.Type
  | otherwise = DTT.Pi DTT.Entity (nPlaceEventType (i-1))

nPlaceStateType :: Int -> DTT.Preterm
nPlaceStateType i
  | i < 0 = DTT.Con $ T.concat ["nPlaceState with: ", T.pack (show i)]
  | i == 0 = DTT.Pi DTT.Entity DTT.Type
  | otherwise = DTT.Pi DTT.Entity (nPlaceStateType (i-1))

nPlacePredType :: Int -> DTT.Preterm
nPlacePredType i
  | i < 0 = DTT.Con $ T.concat ["nPlacePred with: ", T.pack (show i)]
  | i == 0 = DTT.Type
  | otherwise = DTT.Pi DTT.Entity (nPlaceStateType (i-1))

-- | S\NP: \x.\c.(s:state)Xop(s,x)X(ce)
predSR :: Int -> T.Text -> (UDTTpreterm, Signature)
predSR i op | i == 1 = ((Lam (Lam (Sigma Entity (Sigma (App (App (Con op) (Var 2)) (Var 0)) (App (Var 2) (Var 1)))))), [(op, nPlacePredType 1)])
            | i == 2 = ((Lam (Lam (Lam (Sigma Entity (Sigma (App (App (App (Con op) (Var 3)) (Var 2)) (Var 0)) (App (Var 2) (Var 1))))))), [(op, nPlacePredType 2)])
            | otherwise = ((Con $ T.concat ["predSR: pred ",op," of ", T.pack (show i), " arguments"]), [])

-- | NP: 
properNameSR :: T.Text -> (UDTTpreterm, Signature)
properNameSR op = ((Lam (App (Var 0) (Con op))), [(op, DTT.Entity)])

-- | N: 
commonNounSR :: T.Text -> (UDTTpreterm, Signature)
commonNounSR op = ((Lam (Lam (Sigma Entity (Sigma (App (App (Con op) (Var 2)) (Var 0)) (App (Var 2) (Var 1)))))), [(op, nPlacePredType 1)])

-- | GQ:
generalizedQuantifierSR :: T.Text -> (UDTTpreterm, Signature)
generalizedQuantifierSR q = ((Lam (Lam (Lamvec ((App (App (Con q) (Lam (Appvec 1 (App (Var 2) (Var 0)))) ) (Lam (App (App (Var 3) (Var 0)) (Lam Top))) ))))),[(q, DTT.Pi (DTT.Pi DTT.Entity DTT.Type) (DTT.Pi (DTT.Pi DTT.Entity DTT.Type) DTT.Type))])

-- | S\S, S/S: \p.\c.op (pc)
modalSR :: T.Text -> (UDTTpreterm, Signature)
modalSR op = ((Lam (Lam (App (Con op) (App (Var 1) (Var 0))))), [(op, DTT.Pi DTT.Type DTT.Type)])

-- | N\N, N/N
--modifierSR :: T.Text -> (UDTTpreterm,Signature)
--modifierSR op = ((Lam (Lam (Lam (App (App (Var 2) (Var 1)) (Lam (Sigma (App (Con op) (Var 0)) (App (Var 2) (Var 1)))))))), [(op, nPlacePredType 1)])

-- | 
-- >>> S\NP\(S\NP):    \p.\x.\c.op(x,\z.(pz)c)
-- >>> S\NP\NP\(S\NP): \p.\y.\x.\c.op(x,\z.((py)z)c)
intensionalEvent :: Int -> T.Text -> (UDTTpreterm, Signature)
intensionalEvent i op | i == 1 = ((Lam (Lam (Lam (Sigma Entity (Sigma (App (App (App (Con op) (Lam (App (App (Var 4) (Var 0)) (Lam Top)))) (Var 2)) (Var 0)) (App (Var 2) (Var 1))))))), [(op,sg)])
                      | i == 2 = ((Lam (Lam (Lam (Lam (App (App (Con op) (Lam (App (App (Var 4) (Var 3)) (Var 1)))) (Var 1)))))), [(op,sg)])
                      | otherwise = (Con $ T.concat ["intensionalEvent: verb ",op," of ", T.pack (show i), " arguments"],[])
  where sg = DTT.Pi (DTT.Pi DTT.Entity DTT.Type) (DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type))

intensionalState :: Int -> T.Text -> (UDTTpreterm, Signature)
intensionalState i op | i == 1 = ((Lam (Lam (Lam (Sigma Entity (Sigma (App (App (App (Con op) (Lam (App (App (Var 4) (Var 0)) (Lam Top)))) (Var 2)) (Var 0)) (App (Var 2) (Var 1))))))), [(op,sg)])
                      | i == 2 = ((Lam (Lam (Lam (Lam (App (App (Con op) (Lam (App (App (Var 4) (Var 3)) (Var 1)))) (Var 1)))))), [(op,sg)])
                      | otherwise = (Con $ T.concat ["intensionalState: verb ",op," of ", T.pack (show i), " arguments"], [])
  where sg = DTT.Pi (DTT.Pi DTT.Entity DTT.Type) (DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type))

-- | T/T: \p.\v.\c.pv(\e.(op e) X ce)
mannerAdverb :: T.Text -> (UDTTpreterm, Signature)
mannerAdverb op = ((Lam (Lamvec (Lam (App (Appvec 1 (Var 2)) (Lam (Sigma (App (Con op) (Var 0)) (App (Var 3) (Var 0)))))))), [(op, DTT.Pi DTT.Entity DTT.Type)])

-- | N/N, N\N: \n.\x.\c. (nx (\s.(op x) × cs))
nominalModifier :: T.Text -> (UDTTpreterm, Signature)
nominalModifier op = ((Lam (Lam (Lam (App (App (Var 2) (Var 1)) (Lam (Sigma (App (Con op) (Var 2)) (App (Var 2) (Var 1)))))))), [(op, DTT.Pi DTT.Entity DTT.Type)])

-- | S\S: \p.\c.p(\e.(op e) X ce)
eventModifier :: T.Text -> (UDTTpreterm, Signature)
eventModifier op = ((Lam (Lam (App (Var 1) (Lam (Sigma (App (Con op) (Var 0)) (App (Var 2) (Var 1))))))), [(op, DTT.Pi DTT.Entity DTT.Type)])

-- | negation operator
-- S\S: \p.\c.not (pc)
negOperator :: (UDTTpreterm, Signature)
negOperator = ((Lam (Lam (not (App (Var 1) (Var 0))))), [])

-- | negation operator 2
-- S\NP\(S\NP): \p.\x.\c.not ((px)c)
negOperator2 :: (UDTTpreterm, Signature)
negOperator2 = ((Lam (Lam (Lam (not (App (App (Var 2) (Var 1)) (Var 0)))))), [])

-- | argument case marker
-- T/(T\NP[cm])\NP[nc]: \x.\p.px
argumentCM :: (UDTTpreterm, Signature)
argumentCM = ((Lam (Lam (App (Var 0) (Var 1)))), [])

-- | adjunct case marker
-- S1/S1\NP[nc]: \x.\p.\c.p (\e.op(e,x) X ce)
adjunctCM :: T.Text -> (UDTTpreterm, Signature)
adjunctCM c = ((Lam (Lam (Lam (App (Var 1) (Lam (Sigma (App (App (Con c) (Var 3)) (Var 0)) (App (Var 2) (Var 1)))))))), 
  [(c, nPlaceEventType 1)])

-- | adjunct nominal modifier
-- N/N\(T/T\NP[nc]): \p.\n.\x.\c.(nx (\s.p(\z.op(s,z)) X cs)
adjunctNM :: T.Text -> (UDTTpreterm, Signature)
adjunctNM c = ((Lam (Lam (Lam (Lam (Lamvec (App (App (Var 3) (Var 2)) (Lam (Sigma (Appvec 1 (App (Var 5) (Lam (App (App (Con c) (Var 0)) (Var 1))))) (App (Var 3) (Var 1)))))))))), 
  [(c, nPlaceStateType 1)])

andSR :: (UDTTpreterm, Signature)
andSR = ((Lam (Lam (Sigma (Var 1) (Var 1)))), [])

orSR :: (UDTTpreterm, Signature)
orSR = ((Lam (Lam (Pi (not (Var 1)) (Var 1)))), [])

conjunctionSR :: T.Text -> (UDTTpreterm, Signature)
conjunctionSR _ = ((Lam (Lam (Lam (Sigma (App (Var 2) (Lam Top)) (App (Var 2) (Var 1)))))), [])
  -- ((Lam (Lam (Lam (Sigma (App (Var 2) (Lam Top)) (Sigma (App (Var 2) (Var 1)) ((App (App (Con c) (Var 1)) (Var 0)))))))), 
  --[(c, DTT.Pi DTT.Entity (DTT.Pi DTT.Entity DTT.Type))])
