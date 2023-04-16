{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Prelude hiding (id)
import qualified Data.Text.Lazy as T -- text
import Data.Ratio
import Parser.CCG
import DTS.UDTT

{- Some Macros for defining lexical items -}

-- | defines a lexical item.
lexicalitem :: T.Text                    -- ^ A phonetic form
               -> T.Text                 -- ^ A source of a lexical item, such as a number in the CCG textbook.
               -> Integer                -- ^ A score (0 to 100)
               -> Cat                    -- ^ A syntactic category
               -> (Preterm, Signature) -- ^ A semantic representation (in DTS) and a list of signatures
               -> Node
lexicalitem pf' source' score' cat' (sem',sig') = Node {rs=LEX, pf=pf', cat=cat', sem=sem', daughters=[], score=(score' % 100), source=source', sig=sig'}

{- Some Marcos for CCG categories/features -}

-- | Category S with the default feature setting (mainly for stems).
defS :: [FeatureValue] -> [FeatureValue] -> Cat
defS p c = S [F p,F c,F[M],F[M],F[M],F[M],F[M]]

entity :: Preterm
entity = Con "entity"

event :: Preterm
event = Con "evt"

state :: Preterm
state = Con "evt"

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
id :: Preterm
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
verbSR :: T.Text -> Preterm -> T.Text -> (Preterm, Signature)
verbSR daihyo eventuality caseframe = 
  let predname = T.concat [daihyo, "/", caseframe] in
  (verbSR' predname caseframe caseframe, [(predname, nPlaceVerbType eventuality caseframe)])

verbSR' :: T.Text      -- ^ daihyo
          -> T.Text -- ^ A case frame
          -> T.Text -- ^ A case frame (save)
          -> Preterm
verbSR' daihyo caseframe caseframe2 = case T.uncons caseframe of
  --let n = ((fromIntegral $ T.length caseframe)::Int) + 1 in
  Just (_,cs) -> Lam (verbSR' daihyo cs caseframe2)
  Nothing -> (Lam (Sigma event (Sigma (App (argstcore 2 (Con daihyo) caseframe2) (Var 0)) (App (Var 2) (Var 1)))))

argstcore :: Int -> Preterm -> T.Text -> Preterm
argstcore n tm caseframe = 
  case T.uncons caseframe of
    Nothing -> tm
    Just (c,cs) | c == 'ト' -> App (argstcore (n+1) tm cs) (App (Var n) (Lam Top))
                | otherwise -> App (argstcore (n+1) tm cs) (Var n)

nPlaceVerbType :: Preterm     -- ^ Eventuarlity
                  -> T.Text -- ^ A case frame
                  -> Preterm
nPlaceVerbType eventuality caseframe = case T.uncons caseframe of
  Nothing -> Pi eventuality Type
  Just (c,cs) | c == 'ガ' -> Pi entity (nPlaceVerbType eventuality cs)
              | c == 'ヲ' -> Pi entity (nPlaceVerbType eventuality cs)
              | c == 'ニ' -> Pi entity (nPlaceVerbType eventuality cs)
              --  c == 'ト' -> Pi entity (nPlaceVerbType eventuality cs)
              | c == 'ト' -> Pi Type (nPlaceVerbType eventuality cs)
              | c == 'ヨ' -> Pi entity (nPlaceVerbType eventuality cs)
              | otherwise -> nPlaceVerbType eventuality cs

nPlaceEventType :: Int -> Preterm
nPlaceEventType i
  | i < 0 = Con $ T.concat ["nPlaceEvent with: ", T.pack (show i)]
  | i == 0 = Pi event Type
  | otherwise = Pi entity (nPlaceEventType (i-1))

nPlaceStateType :: Int -> Preterm
nPlaceStateType i
  | i < 0 = Con $ T.concat ["nPlaceState with: ", T.pack (show i)]
  | i == 0 = Pi state Type
  | otherwise = Pi entity (nPlaceStateType (i-1))

nPlacePredType :: Int -> Preterm
nPlacePredType i
  | i < 0 = Con $ T.concat ["nPlacePred with: ", T.pack (show i)]
  | i == 0 = Type
  | otherwise = Pi entity (nPlaceStateType (i-1))

-- | S\NP: \x.\c.(s:state)Xop(s,x)X(ce)
predSR :: Int -> T.Text -> (Preterm,Signature)
predSR i op | i == 1 = ((Lam (Lam (Sigma state (Sigma (App (App (Con op) (Var 2)) (Var 0)) (App (Var 2) (Var 1)))))), [(op, nPlacePredType 1)])
            | i == 2 = ((Lam (Lam (Lam (Sigma state (Sigma (App (App (App (Con op) (Var 3)) (Var 2)) (Var 0)) (App (Var 2) (Var 1))))))), [(op, nPlacePredType 2)])
            | otherwise = ((Con $ T.concat ["predSR: pred ",op," of ", T.pack (show i), " arguments"]), [])

-- | NP: 
properNameSR :: T.Text -> (Preterm,Signature)
properNameSR op = ((Lam (App (Var 0) (Con op))), [(op, entity)])

-- | N: 
commonNounSR :: T.Text -> (Preterm,Signature)
commonNounSR op = ((Lam (Lam (Sigma state (Sigma (App (App (Con op) (Var 2)) (Var 0)) (App (Var 2) (Var 1)))))), [(op, nPlacePredType 1)])

-- | GQ:
generalizedQuantifierSR :: T.Text -> (Preterm,Signature)
generalizedQuantifierSR q = ((Lam (Lam (Lamvec ((App (App (Con q) (Lam (Appvec 1 (App (Var 2) (Var 0)))) ) (Lam (App (App (Var 3) (Var 0)) (Lam Top))) ))))),[(q,Pi (Pi entity Type) (Pi (Pi entity Type) Type))])

-- | S\S, S/S: \p.\c.op (pc)
modalSR :: T.Text -> (Preterm,Signature)
modalSR op = ((Lam (Lam (App (Con op) (App (Var 1) (Var 0))))), [(op, Pi Type Type)])

-- | N\N, N/N
--modifierSR :: T.Text -> (Preterm,Signature)
--modifierSR op = ((Lam (Lam (Lam (App (App (Var 2) (Var 1)) (Lam (Sigma (App (Con op) (Var 0)) (App (Var 2) (Var 1)))))))), [(op, nPlacePredType 1)])

-- | 
-- >>> S\NP\(S\NP):    \p.\x.\c.op(x,\z.(pz)c)
-- >>> S\NP\NP\(S\NP): \p.\y.\x.\c.op(x,\z.((py)z)c)
intensionalEvent :: Int -> T.Text -> (Preterm,Signature)
intensionalEvent i op | i == 1 = ((Lam (Lam (Lam (Sigma event (Sigma (App (App (App (Con op) (Lam (App (App (Var 4) (Var 0)) (Lam Top)))) (Var 2)) (Var 0)) (App (Var 2) (Var 1))))))), [(op,sg)])
                      | i == 2 = ((Lam (Lam (Lam (Lam (App (App (Con op) (Lam (App (App (Var 4) (Var 3)) (Var 1)))) (Var 1)))))), [(op,sg)])
                      | otherwise = (Con $ T.concat ["intensionalEvent: verb ",op," of ", T.pack (show i), " arguments"],[])
  where sg = Pi (Pi entity Type) (Pi entity (Pi event Type))

intensionalState :: Int -> T.Text -> (Preterm,Signature)
intensionalState i op | i == 1 = ((Lam (Lam (Lam (Sigma state (Sigma (App (App (App (Con op) (Lam (App (App (Var 4) (Var 0)) (Lam Top)))) (Var 2)) (Var 0)) (App (Var 2) (Var 1))))))), [(op,sg)])
                      | i == 2 = ((Lam (Lam (Lam (Lam (App (App (Con op) (Lam (App (App (Var 4) (Var 3)) (Var 1)))) (Var 1)))))), [(op,sg)])
                      | otherwise = (Con $ T.concat ["intensionalState: verb ",op," of ", T.pack (show i), " arguments"], [])
  where sg = Pi (Pi entity Type) (Pi entity (Pi state Type))

-- | T/T: \p.\v.\c.pv(\e.(op e) X ce)
mannerAdverb :: T.Text -> (Preterm,Signature)
mannerAdverb op = ((Lam (Lamvec (Lam (App (Appvec 1 (Var 2)) (Lam (Sigma (App (Con op) (Var 0)) (App (Var 3) (Var 0)))))))), [(op, Pi entity Type)])

-- | N/N, N\N: \n.\x.\c. (nx (\s.(op x) × cs))
nominalModifier :: T.Text -> (Preterm,Signature)
nominalModifier op = ((Lam (Lam (Lam (App (App (Var 2) (Var 1)) (Lam (Sigma (App (Con op) (Var 0)) (App (Var 2) (Var 1)))))))), [(op, Pi state Type)])

-- | S\S: \p.\c.p(\e.(op e) X ce)
eventModifier :: T.Text -> (Preterm,Signature)
eventModifier op = ((Lam (Lam (App (Var 1) (Lam (Sigma (App (Con op) (Var 0)) (App (Var 2) (Var 1))))))), [(op, Pi event Type)])

-- | negation operator
-- S\S: \p.\c.not (pc)
negOperator :: (Preterm, Signature)
negOperator = ((Lam (Lam (Not (App (Var 1) (Var 0))))), [])

-- | negation operator 2
-- S\NP\(S\NP): \p.\x.\c.not ((px)c)
negOperator2 :: (Preterm, Signature)
negOperator2 = ((Lam (Lam (Lam (Not (App (App (Var 2) (Var 1)) (Var 0)))))), [])

-- | argument case marker
-- T/(T\NP[cm])\NP[nc]: \x.\p.px
argumentCM :: (Preterm, Signature)
argumentCM = ((Lam (Lam (App (Var 0) (Var 1)))), [])

-- | adjunct case marker
-- S1/S1\NP[nc]: \x.\p.\c.p (\e.op(e,x) X ce)
adjunctCM :: T.Text -> (Preterm, Signature)
adjunctCM c = ((Lam (Lam (Lam (App (Var 1) (Lam (Sigma (App (App (Con c) (Var 3)) (Var 0)) (App (Var 2) (Var 1)))))))), [(c, nPlaceEventType 1)])

-- | adjunct nominal modifier
-- N/N\(T/T\NP[nc]): \p.\n.\x.\c.(nx (\s.p(\z.op(s,z)) X cs)
adjunctNM :: T.Text -> (Preterm, Signature)
adjunctNM c = ((Lam (Lam (Lam (Lam (Lamvec (App (App (Var 3) (Var 2)) (Lam (Sigma (Appvec 1 (App (Var 5) (Lam (App (App (Con c) (Var 0)) (Var 1))))) (App (Var 3) (Var 1)))))))))), [(c, nPlaceStateType 1)])

andSR :: (Preterm, Signature)
andSR = ((Lam (Lam (Sigma (Var 1) (Var 1)))), [])

orSR :: (Preterm, Signature)
orSR = ((Lam (Lam (Pi (Not (Var 1)) (Var 1)))), [])

conjunctionSR :: T.Text -> (Preterm, Signature)
conjunctionSR c = ((Lam (Lam (Lam (Sigma (App (Var 2) (Lam Top)) (Sigma (App (Var 2) (Var 1)) ((App (App (Con c) (Var 1)) (Var 0)))))))), [(c, Pi entity (Pi entity Type))])
