module DTS.Alligator.Prover
(
  prove
) where

import qualified DTS.DTT as DT            -- DTT
import qualified Data.Text.Lazy as T      -- text
import qualified Data.List as L           -- base
import qualified Control.Applicative as M -- base
import qualified Control.Monad as M       -- base
import qualified DTS.UDTTwithName as VN
import Data.Time　as Ti
import System.Timeout

-- TEnv : DTTの型環境の型
-- | haddock
type TEnv = [DT.Preterm]

-- -- Judgement : DTTのジャッジメントの定義
-- data Judgement =
--   Judgement TEnv DT.Preterm DT.Preterm
--     deriving (Eq, Show)

type Tterm_Ttype = (DT.Preterm,DT.Preterm)

getTterm :: Tterm_Ttype -> DT.Preterm
getTterm = fst

getTtype :: Tterm_Ttype -> DT.Preterm
getTtype = snd

-- | Preterms of Underspecified Dependent Type Theory (DTT).
data Pseudo_type =
  Prop |
  Set
  deriving (Eq)

data Pseudo_sort =
  Type_prop |
  Type_set
  deriving (Eq)

data Selector = Fst | Snd deriving (Eq, Show)

data Pseudo_term =
  Pseudo_sort |
  Pseudo_type |
  Var Int |               -- ^ Variables
  Con T.Text |            -- ^ Constant symbols
  App Pseudo_term Pseudo_term |   -- ^ Function Applications
  Pair Pseudo_term Pseudo_term |  -- ^ Pairs
  Proj Selector Pseudo_term | -- ^ (First and second) Projections
  Lam Pseudo_term Pseudo_term | --l921~         -- ^ Lambda abstractions
  Pi Pseudo_term Pseudo_term |    -- ^ Dependent function types (or Pi types)
  Sigma Pseudo_term Pseudo_term  -- ^ Dependent product types (or Sigma types)
  deriving (Eq)

fromDTT :: DT.Preterm -> Pseudo_term
fromDTT (DT.Var i) = Var i
fromDTT (DT.Con t) = Con t
fromDTT DT.Type = undefined
fromDTT DT.Kind = undefined
fromDTT (DT.Pi term1 term2) = undefined
fromDTT (DT.Not term) = undefined
fromDTT (DT.Lam term) = undefined
fromDTT (DT.App term1 term2) = undefined
fromDTT (DT.Sigma term1 term2) = undefined
fromDTT (DT.Pair term1 term2) = undefined
fromDTT (DT.Proj selector term1) = undefined
fromDTT DT.Unit = undefined
fromDTT DT.Top = undefined
fromDTT DT.Bot = undefined
fromDTT DT.Nat = undefined
fromDTT DT.Zero = undefined
fromDTT (DT.Succ term) = undefined
fromDTT (DT.Natrec term1 term2 term3) = undefined
fromDTT (DT.Eq term1 term2 term3) = undefined
fromDTT (DT.Refl term1 term2) = undefined
fromDTT (DT.Idpeel term1 term2) = undefined
fromDTT (DT.DRel i t term1 term2) = undefined

toDTTselector :: Selector -> DT.Selector
toDTTselector Fst = DT.Fst
toDTTselector Snd = DT.Snd

toDTT :: Pseudo_term -> DT.Preterm
toDTT Pseudo_sort = DT.Kind
toDTT Pseudo_type = DT.Type
toDTT ( Var i) = DT.Var i
toDTT ( Con t) = DT.Con t
toDTT ( App term1 term2) = DT.App (toDTT term1) (toDTT term2)
toDTT ( Pair term1 term2 )= DT.Pair (toDTT term1) (toDTT term2)
toDTT ( Proj selector term) = DT.Proj (toDTTselector selector) (toDTT term)
toDTT ( Lam term1 term2) = undefined
toDTT ( Pi term1 term2 )= undefined
toDTT ( Sigma term1 term2) = undefined

data Arrowterm =
  Conclusion DT.Preterm |
  Arrow [DT.Preterm] Arrowterm
    deriving (Show)

type AEnv = [Arrowterm]

dne =  (DT.Pi (DT.Con (T.pack "Prop")) (DT.Pi (DT.Pi (DT.Pi (DT.Var 0)  DT.Bot) (DT.Bot)) (DT.Var 1)))
efq = DT.Pi (DT.Con (T.pack "Prop")) (DT.Pi DT.Bot (DT.Var 1))

classic = [dne,efq]



prove ::  TEnv -> DT.Preterm -> DT.Preterm
prove input_env preterm = undefined

arrow_notat :: DT.Preterm -> Arrowterm
--入力にArrowtermがあることはないとする
arrow_notat ( DT.Var i )= Conclusion $ DT.Var i
arrow_notat ( DT.Con i )= Conclusion $ DT.Con i
--arrow_notat Arrowterm context preterm =
arrow_notat preterm = undefined

arrow_segment_notat :: [Tterm_Ttype] -> AEnv
arrow_segment_notat = map ( arrow_notat . getTtype)
{--
 arrow_segment_notat ::

arrow_segment_notat([],[]):-!.

arrow_segment_notat([A:B1|T1],[A:B2|T2]):-
  arrow_notat(B1,B2),
  !,
  arrow_segment_notat(T1,T2).
-}

forward :: Arrowterm -> AEnv
forward arrowterm = undefined

deduce :: AEnv->Arrowterm->Int->DT.Preterm
deduce = undefined
