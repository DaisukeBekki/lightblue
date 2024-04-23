{-# LANGUAGE DuplicateRecordFields, TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}

module DTS.QueryTypes (
  -- * Type system of UDTT
  DTTrule(..)
  -- * ListEx monad
  , ListEx(..)
  , exception
  , record
  -- * UDTT type check
  , TypeCheckQuery(..)
  , TypeCheckResult(..)
  , TypeChecker
  , TypeInferQuery(..)
  , TypeInfer
  -- * DTT proof search
  , LogicSystem(..)
  , ProofSearchSetting(..)
  , ProofSearchQuery(..)
  , ProofSearchResult(..)
  , Prover
  ) where

import Data.Bifunctor (second)       --base
import qualified Control.Applicative as M --base
import qualified Control.Monad as M       --base
import qualified Data.Text.Lazy as T --text
import Interface.Text
import Interface.TeX
import Interface.HTML
import Interface.Tree
import qualified DTS.UDTTdeBruijn as U
import qualified DTS.UDTTvarName as VN
import DTS.Labels (UDTT,DTT)

-- BOTF?
data DTTrule = Var | Con | TypeF | Conv | WK | PiF | PiI | PiE | SigmaF | SigmaI | SigmaE | DisjF | DisjI | DisjE | EnumF | EnumI | EnumE | IqF | IqI | IqE | NatF | NatI | NatE deriving (Eq, Show, Read)

instance SimpleText DTTrule where
  toText = T.pack . show
instance Typeset DTTrule where
  toTeX = T.pack . show
instance MathML DTTrule where
  toMathML rule = T.concat [
    "<mi>",
    case rule of
      PiF -> "ΠF"
      PiI -> "ΠI"
      PiE -> "ΠE"
      SigmaF -> "ΣF"
      SigmaI -> "ΣI"
      SigmaE -> "ΣE"
      DisjF -> "+F"
      DisjI -> "+I"
      DisjE -> "+E"
      EnumF -> "{}F"
      EnumI -> "{}I"
      EnumE -> "{}E"
      IqF -> "=F"
      IqI -> "=I"
      IqE -> "=E"
      NatF -> "NatF"
      NatI -> "NatI"
      NatE -> "NatE"
      _ -> T.pack $ show rule,
    "</mi>"
    ]

-- | ListEx Monad (List with exceptions)

newtype ListEx a = ListEx { result :: [(a, T.Text)] }

instance (Show a) => Show (ListEx a) where
  show ex = show $ result ex 

instance Functor ListEx where
  fmap f m1 = do -- M.liftM
              x1 <- m1
              return $ f x1

instance Applicative ListEx where
  pure = return
  (<*>) m1 m2 = do -- M.ap
                x1 <- m1
                x2 <- m2
                return $ x1 x2

instance Monad ListEx where
  return m = ListEx [(m,"")]
  ListEx xs >>= f = ListEx $ do
      (a,msg) <- xs
      map (\(b,msg') -> (b,T.append msg msg')) $ result $ f a -- [(b, msg)]

instance M.MonadFail ListEx where
  fail s = ListEx []

instance M.Alternative ListEx where
  empty = ListEx []
  ListEx as <|> ListEx bs = ListEx (as ++ bs)

--instance M.Traversal ListEx where
--  traverse = 

exception :: T.Text -> ListEx a
exception msg = ListEx [((),msg)]

record :: T.Text -> ListEx ()
record msg = ListEx [(unit,msg)]

-- | Type checking in DTT

data TypeCheckQuery = TypeCheckQuery {
  sig :: U.Signature
  , ctx :: U.Context
  , trm :: U.Preterm UDTT
  , typ :: U.Preterm DTT
  } deriving (Eq, Show)

instance SimpleText TypeCheckQuery where
  toText (TypeCheckQuery _ ctx trm typ) = T.concat [toText ctx, " |- ", toText trm, " : ", toText typ]

type TypeCheckResult = Tree DTTrule (U.Judgment DTT)

type TypeChecker = Prover -> TypeCheckQuery -> ListEx TypeCheckResult

data TypeInferQuery = TypeInferQuery {
  sig :: U.Signature
  , ctx :: U.Context
  , trm :: U.Preterm UDTT
  } deriving (Eq, Show)

instance SimpleText TypeInferQuery where
  toText (TypeInferQuery _ ctx trm) = T.concat [toText ctx, " |- ", toText trm, " : ?"]

type TypeInfer = Prover -> TypeInferQuery -> ListEx TypeCheckResult

-- | Proof Search in DTT

data LogicSystem = Intuitionistic | Classical deriving (Eq, Show)

data ProofSearchSetting = ProofSearchSetting {
  maxDepth :: Maybe Int
  , maxTime :: Maybe Int
  , system :: Maybe LogicSystem
  } deriving (Eq, Show)

data ProofSearchQuery = ProofSearchQuery {
  sig :: U.Signature
  , ctx :: U.Context
  , typ :: U.Preterm DTT
  } deriving (Eq, Show)

type ProofSearchResult = [Tree DTTrule (U.Judgment DTT)]

type Prover = ProofSearchSetting -> ProofSearchQuery -> ProofSearchResult

instance MathML ProofSearchQuery where
  toMathML ProofSearchQuery{..} = 
    let (vcontext', vtyp') = U.initializeIndex $ do
                             vcontext <- U.fromDeBruijnContextLoop [] $ reverse ctx
                             let varnames = fst $ unzip vcontext
                             vtyp <- U.fromDeBruijnLoop varnames typ
                             return (reverse vcontext, vtyp)
        sig' = U.fromDeBruijnSignature sig
    in T.concat ["<mrow>", toMathML vcontext', "<mo>&vdash;</mo><mo>?</mo><mo>:</mo><mstyle mathcolor='blue' mathbackground='white'>", toMathML vtyp', "</mstyle></mrow>"]

instance MathML ProofSearchResult where
  toMathML results =
    let n = length results in
    T.concat $ map (\(i,diagram) ->
      T.concat [
        "<mrow>",
        T.pack $ show i,
        "th result out of ",
        T.pack $ show n,
        "</mrow><mrow>",
        toMathML $ second U.fromDeBruijnJudgment diagram,
        "</mrow>"
        ]) $ zip [1..] results

{-
data TypeCheckError = IndexOutOfBounds

instance Show TypeCheckError where
  show IndexOutOfBounds = "Index out of bounds"

instance SimpleText TypeCheckError where
  toText = T.pack . show
instance Typeset TypeCheckError where
  toTeX = T.pack . show
instance MathML TypeCheckError where
  toMathML = T.pack . show
-}

{-
instance MathML ProofSearchQuery where
  toMathML (ProofSearchQuery _ cont typ) = -- | prints a proof search query in MathML
    let (vcontext', vtyp') = UDTT.initializeIndex $ do
                               (varnames,vcontext) <- UDTT.fromDeBruijnContextLoop $ DTT.toUDTTcontext cont
                               vtyp <- UDTT.fromDeBruijn varnames $ DTT.toUDTT typ
                               return (vcontext, vtyp)
    in T.concat ["<mrow>", toMathML vcontext', "<mo>&vdash;</mo><mo>?</mo><mo>:</mo>", toMathML vtyp', "</mrow>"]
-}

--data ProofSearchResult = Diagrams [Tree RuleName Judgment] deriving (Eq, Show)

{-
signatureChecker :: TypeChecker -> DTT.Signature -> Bool
signatureChecker = signatureCheckerLoop []
  where signatureCheckerLoop _ _ [] = True
        signatureCheckerLoop typeChecker prevSig (sig:sigs) =
          case typeChecker of
            [] -> False
            (Tree rulename node dtrs):_ -> True
          &&
          signatureCheckerLoop typeChecker (new:prevSig) sigs

contextChecker :: Context -> Bool
contextChecker = contextCheckerLoop []
  where contextCheckerLoop _ [] = True
        contextCheckerLoop prevCon (con:cons) =
-}
