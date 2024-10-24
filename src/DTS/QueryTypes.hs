{-# LANGUAGE DuplicateRecordFields, TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}

module DTS.QueryTypes (
  -- * Type system of UDTT
  DTTrule(..)
  -- * type query
  , DTTProofDiagram
  , TypeChecker
  , TypeInfer
  , LogicSystem(..)
  , ProofSearchSetting(..)
  , Prover
  -- * ListEx monad
  --, ListEx(..)
  --, exception
  --, record
  ) where

--import Data.Bifunctor (second)       --base
import ListT (ListT)                      --list-t
import qualified Data.Text.Lazy as T --text
import Interface.Text
import Interface.TeX
import Interface.HTML
import Interface.Tree
import qualified DTS.DTTdeBruijn as DTTdB
--import qualified DTS.DTTwithName as DTTwN
import qualified DTS.UDTTdeBruijn as UDTTdB
--import qualified DTS.UDTTwithName as UDTTwN
import DTS.GeneralTypeQuery (GeneralTypeQuery(..))

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


-- | Type checking in DTT

type DTTProofDiagram = Tree DTTrule (DTTdB.Judgment)

type TypeChecker = Prover -> UDTTdB.TypeCheckQuery -> ListT IO DTTProofDiagram

type TypeInfer = Prover -> UDTTdB.TypeInferQuery -> ListT IO DTTProofDiagram

data LogicSystem = Intuitionistic | Classical deriving (Eq, Show)

data ProofSearchSetting = ProofSearchSetting {
  maxDepth :: Maybe Int
  , maxTime :: Maybe Int
  , logicSystem :: Maybe LogicSystem
  } deriving (Eq, Show)

type Prover = ProofSearchSetting -> DTTdB.ProofSearchQuery -> ListT IO DTTProofDiagram


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

{-
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
-}
