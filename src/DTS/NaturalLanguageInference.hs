{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}

{-|
Module      : DTS.NaturalLanguageInference
Copyright   : Daisuke Bekki
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

A module for Natural Language Inference 
-}

module DTS.NaturalLanguageInference (
  InferenceSetting(..)
  --, InferencePair(..)
  --, InferenceResult(..)
  , ProverName(..)
  , getProver
  , ParseResult(..)
  , ParseTreeAndFelicityChecks(..)
  , QueryAndDiagrams(..)
  , sequentialParsing
  , parseWithTypeCheck
  , trawlParseResult
  , ParseResultExpress(..)
  , ParseTreeAndFelicityCheckExpress(..)
  , NodeSelector
  , DiagramSelector
  , sequentialTypeCheckExpress
  ) where

import Control.Monad (join)    --base
import Control.Monad.State (lift)         --mtl
import Control.Monad.IO.Class (liftIO)    --base
import Control.Applicative ((<|>))        --base
import Control.Parallel (par,pseq)        --base
import qualified System.IO as S           --base
import qualified Data.Char as C           --base
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.List as L           --base
import ListT (ListT(..),fromFoldable,toReverseList,take,null,uncons,cons) --list-t
import qualified Parser.ChartParser as CP      --lightblue
import qualified Parser.PartialParsing as Partial --lightblue
import qualified Parser.CCG as CCG             --lightblue
import Interface.Tree as Tree                  --lightblue
--import Parser.Language (LangOptions(..),jpOptions)
import qualified DTS.UDTTdeBruijn as UDTT      --lightblue
import qualified DTS.DTTdeBruijn as DTT        --lightblue
import qualified DTS.QueryTypes as QT          --lightblue
import qualified DTS.TypeChecker as TY         --lightblue
import qualified DTS.Prover.Wani.Prove as Wani --lightblue
import qualified JSeM as JSeM                  --jsem

data InferenceSetting = InferenceSetting {
  beam :: Int     -- ^ beam width
  , maxDepth :: Maybe Int -- ^ max depth for prover
  , maxTime :: Maybe Int  -- ^ max time for prover
  , parseSetting :: CP.ParseSetting
  , typeChecker :: QT.TypeChecker
  , proverName :: ProverName
  } 

type InferenceLabel = JSeM.YesNo
--data InferenceLabel = YES | NO | UNK deriving (Eq, Show, Read)

-- data InferencePair = InferencePair {
--   premises :: [T.Text]   -- ^ premises
--   , hypothesis :: T.Text -- ^ a hypothesis
--   } deriving (Eq, Show)

--data InferenceResult = InferenceResult (InferencePair, [CCG.Node], [UDTT.Preterm], DTT.Signature, [Tree QT.DTTrule DTT.Judgment]) --, QT.ProofSearchQuery, QT.ProofSearchResult)) 

data ProverName = Wani | Null deriving (Eq,Show)

instance Read ProverName where
  readsPrec _ r =
    [(Wani,s) | (x,s) <- lex r, map C.toLower x == "wani"]
    ++ [(Null,s) | (x,s) <- lex r, map C.toLower x == "null"]
    -- ++ [(Diag,s) | (x,s) <- lex r, map C.toLower x == "diag"]
    -- ++ [(Coq,s) | (x,s) <- lex r, map C.toLower x == "coq"]

getProver :: ProverName -> QT.ProverBuilder
getProver pn = case pn of
  Wani -> Wani.prove'
  Null -> TY.nullProver

{-- Data structure for sequential parsing and the inference --} 

data ParseResult = 
  SentenceAndParseTrees T.Text (ListT IO ParseTreeAndFelicityChecks) -- ^ A next sentence and its parse results
  | InferenceResults QueryAndDiagrams QueryAndDiagrams 
  | NoSentence 
data ParseTreeAndFelicityChecks = 
  ParseTreeAndFelicityChecks CCG.Node DTT.Signature UDTT.TypeCheckQuery (ListT IO (QT.DTTProofDiagram, ParseResult)) 
  -- ^ A parse result, type check query for its felicity condition, and its results
  -- ^ A type check diagram and the next sentence if this is not the last sentence, or an inference query otherwise.
data QueryAndDiagrams = 
  QueryAndDiagrams DTT.ProofSearchQuery (ListT IO QT.DTTProofDiagram) 
  -- ^ A proof search query for the inference and its results.

{-- Data structure of Express for sequential parsing and the inference --}
data ParseResultExpress =
    SentenceAndParseTreeExpress T.Text ParseTreeAndFelicityCheckExpress
  | InferenceResultsExpress QueryAndDiagrams QueryAndDiagrams
  | NoSentenceExpress

data ParseTreeAndFelicityCheckExpress =
  ParseTreeAndFelicityCheckExpress CCG.Node DTT.Signature UDTT.TypeCheckQuery (IO (QT.DTTProofDiagram, ParseResultExpress))

type NodeSelector = ListT IO CCG.Node -> IO (Maybe CCG.Node)
type DiagramSelector = UDTT.TypeCheckQuery -> IO (Maybe QT.DTTProofDiagram)

-- | Parse sequential texts, and check their semantic felicity condition.
-- | If noInference = True, it does not execute inference.
-- | The specification of this function reflects a view about what are entailments between texts,          
-- | that is an interface problem between natural language semantics and logic
parseWithTypeCheck :: CP.ParseSetting -> QT.Prover -> DTT.Signature -> DTT.Context -> [T.Text] -> ParseResult
parseWithTypeCheck ps prover signtr contxt txts =
  let nodes = sequentialParsing ps txts
  in sequentialTypeCheck ps prover signtr contxt nodes
  
type Discourse = [(T.Text, ListT IO CCG.Node)]

sequentialParsing :: CP.ParseSetting -> [T.Text] -> Discourse
sequentialParsing ps txts = 
  parallelFor txts $ \txt -> (txt, takeNbest (CP.nParse ps) $ join $ fmap fromFoldable $ lift $ Partial.simpleParse ps txt)
    -- | T.Text =simpleParse=>    IO [CCG.Node]
    -- |        =lift=>           ListT IO [CCG.node] 
    -- |        =fmap(foldable)=> ListT IO (ListT IO CCG.Node)
    -- |        =join=>           ListT IO CCG.Node
    -- |        =takeNbest Int => ListT IO CCG.Node

sequentialTypeCheck :: CP.ParseSetting -> QT.Prover -> DTT.Signature -> DTT.Context -> Discourse -> ParseResult
sequentialTypeCheck _ _ _ [] [] = NoSentence     -- ^ Context is empty and no sentece is given 
sequentialTypeCheck ps prover signtr (typ:contxt) [] = -- ^ Context is given and no more sentence (= All parse done)
  if CP.noInference ps
    then NoSentence
    else let psqPos = DTT.ProofSearchQuery signtr contxt $ typ 
             resultPos = takeNbest (CP.nProof ps) $ prover psqPos
             psqNeg = DTT.ProofSearchQuery signtr contxt $ DTT.Pi typ DTT.Bot
             resultNeg = takeNbest (CP.nProof ps) $ prover psqNeg
         in InferenceResults (QueryAndDiagrams psqPos resultPos) (QueryAndDiagrams psqNeg resultNeg)
sequentialTypeCheck ps prover signtr contxt ((text,nodes):rests) = 
  SentenceAndParseTrees text $ 
    parallelM nodes $ \node -> 
         let signtr' = L.nub $ (CCG.sig node) ++ signtr
             tcQueryType = UDTT.Judgment signtr' contxt (CCG.sem node) DTT.Type
             tcQueryKind = UDTT.Judgment signtr' contxt (CCG.sem node) DTT.Kind
         in ParseTreeAndFelicityChecks node signtr' tcQueryType $ 
              let tcDiagrams = takeNbest (CP.nTypeCheck ps) $ (TY.typeCheck prover (CP.verbose ps) tcQueryType)
                                                              <|> (TY.typeCheck prover (CP.verbose ps) tcQueryKind)
              in parallelM tcDiagrams $ \tcDiagram -> 
                   let contxt' = (DTT.trm $ Tree.node tcDiagram):contxt
                   in (tcDiagram, sequentialTypeCheck ps prover signtr' contxt' rests)

-- | Take n element from the top of the list.
-- | If n < 0, it returns all the elements.
takeNbest :: Int -> ListT IO a -> ListT IO a
takeNbest n l
  | n >= 0 = ListT.take n l
  | otherwise = l
 
{-- Trawling functions --}

trawlParseResult :: ParseResult -> ListT IO InferenceLabel
trawlParseResult (SentenceAndParseTrees _ parseTreeAndFelicityChecks) = do
  (ParseTreeAndFelicityChecks _ _ _ felicityCheckAndMores) <- parseTreeAndFelicityChecks 
  (_, parseResult) <- felicityCheckAndMores
  label <- trawlParseResult parseResult
  return label
trawlParseResult (InferenceResults (QueryAndDiagrams _ resultPos) (QueryAndDiagrams _ resultNeg)) = do
  ifYes <- liftIO $ ListT.null resultPos
  ifNo  <- liftIO $ ListT.null resultNeg
  return $ case () of
             _ | not ifYes -> JSeM.Yes
               | not ifNo  -> JSeM.No
               | otherwise -> JSeM.Unk
trawlParseResult NoSentence = fromFoldable []

{-- Express sequential typecheck --}
sequentialTypeCheckExpress :: CP.ParseSetting -> QT.Prover -> DTT.Signature -> DTT.Context -> Discourse -> NodeSelector -> DiagramSelector -> IO ParseResultExpress
sequentialTypeCheckExpress ps prover signtr contxt [] _ _ =
  -- 文が尽きたら、選択により構築された contxt を用いて推論を実行（pos/neg）
  pure $
    if CP.noInference ps
      then NoSentenceExpress
      else case contxt of
             [] -> NoSentenceExpress
             (typ:rest) ->
               let psqPos = DTT.ProofSearchQuery signtr rest typ
                   resultPos = takeNbest (CP.nProof ps) $ prover psqPos
                   psqNeg = DTT.ProofSearchQuery signtr rest (DTT.Pi typ DTT.Bot)
                   resultNeg = takeNbest (CP.nProof ps) $ prover psqNeg
               in InferenceResultsExpress (QueryAndDiagrams psqPos resultPos)
                                          (QueryAndDiagrams psqNeg resultNeg)
sequentialTypeCheckExpress ps prover signtr contxt ((text,nodes):rests) pickNode pickDiag = do
  mnode <- pickNode nodes
  case mnode of
    Nothing -> fail "No node selected"
    Just node -> do
      let signtr' = L.nub $ (CCG.sig node) ++ signtr
          tcQueryType = UDTT.Judgment signtr' contxt (CCG.sem node) DTT.Type
      pure $
        SentenceAndParseTreeExpress text $
          ParseTreeAndFelicityCheckExpress node signtr' tcQueryType $ do
            mDiag <- pickDiag tcQueryType
            case mDiag of
              Nothing -> fail "No typecheck diagram selected"
              Just tcDiagram -> do
                let contxt' = (DTT.trm $ Tree.node tcDiagram):contxt
                next <- sequentialTypeCheckExpress ps prover signtr' contxt' rests pickNode pickDiag
                pure (tcDiagram, next)

{-- Parallel processing --}

parallelM :: ListT IO a -> (a -> b) -> ListT IO b
parallelM lst f = join $ lift $ do
  unc <- uncons lst -- Maybe (a, ListT IO a)
  case unc of
    Nothing -> return $ fromFoldable []
    Just (x,mxs) -> return $ fx `par` mfxs `pseq` (cons fx mfxs)
                    where fx   = f x
                          mfxs = parallelM mxs f

parallelFor :: [a] -> (a -> b) -> [b]
parallelFor [] f = []
parallelFor (x:xs) f = fx `par` fxs `pseq` (fx:fxs)
  where fx = f x
        fxs = parallelFor xs f