{-# LANGUAGE OverloadedStrings #-}

module Parser.FeedBack where

import qualified Data.Map as M
import qualified Parser.CCG as CCG
import qualified Parser.ChartParser as CCG
import qualified Parser.Language.Japanese.Filter.KNPFilter as KNP
import qualified Parser.KWJA as KW
import qualified DTS.DTTdeBruijn as DTT        
import qualified Debug.Trace as D
import qualified Parser.LangOptions as L
import Parser.PartialParsing (simpleParse)
import qualified Data.Text.Lazy as TL 
import qualified Data.Text as T         
import qualified Text.Juman as J
import qualified Text.Show.Unicode as U
import Interface.GPT (callGPT)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.List (nub)
import Data.Maybe (maybeToList)
import Data.Time.Clock (getCurrentTime)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import ListT (ListT(..),fromFoldable,toReverseList,take,uncons,cons) --list-t
import qualified Text.Show.Unicode as U -- unicode-show

type NodeMap = M.Map TL.Text CCG.Node
type AxiomCandidate = ((TL.Text, TL.Text), (DTT.Preterm, DTT.Preterm))
type ArgumentAlignment = [(TL.Text, TL.Text)]
type ContextualAxiomCandidate = (AxiomCandidate, Maybe ArgumentAlignment)

data ArgumentRef = ArgumentRef
    { refTarget :: TL.Text
    , refSid :: Maybe T.Text
    , refId :: Maybe Int
    } deriving (Eq, Show)

data PredicateFrame = PredicateFrame
    { frameWord :: TL.Text
    , frameArgs :: [(TL.Text, ArgumentRef)]
    } deriving (Eq, Show)

data PartOfSpeech = Verb | Adjective | NominalPredicate | Noun | OtherCat
  deriving (Eq)

data RelationLabel
    = Synonym
    | Hypernym
    | Hyponym
    | Similar
    | Inflection
    | Antonym
    | Derivation
    deriving (Eq, Show)

data AxiomRelation = EntailsForward | EntailsBackward | ContradictsForward
  deriving (Eq)

buildNodeMap :: [CCG.Node] -> NodeMap
buildNodeMap nodes = 
    let nodeList = getList nodes 
    in M.fromList nodeList 
    where 
        getList :: [CCG.Node] -> [(TL.Text, CCG.Node)]
        getList nodes = case nodes of 
            [] -> []
            n:ns -> 
                let sf = firstToken $ CCG.pf n
                in (sf, n) : getList ns
        

firstToken :: TL.Text -> TL.Text
firstToken t = case TL.splitOn "/" t of
    (a:_) -> a
    []    -> t

isFeedbackTargetWord :: TL.Text -> Bool
isFeedbackTargetWord word =
    not ("#" `TL.isPrefixOf` word || "＃" `TL.isPrefixOf` word)

-- -- | Node を再帰的に巡回して (Signature, Cat) のリストを返す
-- extractSigCatFromNode :: CCG.Node -> [(DTT.Signature, CCG.Cat)]
-- extractSigCatFromNode node =
--     (CCG.sig node, CCG.cat node) : []
--     -- (CCG.sig node, CCG.cat node) : concatMap extractSigCatFromNode (CCG.daughters node)
            
            
-- | LeafNodeのSignatureを集める
-- extractLeafSigFromNode :: CCG.Node -> [(DTT.Signature, CCG.Cat)]
extractLeafSigFromNode :: CCG.Node -> [CCG.Node]
extractLeafSigFromNode node =
    if null (CCG.daughters node) && not (CCG.sig node == [])
    -- then [(CCG.sig node, CCG.cat node)]
    then [node]
    else concatMap extractLeafSigFromNode (CCG.daughters node)
    
-- Signatureの中の型（各ペアの snd 部分）を抜き出すヘルパー
sigTypes :: DTT.Signature -> [DTT.Preterm]
sigTypes s = map snd s

sameLexicalPartOfSpeech :: DTT.Signature -> CCG.Cat -> DTT.Signature -> CCG.Cat -> Bool
sameLexicalPartOfSpeech sig1 cat1 sig2 cat2
    | isPredicateCategory cat1 && isPredicateCategory cat2 = True
    | otherwise = lexicalPartOfSpeech sig1 cat1 == lexicalPartOfSpeech sig2 cat2

isPredicateCategory :: CCG.Cat -> Bool
isPredicateCategory cat =
    KNP.isPredicate "verb" cat
    || KNP.isPredicate "adj" cat
    || KNP.isPredicate "nom" cat

lexicalPartOfSpeech :: DTT.Signature -> CCG.Cat -> Maybe PartOfSpeech
lexicalPartOfSpeech sig cat =
    case sigTypes sig of
        (DTT.Entity:_) -> Just Noun
        _              -> categoryPartOfSpeech cat

categoryPartOfSpeech :: CCG.Cat -> Maybe PartOfSpeech
categoryPartOfSpeech cat
    | KNP.isPredicate "verb" cat = Just Verb
    | KNP.isPredicate "adj" cat = Just Adjective
    | KNP.isPredicate "nom" cat = Just NominalPredicate
    | otherwise = case cat of
        CCG.NP _ -> Just Noun
        CCG.N    -> Just Noun
        _        -> Just OtherCat

-- sigs1 と sigs2 の間で、
-- 「sigs1 の要素 (sig1,cat1) と sigs2 の要素 (sig2,cat2) の cat が等しく、
-- かつ sig1 と sig2 のいずれかの署名ペアの snd (= 型) が一致する」
-- という条件を満たすペアを抽出する。
matchingPairs :: [(DTT.Signature, CCG.Cat)]
              -> [(DTT.Signature, CCG.Cat)]
              -> [(TL.Text, TL.Text)]
-- matchingPairs :: [CCG.Node] -> [CCG.Node] -> [((TL.Text, TL.Text))]
matchingPairs sigs1 sigs2 =
  [ (fst $ Prelude.head $ fst x, fst $ Prelude.head $ fst y)
  | x@(s1, c1) <- sigs1
  , y@(s2, c2) <- sigs2
  , isFeedbackTargetWord (fst $ Prelude.head s1)
  , isFeedbackTargetWord (fst $ Prelude.head s2)
  , sameLexicalPartOfSpeech s1 c1 s2 c2
  ]

matchingAxiomCandidates :: [(DTT.Signature, CCG.Cat)]
                         -> [(DTT.Signature, CCG.Cat)]
                         -> [AxiomCandidate]
matchingAxiomCandidates sigs1 sigs2 =
  [ ((word1, word2), (typ1, typ2))
  | (s1, c1) <- sigs1
  , (s2, c2) <- sigs2
  , sameLexicalPartOfSpeech s1 c1 s2 c2
  , (word1, typ1) <- maybeToList (firstSigEntry s1)
  , (word2, typ2) <- maybeToList (firstSigEntry s2)
  , isFeedbackTargetWord word1
  , isFeedbackTargetWord word2
  ]
  where
    firstSigEntry :: DTT.Signature -> Maybe (TL.Text, DTT.Preterm)
    firstSigEntry sig = case sig of
        entry:_ -> Just entry
        []      -> Nothing

-- 2つの要素で完全に一致するSignatureを取り除く、このとき、sig1の方も一緒にペアにして残す
compareSigs :: [(DTT.Signature, CCG.Cat)] -> [(DTT.Signature, CCG.Cat)] -> [((TL.Text, TL.Text))]
compareSigs sigs1 sigs2 =
    -- sigs1内の要素の重複を取り除く
    let uniqueSigs1 = nub sigs1
    -- sigs2内の要素の重複を取り除く
        uniqueSigs2 = nub sigs2
    -- sigs1と完全に一致する要素をsigs2から取り除く
        filterSig2 = filter (\x -> not (x `elem` uniqueSigs1)) uniqueSigs2
    in D.trace (concat [U.ushow sigs1, "\n\n", U.ushow sigs2]) reversePair $ matchingPairs uniqueSigs1 filterSig2
    where
        -- ペアの逆を取得するヘルパー関数
        reversePair :: [((TL.Text, TL.Text))] -> [((TL.Text, TL.Text))]
        reversePair pairs = map (\(a,b) -> (b,a)) pairs ++ pairs

compareAxiomCandidates :: [(DTT.Signature, CCG.Cat)] -> [(DTT.Signature, CCG.Cat)] -> [AxiomCandidate]
compareAxiomCandidates sigs1 sigs2 =
    let uniqueSigs1 = nub sigs1
        uniqueSigs2 = nub sigs2
        filterSig2 = filter (\x -> not (x `elem` uniqueSigs1)) uniqueSigs2
    in reverseCandidate $ matchingAxiomCandidates uniqueSigs1 filterSig2
    where
        reverseCandidate :: [AxiomCandidate] -> [AxiomCandidate]
        reverseCandidate candidates = map (\((a,b),(typ1,typ2)) -> ((b,a),(typ2,typ1))) candidates ++ candidates

compareContextualAxiomCandidates :: [TL.Text] -> [TL.Text]
                                -> [(DTT.Signature, CCG.Cat)]
                                -> [(DTT.Signature, CCG.Cat)]
                                -> [ContextualAxiomCandidate]
compareContextualAxiomCandidates premiseTexts hypothesisTexts sigs1 sigs2 =
    let uniqueSigs1 = nub sigs1
        uniqueSigs2 = nub sigs2
        filterSig2 = filter (\x -> not (x `elem` uniqueSigs1)) uniqueSigs2
        forwardCandidates = matchingAxiomCandidates uniqueSigs1 filterSig2
        reverseCandidates = map (\((a,b),(typ1,typ2)) -> ((b,a),(typ2,typ1))) forwardCandidates
        withAlignment sourceTexts targetTexts candidate =
            (candidate, inferArgumentAlignment sourceTexts targetTexts candidate)
    in map (withAlignment hypothesisTexts premiseTexts) reverseCandidates
       ++ map (withAlignment premiseTexts hypothesisTexts) forwardCandidates

compareContextualAxiomCandidatesWithKWJA :: [PredicateFrame]
                                        -> [(DTT.Signature, CCG.Cat)]
                                        -> [(DTT.Signature, CCG.Cat)]
                                        -> [ContextualAxiomCandidate]
compareContextualAxiomCandidatesWithKWJA frames sigs1 sigs2 =
    let uniqueSigs1 = nub sigs1
        uniqueSigs2 = nub sigs2
        filterSig2 = filter (\x -> not (x `elem` uniqueSigs1)) uniqueSigs2
        forwardCandidates = matchingAxiomCandidates uniqueSigs1 filterSig2
        reverseCandidates = map (\((a,b),(typ1,typ2)) -> ((b,a),(typ2,typ1))) forwardCandidates
        withAlignment candidate = (candidate, inferArgumentAlignmentFromKWJA frames candidate)
    in map withAlignment reverseCandidates ++ map withAlignment forwardCandidates


makePrompt :: [((TL.Text, TL.Text))] -> IO T.Text
makePrompt = makePromptWithContext [] []

makePromptWithContext :: [TL.Text] -> [TL.Text] -> [((TL.Text, TL.Text))] -> IO T.Text
makePromptWithContext premiseTexts hypothesisTexts sigPairs =
    let prompt' = TL.toStrict 
            "次の語彙ペアについて、A と B の語彙的な関係ラベルを判定してください。\n\
            \当てはまるラベルを synonym, hypernym, hyponym, similar, inflection, antonym, derivation からすべて選んでください。\n\
            \どれも当てはまらない場合は空リスト [] を返してください。\n\
            \hypernym は A が B の下位語で A -> B が成り立つ場合、hyponym は A が B の上位語で B -> A が成り立つ場合です。\n\
            
            \例:\n\
            \ (美しい, 綺麗) → [\"synonym\"]\n\
            \ (犬, 動物) → [\"hypernym\"]\n\
            \ (動物, 犬) → [\"hyponym\"]\n\
            \(独身, 既婚) → [\"antonym\"]\n\
            \(歩く, 歩いた) → [\"inflection\"]\n\
            \(建てる, 建つ) → [\"derivation\"]\n\
            \(日本人, エンジニア) → []\n\

            \出力ルール: \n\
            \ - 各ペアの順番通りに判定すること \n\
            \ - 出力は JSON 配列のみ \n\
            \ - 各要素はラベル文字列の配列 \n\
            \ - 説明文、Markdown、コードブロックは出力しないこと \n\
            \ - 出力例: [[\"synonym\"],[\"antonym\"],[]] "
            
        prompts = T.concat $ prompt' : makeContextPrompt premiseTexts hypothesisTexts : makePrompt' sigPairs
   in return $ D.trace (U.ushow sigPairs) prompts
   where 
        makeContextPrompt :: [TL.Text] -> [TL.Text] -> T.Text
        makeContextPrompt premises hypotheses
            | null premises && null hypotheses = T.empty
            | otherwise =
                T.concat
                    [ "\n\n文脈情報:\n"
                    , "Premises:\n"
                    , T.concat $ map (\sentence -> T.concat ["- ", TL.toStrict sentence, "\n"]) premises
                    , "Hypothesis:\n"
                    , T.concat $ map (\sentence -> T.concat ["- ", TL.toStrict sentence, "\n"]) hypotheses
                    , "\nこの文脈を考慮して、以下の語彙ペアを判定してください。\n"
                    ]

        makePrompt' :: [((TL.Text, TL.Text))] -> [T.Text]
        makePrompt' sigPairs =  case sigPairs of
            [] -> []
            x:xs ->
                let (word1, word2) = x 
                    promptWord1 = firstToken word1
                    promptWord2 = firstToken word2
                -- in TL.concat ["「",word1, "」は「", word2, "」を意味的に含意するか？\n"] :  makePrompt' xs
                in T.concat ["(", TL.toStrict promptWord1, ", ", TL.toStrict promptWord2, ")\n"] :  makePrompt' xs 


-- | "," か　で区切られた応答を分割してリストにする。スペースは削除する。
splitResponse :: T.Text -> [T.Text]
splitResponse response =
    let parts = T.splitOn "," response
    in map T.strip parts

parseRelationResponse :: T.Text -> [[RelationLabel]]
parseRelationResponse response =
    let groups = bracketGroups $ stripOuterJsonList $ T.strip response
    in if null groups
       then map parseRelationLabels $ T.lines response
       else map parseRelationLabels groups

bracketGroups :: T.Text -> [T.Text]
bracketGroups text = go text []
    where
        go rest groups = case T.breakOn "[" rest of
            (_, afterOpen)
                | T.null afterOpen -> reverse groups
                | otherwise ->
                    let contentStart = T.drop 1 afterOpen
                        (content, afterClose) = T.breakOn "]" contentStart
                    in if T.null afterClose
                       then reverse groups
                       else go (T.drop 1 afterClose) (content:groups)

stripOuterJsonList :: T.Text -> T.Text
stripOuterJsonList text
    | "[[" `T.isPrefixOf` text && "]]" `T.isSuffixOf` text = T.dropEnd 1 $ T.drop 1 text
    | otherwise = text

parseRelationLabels :: T.Text -> [RelationLabel]
parseRelationLabels text = nub $ concatMap (maybeToList . parseRelationLabel . normalizeLabelText) $ T.splitOn "," text

normalizeLabelText :: T.Text -> T.Text
normalizeLabelText =
    T.toLower
    . T.filter (`notElem` (" \t\n\r\"'`[]。" :: String))
    . T.strip

parseRelationLabel :: T.Text -> Maybe RelationLabel
parseRelationLabel label = case label of
    "synonym"    -> Just Synonym
    "hypernym"   -> Just Hypernym
    "hyponym"    -> Just Hyponym
    "similar"    -> Just Similar
    "inflection" -> Just Inflection
    "antonym"    -> Just Antonym
    "derivation" -> Just Derivation
    _            -> Nothing

fitRelationLabels :: Int -> [[RelationLabel]] -> [[RelationLabel]]
fitRelationLabels expected labels =
    Prelude.take expected $ labels ++ repeat []
    
    
-- | 公理を作る
makeAxiom :: (TL.Text, TL.Text) -> T.Text -> DTT.Signature --[(LazyT.Text, Preterm)]
makeAxiom (word1, word2) answer = case answer of
    "Yes" -> [(TL.concat[word1,"-",word2], DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.App (DTT.App (DTT.Con word1) (DTT.Var 1)) (DTT.Var 0)) (DTT.App (DTT.App (DTT.Con word2) (DTT.Var 2)) (DTT.Var 1)))))]
    "No" -> [(TL.concat[word1,"-",word2], DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.App (DTT.App (DTT.Con word1) (DTT.Var 1)) (DTT.Var 0)) (DTT.Pi (DTT.App (DTT.App (DTT.Con word2) (DTT.Var 2)) (DTT.Var 1)) DTT.Bot))))]
    _ -> []

makeAxiomWithType :: AxiomCandidate -> T.Text -> DTT.Signature
makeAxiomWithType = makeAxiomWithTypeAndAlignment Nothing

makeAxiomWithTypeAndAlignment :: Maybe ArgumentAlignment -> AxiomCandidate -> T.Text -> DTT.Signature
makeAxiomWithTypeAndAlignment alignment ((word1, word2), (typ1, typ2)) answer =
    case (predicateDomains typ1, predicateDomains typ2) of
        (Just domains1, Just domains2) -> makeAxiomForDomains alignment domains1 domains2 (word1, word2) answer
        _                              -> makeAxiom (word1, word2) answer

makeAxiomsWithLabels :: Maybe ArgumentAlignment -> AxiomCandidate -> [RelationLabel] -> DTT.Signature
makeAxiomsWithLabels alignment candidate labels =
    nub $ concatMap (makeAxiomForRelation alignment candidate) relations
    where
        relations = nub $ map relationForLabel labels

relationForLabel :: RelationLabel -> AxiomRelation
relationForLabel label = case label of
    Synonym    -> EntailsForward
    Hypernym   -> EntailsForward
    Hyponym    -> EntailsBackward
    Similar    -> EntailsForward
    Inflection -> EntailsForward
    Antonym    -> ContradictsForward
    Derivation -> EntailsForward

makeAxiomForRelation :: Maybe ArgumentAlignment -> AxiomCandidate -> AxiomRelation -> DTT.Signature
makeAxiomForRelation alignment candidate relation = case relation of
    EntailsForward     -> makeAxiomWithTypeAndAlignment alignment candidate "Yes"
    ContradictsForward -> makeAxiomWithTypeAndAlignment alignment candidate "No"
    EntailsBackward    -> makeAxiomWithTypeAndAlignment (invertAlignment alignment) (swapAxiomCandidate candidate) "Yes"

swapAxiomCandidate :: AxiomCandidate -> AxiomCandidate
swapAxiomCandidate ((word1, word2), (typ1, typ2)) = ((word2, word1), (typ2, typ1))

invertAlignment :: Maybe ArgumentAlignment -> Maybe ArgumentAlignment
invertAlignment = fmap (map (\(targetLabel, sourceLabel) -> (sourceLabel, targetLabel)))

predicateDomains :: DTT.Preterm -> Maybe [DTT.Preterm]
predicateDomains typ = collect [] typ
    where
        collect domains DTT.Type = Just (reverse domains)
        collect domains (DTT.Pi domain rest) = collect (domain:domains) rest
        collect _ _ = Nothing

makeAxiomForDomains :: Maybe ArgumentAlignment -> [DTT.Preterm] -> [DTT.Preterm] -> (TL.Text, TL.Text) -> T.Text -> DTT.Signature
makeAxiomForDomains alignment domains1 domains2 (word1, word2) answer =
    case makeConsequentWithExistentials alignment word1 word2 arity1 domains2 of
        Just consequent -> makeAxiomForConsequent consequent
        Nothing             -> []
    where
        arity1 = length domains1
        axiomName = TL.concat [word1, "-", word2]
        antecedent = predicateApp word1 (argumentVars arity1 0)
        makeAxiomForConsequent consequent = case answer of
            "Yes" -> [(axiomName, makePis domains1 (DTT.Pi antecedent consequent))]
            "No"  -> [(axiomName, makePis domains1 (DTT.Pi antecedent (DTT.Pi consequent DTT.Bot)))]
            _     -> []

makePis :: [DTT.Preterm] -> DTT.Preterm -> DTT.Preterm
makePis domains body = foldr DTT.Pi body domains

predicateApp :: TL.Text -> [DTT.Preterm] -> DTT.Preterm
predicateApp word args = foldl DTT.App (DTT.Con word) args

argumentVars :: Int -> Int -> [DTT.Preterm]
argumentVars arity offset
    | arity <= 0 = []
    | otherwise = map DTT.Var [arity - 1 + offset, arity - 2 + offset .. offset]

makeConsequentWithExistentials :: Maybe ArgumentAlignment -> TL.Text -> TL.Text -> Int -> [DTT.Preterm] -> Maybe DTT.Preterm
makeConsequentWithExistentials alignment word1 word2 arity1 domains2 =
    case mapM argumentSpec indexedLabels2 of
        Just specs ->
            let missingTargetIndices = [targetIndex | MissingArg targetIndex <- specs]
                missingDomains = map (domains2 !!) missingTargetIndices
                consequentArgs = map (specToVar missingTargetIndices) specs
            in Just $ makeSigmas missingDomains (predicateApp word2 (reverse consequentArgs))
        Nothing -> Nothing
    where
        arity2 = length domains2
        labels1 = predicateArgumentLabels word1 arity1
        indexedLabels1 = zip labels1 [0..]
        indexedLabels2 = zip (predicateArgumentLabels word2 arity2) [0..]
        argumentSpec (targetLabel, targetIndex) =
            case lookup (alignTargetLabel targetLabel) indexedLabels1 of
                Just sourceIndex -> Just $ ExistingArg sourceIndex
                Nothing
                    | targetLabel == "event" -> Nothing
                    | otherwise              -> Just $ MissingArg targetIndex
        alignTargetLabel label = case alignment >>= lookup label of
            Just sourceLabel -> sourceLabel
            Nothing          -> label
        specToVar missingTargetIndices spec = case spec of
            ExistingArg sourceIndex -> DTT.Var (sourceIndex + 1 + length missingTargetIndices)
            MissingArg targetIndex  -> DTT.Var (missingVarIndex missingTargetIndices targetIndex)
        missingVarIndex missingTargetIndices targetIndex =
            case lookup targetIndex (zip missingTargetIndices [length missingTargetIndices - 1, length missingTargetIndices - 2 .. 0]) of
                Just varIndex -> varIndex
                Nothing       -> 0

data ArgumentSpec = ExistingArg Int | MissingArg Int

makeSigmas :: [DTT.Preterm] -> DTT.Preterm -> DTT.Preterm
makeSigmas domains body = foldr DTT.Sigma body domains

predicateArgumentLabels :: TL.Text -> Int -> [TL.Text]
predicateArgumentLabels word arity =
    case caseLabelsInPredicate word of
        Just caseLabels
            | length caseLabels + 1 == arity -> "event" : caseLabels
        _ -> positionalLabels arity

-- KWJA appends case-frame information to predicate names (for example,
-- "整える/ととのえる/ガヲ").  A bare noun such as "人" must not be
-- interpreted character-by-character as argument labels.
caseLabelsInPredicate :: TL.Text -> Maybe [TL.Text]
caseLabelsInPredicate word = case reverse (TL.splitOn "/" word) of
    (suffix:_:_) 
        | not (TL.null suffix)
        , TL.all (`elem` ("ヨガヲニトノヘデ=" :: String)) suffix ->
            Just $ map TL.singleton $ TL.unpack suffix
    _ -> Nothing

positionalLabels :: Int -> [TL.Text]
positionalLabels arity = map (TL.pack . show) [0 .. arity - 1]

lastToken :: TL.Text -> TL.Text
lastToken t = case reverse (TL.splitOn "/" t) of
    (a:_) -> a
    []    -> t

kwjaPredicateFramesFromTexts :: [TL.Text] -> IO [PredicateFrame]
kwjaPredicateFramesFromTexts texts
    | null nonEmptyTexts = return []
    | otherwise = do
        kwjaData <- KW.callKWJA $ TL.toStrict $ TL.intercalate "。" nonEmptyTexts
        return $ predicateFramesFromKWJA kwjaData
    where
        nonEmptyTexts = filter (not . TL.null) texts

predicateFramesFromKWJA :: [KW.KWJAData] -> [PredicateFrame]
predicateFramesFromKWJA kwjaData = case kwjaData of
    [] -> []
    (KW.KWJA node):rest ->
        case (KW.args node, predicateLemmaAfter rest) of
            (Just args, Just lemma)
                | not (null args) ->
                    PredicateFrame
                        { frameWord = TL.fromStrict lemma
                        , frameArgs = map argToFrameArg args
                        } : predicateFramesFromKWJA rest
            _ -> predicateFramesFromKWJA rest
    _:rest -> predicateFramesFromKWJA rest

predicateLemmaAfter :: [KW.KWJAData] -> Maybe T.Text
predicateLemmaAfter kwjaData = case kwjaData of
    [] -> Nothing
    (KW.Juman (J.JumanWord _ _ genkei hinsi _ _ _ _ _ _ _ _)):_
        | hinsi == "動詞" || hinsi == "形容詞" -> Just genkei
        | otherwise -> Nothing
    (KW.Juman J.EOS):_ -> Nothing
    _:rest -> predicateLemmaAfter rest

argToFrameArg :: KW.Arg -> (TL.Text, ArgumentRef)
argToFrameArg arg =
    ( normalizeArgLabel $ T.pack $ KW.argType arg
    , ArgumentRef
        { refTarget = TL.pack $ KW.target arg
        , refSid = KW.sid arg
        , refId = KW.argId arg
        }
    )

normalizeArgLabel :: T.Text -> TL.Text
normalizeArgLabel =
    TL.fromStrict
    . T.filter (`elem` ("ヨガヲニトノヘデ=" :: String))

inferArgumentAlignmentFromKWJA :: [PredicateFrame] -> AxiomCandidate -> Maybe ArgumentAlignment
inferArgumentAlignmentFromKWJA frames ((word1, word2), _) =
    case (findFrameForWord word1 frames, findFrameForWord word2 frames) of
        (Just sourceFrame, Just targetFrame) ->
            let inferred = nub
                    [ (targetLabel, sourceLabel)
                    | (targetLabel, targetRef) <- frameArgs targetFrame
                    , targetLabel /= "event"
                    , (sourceLabel, sourceRef) <- frameArgs sourceFrame
                    , sourceLabel /= "event"
                    , targetLabel /= sourceLabel
                    , sameArgumentRef targetRef sourceRef
                    ]
            in if null inferred then Nothing else Just inferred
        _ -> Nothing

findFrameForWord :: TL.Text -> [PredicateFrame] -> Maybe PredicateFrame
findFrameForWord word =
    findFirst (wordMatchesFrame (firstToken word))
    where
        findFirst _ [] = Nothing
        findFirst p (x:xs)
            | p x = Just x
            | otherwise = findFirst p xs

wordMatchesFrame :: TL.Text -> PredicateFrame -> Bool
wordMatchesFrame word frame =
    word == frameWord frame
    || normalizePredicateWord word == normalizePredicateWord (frameWord frame)

normalizePredicateWord :: TL.Text -> TL.Text
normalizePredicateWord =
    TL.filter (`notElem` ("するたいるて" :: String))

sameArgumentRef :: ArgumentRef -> ArgumentRef -> Bool
sameArgumentRef ref1 ref2 =
    sameResolvedId ref1 ref2 || sameTarget ref1 ref2
    where
        sameResolvedId a b =
            refSid a /= Nothing
            && refId a /= Nothing
            && refSid a == refSid b
            && refId a == refId b
        sameTarget a b =
            not (TL.null $ normalizeMention $ refTarget a)
            && normalizeMention (refTarget a) == normalizeMention (refTarget b)

inferArgumentAlignment :: [TL.Text] -> [TL.Text] -> AxiomCandidate -> Maybe ArgumentAlignment
inferArgumentAlignment sourceTexts targetTexts ((word1, word2), (typ1, typ2)) =
    case (predicateDomains typ1, predicateDomains typ2) of
        (Just domains1, Just domains2) ->
            let sourceFillers = caseFillersForLabels (predicateArgumentLabels word1 (length domains1)) sourceTexts
                targetFillers = caseFillersForLabels (predicateArgumentLabels word2 (length domains2)) targetTexts
                inferred = [ (targetLabel, sourceLabel)
                           | (targetLabel, targetFiller) <- targetFillers
                           , (sourceLabel, sourceFiller) <- sourceFillers
                           , targetFiller == sourceFiller
                           , targetLabel /= sourceLabel
                           ]
            in if null inferred then Nothing else Just inferred
        _ -> Nothing

caseFillersForLabels :: [TL.Text] -> [TL.Text] -> [(TL.Text, TL.Text)]
caseFillersForLabels labels texts =
    [ (label, filler)
    | label <- labels
    , label /= "event"
    , particle <- maybeToList (caseParticle label)
    , text <- texts
    , filler <- extractCaseFillers particle text
    ]

caseParticle :: TL.Text -> Maybe TL.Text
caseParticle label = case label of
    "ガ" -> Just "が"
    "ヲ" -> Just "を"
    "ニ" -> Just "に"
    "デ" -> Just "で"
    "ト" -> Just "と"
    "ヘ" -> Just "へ"
    _    -> Nothing

extractCaseFillers :: TL.Text -> TL.Text -> [TL.Text]
extractCaseFillers particle text =
    filter (not . TL.null) $ map normalizeMention $ filter (not . TL.null) $ map previousMention $ initSafe $ TL.splitOn particle text

initSafe :: [a] -> [a]
initSafe xs = case xs of
    [] -> []
    [_] -> []
    _ -> init xs

previousMention :: TL.Text -> TL.Text
previousMention text =
    let chunks = TL.split (`elem` ("がをにでとはへ、。，．「」（）() \t\n" :: String)) text
    in case filter (not . TL.null) chunks of
        [] -> ""
        xs -> last xs

normalizeMention :: TL.Text -> TL.Text
normalizeMention mention =
    stripDemonstrative $ TL.dropWhile (`elem` ("　 " :: String)) $ TL.dropWhileEnd (`elem` ("　 " :: String)) mention

stripDemonstrative :: TL.Text -> TL.Text
stripDemonstrative mention =
    case filter (`TL.isPrefixOf` mention) ["その", "この", "あの", "どの"] of
        prefix:_ -> TL.drop (TL.length prefix) mention
        []       -> mention

-- returnFeedBack :: CCG.Node -> CCG.Node -> IO DTT.Signature
-- returnFeedBack premise hyp = do
--     let sigs1 = extractLeafSigFromNode premise
--         sigs2 = extractLeafSigFromNode hyp
--         filterSigs = compareSigs sigs1 sigs2
--     prompts <- makePrompt filterSigs
--     response <- callGPT prompts
--     let answers = splitResponse response
--         axioms = zipWith makeAxiom filterSigs answers
--     -- D.trace (concat (map U.ushow sigs1) ++ "\n\n" ++ concat (map U.ushow sigs2) ++ "\n\n" ++ concat (map U.ushow filterSigs) ++ "\n\n" ++  U.ushow prompts ++ "\n\n" ++ U.ushow answers ++ "\n\n" ++ concat (map U.ushow axioms))  
--     return $ concat axioms



returnFeedBacks :: [(TL.Text, ListT IO CCG.Node)] ->  DTT.Signature
returnFeedBacks nodes = 
    unsafePerformIO $ returnFeedBacks' nodes
    where 
        returnFeedBacks' :: [(TL.Text, ListT IO CCG.Node)] -> IO DTT.Signature
        -- [CCG.Node] -> [[CCG.Node]] -> IO DTT.Signature
        returnFeedBacks' nodes = do
            -- nodeLists :: IO [[CCG.Node]]
            nodeLists <- mapM (\(_, lst) -> toReverseList lst >>= (return . reverse)) $ nodes
            -- premise :: [CCG.Node],  hyp :: [[CCG.Node]]
            let premises = head nodeLists
                hyps =  drop 1 nodeLists
                premiseTexts = Prelude.take 1 $ map fst nodes
                hypothesisTexts = drop 1 $ map fst nodes
            kwjaFrames <- kwjaPredicateFramesFromTexts (premiseTexts ++ hypothesisTexts)
            -- let premiseSigs = concatMap extractLeafSigFromNode premises
            --     hypSigs = concatMap extractLeafSigFromNode (concat hyps)
            let
                premiseNodes = concatMap extractLeafSigFromNode premises
                hypNodes = concatMap extractLeafSigFromNode (concat hyps)
                -- NodeMapを作成
                premiseNodeMap = buildNodeMap premiseNodes
                hypNodeMap = buildNodeMap hypNodes
                -- 
                premiseSigs = map (\node -> (CCG.sig node, CCG.cat node)) premiseNodes
                hypSigs = map (\node -> (CCG.sig node, CCG.cat node)) hypNodes
                contextualAxiomCandidates = compareContextualAxiomCandidatesWithKWJA kwjaFrames premiseSigs hypSigs
                axiomCandidates = map fst contextualAxiomCandidates
                filterSigs = map fst axiomCandidates
            if null filterSigs
                then D.trace (concat (map U.ushow premises) ++ "\n\n" ++ concat (map U.ushow hyps) ++ "\n\n" ++ concat (map U.ushow filterSigs)) return []

            else do
                prompts <- makePromptWithContext premiseTexts hypothesisTexts filterSigs
                response <- callGPT prompts
                let relationLabels = fitRelationLabels (length contextualAxiomCandidates) $ parseRelationResponse response
                    axioms = zipWith (\(candidate, alignment) labels -> makeAxiomsWithLabels alignment candidate labels) contextualAxiomCandidates relationLabels
                -- ★ ここで追記（パスは好きな場所に）
                appendFeedbackLog "axiom_log.txt" filterSigs relationLabels
                appendStructuredFeedbackLog prompts response premiseTexts hypothesisTexts contextualAxiomCandidates relationLabels axioms
                D.trace (concat (map U.ushow premiseSigs) ++ "\n\n" ++ concat (map U.ushow hypSigs) ++ "\n\n" ++ concat (map U.ushow kwjaFrames) ++ "\n\n" ++ concat (map U.ushow contextualAxiomCandidates) ++ "\n\n" ++ concat (map U.ushow filterSigs) ++ "\n\n" ++  U.ushow prompts ++ "\n\n" ++ U.ushow relationLabels ++ "\n\n" ++ concat (map U.ushow axioms))  return $ concat axioms 
                -- D.trace (concat (map U.ushow premiseSigs) ++ "\n\n" ++ concat (map U.ushow hypSigs) ++ "\n\n" ++ 


-- | filterSigs と answers を追記する
appendFeedbackLog
  :: (Show a, Show b)
  => FilePath
  -> [a]        -- filterSigs
  -> [b]        -- answers
  -> IO ()
appendFeedbackLog path filterSigs answers = do
  let block =
        unlines
          [ ""
          , "--- filterSigs ---"
          , unlines (map U.ushow filterSigs)
          , "--- answers ---"
          , unlines (map U.ushow answers)
          , "==================="
          ]
  appendFile path block

-- | Save one JSON object per lexical candidate.  The experiment runner sets
-- LIGHTBLUE_AXIOM_LOG_JSONL and the problem identifiers.  Keeping the raw GPT
-- response as well as the parsed labels makes the run auditable and allows the
-- parser to be changed without paying for the API call again.
appendStructuredFeedbackLog
  :: T.Text
  -> T.Text
  -> [TL.Text]
  -> [TL.Text]
  -> [ContextualAxiomCandidate]
  -> [[RelationLabel]]
  -> [DTT.Signature]
  -> IO ()
appendStructuredFeedbackLog prompt rawResponse premises hypotheses candidates labels axioms = do
  maybePath <- lookupEnv "LIGHTBLUE_AXIOM_LOG_JSONL"
  case maybePath of
    Nothing -> return ()
    Just path -> do
      timestamp <- getCurrentTime
      runId <- envText "LIGHTBLUE_RUN_ID"
      splitName <- envText "LIGHTBLUE_SPLIT"
      jsemId <- envText "LIGHTBLUE_JSEM_ID"
      pairId <- envText "LIGHTBLUE_PAIR_ID"
      let fittedLabels = Prelude.take (length candidates) $ labels ++ repeat []
          fittedAxioms = Prelude.take (length candidates) $ axioms ++ repeat []
          records = zipWith3
            (feedbackRecord timestamp runId splitName jsemId pairId)
            [1 :: Int ..]
            candidates
            (zip fittedLabels fittedAxioms)
      mapM_ (LBS.appendFile path . (`LBS.append` "\n") . encode) records
  where
    envText name = fmap (T.pack . maybe "" id) $ lookupEnv name
    feedbackRecord timestamp runId splitName jsemId pairId index
      (((wordA, wordB), _), alignment)
      (candidateLabels, candidateAxioms) =
        object
          [ "timestamp" .= show timestamp
          , "run_id" .= runId
          , "split" .= splitName
          , "jsem_id" .= jsemId
          , "pair_id" .= pairId
          , "candidate_index" .= index
          , "premises" .= map TL.toStrict premises
          , "hypotheses" .= map TL.toStrict hypotheses
          , "word_a" .= TL.toStrict (firstToken wordA)
          , "word_b" .= TL.toStrict (firstToken wordB)
          , "typed_word_a" .= TL.toStrict wordA
          , "typed_word_b" .= TL.toStrict wordB
          , "argument_alignment" .= maybe [] (map (\(a, b) -> [TL.toStrict a, TL.toStrict b])) alignment
          , "prompt" .= prompt
          , "raw_response" .= rawResponse
          , "parsed_labels" .= map show candidateLabels
          , "generated_axiom_count" .= length candidateAxioms
          ]

-- main :: IO ()
-- main = do
    
--     return ()

--     langOptions <- L.defaultJpOptions
--     let p1 = "太郎がリンゴを食べた"
--         p2 = "太郎はバナナを食べた"
--         h = "太郎は食べ物を食べた"
--         parseSetting = CCG.ParseSetting langOptions 24 10 10 10 True Nothing False False
--     let leafSig = extractLeafSigFromNode (head node)
--         leafSig2 = extractLeafSigFromNode (head node2)
--         filterSigs = compareSigs leafSig leafSig2
--     D.trace (concat (map U.ushow leafSig) ++ "\n\n" ++ concat (map U.ushow leafSig2) ++ "\n\n" ++ concat (map U.ushow filterSigs))  return ()
--     let axioms = returnFeedBacks n
--     U.uprint axioms
--     return ()
    
    
    -- -- let result1 = extractSigCatFromNode (head node)
    --     -- result2 = extractSigCatFromNode (head node2)
    -- let leafSig = extractLeafSigFromNode (head node)
    --     leafSig2 = extractLeafSigFromNode (head node2)
    --     filterSigs = compareSigs leafSig leafSig2
    -- prompts <- makePrompt filterSigs
    -- -- response <- callGPT prompts
    -- let response = "Yes, Yes"
    --     answers = splitResponse response
    --     axioms = zipWith makeAxiom filterSigs answers
    -- -- D.trace (U.ushow result) return ()
    -- D.trace (concat (map U.ushow leafSig) ++ "\n\n" ++ concat (map U.ushow leafSig2) ++ "\n\n" ++ concat (map U.ushow filterSigs) ++ "\n\n" ++  U.ushow prompts ++ "\n\n" ++ U.ushow answers ++ "\n\n" ++ concat (map U.ushow axioms))  return ()
