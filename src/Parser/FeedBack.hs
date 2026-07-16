{-# LANGUAGE OverloadedStrings #-}

module Parser.FeedBack where

import qualified Data.Map as M
import qualified Parser.CCG as CCG
import qualified Parser.ChartParser as CCG
import qualified Parser.Language.Japanese.Filter.KNPFilter as KNP
import qualified DTS.DTTdeBruijn as DTT        
import qualified Debug.Trace as D
import qualified Parser.LangOptions as L
import Parser.PartialParsing (simpleParse)
import qualified Data.Text.Lazy as TL 
import qualified Data.Text as T         
import qualified Text.Show.Unicode as U
import Interface.GPT (callGPT)
import Data.List (nub)
import Data.Maybe (maybeToList)
import System.IO.Unsafe (unsafePerformIO)
import ListT (ListT(..),fromFoldable,toReverseList,take,uncons,cons) --list-t
import qualified Text.Show.Unicode as U -- unicode-show

type NodeMap = M.Map TL.Text CCG.Node
type AxiomCandidate = ((TL.Text, TL.Text), (DTT.Preterm, DTT.Preterm))
type ArgumentAlignment = [(TL.Text, TL.Text)]
type ContextualAxiomCandidate = (AxiomCandidate, Maybe ArgumentAlignment)

data PartOfSpeech = Verb | Adjective | NominalPredicate | Noun | OtherCat
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


makePrompt :: [((TL.Text, TL.Text))] -> IO T.Text
makePrompt = makePromptWithContext [] []

makePromptWithContext :: [TL.Text] -> [TL.Text] -> [((TL.Text, TL.Text))] -> IO T.Text
makePromptWithContext premiseTexts hypothesisTexts sigPairs =
    let prompt' = TL.toStrict 
            "次の語彙ペアについて、「A は B を意味的に含意するか」を判定してください。\n\
            \含意とは、「A であれば必ず B が成立する」場合のみ Yes とし、矛盾がある場合のみ No とします。\n\
            \ 含意でなく、矛盾もしない場合はすべてUnknown とします。論理的に含意・矛盾しないものはすべてUnknownに分類してください。 \n\
            
            \例:\n\
            \ (犬, 動物) → Yes\n\
            \ (医者, 人) → Yes \n\
            \(独身, 既婚) → No\n\
            \(死者, 生存者) → No \n\
            \ (動物, 犬) → Unknown\n\
            \(日本人, エンジニア) → Unknown\n\
            \(本, 教科書) → Unknown\n\
            \(建てる, 建つ) → Yes\n\

            \出力ルール: \n\
            \ - 各ペアの順番通りに判定すること \n\
            \ - 出力は **回答のみ** を「,」で区切ること \n\
            \ - 回答は Yes, No, Unknown のいずれか "
            
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
                in T.concat [ TL.toStrict promptWord1, "は", TL.toStrict promptWord2, "を意味的に含意するか\n"] :  makePrompt' xs 


-- | "," か　で区切られた応答を分割してリストにする。スペースは削除する。
splitResponse :: T.Text -> [T.Text]
splitResponse response =
    let parts = T.splitOn "," response
    in map T.strip parts
    
    
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

predicateDomains :: DTT.Preterm -> Maybe [DTT.Preterm]
predicateDomains typ = collect [] typ
    where
        collect domains DTT.Type = Just (reverse domains)
        collect domains (DTT.Pi domain rest) = collect (domain:domains) rest
        collect _ _ = Nothing

makeAxiomForDomains :: Maybe ArgumentAlignment -> [DTT.Preterm] -> [DTT.Preterm] -> (TL.Text, TL.Text) -> T.Text -> DTT.Signature
makeAxiomForDomains alignment domains1 domains2 (word1, word2) answer =
    case alignedArgumentVars alignment word1 word2 arity1 (length domains2) 1 of
        Just consequentArgs -> makeAxiomForConsequent (predicateApp word2 consequentArgs)
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

alignedArgumentVars :: Maybe ArgumentAlignment -> TL.Text -> TL.Text -> Int -> Int -> Int -> Maybe [DTT.Preterm]
alignedArgumentVars alignment word1 word2 arity1 arity2 offset =
    case mapM (`lookup` indexedLabels1) alignedLabels2 of
        Just indices -> Just $ map (DTT.Var . varIndex) indices
        Nothing
            | arity1 == arity2 -> Just $ argumentVars arity1 offset
            | otherwise        -> Nothing
    where
        labels1 = predicateArgumentLabels word1 arity1
        labels2 = predicateArgumentLabels word2 arity2
        alignedLabels2 = map alignTargetLabel labels2
        indexedLabels1 = zip labels1 [0..]
        varIndex domainIndex = arity1 - 1 - domainIndex + offset
        alignTargetLabel label = case alignment >>= lookup label of
            Just sourceLabel -> sourceLabel
            Nothing          -> label

predicateArgumentLabels :: TL.Text -> Int -> [TL.Text]
predicateArgumentLabels word arity =
    let caseLabels = map TL.singleton $ TL.unpack $ lastToken word
        labels = caseLabels ++ ["event"]
    in if length labels == arity
       then labels
       else map (TL.pack . show) [0 .. arity - 1]

lastToken :: TL.Text -> TL.Text
lastToken t = case reverse (TL.splitOn "/" t) of
    (a:_) -> a
    []    -> t

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
            -- let premiseSigs = concatMap extractLeafSigFromNode premises
                -- hypSigs = concatMap extractLeafSigFromNode (concat hyps)
                premiseNodes = concatMap extractLeafSigFromNode premises
                hypNodes = concatMap extractLeafSigFromNode (concat hyps)
                -- NodeMapを作成
                premiseNodeMap = buildNodeMap premiseNodes
                hypNodeMap = buildNodeMap hypNodes
                -- 
                premiseSigs = map (\node -> (CCG.sig node, CCG.cat node)) premiseNodes
                hypSigs = map (\node -> (CCG.sig node, CCG.cat node)) hypNodes
                contextualAxiomCandidates = compareContextualAxiomCandidates premiseTexts hypothesisTexts premiseSigs hypSigs
                axiomCandidates = map fst contextualAxiomCandidates
                filterSigs = map fst axiomCandidates
            if null filterSigs
                then D.trace (concat (map U.ushow premises) ++ "\n\n" ++ concat (map U.ushow hyps) ++ "\n\n" ++ concat (map U.ushow filterSigs)) return []

            else do
                prompts <- makePromptWithContext premiseTexts hypothesisTexts filterSigs
                response <- callGPT prompts
                let answers = splitResponse response
                    axioms = zipWith (\(candidate, alignment) answer -> makeAxiomWithTypeAndAlignment alignment candidate answer) contextualAxiomCandidates answers
                -- ★ ここで追記（パスは好きな場所に）
                appendFeedbackLog "axiom_log.txt" filterSigs answers
                D.trace (concat (map U.ushow premiseSigs) ++ "\n\n" ++ concat (map U.ushow hypSigs) ++ "\n\n" ++ concat (map U.ushow filterSigs) ++ "\n\n" ++  U.ushow prompts ++ "\n\n" ++ U.ushow answers ++ "\n\n" ++ concat (map U.ushow axioms))  return $ concat axioms 
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
