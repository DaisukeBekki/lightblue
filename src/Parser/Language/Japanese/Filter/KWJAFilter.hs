{-# LANGUAGE OverloadedStrings, DeriveGeneric, BlockArguments #-}

-- {-|
-- Module      : KWJAFilter
-- Licence     : LGPL
-- Copyright   : Asa Tomita
-- Stability   : beta
-- Filtering Function using KWJA
-- -}

module Parser.Language.Japanese.Filter.KWJAFilter (
    kwjaFilter,
) where

import Data.List
import System.Environment (getEnv)
import qualified Data.Map.Strict as M
import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Parser.CCG as CCG
--import qualified Parser.ChartParser as LB (defaultParseSetting, ParseSetting(..))
--import qualified Parser.PartialParsing as LB (simpleParse)
import qualified Parser.JumanKatuyou as JK
import qualified Parser.KWJA as KW
import qualified Parser.Language.Japanese.MyLexicon as LB (verblex)
import qualified Parser.Language.Japanese.Templates as LB
import qualified Text.Juman as J
import qualified Parser.Language.Japanese.Filter.KNPFilter as KF
import qualified Parser.Language.Japanese.Filter.LightblueFilter as LF (getDaihyo, getFeatures)

import qualified Text.Show.Unicode as U 
import qualified Debug.Trace as D

type ConjMap = M.Map (T.Text, T.Text) T.Text
-- type OpenWordsMap = M.Map T.Text [(T.Text, S.Set T.Text)]
type OpenWordsMap = M.Map T.Text [(T.Text, [T.Text])]

-- | kwjaを用いたフィルタリング関数を返す
kwjaFilter :: TL.Text -> IO (Int -> Int -> [CCG.Node] -> [CCG.Node])
kwjaFilter text = do
    kwjaDatas <- KW.callKWJA $ TL.toStrict text
    lb <- getEnv "LIGHTBLUE"
    katuyoulist <- JK.parseKatuyouFromPath $ concat [lb,"src/Parser/Language/Japanese/Juman/JUMAN.katuyou"]
    let conjmap = KF.getConjMap katuyoulist
        commonNounSpans = getCommonNounSpans text kwjaDatas
        predicateSpans = getPredicateSpans text kwjaDatas
        verbStemSpans = getVerbStemSpans text kwjaDatas conjmap
        -- openWordsMap = getOpenWordsMap kwjaDatas conjmap
        filterNode = createFilterFrom text kwjaDatas conjmap commonNounSpans predicateSpans verbStemSpans
    return filterNode

-- | KWJAの形態素の、文中での位置 (開始, 終了) と (表層形, 品詞, 品詞細分類) を求める
-- | 表層形を文頭から順に探して位置を決めるので、同じ表層形が文中の別の場所にあっても区別できる
getWordSpans :: TL.Text -> [KW.KWJAData] -> [((Int, Int), (T.Text, T.Text, T.Text))]
getWordSpans text = go 0
  where
    sentence = TL.toStrict text
    go _ [] = []
    go offset (KW.Juman (J.JumanWord surface _ _ hinsi _ bunrui _ _ _ _ _ _) : rest)
        | T.null surface = go offset rest
        | otherwise = case T.breakOn surface (T.drop offset sentence) of
            -- 表層形が見つからない場合（KWJAが文字を正規化した場合など）は位置を進めない
            (_, unmatched) | T.null unmatched -> go offset rest
            (skipped, _) ->
                let start = offset + T.length skipped
                    end = start + T.length surface
                in ((start, end), (surface, hinsi, bunrui)) : go end rest
    go offset (_ : rest) = go offset rest

-- | KWJAが普通名詞と分析した語の、文中での位置 (開始, 終了) を求める
getCommonNounSpans :: TL.Text -> [KW.KWJAData] -> [(Int, Int)]
getCommonNounSpans text kwjaDatas =
    [ sp | (sp, (_, hinsi, bunrui)) <- getWordSpans text kwjaDatas, hinsi == "名詞", bunrui == "普通名詞" ]

-- | KWJAが動詞・形容詞と分析した語の、文中での開始位置と表層形と品詞を求める
getPredicateSpans :: TL.Text -> [KW.KWJAData] -> [(Int, T.Text, T.Text)]
getPredicateSpans text kwjaDatas =
    [ (start, surface, hinsi) | ((start, _), (surface, hinsi, _)) <- getWordSpans text kwjaDatas, hinsi `elem` ["動詞", "形容詞"] ]

-- | KWJAと活用の種類が食い違う場合 (例: 「含めた」をKWJAは母音動詞、lightblueの辞書は「含む」の五段のみ) に、
-- | 述語のエントリーが全部フィルターアウトされるのを防ぐゲート
-- | KWJAの述語が位置 i から始まり、マス (i,j) にその述語と同じ品詞（動詞、または形容詞・形容動詞）の
-- | LEXエントリーがフィルターで一つも残らなかったとき、その品詞のエントリーで、表層形がKWJAの述語の表層形の
-- | 先頭部分であるものを元に戻す
-- | 品詞ごとに判定するので、例えば「悲しめた」で形容詞「悲し(い)」のエントリーが残っていても、
-- | 動詞「悲し(む)」のエントリーが全部なくなれば戻す
restorePredicates :: [(Int, T.Text, T.Text)] -> Int -> [CCG.Node] -> [CCG.Node] -> [CCG.Node]
restorePredicates predicateSpans i before after =
    after ++ nub (concat [ restore surface hinsi | (start, surface, hinsi) <- predicateSpans, start == i ])
  where
    restore surface hinsi
        | any (isLexOf hinsi) after = []
        | otherwise = [ n | n <- before, isLexOf hinsi n, CCG.pf n `TL.isPrefixOf` TL.fromStrict surface ]
    isLexOf hinsi n = CCG.rs n == CCG.LEX && isCatOf hinsi (CCG.cat n)
    isCatOf hinsi c
        | hinsi == "動詞" = isPredicate "verb" c
        | otherwise       = isPredicate "adj" c || isPredicate "nom" c

-- | KWJAが動詞と分析した語の語幹の、文中での位置 (開始, 終了) と、語彙項目を作るための情報
-- | （語幹, 活用型, 代表表記, 格フレーム）を求める
-- | 格フレームは、その語の直前の基本句（KWJAのノード）の項から取る
getVerbStemSpans :: TL.Text -> [KW.KWJAData] -> ConjMap -> [((Int, Int), (T.Text, T.Text, T.Text, [T.Text]))]
getVerbStemSpans text kwjaDatas conjMap = go 0 [] kwjaDatas
  where
    sentence = TL.toStrict text
    go _ _ [] = []
    go offset _ (KW.KWJA node : rest) = go offset (getKWJAargs $ KW.args node) rest
    go offset args (KW.Juman (J.JumanWord surface _ midasi hinsi _ _ _ katuyogata _ katuyokei _ imis) : rest)
        | T.null surface = go offset args rest
        | otherwise = case T.breakOn surface (T.drop offset sentence) of
            (_, unmatched) | T.null unmatched -> go offset args rest
            (skipped, _) ->
                let start = offset + T.length skipped
                    end = start + T.length surface
                    stem = KF.getStem surface katuyogata katuyokei conjMap
                    entry = [ ((start, start + T.length stem), (stem, katuyogata, daihyoOf midasi imis, args))
                            | hinsi == "動詞", KF.isVerb katuyogata
                            , not (T.null stem), stem `T.isPrefixOf` surface ]
                in entry ++ go end args rest
    go offset args (_ : rest) = go offset args rest
    -- | 意味情報の「代表表記:」の値（なければ見出し語）
    daihyoOf midasi imis =
        case [ T.drop (T.length "代表表記:") w | w <- T.words (T.filter (/= '"') imis), "代表表記:" `T.isPrefixOf` w ] of
            (d:_) -> d
            [] -> midasi

-- | KWJAの活用型に対応する、動詞の語彙項目の素性（品詞の素性, 語幹の活用形の素性）
-- | （Lexicon.hs の jumanPos2Cat の「動詞:...」と同じ対応）
verbFeatures :: T.Text -> Maybe ([CCG.FeatureValue], [CCG.FeatureValue])
verbFeatures katuyogata
    | is "子音動詞カ行促音便形"    = Just ([CCG.V5IKU, CCG.V5YUK], [CCG.Stem])
    | is "子音動詞カ行"           = Just ([CCG.V5k], [CCG.Stem])
    | is "子音動詞サ行"           = Just ([CCG.V5s], [CCG.Stem])
    | is "子音動詞タ行"           = Just ([CCG.V5t], [CCG.Stem])
    | is "子音動詞ナ行"           = Just ([CCG.V5n], [CCG.Stem])
    | is "子音動詞マ行"           = Just ([CCG.V5m], [CCG.Stem])
    | is "子音動詞ラ行イ形"        = Just ([CCG.V5NAS], [CCG.Stem])
    | is "子音動詞ラ行"           = Just ([CCG.V5r], [CCG.Stem])
    | is "子音動詞ワ行文語音便形"   = Just ([CCG.V5TOW], [CCG.Stem])
    | is "子音動詞ワ行"           = Just ([CCG.V5w], [CCG.Stem])
    | is "子音動詞ガ行"           = Just ([CCG.V5g], [CCG.Stem])
    | is "子音動詞バ行"           = Just ([CCG.V5b], [CCG.Stem])
    | is "母音動詞"              = Just ([CCG.V1], [CCG.Stem, CCG.Neg, CCG.Cont, CCG.NegL, CCG.EuphT])
    | otherwise = Nothing
  where is t = t `T.isPrefixOf` katuyogata

-- | KWJAが動詞と分析した語の語幹がマス (i,j) に一致し、そのマスに同じ表層形の動詞のLEXエントリーがないとき、
-- | KWJAの活用型・代表表記・格フレームから動詞の語彙項目を作る
-- | （例:「悲しめた」をKWJAは「悲しめる」(母音動詞) と分析するが、lightblueの辞書には「悲し(む)」しかないので、
-- |   語幹「悲しめ」の語彙項目を作る）
newVerbEntries :: [((Int, Int), (T.Text, T.Text, T.Text, [T.Text]))] -> Int -> Int -> [CCG.Node] -> [CCG.Node]
newVerbEntries verbStemSpans i j nodes =
    concat [ LB.verblex [TL.fromStrict stem] "(KWJA)" posF conjF (TL.fromStrict daihyo) (TL.fromStrict caseframe) LB.event
           | ((start, end), (stem, katuyogata, daihyo, args)) <- verbStemSpans
           , (start, end) == (i, j)
           , not (any (isVerbLexOf stem) nodes)
           , Just (posF, conjF) <- [verbFeatures katuyogata]
           , let caseframe = if null args then "ガ" else T.concat (sortJapanese args) ]
  where isVerbLexOf stem n = CCG.rs n == CCG.LEX && isPredicate "verb" (CCG.cat n) && CCG.pf n == TL.fromStrict stem

-- | KWJAが普通名詞と分析したスパンで、同じ表層形の普通名詞(N)と固有名詞(PN)の
-- | エントリーが両方ある場合、固有名詞のエントリーを除く
filterProperNames :: [(Int, Int)] -> Int -> Int -> [CCG.Node] -> [CCG.Node]
filterProperNames commonNounSpans i j nodes
    | (i, j) `notElem` commonNounSpans = nodes
    | otherwise = filter (not . isShadowedPN) nodes
  where
    commonNounForms = [CCG.pf n | n <- nodes, CCG.rs n == CCG.LEX, CCG.cat n == CCG.N]
    isShadowedPN n = CCG.rs n == CCG.LEX
                     && CCG.source n == "(PN)"
                     && CCG.pf n `elem` commonNounForms

getKWJAargs :: Maybe [KW.Arg] -> [T.Text]
getKWJAargs args = case args of
    Nothing -> []
    Just arg -> case arg of
        [] -> []
        (x : xs) -> normalizeArgType (KW.argType x) : getKWJAargs (Just xs)

-- | 全角数字を取り除く関数
normalizeArgType :: String -> T.Text
normalizeArgType input = 
    let noDigits = filter (not . isFullwidthDigit) input
    in T.pack  $ filter (`elem` args) noDigits
  where
    isFullwidthDigit c = let code = ord c in code >= 0xFF10 && code <= 0xFF19
    args = ['ヨ', 'ガ', 'ヲ', 'ニ', 'ト', 'ノ']


-- 動詞のCCGカテゴリーから項構造を取得
getCCGArg :: CCG.Cat -> [T.Text]
getCCGArg cat = case cat of
    CCG.BS x y -> getCCGArg x ++ getCCGArg y
    CCG.Sbar [CCG.F _] -> []
    -- xの中にCCG.Toがあるとき ["ト"]
    -- if CCG.ToCL `elem` x then ["ト"]
    -- else  []
    CCG.NP [CCG.F [CCG.Niyotte]] -> ["ヨ"]
    CCG.NP [CCG.F [CCG.Ga]] -> ["ガ"]
    CCG.NP [CCG.F [CCG.O]] -> ["ヲ"]
    CCG.NP [CCG.F [CCG.Ni]] -> ["ニ"]
    CCG.NP [CCG.F [CCG.To]] -> ["ト"] -- CCG.Toは実際にはない
    CCG.NP [CCG.F [CCG.No]] -> ["ノ"]
    _ -> []

createFilterFrom ::
    TL.Text -> 
    [KW.KWJAData] ->
    ConjMap ->
    -- | spans that KWJA analyzed as common nouns
    [(Int, Int)] ->
    -- | starting positions, surface forms and parts of speech of the words that KWJA analyzed as predicates
    [(Int, T.Text, T.Text)] ->
    -- | spans of the stems of the words that KWJA analyzed as verbs, with (stem, conjugation type, daihyo, case frame)
    [((Int, Int), (T.Text, T.Text, T.Text, [T.Text]))] ->
    -- | starting index of the span
    Int ->
    -- | ending index of the span
    Int ->
    -- | input nodes (to filter)
    [CCG.Node] ->
    -- | output nodes (filtered)
    [CCG.Node]
createFilterFrom text kwjaDatas conjMap commonNounSpans predicateSpans verbStemSpans i j ccgNodes =
    -- kwjaDataから、名詞、動詞、副詞、助詞の表層形を取得
    let openWords = getOpenWordsMap kwjaDatas conjMap
        -- 普通名詞と分析されたスパンでは固有名詞のエントリーを除く
        pnFilteredNodes = filterProperNames commonNounSpans i j ccgNodes
    -- 最後のマスのインデックスを取得
        startIndex = fromIntegral (TL.length text - 1)
        endIndex = fromIntegral $ TL.length text
    in
        -- i j が 最後のマスのときは動詞語幹をフィルターアウト
        -- 述語のエントリーが全部なくなる場合はゲートで元のエントリーを残す
        if (i,j) == (startIndex, endIndex)
            then createFilterFrom' openWords (filterStemEndParse pnFilteredNodes)
        else let filtered = restorePredicates predicateSpans i pnFilteredNodes (createFilterFrom' openWords pnFilteredNodes)
             in filtered ++ newVerbEntries verbStemSpans i j filtered


filterStemEndParse :: [CCG.Node] -> [CCG.Node]
filterStemEndParse [] = []
filterStemEndParse (c:cs) =
    -- featureを取得
    let (f1', f2') = LF.getFeatures (CCG.cat c) --  ([v:1],[stem,neg,cont,neg+l,euph:t])
    -- LEXかつverbかつstemをフィルターアウト
    in if (CCG.rs c == CCG.LEX) && (hasCommonElements f1' LB.verb) && (CCG.Stem `elem` f2')
        -- then D.trace ("filterStemEnd: " ++ (U.ushow $ CCG.sig c)) filterStemEndParse cs
        then filterStemEndParse cs
    else
        -- それ以外はそのまま
        c : filterStemEndParse cs
    where 
        hasCommonElements :: Eq a => [a] -> [a] -> Bool
        hasCommonElements list1 list2 = not (null (list1 `intersect` list2))

-- [("名詞"歌"),("動詞","歌")] -> toFilterNode -> FilteredNode
createFilterFrom' :: OpenWordsMap -> [CCG.Node] -> [CCG.Node]
-- CCGnodeが空の場合は空
createFilterFrom' _ [] = []
createFilterFrom' openWordsMap (c : cs)
    -- フィルターする対象がない場合
    | M.null openWordsMap = c:cs
    -- LEX以外はそのまま
    | CCG.rs c /= CCG.LEX =
        c : createFilterFrom' openWordsMap cs
    --　CompNは残す
    | CCG.source c == "(CompN)" =
        c : createFilterFrom' openWordsMap cs
    -- 名詞のフィルタリング
    | isNoun (CCG.cat c) =
        filterNodes ("名詞", "noun") openWordsMap c cs
    -- 述語のフィルタリング
    | isPredicate "verb" (CCG.cat c) =
        filterNodes ("動詞", "verb") openWordsMap c cs
    -- 形容詞のフィルタリング
    | isPredicate "adj" (CCG.cat c) =
        filterNodes ("形容詞", "adj") openWordsMap c cs
    -- 形容動詞のフィルタリング
    | isPredicate "nom" (CCG.cat c) =
        filterNodes ("形容詞", "nom") openWordsMap c cs
    -- その他
    | otherwise =
        c : createFilterFrom' openWordsMap cs

-- ("動詞", "verb") -> openwords ->  node -> nodes
filterNodes :: (T.Text, T.Text) -> OpenWordsMap -> CCG.Node -> [CCG.Node] -> [CCG.Node]
filterNodes (key, _) owmap node nodes = case M.lookup key owmap of
    -- [("歌","歌う"",["ガ","ヲ"]),("歌","歌う",["ガ"])]
    Just xs
        -- xsの中にCCGNodeの表層形と格フレームが一致するものがあれば、nodeは残す
        | (CCG.pf node, getCCGArg $ CCG.cat node) `elem` map toPair xs ->
            node : createFilterFrom' owmap nodes
        -- 表層形のみ一致し、格フレームが異なる場合
        | CCG.pf node `elem` map (TL.fromStrict . fst) xs -> 
            let newCaseFrame = getCaseframe (TL.toStrict $ CCG.pf node) xs
                (f1', f2') = LF.getFeatures (CCG.cat node)
                newNode = case uncons (CCG.sig node) of
                    Just _ -> let (text, _) = head (CCG.sig node)
                                   in LB.verblex [(CCG.pf node)] "(KWJA)" f1' f2' (LF.getDaihyo text) (TL.fromStrict newCaseFrame) LB.event
                    Nothing -> let text = CCG.pf node
                               in LB.verblex [(CCG.pf node)] "(KWJA)" f1' f2' (LF.getDaihyo text) (TL.fromStrict newCaseFrame) LB.event
            in newNode ++ createFilterFrom' owmap nodes
        -- 表層形も格フレームも一致しない場合
        | otherwise -> createFilterFrom' owmap nodes
    -- キーが存在しない場合
    Nothing -> node : createFilterFrom' owmap nodes
    where
    toPair (word, caseframe) = (TL.fromStrict word, sortJapanese caseframe)
    getCaseframe :: T.Text -> [(T.Text, [T.Text])] -> T.Text
    getCaseframe pf openWords = case openWords of
        [] -> ""
        (word, caseframe) : xs ->
            if pf == word
                then T.concat $ sortJapanese $ caseframe
                else getCaseframe pf xs

--CCGが名詞どうかの判定
isNoun :: CCG.Cat -> Bool
isNoun cat = case cat of
    CCG.N -> True
    CCG.NP _ -> True
    CCG.T _ _ _ `CCG.SL` (CCG.T _ _ _ `CCG.BS` (CCG.NP _)) -> True
    _ -> False

-- 動詞、形容詞、状詞
isPredicate :: T.Text -> CCG.Cat -> Bool
isPredicate pos cat = case pos of
    "verb" -> isPredicate' (LB.verb \\ [CCG.VK, CCG.VS, CCG.VSN, CCG.VZ]) cat
    "adj" -> isPredicate' LB.adjective cat
    -- "nom" -> False
    -- "nom" -> isPredicate' LB.nomPred cat
    "nom" -> isNaAdj [CCG.Nda, CCG.Nna, CCG.Nni] cat
    _ -> False

-- CCG.Catの素性を見てfeatureValueに入っているかを判定
isPredicate' :: [CCG.FeatureValue] -> CCG.Cat -> Bool
isPredicate' fvs cat = case cat of
    CCG.S _ -> False
    _ -> isPredicate'' cat
  where
    isPredicate'' c = case c of
        CCG.S f -> case head f of
            CCG.F fv ->
                if not $ null $ intersect fv fvs
                    then True
                    else False
            _ -> False
        -- S\Sは除外
        (CCG.S _) `CCG.BS` (CCG.S _) -> False
        -- 右が用言なら除外
        s `CCG.BS` x ->
            if isPredicate'' x
                then False
                else isPredicate'' s
        _ -> False

-- CCG.Catの素性を見てfeatureValueに入っているかを判定
isNaAdj :: [CCG.FeatureValue] -> CCG.Cat -> Bool
isNaAdj fvs cat = case cat of
    CCG.S _ -> False
    _ -> isNaAdj' cat
  where
    isNaAdj' c = case c of
        CCG.S f -> case head f of
            CCG.F fv ->
                -- catのfvがfvsと完全に一致する
                if fv == fvs
                    then True
                    else False
            _ -> False
        -- S\Sは除外
        (CCG.S _) `CCG.BS` (CCG.S _) -> False
        -- 右が用言なら除外
        s `CCG.BS` x ->
            if isNaAdj' x
                then False
                else isNaAdj' s
        _ -> False

-- openwordsのHashMapを作る
getOpenWordsMap :: [KW.KWJAData] -> ConjMap -> OpenWordsMap
getOpenWordsMap kwjas conjmap =
    let openWords = getOpenWords kwjas conjmap
     in M.fromListWith (++) $ map (\(pos, (stem, caseframes)) -> (pos, [(stem, caseframes)])) openWords
    --  in M.fromListWith (++) $ map (\(pos, (stem, caseframes)) -> (pos, [(stem, S.fromList caseframes)])) openWords

-- KWJADataからopenWordsとその項構造を抽出 -- [("動詞", ("歌", ["ガ","ヲ"]))]
getOpenWords :: [KW.KWJAData] -> ConjMap -> [(T.Text, (T.Text, [T.Text]))]
getOpenWords kwjaData conjmap = case kwjaData of
    [] -> []
    -- 2文以上の場合、1文目のEOSのあと2文目で再帰する
    (KW.Juman (J.EOS)) : xs -> getOpenWords xs conjmap
    -- knpNodeかつ後ろが動詞のときは、語幹と格フレームを取得
    (KW.KWJA kwjanode) : (x : x2 : xs) ->
        -- 複合動詞の場合
        if isCV x x2
            then case (x, x2) of
                (KW.Juman (J.JumanWord nyuryoku _ _ hinsi _ _ _ katuyogata _ katuyokei _ _), KW.Juman (J.JumanWord nyuryoku2 _ _ hinsi2 _ _ _ katuyogata2 _ katuyokei2 _ _)) ->
                    -- x2がisverbでないとき
                    if KF.isVerb katuyogata2
                        then do
                            -- 語幹を取得
                            let stem = KF.getStem nyuryoku katuyogata katuyokei conjmap
                                stem2 = KF.getStem nyuryoku2 katuyogata2 katuyokei2 conjmap
                                kwjaCaseFrame = getKWJAargs $ KW.args kwjanode
                            (hinsi, (stem, kwjaCaseFrame)) : (hinsi2, (stem2, kwjaCaseFrame)) : getOpenWords xs conjmap
                        else -- x2が["カ変動詞来","カ変動詞","サ変動詞","ザ変動詞"]のときは、x1のみ抽出
                        do
                            let stem = KF.getStem nyuryoku katuyogata katuyokei conjmap
                                kwjaCaseFrame = getKWJAargs $ KW.args kwjanode
                            (hinsi, (stem, kwjaCaseFrame)) : getOpenWords xs conjmap
                (_, _) -> getOpenWords (x2 : xs) conjmap
            else -- 複合動詞以外の場合
            case x of
                (KW.Juman (J.JumanWord nyuryoku _ _ hinsi _ _ _ katuyogata _ katuyokei _ _)) ->
                    -- 動詞のとき
                    if hinsi == "動詞" && KF.isVerb katuyogata
                        then do
                            -- 語幹と格フレームの取得
                            let stem = KF.getStem nyuryoku katuyogata katuyokei conjmap
                                kwjaCaseFrame = getKWJAargs $ KW.args kwjanode
                            (hinsi, (stem, kwjaCaseFrame)) : getOpenWords (x2 : xs) conjmap
                        else -- 形容詞のとき

                            if hinsi == "形容詞"
                                then do
                                    -- 語幹を取得
                                    let stem = KF.getStem nyuryoku katuyogata katuyokei conjmap
                                        kwjaCaseFrame = getKWJAargs $ KW.args kwjanode
                                    (hinsi, (stem, kwjaCaseFrame)) : getOpenWords (x2 : xs) conjmap
                                else --　名詞のとき

                                    if hinsi == "名詞"
                                        then (hinsi, (nyuryoku, [])) : getOpenWords (x2 : xs) conjmap
                                        else getOpenWords (x2 : xs) conjmap
                _ -> getOpenWords (x2 : xs) conjmap
    KW.Juman (J.AltWord _ _ _ _ _ _ _ _ _ _ _ _) : xs ->
        getOpenWords xs conjmap
    KW.Juman _ : xs -> getOpenWords xs conjmap
    (KW.Err _ _) : xs -> getOpenWords xs conjmap
    _ : xs -> getOpenWords xs conjmap
  where
    -- 引数2つともJumanDataで、動詞のときは複合動詞
    isCV :: KW.KWJAData -> KW.KWJAData -> Bool
    isCV (KW.Juman juman1) (KW.Juman juman2) = case (juman1, juman2) of
        (J.JumanWord _ _ _ hinsi _ _ _ _ _ _ _ _, J.JumanWord _ _ _ hinsi2 _ _ _ _ _ _ _ _) ->
            if (hinsi, hinsi2) == ("動詞", "動詞")
                then True
                else False
        _ -> False
    isCV _ _ = False

-- | 五十音順（濁点・半濁点を正規化して比較）
sortJapanese :: [T.Text] -> [T.Text]
sortJapanese t = sort t