{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

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
import qualified Data.Set as S
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

type ConjMap = M.Map (T.Text, T.Text) T.Text
type OpenWordsMap = M.Map T.Text [(T.Text, S.Set T.Text)]

-- | kwjaを用いたフィルタリング関数を返す
kwjaFilter :: TL.Text -> IO (Int -> Int -> [CCG.Node] -> [CCG.Node])
kwjaFilter text = do
    kwjaDatas <- KW.callKWJA $ TL.toStrict text
    lb <- getEnv "LIGHTBLUE"
    katuyoulist <- JK.parseKatuyouFromPath $ concat [lb,"src/Parser/Language/Japanese/Juman/JUMAN.katuyou"]
    let conjmap = KF.getConjMap katuyoulist
        -- openWordsMap = getOpenWordsMap kwjaDatas conjmap
        filterNode = createFilterFrom kwjaDatas conjmap
    return filterNode

getKWJAargs :: Maybe [KW.Arg] -> [T.Text]
getKWJAargs args = case args of
    Nothing -> []
    Just arg -> case arg of
        [] -> []
        (x : xs) -> (T.pack $ KW.argType x) : getKWJAargs (Just xs)

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
    [KW.KWJAData] ->
    ConjMap ->
    -- | starting index of the span
    Int ->
    -- | ending index of the span
    Int ->
    -- | input nodes (to filter)
    [CCG.Node] ->
    -- | output nodes (filtered)
    [CCG.Node]
createFilterFrom kwjaDatas conjMap _ _ ccgNodes =
    -- knpDataから、名詞、動詞、副詞、助詞の表層形を取得
    let openWords = getOpenWordsMap kwjaDatas conjMap
    in createFilterFrom' openWords ccgNodes

-- [("名詞"歌"),("動詞","歌")] -> toFilterNode -> FilteredNode
createFilterFrom' :: OpenWordsMap -> [CCG.Node] -> [CCG.Node]
-- CCGnodeが空の場合は空
createFilterFrom' _ [] = []
createFilterFrom' openWordsMap (c : cs)
    -- フィルターする対象がない場合
    | M.null openWordsMap = cs
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
        | (CCG.pf node, S.fromList $ getCCGArg $ CCG.cat node) `elem` map toPair xs ->
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
    toPair (word, caseframe) = (TL.fromStrict word, caseframe)
    getCaseframe :: T.Text -> [(T.Text, S.Set T.Text)] -> T.Text
    getCaseframe pf openWords = case openWords of
        [] -> ""
        (word, caseframe) : xs ->
            if pf == word
                then T.concat $ S.toList caseframe
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
     in M.fromListWith (++) $ map (\(pos, (stem, caseframes)) -> (pos, [(stem, S.fromList caseframes)])) openWords

-- KWJADataからopenWordsとその項構造を抽出 -- [("動詞", ("歌", ["ガ","ヲ"]))]
getOpenWords :: [KW.KWJAData] -> ConjMap -> [(T.Text, (T.Text, [T.Text]))]
getOpenWords kwjaData conjmap = case kwjaData of
    [] -> []
    (KW.Juman (J.EOS)) : _ -> []
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
