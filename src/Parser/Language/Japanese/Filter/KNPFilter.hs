{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- {-|
-- Module      : KNPFilter
-- Licence     : LGPL
-- Copyright   : Asa Tomita
-- Stability   : beta
-- Filtering Function using KNP
-- -}


module Parser.Language.Japanese.Filter.KNPFilter (
    ConjMap,
    isVerb,
    getStem,
    getConjMap,
    knpFilter,
    myMapM
    ) where

import System.Environment (getEnv)
import qualified Text.KNP as K
import qualified Text.Juman as J
import qualified Parser.JumanKatuyou as JK
import qualified Parser.Language.Japanese.Filter.LightblueFilter as LF (getFeatures,getDaihyo)
import qualified Data.Text as T    --text
import qualified Data.Text.Lazy as TL --text
import qualified Parser.CCG as CCG   --lightblue
--import qualified Parser.ChartParser as LB  --lightblue
import qualified Parser.Language.Japanese.Templates as LB --lightblue
--import qualified Parser.Language.Japanese.Lexicon as LB () --lightblue
import qualified Parser.Language.Japanese.MyLexicon as LB (verblex)
import qualified Text.Show.Unicode as U
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace
import Data.List

type ConjMap = M.Map (T.Text,T.Text) T.Text
type OpenWordsMap = M.Map T.Text [(T.Text, S.Set T.Text)]


myMapM :: Monad m => (t -> m a) -> [t] -> m [a]
-- インデックスつきmap
myMap _ [] = []
-- mapして残りの数を表示
myMap f (x:xs) =
    trace (show $ (length xs) + 1) (f x : myMap f xs)
myMapM f as = sequence (myMap f as)


-- | テキストを受け取って、lightblueでパーズして返す
knpFilter :: TL.Text -> IO (Int -> Int -> [CCG.Node] -> [CCG.Node])
knpFilter text = do
    knp <- K.fromText $ TL.toStrict text
    lb <- getEnv "LIGHTBLUE"
    katuyoulist <- JK.parseKatuyouFromPath $ concat [lb,"src/Parser/Language/Japanese/Juman/JUMAN.katuyou"]
    U.uprint katuyoulist
    let knpData = map (K.knpParser) knp
        conjmap = getConjMap katuyoulist
        filterNode = createFilterFrom knpData conjmap
    return filterNode


getKNPArg :: Maybe [(T.Text, T.Text, Int)] -> [T.Text]
getKNPArg x = case x of
    Nothing -> []
    Just y -> case y of
        [] -> []
        (cf,_,_):xs -> cf : getKNPArg (Just xs)

-- 動詞のCCGカテゴリーから項構造を取得
getCCGArg :: CCG.Cat -> [T.Text]
getCCGArg cat = case cat of
    CCG.BS x y -> getCCGArg x ++ getCCGArg y
    CCG.Sbar [CCG.F x] ->
        -- xの中にCCG.Toがあるとき ["ト"]
        if CCG.ToCL `elem` x then ["ト"]
        else  []
    CCG.NP [CCG.F [CCG.Niyotte]] -> ["ヨ"]
    CCG.NP [CCG.F  [CCG.Ga]] -> ["ガ"]
    CCG.NP [CCG.F  [CCG.O]] -> ["ヲ"]
    CCG.NP [CCG.F  [CCG.Ni]] -> ["ニ"]
    CCG.NP [CCG.F  [CCG.To]] -> ["ト"]
    CCG.NP [CCG.F  [CCG.No]] -> ["ノ"]
    _ -> []

createFilterFrom ::
  [K.KNPData]
  -> ConjMap
  -> Int           -- ^ starting index of the span
  -> Int           -- ^ ending index of the span
  -> [CCG.Node]    -- ^ input nodes (to filter)
  -> [CCG.Node]    -- ^ output nodes (filtered)
createFilterFrom knpData conjMap _ _ ccgNodes = do
    -- knpDataから、名詞、動詞、副詞、助詞の表層形を取得
    let openWords = getOpenWordsMap knpData conjMap
    createFilterFrom' openWords ccgNodes

-- [("名詞"歌"),("動詞","歌")] -> toFilterNode -> FilteredNode
createFilterFrom' :: OpenWordsMap -> [CCG.Node] -> [CCG.Node]
-- CCGnodeが空の場合は空
createFilterFrom' _ [] = []
createFilterFrom' openWordsMap (c:cs) =
    -- フィルターする対象がない場合
    if openWordsMap == M.empty then cs
    --　重複を扱うためにキーをリストにする
    else do
        -- LEXで、表層形が一致する単語がopenWordsにある
        if (CCG.rs c == CCG.LEX) then
            -- 名詞
            if isNoun $ CCG.cat c then
                filterNodes ("名詞", "noun") openWordsMap c cs
            -- 動詞
            else if isPredicate "verb" $ CCG.cat c then
                filterNodes ("動詞","verb") openWordsMap c cs
            -- 形容詞
            else if isPredicate "adj" $ CCG.cat c then
                filterNodes ("形容詞", "adj") openWordsMap c cs
            -- 形容動詞は形容詞として分析される
            else if isPredicate "nom" $ CCG.cat c then
                filterNodes ("形容詞", "nom") openWordsMap c cs
            -- 名詞、動詞、形容詞以外はそのまま
            else c:createFilterFrom' openWordsMap cs
        -- LEX以外はそのまま
        else c:createFilterFrom' openWordsMap cs


-- ("動詞", "verb") -> openwords ->  node -> nodes
filterNodes :: (T.Text, T.Text) -> OpenWordsMap -> CCG.Node -> [CCG.Node] -> [CCG.Node]
filterNodes (key, enkey) owmap node nodes = case M.lookup key owmap of
    -- [("歌","歌う"",["ガ","ヲ"]),("歌","歌う",["ガ"])]
    Just xs ->
        -- xsの中にCCGNodeの表層形と格フレームが一致するものがあれば、nodeは残す
        if (CCG.pf node, S.fromList $ getCCGArg $ CCG.cat node) `elem` (map (\(word,caseframe) -> (TL.fromStrict word, caseframe)) xs) then
            node:createFilterFrom' owmap nodes
        else
            --  表層系のみ一致する場合（格フレームが異なる場合）
            if CCG.pf node `elem` (map (\(word, _) -> TL.fromStrict word) xs) then do
                let newCaseFrame = getCaseframe (TL.toStrict $ CCG.pf node) xs
                -- ["行","い","逝"] "(103)" [V5IKU] [Stem] "行く/いく" "ガニ" event,
                let (f1',f2') =  LF.getFeatures (CCG.cat node)
                case uncons $ CCG.sig node of
                    Just _ -> do
                        let (text,_) = head $ CCG.sig node
                        let newNode = LB.verblex [(CCG.pf node)] "(KNP)" f1' f2' (LF.getDaihyo text) (TL.fromStrict newCaseFrame) LB.event
                        newNode ++ createFilterFrom' owmap nodes
                    Nothing -> do
                        let text = CCG.pf node
                        let newNode = LB.verblex [(CCG.pf node)] "(KNP)" f1' f2' (LF.getDaihyo text) (TL.fromStrict newCaseFrame) LB.event
                        newNode ++ createFilterFrom' owmap nodes
            -- 表層系も一致しない場合
            else
            createFilterFrom' owmap nodes
    -- そもそも key がない
    Nothing -> node:createFilterFrom' owmap nodes
    where
         getCaseframe :: T.Text -> [(T.Text, S.Set T.Text)] -> T.Text
         getCaseframe pf openWords = case openWords of
            [] -> ""
            (word,caseframe):xs ->
                if pf == word then T.concat $ S.toList caseframe
                else getCaseframe pf xs

--名詞どうかの判定
isNoun :: CCG.Cat -> Bool
isNoun cat = case cat of
    CCG.N -> True
    CCG.NP _ -> True
    CCG.T _ _ _ `CCG.SL` (CCG.T _ _ _ `CCG.BS` (CCG.NP _)) -> True
    _ -> False

-- 動詞、形容詞、状詞
isPredicate :: T.Text -> CCG.Cat -> Bool
isPredicate pos cat = case pos of
    "verb" -> isPredicate' LB.verb cat
    "adj" -> isPredicate' LB.adjective cat
    "nom" -> isPredicate' LB.nomPred cat
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
                    if not $ null $ intersect fv fvs then True
                    else  False
                _ -> False
            -- S\Sは除外
            (CCG.S _) `CCG.BS` (CCG.S _) ->  False
            -- 右が用言なら除外
            s `CCG.BS` x ->
                if isPredicate'' x then  False
                else isPredicate'' s
            _ -> False

-- openwordsのHashMapを作る
getOpenWordsMap :: [K.KNPData] -> ConjMap -> OpenWordsMap
getOpenWordsMap knps conjmap =
    let openWords = getOpenWords knps conjmap in
       M.fromListWith (++) $ map (\(pos,(stem,caseframes)) -> (pos, [(stem, S.fromList caseframes)])) openWords

-- KNPDataからopenWordsとその項構造を抽出 -- [("動詞", ("歌", ["ガ","ヲ"]))]
getOpenWords :: [K.KNPData] -> ConjMap -> [(T.Text,(T.Text,[T.Text]))]
getOpenWords knpData conjmap = case knpData of
    [] -> []
    (K.Juman (J.EOS)):_ -> []
    -- knpNodeかつ後ろが動詞のときは、語幹と格フレームを取得
    (K.KNPNode _ _ _ (Just cat) _ caseFrame):(x:xs) -> case x of
        (K.Juman (J.JumanWord nyuryoku _ _ hinsi _ _ _ katuyogata _ katuyokei _ _)) ->
            -- 動詞のとき
            if T.isPrefixOf "用言" cat  && isVerb katuyogata then do
                -- 語幹と格フレームの取得
                let stem = getStem nyuryoku katuyogata katuyokei conjmap
                    knpCaseFrame = "ガ" : (getKNPArg caseFrame)
                (hinsi,(stem,knpCaseFrame)) : getOpenWords xs conjmap
            -- knpが動詞以外のとき
            else getOpenWords (x:xs) conjmap
        -- それ以外のとき
        _ -> getOpenWords (x:xs) conjmap
    K.Juman (J.JumanWord nyuryoku _ _ hinsi _ _ _ katuyogata _ katuyokei _ _):xs ->
        -- 形容詞のとき
        if hinsi `elem` ["形容詞"] && isVerb katuyogata then
            -- 語幹を取得
            let stem = getStem nyuryoku katuyogata katuyokei conjmap in
                (hinsi,(stem,[])) : getOpenWords xs conjmap
        --　名詞のとき
        else if hinsi `elem` ["名詞"] then
            (hinsi,(nyuryoku,[])) : getOpenWords xs conjmap
        else getOpenWords xs conjmap
    K.Juman (J.AltWord _ _ _ _ _ _ _ _ _ _ _ _):xs ->
        getOpenWords xs conjmap
    K.Juman _ : xs -> getOpenWords xs conjmap
    (K.Err _ _):xs -> getOpenWords xs conjmap
    _:xs -> getOpenWords xs conjmap

-- jumandataの活用系情報から語幹を計算する
getStem :: T.Text -> T.Text -> T.Text -> ConjMap -> T.Text
getStem nyuryoku katuyogata katuyokei conjmap = do
    let gobi = M.lookup (katuyogata,katuyokei) conjmap
    case gobi of
        Just gobi' -> do
            let split_stem = T.stripSuffix gobi' nyuryoku
            -- *　のときは何もしない
            case split_stem of
                Just stem -> stem
                Nothing -> nyuryoku
        -- 活用語尾がJUMAN.katuyouに登録されてない場合
        Nothing -> ""

-- 動詞かどうかの判定
isVerb ::  T.Text -> Bool
isVerb conj =
    -- 一旦カ変、サ変は除外
    if not $ conj `elem`  ["カ変動詞来","カ変動詞","サ変動詞","ザ変動詞"]
        then True
    else False

-- 活用形のHashMapを取得
getConjMap ::  [JK.JumanKatuyouGata] -> ConjMap
getConjMap jumankatuyougatas = do
    let conjList = getConjList jumankatuyougatas
    M.fromList conjList

getConjList :: [JK.JumanKatuyouGata] -> [((T.Text,T.Text),T.Text)] -- (活用型,活用形),入力, 開始位置
getConjList jumankatuyougatas =
    concat $ map getConjList' jumankatuyougatas
    where
        -- 再帰用関数
        getConjList' :: JK.JumanKatuyouGata -> [((T.Text,T.Text),T.Text)]
        getConjList' (JK.KatuyouGata (kg,kks)) = map (\(JK.KatuyouKei (kk,gobi)) -> ((kg,kk),gobi)) kks
        getConjList' _ = []
