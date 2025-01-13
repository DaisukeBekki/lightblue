{-# LANGUAGE DeriveGeneric #-}

-- {-|
-- Module      : LightblueFilter
-- Licence     : LGPL
-- Copyright   : Asa Tomita
-- Stability   : beta
-- An interface module with lightblue parser
-- -}

module Parser.Language.Japanese.Filter.LightblueFilter (
  reforge,
  reforgeWithTimeOut,
  noReforgeWithTimeOut,
  lookupChart,
  createFilterFrom',
  createFilterFrom,
  getCompVerbList,
  getFeatures,
  getDaihyo,
  removeEmptyCategory,
  getVerbPosConjDaihyo,
  writeCompVerbList
  ) where

import qualified Data.Text.Lazy as T --text
import qualified Parser.CCG as CCG   --lightblue
import qualified Parser.ChartParser as LB --lightblue
import qualified Parser.PartialParsing as LB --lightblue
import Parser.Language (defaultJpOptions)
import qualified Parser.Language.Japanese.Templates as CCG-- lightblue (verbSR,verbCat)
import qualified Parser.Language.Japanese.MyLexicon as CCG
import qualified Parser.Language.Japanese.Lexicon as CCG
import qualified Parser.Language.Japanese.Juman.CallJuman as JU
import qualified DTS.DTTdeBruijn as DTS
import qualified Parser.ABC as ABC
-- import qualified Interface as I -- lightblue
import qualified System.IO as S
import qualified Data.Map as M
-- import qualified Data.List
import qualified Debug.Trace as D
import qualified Text.Show.Unicode as U
-- import qualified Data.Map.Strict as M
import Data.Time -- 実行時間計測用
import System.IO.Unsafe
-- import Data.Char
import Data.List.Split
import System.FilePath  --filepath
import System.Timeout (timeout)
import Witherable(ordNub)

-- type VVMap = M.Map T.Text (T.Text, T.Text)

-- The type for node filters
type Filter = T.Text -> IO (Int -> Int -> [CCG.Node] -> [CCG.Node])

nullFilter :: Filter
nullFilter = \_ -> return (\_ _ -> id) 

-- MorphAnalyzerName = JUMAN | JUMANPP | KWJA
morphAnalyzer = JU.JUMANPP

ifDebug :: Maybe (Int,Int)
ifDebug = Nothing    -- No Dump
-- ifDebug = (10,12) -- Dump Nodes in the box (10,12)

lookupChart :: Int -> Int -> LB.Chart -> [CCG.Node]
lookupChart i j chart =
  case (M.lookup (i,j) chart) of Just list -> D.trace (U.ushow $ map CCG.cat list) list
                                 Nothing   -> []

-- 複合動詞のリストをファイルに書き込む
writeCompVerbList :: FilePath -> IO ()
writeCompVerbList filepath = do
  let textFilePath = addExtension (filepath </> "comp_verb") "txt"
  cs <- readFile textFilePath
  let list = lines cs
  let newList = ordNub list
  let writeFilePath = addExtension (filepath </> "ordNum_comp_verb") "txt"
  writeFile writeFilePath $ unlines newList

-- comp_verb.txtを読み込んで、fst,snd,fullのトリプレットを返す
getCompVerbList :: IO [(T.Text,T.Text,T.Text)]
-- getCompVerbList :: IO VVMap
getCompVerbList = do
  -- let dirName = takeDirectory filepath
  -- let baseName = takeBaseName filepath
  -- let textFilePath = addExtension (dirName </> "ABCverbs" </> baseName) "txt"
  let textFilePath = "./src/Text/comp_verb.txt"
  cs <- readFile textFilePath
  let list = lines cs
  let list' = map splitVerbs list
  -- リストの重複を削除
  let newList = ordNub list' in
    -- D.trace (U.ushow $ length newList) (return newList)
    -- return newList
    return newList
  where
    splitVerbs str = do
      let verblist = splitOn " " str
      let fstVerb = head verblist
      let sndVerb = head $ tail verblist
      let verb = fstVerb ++ sndVerb in
        (T.pack verb, T.pack fstVerb, T.pack sndVerb)

-- | convert ABCTree to linguistically-valid CCG trees
reforgeWithTimeOut ::
  [(T.Text,T.Text,T.Text)] -- compVerbList
  -> Int     -- ^ beam width
  -> ABC.ABCTree   -- ^ input ABCTree -> IO [CCG.Node] -- ^ output CCG trees
  -> IO (String,CCG.Node,LB.Chart)
reforgeWithTimeOut compVerbs beam abcTree = do
  --時間を出力
  time <- getZonedTime
  S.hPutStrLn S.stderr (show time)
  -- timeoutの処理 (5分)
  -- timeout :: Int -> IO a -> IO (Maybe a)
  reforgedTree <-  D.trace ((U.ushow sentence)++ "\n") (timeout (20 * 60 *1000*1000) $ LB.simpleParse' ifDebug beam True filterFunction $ T.fromStrict sentence)
  case reforgedTree of
    -- タイムアウトしなかった場合
    Just (nodes, chart) -> do
      return (T.unpack $ T.fromStrict sentence, head $ nodes, chart)
    -- タイムアウトした場合
    Nothing -> do
      -- dummyNode <- LB.simpleParse beam "a"
      (dummyN,dummyC) <- LB.simpleParse' ifDebug beam True nullFilter "a"
      -- タイムアウトした場合は標準出力にsentenceを表示
      D.trace ("timeout: ") (putStrLn $ T.unpack $ T.fromStrict sentence)
      return ("", head dummyN,dummyC)
  -- (nodes,_) <- D.trace ((U.ushow sentence)++ "\n") (LB.simpleParse' ifDebug beam False filter $ T.fromStrict sentence) -- | use chart for debug
  where
    sentence = ABC.abcTree2sentence abcTree
    filterFunction = D.trace ("createFilterFrom") (createFilterFrom compVerbs abcTree)


noReforgeWithTimeOut ::
 [(T.Text,T.Text,T.Text)] -- compVerbList
  -> Int     -- ^ beam width
  -> ABC.ABCTree   -- ^ input ABCTree -> IO [CCG.Node] -- ^ output CCG trees
  -> IO (String,CCG.Node,LB.Chart)
noReforgeWithTimeOut _ beam abcTree = do
  --時間を出力
  time <- getZonedTime
  S.hPutStrLn S.stderr (show time)
  -- timeoutの処理 (5分)
  -- timeout :: Int -> IO a -> IO (Maybe a)
  reforgedTree <-  D.trace ((U.ushow sentence)++ "\n") (timeout (15 * 60 *1000*1000) $ LB.simpleParse' ifDebug beam True filterFunction $ T.fromStrict sentence)
  case reforgedTree of
    -- タイムアウトしなかった場合
    Just (nodes, c) -> do
      return (T.unpack $ T.fromStrict sentence, head $ nodes,c)
    -- タイムアウトした場合
    Nothing -> do
      (dummyN,dummyC) <- LB.simpleParse' ifDebug beam True nullFilter "a"
      -- タイムアウトした場合は標準出力にsentenceを表示
      D.trace ("timeout: ") (putStrLn $ T.unpack $ T.fromStrict sentence)
      return ("", head dummyN,dummyC)
  -- (nodes,_) <- D.trace ((U.ushow sentence)++ "\n") (LB.simpleParse' ifDebug beam False filter $ T.fromStrict sentence) -- | use chart for debug
  where
    sentence = ABC.abcTree2sentence abcTree
    filterFunction = nullFilter

reforge ::
  [(T.Text,T.Text,T.Text)]
  -> Int     -- ^ beam width
  -> ABC.ABCTree   -- ^ input ABCTree -> IO [CCG.Node] -- ^ output CCG trees
  -> IO (String,CCG.Node)
reforge compVerbList beam abcTree = do
    -- 文の長さに制限をかける
  (nodes,_) <- D.trace ((U.ushow sentence)++ "\n" ++  (showIOUtcTime getCurrentTime)) (LB.simpleParse' ifDebug beam True filterFunction $ T.fromStrict sentence) -- | use chart for debug
  return  (T.unpack $ T.fromStrict sentence, head $ nodes)
  where
    sentence = ABC.abcTree2sentence abcTree
    filterFunction = createFilterFrom compVerbList abcTree

-- reforges ::
--   [(T.Text,T.Text,T.Text)]
--   -> Int     -- ^ beam width
--   -> ABC.ABCTree   -- ^ input ABCTree -> IO [CCG.Node] -- ^ output CCG trees
--   -> IO (String,[CCG.Node])
-- reforges compVerbList beam abcTree = do
--     -- 文の長さに制限をかける
--   (nodes,_) <- D.trace ((U.ushow sentence)++ "\n" ++  (showIOUtcTime getCurrentTime)) (LB.simpleParse' ifDebug beam True filter $ T.fromStrict sentence) -- | use chart for debug
--   return  (T.unpack $ T.fromStrict sentence, nodes)
--   where
--     sentence = ABC.abcTree2sentence abcTree
--     filter = createFilterFrom compVerbList abcTree


-- UTCTime型をIOモナドから取得してshows関数を適用し、文字列に変換する関数
showIOUtcTime :: IO UTCTime -> String
showIOUtcTime ioUtcTime = show $ unsafePerformIO ioUtcTime

-- -- chartを返す
-- reforgeC ::
--   [(T.Text,T.Text,T.Text)]
--   -> Int     -- ^ beam width
--   -> ABC.ABCTree   -- ^ input ABCTree
--   -> IO LB.Chart -- ^ output Chart
-- reforgeC compVerbList beam abcTree = do
--   let sentence = ABC.abcTree2sentence abcTree
--   let filterFunction = createFilterFrom compVerbList abcTree
--   (_,chart) <- LB.simpleParse' ifDebug beam False filterFunction $ T.fromStrict sentence -- | use chart for debug
--   return chart

-- -- | convert ABCTree to linguistically-valid CCG trees
-- reforge' ::
--   [T.Text]
--   -> Int     -- ^ beam width
--   -> ABC.ABCTree   -- ^ input ABCTree
--   -> IO [CCG.Node] -- ^ output CCG trees
-- reforge' compVerbList beam abcTree = do
--   let sentence = ABC.abcTree2sentence abcTree
--   (nodes,chart) <- LB.simpleParse' ifDebug beam True nullFilter $ T.fromStrict sentence -- | use chart for debug
--   return nodes

-- reforgeC' :: Int     -- ^ beam width
--   -> ABC.ABCTree   -- ^ input ABCTree
--   -> IO LB.Chart -- ^ output CCG trees
-- reforgeC' beam abcTree = do
--   let sentence = ABC.abcTree2sentence abcTree
--   (_,chart) <- LB.simpleParse' ifDebug beam False nullFilter $ T.fromStrict sentence -- | use chart for debug
--   return chart

-- | Chartの(i,j)区間のccgNodeのうち、用言については、ABCTreebankから得られた同区間の用言リストに
-- | （対応するものが）含まれるもののみに絞り、用言以外はそのまま、とするフィルターを返す。
createFilterFrom ::
 [(T.Text,T.Text,T.Text)]
  -> ABC.ABCTree
  -> T.Text 
  -> IO (Int           -- ^ starting index of the span
        -> Int           -- ^ ending index of the span
        -> [CCG.Node]    -- ^ input nodes (to filter)
        -> [CCG.Node]    -- ^ output nodes (filtered)
        )
createFilterFrom compVerbList abcTree _ = -- i j ccgNodes = 
  return $ \i j ccgNodes ->
    -- ABCの用言のリストのうち、Chartの(i,j)区間のみ取り出す
    let abcVerbList = filter (\(_,_,start,end) -> if (start,end+1) == (i,j)
      -- ABCの用言のリストのstartとendがChartの(i,j)区間と一致するならばTrueそうでなければFalse
                                                    then True
                                                    else False) $ ABC.abcTree2verbList abcTree in
      -- createFilterFrom' (Chart(i,j)区間の用言のカテゴリーのリスト) (ccgNode)
      -- D.trace (show(i,j)++"\n"++U.ushow(abcVerbList)) $ createFilterFrom' (map (\(abcPf,abcCat,_,_) -> (T.fromStrict abcPf, abcCat)) abcVerbList) ccgNodes
    D.trace ("createFilterFrom'") createFilterFrom' compVerbList (map (\(abcPf,abcCat,_,_) -> (T.fromStrict abcPf, abcCat)) abcVerbList) ccgNodes

-- proなどを削除する
removeEmptyCategory :: String -> String
removeEmptyCategory str =
  let alpha = ['a'..'z'] ++ ['-'] in
    filter (not . (`elem` alpha)) str

-- | createFilterFrom の内部ループ。ccgNodesで再帰。
createFilterFrom' ::
  [(T.Text,T.Text,T.Text)]
  ->[(T.Text, ABC.CCGcat)] -- ^ ABCの(表層形,統語範疇)
  -> [CCG.Node]    -- ^ input nodes (to filter)
  -> [CCG.Node]    -- ^ output nodes (filtered)
createFilterFrom' _ [] cs = cs
createFilterFrom' _ _ [] = [] -- ccgNodesが空リストなら空リストを返す
createFilterFrom' compVerbList abcVerbList (c:cs) =
  let (abcPfs,_) = unzip abcVerbList in
    -- cの表層形とabcCatsの表層形が一致かつcが述語
    if (any (== (CCG.pf c)) abcPfs) && (isPredicate' $ CCG.cat c)
      -- abcCat : ABCverbListのうち、cと一致するものの統語範疇
      then let abcCat = lookup' (CCG.pf c) abcVerbList in
        -- newNode : 書き換えた新しいCCGNode
        let newNode = updateCat c abcCat in
          -- D.trace ("match \n " ++ (U.ushow $ CCG.cat c) ++ "\n" ++ (U.ushow $ CCG.sig c) ++ "\nnew Node:" ++ (U.ushow $ CCG.cat newNode)++(U.ushow $ CCG.pf newNode)++"\n") (newNode: createFilterFrom' compVerbList abcVerbList cs)
          D.trace ("match \n " ++ (U.ushow c) ++ "\nnew Node:" ++ (U.ushow $ CCG.cat newNode)++(U.ushow $ CCG.pf newNode)++"\n") (newNode: createFilterFrom' compVerbList abcVerbList cs)
        -- newNode: createFilterFrom' abcVerbList cs
    else do
      -- proなどを削除した表層形
      let pf = removeEmptyCategory $ T.unpack $ CCG.pf c
      -- compVerbListの中身を分解
      -- unzip3 :: [(a, b, c)] -> ([a], [b], [c])
      let (compVerbs, _ , _) = unzip3 compVerbList in
        -- もし複合動詞で、cが述語だった場合
        if ((T.pack pf) `elem` compVerbs) && (isPredicate' $ CCG.cat c) then do
           -- 格フレーム
          let abcCat = lookup' (CCG.pf c) abcVerbList
          let caseframe = getCaseframe $ createStructure (discardCCGStructure $ CCG.cat c) abcCat
          -- 辞書を作る
          let (_,fstVerb,sndVerb) = D.trace ("lookupCompVerbs:" ++ (U.ushow $ CCG.pf c)) lookupCompVerbs (T.pack pf) compVerbList
          -- 複合動詞の二つ目の動詞の活用形を探す
          -- let ((f1,f2),daihyo) = unsafePerformIO $ getVerbPosConjDaihyo fstVerb sndVerb
          let (daihyo) = unsafePerformIO $ getVerbPosConjDaihyo fstVerb sndVerb
          let (f1',f2') = getFeatures (CCG.cat c)
          -- daihyo
          -- let daihyo = getDaihyo' (CCG.sig c) fstVerb
          -- verblex ["き","やってき"] "(155)" [VK] [Cont,ModM,EuphT] "来る/くる" "ガニ" event,
          -- verblistが空（動詞じゃなかったときなど）
          if daihyo == "" then D.trace ("noUpdate:" ++ U.ushow ((f1',f2'),daihyo)) c:createFilterFrom' compVerbList abcVerbList cs
          else
            let newNodes = CCG.verblex [(T.pack pf)] "(ABC)" f1' f2' daihyo caseframe CCG.event in
              D.trace ((U.ushow $ CCG.cat c) ++ "\t" ++ (U.ushow $ CCG.pf c) ++ "\n" ++ "update: " ++ U.ushow newNodes) (head newNodes : createFilterFrom' compVerbList abcVerbList cs)
        -- 複合動詞じゃない場合はそのまま
        else c:createFilterFrom' compVerbList abcVerbList cs

getVerbPosConjDaihyo :: T.Text -> T.Text -> IO (T.Text)
getVerbPosConjDaihyo fstverb sndverb = do
  --lexResource <- CCG.lexicalResourceBuilder morphAnalyzer
  (_,lexicon) <- CCG.setupLexicon defaultJpOptions sndverb
  -- let lexicon = CCG.lookupLexicon sndverb lexicon'
  if null lexicon then D.trace ("Empty lexicon") (return $ (""))
  else do
    -- lexiconのcatが述語のもの(isPredicate'がTrueのもの)
    -- let filter_lex' = D.trace ("filter_lex': " ++ (U.ushow $ filter (\x -> isPredicate' (CCG.cat x)) lexicon) ) filter (\x -> isPredicate' (CCG.cat x)) lexicon
    let filter_lex' = filter (\x -> isPredicate' (CCG.cat x)) lexicon
    -- sigの前半部分がsndverbと一致
    -- CCG.sig x は [[(T.Text,Preterm)]]
    -- let filter_lex = D.trace ("filter_lex:" ++ (U.ushow $ filter (isSig sndverb) filter_lex')) filter (isSig sndverb) filter_lex'
    let filter_lex = filter (isSig sndverb) filter_lex'
    if null filter_lex then D.trace ("null_filter: " ++ (U.ushow $ T.append fstverb sndverb)) return $ T.append fstverb sndverb
    else
      let lex_sig = head $ map CCG.sig filter_lex in
      -- filter_lexのheadでgetFeaturesする
        return $ D.trace("lex_sig") (getDaihyo' lex_sig fstverb sndverb)
      -- return $ D.trace("lex_cats: " ++ U.ushow lex_cats ++ "\nlex_sig: "++ U.ushow lex_sig) (getFeatures $ head lex_cats, getDaihyo' lex_sig fstverb sndverb)
    where
      isSig :: T.Text -> CCG.Node -> Bool
      isSig verb node  =
        -- [(T.Text,Preterm)]
        let (daihyo,_):_ = CCG.sig node in
          -- daihyoの一番左を取り出す
          let leftDaihyo = head $ T.splitOn "/" daihyo in
            if verb == leftDaihyo then True
            else False


getDaihyo' :: DTS.Signature -> T.Text -> T.Text -> T.Text
getDaihyo' sig fstverb sndverb =
  -- []だったらfstが取れないのでfstとsndをappend
  if null sig then D.trace ("null sig") T.append fstverb sndverb
  else
    let daihyo = fst $ head sig in
      let splitList = T.splitOn "/" daihyo in
        T.append fstverb (head splitList)

-- (pos:(conj:pmf)) を取り出す
-- ([v:1],[stem,neg,cont,neg+l,euph:t])
getFeatures :: CCG.Cat -> ([CCG.FeatureValue],[CCG.FeatureValue])
getFeatures ccgCat = case ccgCat of
  CCG.BS x y -> case (x,y) of
    (CCG.S (pos:(conj:_)) , _) -> (getFeatureValue pos, getFeatureValue conj)
    _ ->  getFeatures x
  CCG.SL x _ -> getFeatures x
  _ -> ([],[])
  where
    getFeatureValue feature = case feature of
      CCG.F featureValue -> featureValue -- featureValue :: [FeatureValue]
      CCG.SF _ featureValue -> featureValue

-- "遊び疲れ" -> CompVerbList -> ("遊び疲れ","遊び","疲れ")
lookupCompVerbs :: T.Text -> [(T.Text,T.Text,T.Text)] -> (T.Text,T.Text,T.Text)
lookupCompVerbs text verblist = case verblist of
  [] -> ("","","")
  (verb,fstVerb,sndVerb):xs ->
    if text == verb then  (verb,fstVerb,sndVerb)
    else lookupCompVerbs text xs



-- | lookupのabcVerbList版
lookup' ::  T.Text -> [(T.Text, ABC.CCGcat)] -> ABC.CCGcat
lookup' pf list = case lookup pf list of
  Just x -> x
  -- リストになかったときはリストの先頭の統語範疇を返す
  Nothing -> snd $ head list

-- Nodeのcat部分を更新
updateCat :: CCG.Node -> ABC.CCGcat -> CCG.Node
updateCat ccg abc =
  let cat = CCG.cat ccg in
    let newCat = createStructure (discardCCGStructure cat) abc in
    -- sig :: Signature　「学生」　student(x) student:entity→type
      -- |type Signature = [(T.Text,Preterm)]
    -- sem :: Preterm
      let (text,_) = head $ CCG.sig ccg in -- PretermはCCG.eventを使う
    -- verbSR "如し/ごとし" event "ガト"
    --verbSR : T.Text -> Preterm -> T.Text -> (Preterm, Signature)
        let (sem,sig) = CCG.verbSR (getDaihyo text) CCG.event (getCaseframe newCat) in
        -- D.trace ("sig:\n" ++ (U.ushow $ CCG.sig ccg) ++ "\npf:\n" ++ (U.ushow $ CCG.pf ccg))  (ccg {
        ccg {
          CCG.sig = sig,
          CCG.sem = sem,
          CCG.cat = newCat}

-- "回る/まわる/ガヲ　"から　"回る/まわる"を取り出す
getDaihyo :: T.Text -> T.Text
getDaihyo text =
  -- list :: (回る,まわる,ガヲ)　や　(無い/ない)
  let list = T.splitOn "/" text in
    -- 3のときは3つ目が格フレーム
    if (length list == 3) then
      -- (回る/まわる,ガヲ)
      T.intercalate "/" $ init list
    else
      -- 格フレームがない場合
      T.intercalate "/" list


-- CCGcatから格フレームのみ取り出す
getCaseframe :: CCG.Cat -> T.Text
getCaseframe cat =
  T.intercalate "" $ getCaseframe' cat
  where
  -- getCaseframe' :: CCG.Cat -> [T.Text]
  getCaseframe' cat' = case cat of
    CCG.SL x y -> getCaseframe' x ++ getCaseframe' y
    CCG.BS x y -> getCaseframe' x ++ getCaseframe' y
    _ -> getCaseframe'' cat' : []
  -- getCaseframe'' :: CCG.Cat -> T.Text
  getCaseframe'' cat' =
    case cat' of
    CCG.NP [CCG.F [CCG.Ga]] -> "ガ"
    CCG.NP [CCG.F [CCG.O]] -> "ヲ"
    CCG.NP [CCG.F [CCG.Ni]] -> "ニ"
    CCG.NP [CCG.F [CCG.To]] -> "ト"
    CCG.NP [CCG.F [CCG.Niyotte]] -> "ヨッテ"
    CCG.NP [CCG.F [CCG.No]] -> "ノ"
    _ -> ""

-- | ccgCatが（日本語文法上）述語ならTrue，そうでなければaを返す
-- isPredicate :: CCG.Cat -> Bool
-- isPredicate ccgCat = case ccgCat of
--   CCG.BS x y -> case (x,y) of
--                   (CCG.S _, CCG.NP _) -> True
--                   (CCG.S _, CCG.Sbar _) -> True
--                   _ -> isPredicate x
--   CCG.SL x _ -> isPredicate x
--   _ -> False


-- catのfeatureのAとNで始まるやつをはじく
isPredicate' :: CCG.Cat -> Bool
isPredicate' ccgCat = case ccgCat of
  CCG.BS x y -> case (x,y) of
    (CCG.S (pos:(_:_)),  CCG.NP _) -> case pos of -- pos: Feature
      CCG.F featureValue -- featureValue: [FeatureValue]
        -- | elemOf featureValue -> D.trace ("ignore :"++ U.ushow pos) False
        | elemOf featureValue ->  False
        | otherwise -> True
      CCG.SF _ featureValue
        -- | elemOf featureValue ->  D.trace ("ignore :"++ U.ushow pos) False
        | elemOf featureValue ->  False
        | otherwise -> True
    (CCG.S (pos:(_:_)), CCG.Sbar _) -> case pos of -- pos :Feature
      CCG.F featureValue
        -- | elemOf featureValue -> D.trace ("ignore :"++ U.ushow pos) False
        | elemOf featureValue ->  False
        | otherwise -> True
      CCG.SF _ featureValue
        -- | elemOf featureValue -> D.trace ("ignore :"++ U.ushow pos) False
        | elemOf featureValue ->  False
        | otherwise -> True
    _ -> isPredicate' x
  CCG.SL x _ -> isPredicate' x
  _ -> False
  -- featureの要素いずれかがadj/nonPredならばTrueになる関数
  where
    elemOf list =
    -- D.trace (U.ushow list) (or (map ((\l x -> elem x l) (CCG.adjective ++ CCG.nomPred)) list))
      or (map ((\l x -> elem x l) (CCG.adjective ++ CCG.nomPred)) list)


-- -- | ccgCatと、abcCatが対応関係にあるとみなす。
-- correspondsTo :: CCG.Cat -> ABC.CCGcat -> Bool
-- correspondsTo ccgCat abcCat = case (ccgCat, abcCat) of
--   (CCG.S _, a) -> a `elem` [ABC.S,ABC.Sa,ABC.Se,ABC.Simp,ABC.Sm,ABC.Snml,ABC.Srel,ABC.Srel,ABC.Ssmc,ABC.Ssub]
--   (CCG.NP _, a) -> a `elem` [ABC.NP,ABC.NPq,ABC.NPR,ABC.PP,ABC.PPs,ABC.PPs2,ABC.PPo1,ABC.PPo2]
--   (CCG.N, a) -> a `elem` [ABC.N,ABC.Ns]
--   (CCG.Sbar _, a) -> a `elem` [ABC.CP,ABC.CPf,ABC.CPt,ABC.CPt_sbj,ABC.CPq,ABC.CPq_sbj,ABC.CPx]
--   (CCG.SL x y, ABC.Slash x' y') -> correspondsTo x x' && correspondsTo y y'
--   -- (CCG.BS x y, ABC.BSlash x' y') -> correspondsTo x x' && correspondsTo y y'
--   (CCG.BS x y, ABC.BSlash y' x') -> correspondsTo x x' && correspondsTo y y'
--   _ -> False

-- | ABCのカテゴリ(PPsなど)をlightblueのカテゴリ(NPgaなど)に変換する
abc2ccgCat :: ABC.CCGcat -> CCG.Cat
abc2ccgCat abc = case abc of
  abc'
    -- N : 名詞
    | abc' `elem` [ABC.N, ABC.Ns] -> CCG.N
    -- NP : 名詞句
    | abc' `elem` [ABC.NP, ABC.NPq] -> CCG.NP [CCG.F [CCG.Exp]] --直す
    -- PPs : 第一主語, PPs2 : 第2主語
    | abc' `elem` [ABC.PPs,ABC.PPs2] -> CCG.NP [CCG.F [CCG.Ga]]
    -- PPo1 : 第一目的語
    | abc' == ABC.PPo1 -> CCG.NP [CCG.F [CCG.O]]
    -- PPo2 : 第二目的語
    | abc' == ABC.PPo2 -> CCG.NP [CCG.F [CCG.Ni]]
    | otherwise -> CCG.LPAREN

-- | lightblueのボックスの中の述語の項構造を捨てる
discardCCGStructure :: CCG.Cat -> CCG.Cat
discardCCGStructure ccg = case ccg of
  CCG.SL x _ -> discardCCGStructure x
  CCG.BS x _ -> discardCCGStructure x
  CCG.S feature -> CCG.S feature
  -- x/y x\y, S 以外の場合はないはず？？
  _ -> CCG.N

-- | lightblueのSとABCのNPgaを組み合わせる
createStructure :: CCG.Cat -> ABC.CCGcat -> CCG.Cat
createStructure ccgCat abcCat = case abcCat of
  ABC.Slash y x -> CCG.SL (createStructure ccgCat y) (abc2ccgCat x)
  ABC.BSlash x y -> CCG.BS (createStructure ccgCat y) (abc2ccgCat x)
  abcCat'
    | abcCat' `elem` [ABC.S, ABC.Sa, ABC.Se, ABC.Simp, ABC.Sm, ABC.Snml, ABC.Srel, ABC.Ssmc, ABC.Ssub] -> ccgCat
  _ -> CCG.LPAREN

