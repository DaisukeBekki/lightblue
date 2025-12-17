{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface.Express.Sentence_process (
-- data
--    Test(..),
   InputSentences(..)
--  --type
--    jsemData,
--    emptyTest,
--  -- function
--    jsemSearch,
--    sentenceLookup,
--    eithertostring,
--    sentence_filter_count,
--    sentence_filter,
--    chart2nodelist,
--    maybe_nodes2nodes,
--    expressCat,
--    expressDtrs,
--    expressSem,
--    expressSig,
--    expressScore,
--    stlist2string,
--    scorelist2string,
--    add_space
) where

import  qualified Data.Text as StrictT
import  qualified Data.Text.Lazy as T
import  qualified Data.Map as Map
import  System.IO.Unsafe (unsafePerformIO)
-- import  qualified JSeM
-- import  qualified Corpus.JSeM as C
--import  Parser.CCG (Node(..),RuleSymbol(..),Cat(..),isBaseCategory,Feature(..),FeatureValue(..))
-- import qualified Parser.CCG as CCG
-- import  qualified Parser.ChartParser as CP

--Test：JSeMDataからjsem_id・answer・premises・hypothesisをとったもの
-- data Test = Test{
--      jsem_id_w    :: StrictT.Text,
--      answer_w     :: JSeM.JSeMLabel,
--      premises_w   :: [StrictT.Text],
--      hypothesis_w :: StrictT.Text
-- } deriving (Eq, Show)


--Sentences : 入力文
data InputSentences = InputSentences {
    input_Sentence :: StrictT.Text,
    sen_beam :: Int
} deriving (Eq,Show)

--文探索
-- type Sentence = T.Text

-- type SentenceMap = Map.Map Int Sentence

-- IOを取り除く
-- jsemData :: [C.JSeMData]
-- jsemData = unsafePerformIO C.fetchJSeMData


-- emptyTest :: Test
-- emptyTest = Test { jsem_id_w = "", answer_w = JSeM.OTHER, premises_w = [], hypothesis_w = "" } 


-- jsemSearch :: [C.JSeMData] -> String -> Test
-- jsemSearch jsemD sentID = do
--       case jsemD of
--           [] -> emptyTest
--           (C.JSeMData {C.jsem_id = i, C.link = l, C.description = d, C.answer = a, C.phenomena = ph, C.inference_type = it, C.note = n, C.premises = p, C.hypothesis = h} : xs )-> 
--                      -- StrictT.Text型に変換したsentIDとiが異なる場合
--                      if(i /= StrictT.pack sentID)
--                           then jsemSearch xs sentID
--                           else Test{jsem_id_w = i, answer_w = a, premises_w = p, hypothesis_w = h}


-- sentenceLookup :: Int -> SentenceMap -> Either Sentence Sentence
-- sentenceLookup number smap = case Map.lookup number smap of
--   Nothing -> Left $ "存在しません"
--   Just sentence -> Right sentence 


--Either型からString型にする
-- eithertostring :: Either Sentence Sentence -> Sentence
-- eithertostring result =
--   case result of Left sentence -> sentence
--                  Right sentence -> sentence


-- '。'を取り除いた文字数
-- sentence_filter_count :: StrictT.Text -> Int
-- sentence_filter_count sentence = case (StrictT.null sentence) of
--      True -> 0
--      False ->  let sen_filter = StrictT.filter (/='。') sentence
--                     in StrictT.length sen_filter


-- "。"をとって、(') を文の先頭と末尾につける
-- sentence_filter :: StrictT.Text -> String
-- sentence_filter sentence = case (StrictT.null sentence) of
--      -- nullだったら" "をsentenceに代入
--      True -> " "
--      -- sen_filter：sentenceから。を取り除いたStrictT.Text型
--      False -> let sen_filter = StrictT.filter (/='。') sentence in
--                    -- sen_filterの先頭に"をつける
--                    let plus_sen1 = StrictT.cons '"' sen_filter in
--                    -- plus_sen1の末尾に"をつける
--                    let plus_sen2 = StrictT.snoc plus_sen1 '"'
--                     -- Text型からString型にして返す
--                     in StrictT.unpack plus_sen2


-- CP.Chart型からデータを取り出し、それを [[CCG.Node]] 型のリスト nodelist に変換する   
-- chart2nodelist :: Int -> Int -> CP.Chart -> [[CCG.Node]] -> [[CCG.Node]]
-- chart2nodelist end start cp_chart nodelist = case end of
--     0 -> nodelist
--     otherwise -> case start of 
--                             -- cp_chartの(start, end)に対応するエントリを探して格納、Maybe型
--                             0 -> let maybe_node = Map.lookup (start, end) cp_chart
--                                       -- Maybe型を外す
--                                       in let node = maybe_nodes2nodes maybe_node
--                                       -- nodeをnodelistに追加し、新しいnodesリストを作成する
--                                       in let nodes = nodelist ++ [node]
--                                       -- endを1減らし、startもendより1減らす
--                                       in let end_re = end - 1
--                                       in let start_re = end_re - 1
--                                       -- 再帰的にchart2nodelistを呼び出す
--                                       in chart2nodelist end_re start_re cp_chart nodes
--                             otherwise -> let maybe_node = Map.lookup (start, end) cp_chart
--                                                   in let node = maybe_nodes2nodes maybe_node
--                                                   in let  nodes = nodelist ++ [node]
--                                                   -- startを1減らす
--                                                   in let start_re = start -1
--                                                   in chart2nodelist end start_re cp_chart nodes



-- maybe_nodes2nodes :: Maybe [CCG.Node] -> [CCG.Node]
-- maybe_nodes2nodes nodes = case nodes of
--        Nothing -> []
--        Just nodes -> nodes


-- expressCat ::  [CCG.Node] -> String
-- expressCat ccgnodes = case ccgnodes of
--    [] -> "Node_is_empty!"
--    nodes -> let one = head nodes -- one:nodesの最初の要素
--                     in show $ CCG.cat one -- 最初のnodeのcatをString型にして返す

-- expressDtrs ::  [CCG.Node] ->  String
-- expressDtrs ccgnodes = case ccgnodes of
--    [] -> "Node_is_empty!"
--    nodes -> let one = head nodes
--                     in show $ CCG.daughters one

-- expressSem ::  [CCG.Node] ->  String
-- expressSem ccgnodes = case ccgnodes of
--    [] -> "Node_is_empty!"
--    nodes -> let one = head nodes
--                     in show $ CCG.sem one

-- expressSig ::  [CCG.Node] ->  String
-- expressSig ccgnodes = case ccgnodes of
--    [] -> "Node_is_empty!"
--    nodes -> let one = head nodes
--                     in show $ CCG.sig one

-- expressScore ::  [CCG.Node] -> StrictT.Text
-- expressScore ccgnodes = case ccgnodes of
--     [] -> "No_socre!"
--     nodes -> let one = head nodes
--                      in T.toStrict $ CCG.showScore one
    

-- [Cat] -> "1_cat 2_cat ... n_cat " にする
-- stlist2string :: [String] -> String
-- stlist2string stringlist = case stringlist of
--               [] -> "Node_is_empty!"
--                         -- stlistの要素をString型からStrictT.Text型にする
--               stlist -> let st_textlist = map StrictT.pack stlist
--                          -- 各テキストの後ろにスペース' 'を入れる
--                          in let spacelist = add_space st_textlist
--                          -- Data.Text.concat :: [Text] -> Text
--                          in let concatcat = StrictT.concat spacelist
--                          -- Data.Text.cons :: Char -> Text -> Text
--                          -- テキストの前に文字"を入れる
--                          in let plus_sen1 = StrictT.cons '"' concatcat 
--                          -- テキストの最後に文字"を入れる
--                          in let plus_sen2 =StrictT.snoc plus_sen1 '"'
--                          -- Data.Text.unpack :: Text -> String
--                          in StrictT.unpack plus_sen2


-- [score] -> "1_score  2_score ... n_score " にする
-- scorelist2string :: [StrictT.Text] -> String
-- scorelist2string  stringlist = case stringlist of
--               [] -> "No_score!"
--                         -- -- 各テキストの後ろにスペース' 'を入れる
--               stlist -> let spacelist = add_space stlist
--                          -- Data.Text.concat :: [Text] -> Text
--                          in let concatcat = StrictT.concat spacelist
--                          -- Data.Text.cons :: Char -> Text -> Text
--                          -- テキストの前に文字"を入れる
--                          in let plus_sen1 = StrictT.cons '"' concatcat 
--                          -- テキストの最後に文字"を入れる
--                          in let plus_sen2 =StrictT.snoc plus_sen1 '"'
--                          -- Data.Text.unpack :: Text -> String
--                          in StrictT.unpack plus_sen2

-- -- リストを受け取って各Textの後ろに(' ')スペースを入れる
-- add_space :: [StrictT.Text] -> [StrictT.Text]
-- add_space textlist = case textlist of
--    [] -> []
--    x : xs -> StrictT.snoc x ' ' : add_space xs             