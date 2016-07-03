{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Juman辞書ファイルをlightblueの辞書ファイルに変換する
--   To compile: cabal configure; cabal build; cabal install
--   To run: parseMorita > ../Juman.dic
--   Juman.dic: 応	11	動詞:ザ変動詞	応ずる	ContentW.dic
--              おう	11	動詞:ザ変動詞	応ずる	ContentW.dic

import Prelude 
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe as Maybe
import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.IO as S
import qualified Data.Time as Time
import Control.Monad

main :: IO()
main = do
  start <- Time.getCurrentTime
  S.hPutStrLn S.stderr "Reading Kyodai Case Frames..."
  jumancaseframe <- T.readFile "KawaharaFrame.txt"
  mapM_ (parseJumanFile (T.lines jumancaseframe)) [
    "dic/ContentW.dic",
    "dic/Noun.hukusi.dic", -- (名詞 (副詞的名詞 ((読み とおり)(見出し語 通り とおり)(意味情報 "代表表記:通り/とおり"))))
    "dic/Noun.koyuu.dic", -- (名詞 (人名 ((読み やまだ)(見出し語 山田)(意味情報 "人名:日本:姓:7:0.00607"))))
    "dic/Prefix.dic",
    "dic/Suffix.dic",
    "dic/Special.dic",
    "dic/Onomatopoeia.dic", -- Juman++にはないので注意
    --"dic/Noun.suusi.dic",
    --"dic/Postp.dic"   のエントリは MyLexicon.hs で別途定義
    --"dic/Assert.dic"  は不要
    --"dic/AuxV.dic"   のエントリは MyLexicon.hs で別途定義
    --"dic/Demonstrative.dic" のエントリは要対応
    "wiktionarydic/wiktionary.dic",
    "wikipediadic/wikipedia.dic",
    "autodic/Auto.dic"
    ]
  stop <- Time.getCurrentTime
  let time = Time.diffUTCTime stop start
  S.hPutStrLn S.stderr $ "Total Execution Time (ParseMorita): " ++ show time

-- | The function "parseJumanFile" parses a given Juman file.
-- 
parseJumanFile :: [T.Text]    -- ^ Juman case frames (splitted into a list of Text)
                  -> FilePath -- ^ The name of the juman file (that is currently processed)
                  -> IO()
parseJumanFile jumancaseframes jumanFile = do  
  S.hPutStrLn S.stderr $ "Processing Juman file:" ++ jumanFile
  jumandic <- T.readFile $ "/home/bekki/bigdata/JUMAN++/jumanpp-dic-20160401/"++jumanFile
  mapM_ (parseJumanLine jumanFile jumancaseframes) $ filter (/="") (T.lines jumandic) --empty lines are ignored.

parseJumanLine :: FilePath    -- ^ Juman file name (e.g. "dic/ContentW.dic")
                  -> [T.Text] -- ^ Juman Case Frame (e.g. Kawahara.txt, splitted by lines)
                  -> T.Text   -- ^ a line in a Juman dictionary
                  -> IO()
parseJumanLine jumanFile jcf text = 
      case parse jumanWordParser "" text of
        Left err -> do 
                    S.hPutStrLn S.stderr $ show err
                    T.hPutStrLn S.stderr text
        Right jwc -> mapM_ T.putStrLn $ encodeJWC jwc jcf (filePath2filename jumanFile)

filePath2filename :: FilePath -> T.Text
filePath2filename filepath =  T.intercalate "." $ init $ T.split (=='.') $ last $ T.split (=='/') $ T.pack filepath

-- | "JumanWordContainer" is a type for containing a juman word,
--  in the format:
--    JWC hinshi [(midashigo,score)] yomi map(=itemname,itemvalue) 
--      or
--    JRengo word1 word2
--      or
--    JComment comment (that follows ';' in a dictionary file)
data JumanWordContainer = 
  JWC T.Text [(T.Text,T.Text)] T.Text (M.Map T.Text T.Text)
  | JRengo JumanWordContainer JumanWordContainer
  | JComment T.Text
    deriving (Eq,Show)

-- | The function "encodeJumanWordContainer" returns a text encoding 
--  (which is a format used in our Japanese parser as a lexical entry)
--  of a given JumanWordContainer. Since a single JumanWordContainer may 
--  contain multiple 'midashigo', it returns a list of thier encodings.
encodeJWC :: JumanWordContainer -> [T.Text] -> T.Text -> [T.Text]
encodeJWC (JWC pos midashigoList yomi info) jumancaseframes source = 
  -- If "info" does not contain the representative form (=代表表記), 
  -- the first one of the "midashigoList" is used instead.
  let daihyo = case (M.lookup "代表表記" info) of 
                Just daihyopair  -> head $ T.split (=='/') daihyopair
                Nothing -> fst $ head $ midashigoList in
  --  If "midashigoList" does not contain the plain reading (=読み), 
  --  we add it with a low score.
  let midashigoList2 = case L.lookup yomi midashigoList of
                         Just _ -> midashigoList
                         Nothing -> midashigoList ++ [(yomi,"11")] in
  -- 見出し語は二番目以降、scoreを2ずつ下げる
  let midashigoList3 = degradeScore midashigoList2 in 
  case (M.lookup "可能動詞" info) of -- 可能動詞のエントリは辞書に登録しない
    Just _ -> []
    Nothing -> Maybe.mapMaybe (printJWC daihyo) midashigoList3 -- 返り値
    where printJWC daihyo (midashigo,score) = 
            let (midashigo2,daihyo2,yomi2,caseframe) = 
                  case () of
                    _ | ("動詞:サ変動詞" `T.isPrefixOf` pos || "動詞:ザ変動詞" `T.isPrefixOf` pos) -> (T.init $ T.init midashigo, daihyo, yomi, findCaseFrame daihyo jumancaseframes)
                      | "動詞:" `T.isPrefixOf` pos -> (T.init midashigo, daihyo, yomi, findCaseFrame daihyo jumancaseframes)
                      | "名詞:サ変名詞" `T.isPrefixOf` pos -> (midashigo, daihyo, yomi, findCaseFrame daihyo jumancaseframes)
                      | "形容詞:イ" `T.isPrefixOf` pos -> (T.init midashigo, daihyo, yomi, T.empty)
                      | "形容詞:ナ" `T.isPrefixOf` pos -> (T.init midashigo, T.init daihyo, T.init yomi, T.empty)
                      | "形容詞:タル" `T.isPrefixOf` pos -> (T.init $ T.init midashigo, T.init $ T.init daihyo, T.init $ T.init yomi, T.empty)
                      | otherwise -> (midashigo, daihyo, yomi, T.empty) in
            if (midashigo2, pos) `L.elem` blackList
            then Nothing
            else return $ T.concat [midashigo2,"\t",T.pack $ show $ score,"\t",pos,"\t",daihyo2,"\t",yomi2,"\t",source,"\t",caseframe]
-- | JRengo and JComment are discarded (i.e. returns []).  
encodeJWC (JRengo _ _) _ _ = []
encodeJWC (JComment _) _ _ = [] --[T.concat [comment,": ",source]]

degradeScore :: [(T.Text,T.Text)] -> [(T.Text,Integer)]
degradeScore midashigoList = degradeScoreLoop 0 midashigoList
  where degradeScoreLoop i ml = case ml of
          [] -> []
          ((midashigo,score):ms) -> let scoreInR = 109 - 1 * (read (T.unpack score)::Integer) in -- 1.0なら99, 1.6なら93
                                    (midashigo, scoreInR - i):(degradeScoreLoop 2 ms)
--                                    (midashigo, scoreInR - i):(degradeScoreLoop (i+1) ms)

-- | The function to search and find a caseframe (from jumancaseframes) 
--   with respect to the (midashigo, daihyo) pair.
findCaseFrame :: T.Text      -- ^ 代表表記
                 -> [T.Text] -- ^ A list of a data of the form "する/する	ガト#ガヲ#ガニト#ガ#ガヲニト#ガニ#ガヲニ"
                 -> T.Text
findCaseFrame daihyo jumancaseframes =
  let filteredList = filter (\l -> let midashigolist = (T.split (=='/') $ head l) in -- ^ e.g. ["走る","はしる"]
                                   daihyo `elem` midashigolist)
                                   $ map (T.split (=='\t')) jumancaseframes in
  if filteredList == []
    then T.empty
    else T.intercalate "#" $ distillList $ do
                                           cfsline <- filteredList 
                                           cf <- T.split (=='#') $ head $ tail cfsline
                                           guard (T.length cf <= 4)
                                           return cf
    -- ここでは単にfilteredListのheadを取るのではなしに、一度#でばらして、全部集めてリストを作り、    
    -- distillListで「漉す」作業をしたものを、KawaharaFrame.txtに登録している。

-- | リストから重複要素を削除
distillList :: Eq a => [a] -> [a]
distillList list = distillListLoop list [] 
  where distillListLoop from to = case from of
          [] -> to
          (x:xs) -> if x `elem` to
                       then distillListLoop xs to
                       else distillListLoop xs (x:to)

{-
-- | updateList listA listOflistB: updates listB in such a way that:
--     if some element (=b) of listOflistB is a sublist of listA, then replace b with listA
--     if some element (=b) of listOflistB is a superlist of listA, then no update.
updateList :: Eq a => [a] -> [[a]] -> [[a]]
updateList listA listOflistB = case listOflistB of
  [] -> [listA]
  (listB:listBs) -> case () of
                      _ | L.elem listA (L.subsequences listB) -> listOflistB
                        | L.elem listB (L.subsequences listA) -> updateList listA listBs
                        | otherwise -> listB:(updateList listA listBs)
-}

blackList :: [(T.Text, T.Text)] -- Juman辞書項目でCCG辞書から除きたいもののリスト
blackList = [
  --("彼","名詞:普通名詞"),
  --("彼女","名詞:普通名詞"),
  ("く","動詞:カ変動詞"),
  ("来","動詞:カ変動詞来"),
  ("やってく","動詞:カ変動詞"),
  ("おはしま","動詞:子音動詞サ行"),
  ("異","名詞:普通名詞"),
  ("来","名詞:普通名詞"),
  ("進","名詞:普通名詞"),
  ("それ","名詞:普通名詞"),
  ("誰","名詞:普通名詞"),
  ("ティエラデントロ","名詞:普通名詞"),
  ("何","名詞:普通名詞"),
  ("なに","名詞:普通名詞"),
  ("どこ","名詞:普通名詞"),
  ("っぽい","名詞:普通名詞"),
  ("で","接続詞"),
  ("多分","副詞"),
  ("べた","副詞"),
  ("ぽい","副詞"),
  ("あまり","形容詞:ナ形容詞"),
  ("とこ","名詞:副詞的名詞")
  ]

-- | Parsers for Juman dictionary
wordParser :: Parser T.Text
wordParser = do
  word <- many1 (try (letter <|> char '\"'))
  return $ T.pack word  

yomiParser :: Parser T.Text
yomiParser = do
  string "(読み"
  space
  yomi <- mixedParser
  try (char ')' <|> do{space; _ <- mixedParser; char ')'}) -- wikipedia.dicでは、語尾が長音のものについて「読み」が二つ指定されていることがある
  return yomi

{-
yomi2Parser :: Parser T.Text
yomi2Parser = do
  string "(読み"
  space
  yomi <- mixedParser
  space
  _ <- mixedParser
  char ')'
  return yomi
-}

midashigoListParser :: Parser [(T.Text,T.Text)]
midashigoListParser =
  try (do
    string "(見出し語"
    space
    midashigoList <- sepBy1 (midashigoWithScoreParser <|> midashigoParser) space
    char ')'
    return midashigoList
    )
  <|>
  try (do
    string "(見出し語"
    space
    midashigoList <- many1 midashigoWithScoreParser
    char ')'
    return midashigoList
    )

midashigoParser :: Parser (T.Text,T.Text)
midashigoParser =
  do
  midashigo <- mixedParser
  return $ (midashigo,(T.pack "10"))

midashigoWithScoreParser :: Parser (T.Text,T.Text)
midashigoWithScoreParser =
  do
  char '('
  midashigo <- mixedParser
  space
  d1 <- digit
  char '.'
  d2 <- digit
  char ')'
  --let ratio = toInteger (read [d1,d2]::Int) % 10
  return $ (midashigo,(T.pack [d1,d2]))

katsuyokeiParser :: Parser T.Text
katsuyokeiParser = do
  string "(活用型"
  space  
  katsuyokei <- wordParser
  char ')'
  return katsuyokei 

imijohoListParser :: Parser (M.Map T.Text T.Text)
imijohoListParser = do
  string "(意味情報"
  space
  char '\"'
  imijohoList <- sepBy1 (try imijohoParser <|> do{w <- mixedParser;return (w,T.empty)}) space
  string "\")"
  return $ M.fromList imijohoList 

mixedParser :: Parser T.Text
mixedParser = do
  word <- many1 (noneOf " \")")
  return $ T.pack word

imijohoParser :: Parser (T.Text,T.Text)
imijohoParser = do
  itemname <- wordParser
  char ':'
  item <- mixedParser 
  return (itemname,item) 

jumanWordParser :: Parser JumanWordContainer
jumanWordParser = do
  try ( do -- 任意スペース後に;が現れる行はコメント行
    spaces
    char ';'
    comment <- many anyChar
    return $ JComment (T.pack comment)
    )
  <|>
  -- (名詞 (普通名詞 ((読み あいきどう)(見出し語 合気道 (あいきどう 1.6))(意味情報 "代表表記:合気道/あいきどう カテゴリ:抽象物 ドメイン:スポーツ"))))
  try ( do 
    char '('
    pos <- wordParser
    space
    char '('
    subpos <- wordParser
    space
    char '('
    yomi <- yomiParser
    midashigoList <- midashigoListParser
    imimap <- imijohoListParser  
    string ")))"
    return $ JWC (T.intercalate ":" [pos,subpos]) midashigoList yomi imimap
    )
  <|>
  -- (動詞 ((読み あう)(見出し語 会う 逢う 遭う あう)(活用型 子音動詞ワ行)(意味情報 "代表表記:会う/あう 反義:動詞:分かれる/わかれる;動詞:別れる/わかれる")))
  -- (形容詞 ((読み あかい)(見出し語 赤い あかい)(活用型 イ形容詞アウオ段)(意味情報 "代表表記:赤い/あかい 名詞派生:赤/あか")))
  -- (形容詞 ((読み あいまいだ)(見出し語 曖昧だ (曖まいだ 1.6) (あい昧だ 1.6) (あいまいだ 1.6))(活用型 ナ形容詞)(意味情報 "代表表記:曖昧だ/あいまいだ")))
  try ( do 
    char '('
    pos <- wordParser
    space
    char '('
    yomi <- yomiParser
    midashigoList <- midashigoListParser
    katsuyokei <- katsuyokeiParser
    imimap <- imijohoListParser  
    string "))"
    return $ JWC (T.concat [pos,":",katsuyokei]) midashigoList yomi imimap
    )
  <|>
  try ( do  -- (活用型 ...)がないパターン
    char '('
    pos <- wordParser
    space
    char '('
    yomi <- yomiParser
    midashigoList <- midashigoListParser
    imimap <- imijohoListParser
    string "))"
    return $ JWC pos midashigoList yomi imimap
    )
  <|>
  -- (接尾辞 (形容詞性述語接尾辞 ((見出し語 (気だ 2.0)(げだ 2.0))(読み げだ)(活用型 ナノ形容詞)(意味情報 "代表表記:気だ/げだ 準内容語 カテゴリ:抽象物"))))
  try ( do --for Prefix.dic 接尾語
    char '('
    pos <- wordParser
    space
    char '('
    subpos <- wordParser
    space
    char '('
    midashigoList <- midashigoListParser
    optional space
    yomi <- yomiParser
    optional (try katsuyokeiParser)
    imimap <- imijohoListParser
    string ")))"
    return $ JWC (T.intercalate ":" [pos,subpos]) midashigoList yomi imimap
    )
  <|>
  -- (特殊 (句点 ((見出し語
  -- (特殊 (読点 ((見出し語
  -- (特殊 (括弧始
  -- (特殊 (括弧終
  try ( do -- for Special.dic 特殊
    char '('
    pos <- wordParser
    space
    char '('
    subpos <- wordParser
    space
    char '('
    midashigoList <- midashigoListParser
    yomi <- yomiParser
    string ")))"
    return $ JWC (T.intercalate ":" [pos,subpos]) midashigoList yomi M.empty
    )
  <|>
  -- (名詞 (普通名詞 ((読み アルバカーキー アルバカーキ)(見出し語 (アルバカーキー 1.1) (アルバカーキ 1.1))(意味情報 "自動獲得:Wikipedia Wikipediaページ内一覧:アメリカ合衆国における人体実験 読み不明"))))
  try ( do --for JUMAN++/wikipedia.dic
    char '('
    pos <- wordParser
    space
    char '('
    subpos <- wordParser
    space
    char '('
    yomi <- yomiParser
    midashigoList <- midashigoListParser
    imimap <- imijohoListParser
    string ")))"
    return $ JWC (T.intercalate ":" [pos,subpos]) midashigoList yomi imimap
    )
  <|>
  -- (連語 ((名詞 (地名 ((読み しもきた)(見出し語 下北)(意味情報 "代表表記:下北/しもきた 地名:日本:青森県:郡 連語")))) (接尾辞 (名詞性特殊接尾辞 ((見出し語 郡)(読み ぐん)(意味情報 "代表表記:郡/ぐん 住所末尾 準内容語 カテゴリ:組織・団体:場所-その他 連語"))))))
  try ( do --for Noun.koyuu.dic 連語
    string "(連語 ("
    word1 <- jumanWordParser
    space
    word2 <- jumanWordParser
    string "))"
    return $ JRengo word1 word2
    )

