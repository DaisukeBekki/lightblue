{-|
Juman辞書ファイルをlightblueの辞書ファイルに変換する
Usage: main > ../Juman.dic
Juman.dic: 応	11	動詞:ザ変動詞	応ずる	ContentW.dic
           おう	11	動詞:ザ変動詞	応ずる	ContentW.dic
-}

module Parser.Language.Japanese.Juman.ConvertJumanDic (
  buildJumanLexicon
  ) where

import System.FilePath ((</>))           --filepath
import Control.Monad (forM)              --base
import qualified Data.Map as M           --base
import qualified Data.List as L          --base
import qualified System.Environment as E --base
import qualified Data.Maybe as Maybe     --base
import qualified Data.Text as T          --text
import qualified Data.Text.IO as T       --text
import qualified System.IO as S          --base
import qualified Data.Time as Time       --time
import Parser.Language.Japanese.Juman.ParseJumanDic (JumanWordContainer(..),parseJumanDicList)
import Parser.Language.Japanese.Juman.Config (Config(..),fetchConfig)


buildJumanLexicon :: IO()
buildJumanLexicon = do
  start <- Time.getCurrentTime
  config <- fetchConfig
  let kyodaiCaseFrameFilePath = (moduleDir config) </> (kyodaiCaseFrameFileName config)
      jumanDicFilePath        = (moduleDir config) </> (jumanDicFileName config)
  jumanCaseFrames <- T.lines <$> T.readFile kyodaiCaseFrameFilePath -- ^ Juman case frames (splitted into a list of Text)
  S.hPutStrLn S.stderr "\nKyodai Case Frame loaded.\n\nParsing Juman dictionary..."
  jwcs <- parseJumanDicList -- :: [(FilePath,JWC)]
  S.withFile jumanDicFilePath S.WriteMode $ \h -> 
    mapM_ (mapM_ (T.hPutStrLn h) . uncurry (encodeJWC jumanCaseFrames)) jwcs
  stop <- Time.getCurrentTime
  let time = Time.diffUTCTime stop start
  S.hPutStrLn S.stderr $ "\nTotal Execution Time (buildJumanLexicon): " ++ show time
  --where jumanFileName = T.intercalate "." $ init $ T.split (=='.') $ last $ T.split (=='/') $ T.pack jumanFilePath

-- | This function returns a text encoding 
--  (which is a format used in our Japanese parser as a lexical entry)
--  of a given JumanWordContainer. Since a single JumanWordContainer may 
--  contain multiple 'midashigo', it returns a list of thier encodings.
encodeJWC :: [T.Text] -> FilePath -> JumanWordContainer -> [T.Text]
encodeJWC jumancaseframes source (JWC pos midashigoList yomi info) = 
  -- | If "info" does not contain the representative form (=代表表記), 
  -- | the first one of the "midashigoList" is used instead.
  let daihyo = case (M.lookup "代表表記" info) of 
                Just daihyopair  -> head $ T.split (=='/') daihyopair
                Nothing -> fst $ head $ midashigoList 
      -- | If "midashigoList" does not contain the plain reading (=読み), 
      -- | we add it with a low score.
      midashigoList2 = case L.lookup yomi midashigoList of
                         Just _ -> midashigoList
                         Nothing -> midashigoList ++ [(yomi,"11")]
      -- | 見出し語は二番目以降、scoreを2ずつ下げる
      midashigoList3 = degradeScore midashigoList2 in 
  case (M.lookup "可能動詞" info) of -- | 可能動詞のエントリは辞書に登録しない
    Just _ -> []
    Nothing -> Maybe.mapMaybe (printJWC daihyo) midashigoList3 -- | 返り値
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
            else return $ T.concat [midashigo2,"\t",T.pack $ show $ score,"\t",pos,"\t",daihyo2,"\t",yomi2,"\t",T.pack source,"\t",caseframe]
-- | JRengo and JComment are discarded (i.e. returns []).  
encodeJWC _ _ (JRengo _ _) = []
encodeJWC _ _ (JComment _) = [] --[T.concat [comment,": ",source]]
encodeJWC _ _ (ParseError _ _) = []

degradeScore :: [(T.Text,T.Text)] -> [(T.Text,Integer)]
degradeScore midashigoList = degradeScoreLoop 0 midashigoList
  where degradeScoreLoop i ml = case ml of
          [] -> []
          ((midashigo,score):ms) -> let scoreInR = 120 - 2 * (read (T.unpack score)::Integer) in -- 1.0なら100, 1.6なら88
                                    (midashigo, scoreInR - i):(degradeScoreLoop 1 ms)
--                                    (midashigo, scoreInR - i):(degradeScoreLoop (i+1) ms)

-- | The function to search and find a caseframe (from jumancaseframes) 
--   with respect to the (midashigo, daihyo) pair.
findCaseFrame :: T.Text -> [T.Text] -> T.Text
findCaseFrame daihyo jumancaseframes =
  let filteredList = filter (\l -> let midashigolist = (T.split (=='/') $ head l) in daihyo `elem` midashigolist) $ map (T.split (=='\t')) jumancaseframes in
  if filteredList == []
    then T.empty
    else T.intercalate "#" $ filter (\l -> length (T.split (==',') l) < 4) $ distillList $ concat $ concat $ map (map (T.split (=='#'))) $ map tail $ filteredList
         -- ここでは単にfilteredListのheadを取るのではなしに、一度#でばらして、全部集めてリストを作り、
         -- それをdistillListで「漉す」作業をしたものを、KawaharaFrame.txtに登録している。

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
  ("やってく","動詞:カ変動詞"),
  ("だ","名詞:普通名詞")
  ]

