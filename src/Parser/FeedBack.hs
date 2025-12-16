{-# LANGUAGE OverloadedStrings #-}

module Parser.FeedBack where

import qualified Parser.CCG as CCG
import qualified Parser.ChartParser as CCG
import qualified DTS.DTTdeBruijn as DTT        
import qualified Debug.Trace as D
import qualified Parser.LangOptions as L
import Parser.PartialParsing (simpleParse)
import qualified Data.Text.Lazy as TL 
import qualified Data.Text as T         
import qualified Text.Show.Unicode as U
import Interface.GPT (callGPT)
import Data.List (nub)
import System.IO.Unsafe (unsafePerformIO)
import ListT (ListT(..),fromFoldable,toReverseList,take,uncons,cons) --list-t
import qualified Text.Show.Unicode as U -- unicode-show




-- | Node を再帰的に巡回して (Signature, Cat) のリストを返す
extractSigCatFromNode :: CCG.Node -> [(DTT.Signature, CCG.Cat)]
extractSigCatFromNode node =
    (CCG.sig node, CCG.cat node) : []
    -- (CCG.sig node, CCG.cat node) : concatMap extractSigCatFromNode (CCG.daughters node)
            
            
-- | LeafNodeのSignatureを集める
extractLeafSigFromNode :: CCG.Node -> [(DTT.Signature, CCG.Cat)]
extractLeafSigFromNode node =
    if null (CCG.daughters node) && not (CCG.sig node == [])
    then [(CCG.sig node, CCG.cat node)]
    else concatMap extractLeafSigFromNode (CCG.daughters node)
    
-- 署名の中の型（各ペアの snd 部分）を抜き出すヘルパー
sigTypes :: DTT.Signature -> [DTT.Preterm]
sigTypes s = map snd s

-- sigs1 と sigs2 の間で、
-- 「sigs1 の要素 (sig1,cat1) と sigs2 の要素 (sig2,cat2) の cat が等しく、
-- かつ sig1 と sig2 のいずれかの署名ペアの snd (= 型) が一致する」
-- という条件を満たすペアを抽出する。
matchingPairs :: [(DTT.Signature, CCG.Cat)]
              -> [(DTT.Signature, CCG.Cat)]
              -> [(TL.Text, TL.Text)]
matchingPairs sigs1 sigs2 =
  [ (fst $ Prelude.head $ fst x, fst $ Prelude.head $ fst y)
  | x@(s1, c1) <- sigs1
  , y@(s2, c2) <- sigs2
  , c1 == c2
  , not . null $ [ () | t1 <- sigTypes s1, t2 <- sigTypes s2, t1 == t2 ]
  ]

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


makePrompt :: [((TL.Text, TL.Text))] -> IO T.Text
makePrompt sigPairs = 
    let prompt' = TL.toStrict 
            "次の語彙ペアについて、「A は B を意味的に含意するか」を判定してください。\n\
                \含意とは、「A であれば必ず B が成立する」場合のみ Yes とし、矛盾がある場合のみ No とします。\n\
                \成立が不明な場合は Unknown とします。\n\
                \出力ルール:\n\
                \  - 各ペアの順番通りに判定すること\n\
                \  - 回答は Yes, No, Unknown のいずれか\n\
                \  - 区切り文字は「,」\n\
                \"
        prompts = T.concat $ prompt'  : makePrompt' sigPairs
   in return $ D.trace (U.ushow sigPairs) prompts
   where 
        makePrompt' :: [((TL.Text, TL.Text))] -> [T.Text]
        makePrompt' sigPairs =  case sigPairs of
            [] -> []
            x:xs ->
                let (word1, word2) = x 
                -- in TL.concat ["「",word1, "」は「", word2, "」を意味的に含意するか？\n"] :  makePrompt' xs
                in T.concat [ TL.toStrict word1, "は", TL.toStrict word2, "を意味的に含意するか\n"] :  makePrompt' xs 


-- | "," か　で区切られた応答を分割してリストにする。スペースは削除する。
splitResponse :: T.Text -> [T.Text]
splitResponse response =
    let parts = T.splitOn "," response
    in map T.strip parts
    
    
-- | 公理を作る
makeAxiom :: (TL.Text, TL.Text) -> T.Text -> DTT.Signature --[(LazyT.Text, Preterm)]
makeAxiom (word1, word2) answer = case answer of
    "Yes" -> [(TL.concat[word1,"-",word2], DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.App (DTT.App (DTT.Con word1) (DTT.Var 1)) (DTT.Var 0)) (DTT.App (DTT.App (DTT.Con word2) (DTT.Var 2)) (DTT.Var 1)))))]
    _ -> []

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
            let premiseSigs = concatMap extractLeafSigFromNode premises
                hypSigs = concatMap extractLeafSigFromNode (concat hyps)
                filterSigs = compareSigs premiseSigs hypSigs
            if null filterSigs
                then D.trace (concat (map U.ushow premises) ++ "\n\n" ++ concat (map U.ushow hyps) ++ "\n\n" ++ concat (map U.ushow filterSigs)) return []

            else do
                prompts <- makePrompt filterSigs
                response <- callGPT prompts
                let answers = splitResponse response
                    axioms = zipWith makeAxiom filterSigs answers
                D.trace (concat (map U.ushow premiseSigs) ++ "\n\n" ++ concat (map U.ushow hypSigs) ++ "\n\n" ++ concat (map U.ushow filterSigs) ++ "\n\n" ++  U.ushow prompts ++ "\n\n" ++ U.ushow answers ++ "\n\n" ++ concat (map U.ushow axioms))  return $ concat axioms 
                -- D.trace (concat (map U.ushow premiseSigs) ++ "\n\n" ++ concat (map U.ushow hypSigs) ++ "\n\n" ++ 




-- main :: IO ()
-- main = do
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

