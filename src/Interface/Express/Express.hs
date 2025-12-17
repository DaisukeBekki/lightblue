{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Interface.Express.Express (
    showExpress
  , setDisplaySetting
  , setDisplayOptions
  ) where

import Yesod
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text as TS          -- strict text for JSON
import qualified Interface.Express.Lightblue as L
import qualified Interface.Express.Sentence_process as SP
import Data.List
import qualified Interface.Express.WidgetExpress as WE
import qualified Data.Text.Lazy.IO as TL
import qualified DTS.NaturalLanguageInference as NLI
import Text.Julius (juliusFile)
import  Text.Cassius (cassiusFile)
import System.Process (callCommand)
import Control.Concurrent (forkIO)
import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, IOException)
import System.Info (os)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Parser.CCG as CCG (showScore, getLeafNodesFromNode, pf, cat, sem)
import qualified Parser.ChartParser as CP
import qualified Parser.LangOptions as PL (defaultJpOptions)
import qualified Parser.Language.Japanese.Lexicon as JP (setupLexicon)
import qualified Parser.PartialParsing as Partial
import qualified Data.Map as M
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Data.Aeson (Value, object, (.=))
import qualified Data.Store as Store
import qualified Data.ByteString as BS
import qualified DTS.UDTTdeBruijn as UDTT
import Interface.Text (SimpleText(..))
import Data.Char (toLower)

-- アプリケーションの状態として ParseResult を保持するための IORef を定義
{-# NOINLINE currentParseResultRef #-}
currentParseResultRef :: IORef (Maybe NLI.ParseResult)
currentParseResultRef = unsafePerformIO $ newIORef Nothing

-- 表示設定を保持する IORef
{-# NOINLINE currentDisplaySettingRef #-}
currentDisplaySettingRef :: IORef WE.DisplaySetting
currentDisplaySettingRef = unsafePerformIO $ newIORef WE.defaultDisplaySetting

setDisplaySetting :: WE.DisplaySetting -> IO ()
setDisplaySetting dsp = atomicWriteIORef currentDisplaySettingRef dsp

-- | Helper: set by CLI options without importing WidgetExpress from caller
setDisplayOptions :: Maybe Int -> Bool -> Bool -> Bool -> IO ()
setDisplayOptions mDepth noShowCat noShowSem leafVert = do
  let base = WE.defaultDisplaySetting
      base' = maybe base (\d -> base { WE.defaultExpandDepth = d }) mDepth
      base'' = if noShowCat then base' { WE.showCat = False } else base'
      base''' = if noShowSem then base'' { WE.showSem = False } else base''
      dsp = base''' { WE.leafVertical = leafVert }
  setDisplaySetting dsp

data App = App

mkYesod "App" [parseRoutes|
/parsing ParsingR GET
/span SpanR GET
/span/node NodeR GET
/export/sem ExportSemR GET
/export/sem/text ExportSemTextR GET
/export/node ExportNodeR GET
|]

instance Yesod App

-- printExpressInterface を showExpress に変更し、ParseResult を引数に取る
showExpress :: NLI.ParseResult -> IO ()
showExpress initialParseResult = do
  -- 初期ParseResultをIORefに保存
  atomicWriteIORef currentParseResultRef (Just initialParseResult)

  -- 環境変数から表示設定を読み込み、反映
  applyEnvDisplayOptions

  let port = 3000
  let url = "http://localhost:" ++ show port ++ "/parsing" -- 初めから /parsing を開く

  -- ブラウザ選択（環境変数 LB_EXPRESS_BROWSER: chrome|firefox|default）
  mBrowser <- lookupEnv "LB_EXPRESS_BROWSER"
  let browserSel = fmap (map toLower) mBrowser
      openBrowserCommand =
        case os of
          "darwin" ->
            case browserSel of
              Just "chrome"  -> "open -a \"Google Chrome\" " ++ url
              Just "firefox" -> "open -a \"Firefox\" " ++ url
              _              -> "open " ++ url
          "linux"  ->
            case browserSel of
              Just "chrome"  -> "google-chrome " ++ url ++ " || google-chrome-stable " ++ url ++ " || chromium " ++ url ++ " || chromium-browser " ++ url ++ " || xdg-open " ++ url
              Just "firefox" -> "firefox " ++ url ++ " || xdg-open " ++ url
              _              -> "xdg-open " ++ url
          "mingw32" ->
            case browserSel of
              Just "chrome"  -> "start chrome " ++ url
              Just "firefox" -> "start firefox " ++ url
              _              -> "start " ++ url
          _        -> "echo 'Unsupported OS for auto-opening browser.'"

  putStrLn $ "Starting Yesod server on " ++ url

  _ <- forkIO $ do
    callCommand openBrowserCommand `catch` \e -> do
      hPutStrLn stderr $ "Failed to open browser: " ++ show (e :: IOException)

  warp port App

-- 環境変数 LB_EXPRESS_DEPTH / LB_EXPRESS_NOSHOWCAT / LB_EXPRESS_NOSHOWSEM から設定を反映
applyEnvDisplayOptions :: IO ()
applyEnvDisplayOptions = do
  mDepthStr <- lookupEnv "LB_EXPRESS_DEPTH"
  let mDepth = mDepthStr >>= readMaybe
  mNoShowCat <- lookupEnv "LB_EXPRESS_NOSHOWCAT"
  mNoShowSem <- lookupEnv "LB_EXPRESS_NOSHOWSEM"
  mLeafVertical <- lookupEnv "LB_EXPRESS_LEAFVERTICAL"
  let toBool v = case v of { Just "1" -> True; Just "true" -> True; Just "True" -> True; _ -> False }
      noShowCat = toBool mNoShowCat
      noShowSem = toBool mNoShowSem
      leafVert = toBool mLeafVertical
      base = WE.defaultDisplaySetting
      base' = maybe base (\d -> base { WE.defaultExpandDepth = d }) mDepth
      base'' = if noShowCat then base' { WE.showCat = False } else base'
      base''' = if noShowSem then base'' { WE.showSem = False } else base''
      dsp = base''' { WE.leafVertical = leafVert }
  atomicWriteIORef currentDisplaySettingRef dsp

getParsingR :: Handler Html
getParsingR = do
     -- IORef から ParseResult を読み込む
     mpr <- liftIO $ readIORef currentParseResultRef
     case mpr of
        Nothing -> do
          -- 初期値が設定されていない場合はエラーメッセージを表示
          defaultLayout $ do
            [whamlet|
              <div class="error-message">
                <p>ParseResult is not set...
            |]
        Just pr -> do
          let text_sen = case pr of
                           NLI.SentenceAndParseTrees sentence _ -> sentence
                           _ -> T.empty
          -- tabs個のnode
          -- parseSentence' :: NLI.ParseResult -> IO ([CCG.Node])
          nodes <- liftIO $ L.parseSentence' pr

          -- 各 node ごとの葉ノードを取得
          let leafNodesList = map CCG.getLeafNodesFromNode nodes

          let scores = Prelude.map CCG.showScore nodes

          -- タブ数はノード数に合わせる
          let numTabs = Prelude.length nodes
          let tabs = [1..numTabs]

          -- tabs個のType Check Query
          -- parseSentenceForQuery :: NLI.ParseResult -> IO ([UDTT.TypeCheckQuery])
          tcqs <- liftIO $ L.parseSentenceForQuery pr
     
          -- tcds 
          -- parseSentenceForDiagram :: NLI.ParseResult -> IO ([[QT.DTTProofDiagram]])
          tcds <- liftIO $ L.parseSentenceForDiagram pr

          let tcdLengths = Prelude.map Prelude.length tcds
          liftIO $ putStrLn $ "length: " ++ show tcdLengths

          let tabClasses = Prelude.map (\tcdList -> if Data.List.null tcdList then "tab-label error" :: T.Text else "tab-label" :: T.Text) tcds
          
          -- 表示設定（CLI等から設定された値を参照）
          dsp <- liftIO $ readIORef currentDisplaySettingRef
          let catChecked = not (WE.showCat dsp)
          let semChecked = not (WE.showSem dsp)

          defaultLayout $ do
            [whamlet|
              <div id="parsing-view" style="display:block;">
                <div class="header">
                  <div class="parsing-content">
                    <div class="sentence-line">sentence: <span id="sentence-text">#{text_sen}</span>
                    <div class="span-controls">
                      <input id="span-surface" type="text" placeholder="部分文字列をドラッグ選択">
                      <input id="span-beam" type="number" value="32" min="1" max="256">
                      <select id="span-results"></select>
                      <span id="span-status"></span>
                    <div class="toggle-group">
                      <label for="cat-toggle" id="catbtn" class="toggle">cat
                      <label for="sem-toggle" id="sembtn" class="toggle">sem

                <input type="checkbox" id="cat-toggle" :catChecked:checked/>
                <input type="checkbox" id="sem-toggle" :semChecked:checked/>

                <div id="span-preview-title" class="span-preview-title"></div>
                <div id="span-preview-list" class="span-preview-list">

                <div class="container-tab">
                  <div .tab-wrap>
                    $forall (tabNum, node, tcq, tcdList, tabClass, score, leafNodes) <- Data.List.zip7 tabs nodes tcqs tcds tabClasses scores leafNodesList
                      <input id="TAB-#{tabNum}" type="radio" name="TAB" class="tab-switch" :tabNum == 1:checked>
                      <label for="TAB-#{tabNum}" class=#{tabClass}>#{tabNum} (score: #{score})
                      <div class="tab-content">
                        <div class="tab-leaves">
                          <h2>Leaf Nodes
                          <div .leaf-node-list :WE.leafVertical dsp:.vertical>
                            $forall leaf <- leafNodes
                              <div .leaf-node-item>^{WE.widgetizeWith dsp leaf}
                        <div class="tab-node">
                          <h1>Node
                        <div class="tab-node-content">
                          <div .tab-node-inner>^{WE.widgetizeWith dsp node}
                        <div class="tab-tcq">
                          <h1>Type Check Query
                          <div class="tab-tcq-content">^{WE.widgetizeWith dsp tcq}
                        <div class="tab-tcd">
                          <h1>Type Check Diagram
                          $if Data.List.null tcdList
                            <p .error-message>⚠️ Type Check Failed... ⚠️
                          $else
                            <div class="tab-tcds-content">
                              <div .tab-tcds-inner>^{Prelude.mapM_ (WE.widgetizeWith dsp) $ tcdList}
            |]
            myDesign
            myFunction
 
-- JSON: span parse results
getSpanR :: Handler Value
getSpanR = do
  mpr <- liftIO $ readIORef currentParseResultRef
  case mpr of
    Nothing -> return $ object ["error" .= ("ParseResult is not set" :: TS.Text)]
    Just pr -> do
      let text_sen = case pr of
                        NLI.SentenceAndParseTrees sentence _ -> sentence
                        _ -> T.empty
      mSurface <- lookupGetParam "surface"
      mStart <- lookupGetParam "start"
      mEnd <- lookupGetParam "end"
      mBeam <- lookupGetParam "beam"
      let beam :: Int
          beam = case mBeam >>= (readMaybe . TS.unpack) of
                   Just b -> b
                   Nothing -> 32
      chart <- liftIO $ L.parse beam text_sen
      let entries = M.toList chart
          normalize :: TS.Text -> TS.Text
          normalize = TS.filter (\c -> not (c `elem` delims))
          delims :: [Char]
          delims = [' ', '　', '。', '、', '，', '．', ',', '.', '!', '！', '?', '？']
      -- Token-based span finder: substring -> (i,j)
      jpOptions <- liftIO PL.defaultJpOptions
      (tokens, _) <- liftIO $ JP.setupLexicon jpOptions text_sen
      let tokensN = map (normalize . T.toStrict) tokens
          nTok = length tokensN
          matchSpan :: TS.Text -> Maybe (Int,Int)
          matchSpan target
            | TS.null target = Nothing
            | otherwise =
                let go s e = if s >= nTok then Nothing
                              else if e > nTok then go (s+1) (s+2)
                              else let catse = TS.concat (take (e-s) (drop s tokensN)) in
                                   if catse == target then Just (s,e) else go s (e+1)
                in go 0 1
          pickBySurface :: TS.Text -> Maybe ((Int,Int), [CP.Node])
          pickBySurface surf =
            let target = normalize surf in
            let matches = [ (ij, ns)
                          | (ij, ns) <- entries
                          , not (null ns)
                          , let pfs = map (normalize . T.toStrict . CCG.pf) ns
                          , any (== target) pfs
                          ] in
            case matches of
              (x:_) -> Just x
              _ -> Nothing
          pickByIndex :: Int -> Int -> [((Int,Int), [a])] -> Maybe ((Int,Int), [a])
          pickByIndex i j = find (\((i',j'),_) -> i'==i && j'==j)
      chosen <- case (mStart >>= readMaybe . TS.unpack, mEnd >>= readMaybe . TS.unpack) of
                  (Just i, Just j) -> return $ pickByIndex i j entries
                  _ -> case mSurface of
                         Just surf ->
                           case matchSpan (normalize surf) of
                             Just (i,j) -> return $ pickByIndex i j entries
                             Nothing    -> return $ pickBySurface surf
                         Nothing -> return Nothing
      case chosen of
        Nothing -> do
          -- Fallback: parse the substring independently
          case mSurface of
            Nothing -> return $ object ["nodes" .= ([] :: [TS.Text]), "message" .= ("No match" :: TS.Text)]
            Just surf -> do
              subChart <- liftIO $ L.parse beam (T.fromStrict surf)
              let pr = Partial.extractParseResult beam subChart
              case pr of
                Partial.Failed -> return $ object ["nodes" .= ([] :: [TS.Text]), "message" .= ("No match" :: TS.Text)]
                Partial.Full ns -> do
                  let toNodeObj n = object
                        [ "pf" .= (T.toStrict $ CCG.pf n)
                        , "cat" .= (TS.pack $ show $ CCG.cat n)
                        , "score" .= (T.toStrict $ CCG.showScore n)
                        ]
                  return $ object
                    [ "pf" .= surf
                    , "nodes" .= map toNodeObj ns
                    , "subparse" .= True
                    ]
                Partial.Partial ns -> do
                  let toNodeObj n = object
                        [ "pf" .= (T.toStrict $ CCG.pf n)
                        , "cat" .= (TS.pack $ show $ CCG.cat n)
                        , "score" .= (T.toStrict $ CCG.showScore n)
                        ]
                  return $ object
                    [ "pf" .= surf
                    , "nodes" .= map toNodeObj ns
                    , "subparse" .= True
                    ]
        Just ((i,j), ns) -> do
          let toNodeObj n = object
                [ "pf" .= (T.toStrict $ CCG.pf n)
                , "cat" .= (TS.pack $ show $ CCG.cat n)
                , "score" .= (T.toStrict $ CCG.showScore n)
                ]
          return $ object
            [ "span" .= object ["start" .= i, "end" .= j]
            , "pf" .= (T.toStrict $ CCG.pf (head ns))
            , "nodes" .= map toNodeObj ns
            , "subparse" .= False
            ]


--CSS（cassius）
myDesign :: Widget
myDesign = do
    toWidget $(cassiusFile "src/Interface/Express/templates/express.cassius")

-- julius file for javascript
myFunction :: Widget
myFunction = do
    toWidget $(juliusFile "src/Interface/Express/templates/express.julius")

-- HTML snippet: render a single node like Leaf Node layout
getNodeR :: Handler Html
getNodeR = do
  mpr <- liftIO $ readIORef currentParseResultRef
  case mpr of
    Nothing -> return [shamlet|<div class="error-message">ParseResult is not set|]
    Just pr -> do
      let text_sen = case pr of
                        NLI.SentenceAndParseTrees sentence _ -> sentence
                        _ -> T.empty
      mStart <- lookupGetParam "start"
      mEnd <- lookupGetParam "end"
      mSurface <- lookupGetParam "surface"
      mBeam <- lookupGetParam "beam"
      mIndex <- lookupGetParam "index"
      let parseInt :: TS.Text -> Maybe Int
          parseInt = readMaybe . TS.unpack
      let beam :: Int
          beam = case mBeam >>= parseInt of
                   Just b -> b
                   Nothing -> 32
          idx :: Int
          idx = case mIndex >>= parseInt of
                  Just i -> i
                  Nothing -> 0
      chart <- liftIO $ case (mStart >>= parseInt, mEnd >>= parseInt, mSurface) of
                          (Just _, Just _, _) -> L.parse beam text_sen
                          (_, _, Just surf)   -> L.parse beam (T.fromStrict surf)
                          _                   -> L.parse beam text_sen
      let entries = M.toList chart
          normalize :: TS.Text -> TS.Text
          normalize = TS.filter (\c -> not (c `elem` delims))
          delims :: [Char]
          delims = [' ', '　', '。', '、', '，', '．', ',', '.', '!', '！', '?', '？']
      jpOptions <- liftIO PL.defaultJpOptions
      (tokens, _) <- liftIO $ JP.setupLexicon jpOptions text_sen
      let tokensN = map (normalize . T.toStrict) tokens
          nTok = length tokensN
          matchSpan :: TS.Text -> Maybe (Int,Int)
          matchSpan target
            | TS.null target = Nothing
            | otherwise =
                let go s e = if s >= nTok then Nothing
                              else if e > nTok then go (s+1) (s+2)
                              else let catse = TS.concat (take (e-s) (drop s tokensN)) in
                                   if catse == target then Just (s,e) else go s (e+1)
                in go 0 1
          pickByIndex :: Int -> Int -> [((Int,Int), [a])] -> Maybe ((Int,Int), [a])
          pickByIndex i j = find (\((i',j'),_) -> i'==i && j'==j)
      chosen <- case (mStart >>= parseInt, mEnd >>= parseInt, mSurface) of
                  (Just i, Just j, _) -> return $ pickByIndex i j entries
                  (_, _, Just surf) ->
                    case matchSpan (normalize surf) of
                      Just (i,j) -> return $ pickByIndex i j entries
                      Nothing    -> return Nothing
                  _ -> return Nothing
      nodeM <- case chosen of
                 Just (_, ns) -> return $ if null ns then Nothing else (ns !!? idx)
                 Nothing -> do
                   case mSurface of
                     Nothing -> return Nothing
                     Just surf -> do
                       subChart <- liftIO $ L.parse beam (T.fromStrict surf)
                       let prSub = Partial.extractParseResult beam subChart
                       case prSub of
                         Partial.Failed   -> return Nothing
                         Partial.Full ns  -> return $ ns !!? idx
                         Partial.Partial ns -> return $ ns !!? idx
      dsp <- liftIO $ readIORef currentDisplaySettingRef
      let dspCollapsed = dsp { WE.defaultExpandDepth = 0 }
      case nodeM of
        Nothing -> defaultLayout $ do
          [whamlet|<div class="error-message">No node|]
        Just node -> defaultLayout $ do
          WE.widgetizeWith dspCollapsed node
  where
    (!!?) :: [a] -> Int -> Maybe a
    (!!?) xs n = if n < 0 || n >= length xs then Nothing else Just (xs !! n)

-- Text export: UDTT preterm (semantics) as plain text for a selected tab's node
getExportSemTextR :: Handler TypedContent
getExportSemTextR = do
  mpr <- liftIO $ readIORef currentParseResultRef
  case mpr of
    Nothing -> sendResponse (TypedContent "text/plain" (toContent ("ParseResult is not set" :: TS.Text)))
    Just pr -> do
      nodes <- liftIO $ L.parseSentence' pr
      mTab <- lookupGetParam "tab"
      let parseInt :: TS.Text -> Maybe Int
          parseInt = readMaybe . TS.unpack
          tab = maybe 1 id (mTab >>= parseInt)
          idx = tab - 1
      case nodes !!? idx of
        Nothing -> sendResponse (TypedContent "text/plain" (toContent ("Invalid tab index" :: TS.Text)))
        Just node -> do
          let semTerm = CCG.sem node
              txt = T.toStrict (toText semTerm)
          addHeader "Content-Type" "text/plain; charset=utf-8"
          sendResponse (TypedContent "text/plain" (toContent txt))
  where
    (!!?) :: [a] -> Int -> Maybe a
    (!!?) xs n = if n < 0 || n >= length xs then Nothing else Just (xs !! n)

-- Binary export: UDTT preterm (semantics) of a selected tab's node
getExportSemR :: Handler TypedContent
getExportSemR = do
  mpr <- liftIO $ readIORef currentParseResultRef
  case mpr of
    Nothing -> sendResponse (TypedContent "text/plain" (toContent ("ParseResult is not set" :: TS.Text)))
    Just pr -> do
      nodes <- liftIO $ L.parseSentence' pr
      mTab <- lookupGetParam "tab"
      let parseInt :: TS.Text -> Maybe Int
          parseInt = readMaybe . TS.unpack
          tab = maybe 1 id (mTab >>= parseInt)
          idx = tab - 1
      case nodes !!? idx of
        Nothing -> sendResponse (TypedContent "text/plain" (toContent ("Invalid tab index" :: TS.Text)))
        Just node -> do
          let semTerm = CCG.sem node
              bs :: BS.ByteString
              bs = Store.encode semTerm
              fname = TS.pack $ "sem_tab" ++ show tab ++ ".udtt"
          addHeader "Content-Type" "application/octet-stream"
          addHeader "Content-Disposition" (TS.concat ["attachment; filename=\"", fname, "\""])
          sendResponse (TypedContent "application/octet-stream" (toContent bs))
  where
    (!!?) :: [a] -> Int -> Maybe a
    (!!?) xs n = if n < 0 || n >= length xs then Nothing else Just (xs !! n)

-- Binary export: CCG node (syntax tree) of a selected tab (placeholder)
getExportNodeR :: Handler TypedContent
getExportNodeR = do
  addHeader "Content-Type" "text/plain"
  sendResponse (TypedContent "text/plain" (toContent ("Not implemented yet" :: TS.Text)))