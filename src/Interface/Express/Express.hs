{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Interface.Express.Express (
    showExpress
  ) where

import Yesod
import qualified Data.Text.Lazy as T      --text
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
import qualified Parser.CCG as CCG (showScore, getLeafNodesFromNode)

-- アプリケーションの状態として ParseResult を保持するための IORef を定義
{-# NOINLINE currentParseResultRef #-}
currentParseResultRef :: IORef (Maybe NLI.ParseResult)
currentParseResultRef = unsafePerformIO $ newIORef Nothing

data App = App

mkYesod "App" [parseRoutes|
/parsing ParsingR GET
|]

instance Yesod App

-- printExpressInterface を showExpress に変更し、ParseResult を引数に取る
showExpress :: NLI.ParseResult -> IO ()
showExpress initialParseResult = do
  -- 初期ParseResultをIORefに保存
  atomicWriteIORef currentParseResultRef (Just initialParseResult)

  let port = 3000
  let url = "http://localhost:" ++ show port ++ "/parsing" -- 初めから /parsing を開く

  let openBrowserCommand = case os of
        "darwin" -> "open " ++ url
        "linux"  -> "xdg-open " ++ url
        "mingw32" -> "start " ++ url
        _        -> "echo 'Unsupported OS for auto-opening browser.'"

  putStrLn $ "Starting Yesod server on " ++ url

  _ <- forkIO $ do
    callCommand openBrowserCommand `catch` \e -> do
      hPutStrLn stderr $ "Failed to open browser: " ++ show (e :: IOException)

  warp port App

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
          let sentence = SP.InputSentences { SP.input_Sentence = "太郎がパンを食べた。", SP.sen_beam = 24}
          let text_sen = T.fromStrict $ SP.input_Sentence sentence
          -- tabs個のnode
          -- parseSentence' :: NLI.ParseResult -> IO ([CCG.Node])
          nodes <- liftIO $ L.parseSentence' pr

          -- 各 node ごとの葉ノードを取得
          let leafNodesList = map CCG.getLeafNodesFromNode nodes

          let scores = Prelude.map CCG.showScore nodes

          -- nodesの長さをbeamとする
          let beam = 24 :: Int
          -- Number of nodes (beam): 1 ???
          -- liftIO $ hPutStrLn stderr $ "Number of nodes (beam): " ++ show beam
          let tabs = [1..beam]

          -- tabs個のType Check Query
          -- parseSentenceForQuery :: NLI.ParseResult -> IO ([UDTT.TypeCheckQuery])
          tcqs <- liftIO $ L.parseSentenceForQuery pr
     
          -- tcds 
          -- parseSentenceForDiagram :: NLI.ParseResult -> IO ([[QT.DTTProofDiagram]])
          tcds <- liftIO $ L.parseSentenceForDiagram pr

          let length = Prelude.map Prelude.length tcds
          liftIO $ putStrLn $ "length: " ++ show length

          let tabClasses = Prelude.map (\tcdList -> if Data.List.null tcdList then "tab-label error" :: T.Text else "tab-label" :: T.Text) tcds
          
          defaultLayout $ do
            [whamlet|
              <div id="parsing-view" style="display:block;">
                <div class="header">
                  <div class="parsing-content">
                    <p>sentence: #{text_sen}

                <input type="checkbox" id="cat-toggle"/>
                <input type="checkbox" id="sem-toggle"/>
                <label for="cat-toggle" id="catbtn"><b>&ensp;cat&ensp;&ensp;</b></label><br>
                <label for="sem-toggle" id="sembtn"><b>&ensp;sem&ensp;</b></label>

                <div class="container-tab">
                  <div .tab-wrap>
                    $forall (tabNum, node, tcq, tcdList, tabClass, score, leafNodes) <- Data.List.zip7 tabs nodes tcqs tcds tabClasses scores leafNodesList
                      <input id="TAB-#{tabNum}" type="radio" name="TAB" class="tab-switch" :tabNum == 1:checked>
                      <label for="TAB-#{tabNum}" class=#{tabClass}>#{tabNum} (score: #{score})
                      <div class="tab-content">
                        <div class="tab-leaves">
                          <h2>Leaf Nodes
                          <div .leaf-node-list>
                            $forall leaf <- leafNodes
                              <div .leaf-node-item>^{WE.widgetize leaf}
                        <div class="tab-node">
                          <h1>Node
                          <div class="tab-node-content">^{WE.widgetize node}
                        <div class="tab-tcq">
                          <h1>Type Check Query
                          <div class="tab-tcq-content">^{WE.widgetize tcq}
                        <div class="tab-tcd">
                          <h1>Type Check Diagram
                          $if Data.List.null tcdList
                            <p .error-message>⚠️ Type Check Failed... ⚠️
                          $else
                            <div class="tab-tcds-content">^{Prelude.mapM_ WE.widgetize $ tcdList}
            |]
            myDesign
            myFunction
 
--CSS（cassius）
myDesign :: Widget
myDesign = do
    toWidget $(cassiusFile "src/Interface/Express/templates/express.cassius")

-- julius file for javascript
myFunction :: Widget
myFunction = do
    toWidget $(juliusFile "src/Interface/Express/templates/express.julius")