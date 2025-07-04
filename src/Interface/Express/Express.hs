{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Interface.Express.Express (
    printExpressInterface
  ) where

import Prelude
import Yesod
import qualified Data.Text.Lazy as T
import qualified Interface.Express.Lightblue as L
import qualified Interface.Express.Sentence_process as SP
import Data.List
import qualified Interface.Express.WidgetExpress as WE
-- import  Text.Hamlet
-- import  Text.Cassius 
-- import  Text.Julius
import Interface.Express.Yesod.Foundation
import qualified Interface.Express.Yesod.Application as A
import qualified Data.Text.Lazy.IO as TL
-- import Application (appMain)

printExpressInterface :: IO ()
printExpressInterface = do
  -- getParsing :: Handler Html
  html <- A.handler $ getParsing
  -- renderHtml :: Html -> Data.Text.Lazy.Texts
--   TL.writeFile "output6.html" (renderHtml html)
  -- T.hPutStrLn html みたいなことをしたい
  putStrLn "Express application started successfully."

getParsing :: Handler Html
getParsing = do
     -- 後で標準入力で受け取れるように変更
     let sentence = SP.InputSentences { SP.input_Sentence = "花子が走った。", SP.sen_beam = 24}
     let text_sen = T.fromStrict $ SP.input_Sentence sentence
     let beam = SP.sen_beam sentence
     let tabs = [1..beam]
     -- tabs個のnode
     -- parseSentence' :: Int -> Int -> T.Text -> IO ([CCG.Node])
     nodes <- liftIO $ L.parseSentence' beam beam text_sen

     -- tabs個のType Check Query
     -- parseSentenceForQuery :: Int -> Int -> T.Text -> IO ([UDTT.TypeCheckQuery])
     tcqs <- liftIO $ L.parseSentenceForQuery beam beam text_sen
     
     -- tcds 
     tcds <- liftIO $ L.parseSentenceForDiagram beam beam text_sen
     let tabClasses = Prelude.map (\tcdList -> if Data.List.null tcdList then "tab-label error" :: T.Text else "tab-label" :: T.Text) tcds

     defaultLayout $ do
          [whamlet|
            <head>
                <script src="js_file.js">
                <script src="js_toggle.js">
                <link rel="stylesheet" href="css_file.css">
          |]
          [whamlet|
            <div class="header">
              <div class="parsing-content">
                <p>sentence: #{text_sen}
                <br>beam: #{beam}
            <div class="container-tab">
              <div .tab-wrap>
                $forall (tabNum, node, tcq, tcdList, tabClass) <- Data.List.zip5 tabs nodes tcqs tcds tabClasses
                  <input id="TAB-#{tabNum}" type="radio" name="TAB" class="tab-switch" :tabNum == 1:checked>
                  <label for="TAB-#{tabNum}" class=#{tabClass}>#{tabNum}
                  <div class="tab-content">
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
                        <div class="tab-tcds-content">^{Prelude.mapM_ WE.widgetize $ Prelude.take 1 tcdList}
          |]