{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Interface.Express.Express (
    main
  ) where

import Prelude
import Yesod
-- import  Text.Hamlet
-- import  Text.Cassius
-- import  Text.Julius
import Interface.Express.Yesod.Foundation
import qualified Interface.Express.Yesod.Application as A
import qualified Data.Text.Lazy.IO as TL
-- import Application (appMain)

main :: IO ()
main = do
    putStrLn "Starting the application..."

getParsing :: Handler Html
getParsing = do
    defaultLayout $ do
        setTitle "Light Blue"
        [whamlet|<h1>Light Blue|]
        [whamlet|<p>Welcome to Light Blue!|]
        [whamlet|<p>Use the menu to navigate.|]
        [whamlet|<p>Enjoy!|]