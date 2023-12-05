{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Control.Monad (forM_)
import Interface (Style(..),headerOf,footerOf)
import Interface.HTML (MathML(..),startMathML,endMathML)
--import Interface.Text (SimpleText(..))
--import Interface.Tree (Tree(..))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified DTS.UDTTdeBruijn as U
import DTS.QueryTypes (TypeCheckQuery(..))
import DTS.TypeChecker (typeCheck, nullProver)

main :: IO()
main = do
  let tcq = TypeCheckQuery {
        -- | [(T.Text, Preterm DTT)]
        sig = [("boy", U.Pi U.Entity U.Type), ("run", U.Pi U.Entity U.Type)]
        -- | [Preterm DTT]
        , ctx = []
        -- | Preterm UDTT
        , trm = U.Pi (U.Sigma U.Entity (U.App (U.Con "boy") (U.Var 0))) (U.App (U.Con "run") (U.Proj U.Fst (U.Var 0)))
        -- | Preterm DTT
        , typ = U.Type 
        }
      tcResults = typeCheck nullProver tcq
  putStrLn $ headerOf HTML
  forM_ tcResults $ \tcResult -> do
    T.putStrLn $ T.concat [startMathML, toMathML tcResult, endMathML]
  putStrLn $ footerOf HTML


