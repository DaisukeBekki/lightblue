{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Control.Monad (forM_,when)
import Interface (Style(..),headerOf,footerOf)
import Interface.HTML (MathML(..),startMathML,endMathML)
--import Interface.Text (SimpleText(..))
--import Interface.Tree (Tree(..))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified DTS.UDTTdeBruijn as U
import qualified DTS.UDTTvarName as V
import DTS.QueryTypes (TypeCheckQuery(..),ListEx(..))
import DTS.TypeChecker (typeCheck, nullProver)

main :: IO()
main = do
  let tcq = TypeCheckQuery {
        -- | [(T.Text, Preterm DTT)]
        sig = [("boy", U.Pi U.Entity U.Type), ("run", U.Pi U.Entity U.Type), ("john", U.Entity)]
        -- | [Preterm DTT]
        , ctx = []
        -- | Preterm UDTT
        , trm = U.toDeBruijn [] $
                V.Pi
                  (V.VarName 'u' 0)
                  (V.Sigma
                     (V.VarName 'x' 0)
                     V.Entity
                     (V.App (V.Con "boy") (V.Var (V.VarName 'x' 0))))
                  (V.Sigma
                     (V.VarName 'u' 1)
                     (V.App
                        (V.Con "run")
                        (V.Proj V.Fst (V.Var (V.VarName 'u' 0))))
                     (V.Eq
                        V.Entity
                        (V.Proj V.Fst (V.Var (V.VarName 'u' 0)))
                       (V.Con "john")))
        -- | Preterm DTT
        , typ = U.Type 
        }
      ListEx (tcResults, err) = typeCheck nullProver tcq
  putStrLn $ headerOf HTML
  T.putStrLn $ T.concat ["Type check: ", startMathML, toMathML (U.Judgment (sig tcq) (ctx tcq) (trm tcq) (typ tcq)), endMathML]
  forM_ tcResults $ \tcResult -> do
    T.putStrLn $ T.concat [startMathML, toMathML tcResult, endMathML]
  when (err /= T.empty) $ T.putStrLn err -- $ T.concat ["Error: ", err]
  putStrLn $ footerOf HTML


