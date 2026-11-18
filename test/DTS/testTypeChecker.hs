{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Control.Monad (forM_,when)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Interface (Style(..),headerOf,footerOf)
import Interface.HTML (MathML(..),startMathML,endMathML)
--import Interface.Text (SimpleText(..))
--import Interface.Tree (Tree(..))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import ListT (toList)
import qualified DTS.DTTdeBruijn as D
import qualified DTS.UDTTdeBruijn as U
import qualified DTS.UDTTwithName as V
import DTS.QueryTypes (defaultProofSearchSetting)
import DTS.TypeChecker (typeCheck, nullProver)

main :: IO()
main = do
  let tcq = U.Judgment {
        -- | [(T.Text, Preterm DTT)]
        U.signtr = [("boy", D.Pi D.Entity D.Type), ("run", D.Pi D.Entity D.Type), ("john", D.Entity)]
        -- | [Preterm DTT]
        , U.contxt = []
        -- | Preterm UDTT
        , U.trm = V.toDeBruijn [] $
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
        , U.typ = D.Type
        }
  tcResults <- toList $ typeCheck (nullProver defaultProofSearchSetting) False tcq
  putStrLn $ headerOf HTML
  T.putStrLn $ T.concat ["Type check: ", startMathML, toMathML tcq, endMathML]
  forM_ tcResults $ \tcResult -> do
    T.putStrLn $ T.concat [startMathML, toMathML tcResult, endMathML]
  putStrLn $ footerOf HTML
  when (null tcResults) $ do
    hPutStrLn stderr "typeCheck: no derivation found"
    exitFailure


