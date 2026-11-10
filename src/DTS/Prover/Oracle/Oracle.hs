{-# LANGUAGE OverloadedStrings #-}
module DTS.Prover.Oracle.Oracle (oracle, oracleBuilder) where

import Control.Monad (unless)
import qualified DTS.DTTdeBruijn as DdB
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as T
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath ((</>))
import DTS.Prover.Oracle.OracleUtils (loadWordEmbeddings, calcPScore)

resolveEmbeddingPath :: Maybe FilePath -> IO FilePath
resolveEmbeddingPath (Just path) = return path
resolveEmbeddingPath Nothing = do
    mLightblue <- lookupEnv "LIGHTBLUE"
    case mLightblue of
        Just lb | not (null lb) -> return (lb </> "data" </> "Embedding.csv")
        _ -> die $ unlines
            [ "LIGHTBLUE environment variable is not set."
            , "Set LIGHTBLUE to the lightblue directory or specify --oracleEmbedding <filepath>."
            , "To download Embedding.csv, run scripts/fetch-oracle-embedding.sh or visit:"
            , "https://github.com/DaisukeBekki/lightblue/releases/tag/oracle-embedding-v1"
            ]

ensureEmbeddingExists :: FilePath -> IO ()
ensureEmbeddingExists embPath = do
    exists <- doesFileExist embPath
    unless exists $ die $ unlines
        [ "Oracle embedding file not found: " ++ embPath
        , "Run scripts/fetch-oracle-embedding.sh or download it from:"
        , "https://github.com/DaisukeBekki/lightblue/releases/tag/oracle-embedding-v1"
        ]

oracle :: (DdB.ConName, DdB.ConName) -> IO Float
oracle (childName, parentName) = do
    oracleFun <- oracleBuilder Nothing
    return $ oracleFun childName parentName

oracleBuilder :: Maybe FilePath -> IO (DdB.ConName -> DdB.ConName -> Float)
oracleBuilder mEmbPath = do
    embPath <- resolveEmbeddingPath mEmbPath
    ensureEmbeddingExists embPath
    let alpha = 0.1
    embMap <- loadWordEmbeddings embPath
    return $ \childName parentName ->
        let stripTilde = T.filter (/= '~')
            pResult = M.lookup (stripTilde parentName) embMap
            cResult = M.lookup (stripTilde childName)  embMap
        in case (pResult, cResult) of
            (Just pEmbs, Just cEmbs) ->
                let allScores = [ calcPScore alpha pEmb cEmb | pEmb <- pEmbs, cEmb <- cEmbs ]
                in if null allScores
                   then 0.0
                   else maximum allScores
            _ -> 0.0
