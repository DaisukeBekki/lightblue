{-# OPTIONS -Wall #-}
{-# LANGUAGE ExtendedDefaultRules, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parser.Language.Japanese.Juman.Config (
  Config(..),
  readConfig,
  getConfig,
  fetchConfig,
  testConfig
  ) where

import qualified GHC.Generics          as G --base
import qualified Data.Text             as T --text
import qualified Data.Aeson            as A --aeson
import qualified Data.ByteString.Char8 as B --bytestring 
import qualified Data.Yaml             as Y --yaml
import qualified System.Environment    as E --base
import System.FilePath ((</>))              --filepath
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory) --directory
import Control.Exception (throw)
import Control.Monad (when)
default(T.Text)

-- | プログラムに実行環境を指定するyamlファイルを渡す。
-- | そのyamlファイルの仕様を以下で定義。
data Config = Config {
  jumanDicDir :: String           -- ^ Original data folder
  , jumanDicFileName :: String    -- ^ the file name of the converted juman data (text file)
  , kyodaiCaseFrameData :: String -- ^ Original data (XML file)
  , kyodaiCaseFrameFileName :: String -- ^ the file name of the converted case frame data (text file)
  , moduleDir :: String
  } deriving (Show, G.Generic)

instance A.FromJSON Config

testConfig :: IO()
testConfig = do
  config <- fetchConfig
  checkDir $ jumanDicDir config
  checkFile $ moduleDir config </> jumanDicFileName config
  checkFile $ kyodaiCaseFrameData config
  checkDir $ moduleDir config
  checkFile $ moduleDir config </> kyodaiCaseFrameFileName config
  putStrLn "\ntestConfig: All values in config.yaml are well-formed."

readConfig :: FilePath -> IO(Config)
readConfig filePath = do
  checkFile filePath
  content <- B.readFile filePath
  let parsedConfig = Y.decodeEither' content :: Either Y.ParseException Config
  case parsedConfig of
    Left parse_exception -> error $ "Could not parse config file " ++ filePath ++ ": " ++ (show parse_exception)
    Right config -> return config

getConfig :: IO(FilePath,Config)
getConfig = do
  (filePath:_) <- E.getArgs
  checkFile filePath
  config <- readConfig filePath
  return (filePath, config)

fetchConfig :: IO(Config)
fetchConfig = do
  lightbluePath <- E.getEnv "LIGHTBLUE"
  checkDir lightbluePath
  let configPath = lightbluePath ++ "src/Parser/Language/Japanese/Juman/config.yaml"
  checkFile configPath
  readConfig configPath

-- | Check if filepath is an existng directory
checkDir :: FilePath -> IO()
checkDir filePath = do
  let whenM s r = s >>= flip when r
  whenM (not <$> doesDirectoryExist filePath) $ throw (userError $ filePath ++ " does not exist.") 

-- | Check if filepath is an existing file
checkFile :: FilePath -> IO()
checkFile filePath = do
  let whenM s r = s >>= flip when r
  whenM (not <$> doesFileExist filePath) $ throw (userError $ filePath ++ " does not exist.") 

