{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import CallJuman
import Data.Text.Lazy.IO as T

main :: IO()
main = do
--  (stdin, stdout, stderr, procHandle) <- runInteractiveCommand "echo 'イタリア人テノール' | chasen"
--  t <- hGetContents stdout
--  putStrLn t
  list <- jumanCompoundNoun "がパン"
  mapM_ (\(a,b) -> do T.putStr a; T.putStr ":"; T.putStrLn b;) list
  --mapM_ T.putStrLn list
