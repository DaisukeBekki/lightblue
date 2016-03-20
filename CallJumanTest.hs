{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude as P
import Data.Text.Lazy as T
import Data.Text.Lazy.IO as T
-- import System.IO as S
import System.Process as S

main :: IO()
main = do
  let sentence = "関数戻り値"
  (_, handle, _, _) <- S.runInteractiveCommand $ T.unpack $ T.concat ["echo ", sentence, " | nkf -e | juman | nkf -w"]
  t <- T.hGetContents handle
  -- return $ findCompNouns [] $ map ((\l -> ((l!!0),(l!!3),(l!!5))) . 
  mapM_ (mapM_ T.putStrLn) $ P.map (T.split (==' ')) $ P.filter (\t -> not (T.isPrefixOf "@" t)) $ P.takeWhile (/= "EOS") $ T.lines t
    --  
    -- 
