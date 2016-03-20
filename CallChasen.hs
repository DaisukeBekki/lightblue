{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module CallChasen (
  chasenCompoundNoun
  ) where
  
import Prelude as P
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified System.Process as S
--import qualified System.IO as S

chasenCompoundNoun :: T.Text -> IO([T.Text])
chasenCompoundNoun text = do
  (_, stdout, _, _) <- S.runInteractiveCommand $ T.unpack $ T.concat ["echo ", text, " | chasen"]
  t <- T.hGetContents stdout
  let list = map ((\l -> (l!!0):(T.split (=='-') (l!!3))) . (T.split (=='\t'))) $ takeWhile (/= "EOS") $ T.lines t
  return $ if all (\l -> l!!1 == "名詞") list 
           then map (\l -> head l) list   
           else []  
    

  