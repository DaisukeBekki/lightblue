{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Interface.StanfordNLP (
  Indexed(..),
  node2NLP
  ) where

import Prelude hiding (id)
import qualified Data.Text.Lazy as T      -- text
import qualified Control.Applicative as M -- base
import qualified Control.Monad as M       -- base
import qualified Parser.CCG as CCG
import Interface.Text

{- Initializing or Re-indexing -}

-- | Indexed monad controls indices to be attached to preterms.
newtype Indexed a = Indexed { indexing :: Int -> Int -> [T.Text] -> [T.Text] -> (a,Int,Int,[T.Text],[T.Text]) }

instance Monad Indexed where
  return m = Indexed (\c t cs ts -> (m,c,t,cs,ts))
  (Indexed m) >>= f = Indexed (\c t cs ts -> let (m',c',t',cs',ts') = m c t cs ts;
                                                 (Indexed n) = f m' in
                                             n c' t' cs' ts')

instance Functor Indexed where
  fmap = M.liftM

instance M.Applicative Indexed where
  pure = return
  (<*>) = M.ap

-- | A sequential number for variable names (i.e. x_1, x_2, ...) in a context
childIndex :: Indexed Int
childIndex = Indexed (\c t cs ts -> (c,c+1,t,cs,ts))

terminalIndex :: Indexed Int
terminalIndex = Indexed (\c t cs ts -> (t,c,t+1,cs,ts))

pushChild :: T.Text -> T.Text -> Indexed T.Text
pushChild id text = Indexed (\c t cs ts -> (id,c,t,text:cs,ts))

pushTerminal :: T.Text -> T.Text -> Indexed T.Text
pushTerminal id text = Indexed (\c t cs ts -> (id,c,t,cs,text:ts))

popResults :: Indexed ([T.Text],[T.Text])
popResults = Indexed (\c t cs ts -> ((cs,ts),c,t,cs,ts))

{-
popChildren :: Indexed [T.Text]
popChildren = Indexed (\c t cs ts -> (cs,c,t,cs,ts)

popTerminals :: T.Text -> Indexed [T.Text]
popTerminals = Indexed (\c t cs ts -> (ts,c,t,cs,ts)
-}

initializeIndex :: Indexed a -> a
initializeIndex (Indexed m) = let (m',_,_,_,_) = m 0 0 [] [] in m'

loop :: Int -> CCG.Node -> Indexed T.Text
loop i node = 
  case CCG.daughters node of
    [] -> do
          j <- terminalIndex
          let id = T.concat ["s", T.pack $ show i, "_", T.pack $ show j]
          pushTerminal id $ T.concat ["<token surf='", CCG.pf node, "' category='", toText $ CCG.cat node, "' id='", id, "'>"]
    _ -> do
         ids <- mapM (loop i) $ CCG.daughters node
         j <- childIndex
         let id = T.concat ["s", T.pack $ show i, "_sp", T.pack $ show j]
         pushChild id $ T.concat ["<span child='", T.unwords ids, "' rule='", toText $ CCG.rs node, "' category='", toText $ CCG.cat node,"' id='", id, "'>"]

node2NLP :: Int -> CCG.Node -> ([T.Text],[T.Text],T.Text)
node2NLP i node = initializeIndex $ do
                    id <- loop i node
                    (cs,ts) <- popResults
                    return (cs,ts,id)

header :: T.Text -> T.Text -> T.Text
header id sentence = T.concat ["<?xml version='1.0' encoding='UTF-8'?><root><document id='d0'><sentences><sentence id='", id, ">", sentence, "<tokens>"]

mediate :: T.Text -> T.Text -> T.Text
mediate id score = T.concat ["</tokens><ccg score='", score, "' id='s0_ccg0' root='", id, "'>"]

footer :: T.Text
footer = "</ccg></sentence></sentences></document></root>"