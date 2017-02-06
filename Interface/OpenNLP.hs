{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Interface.OpenNLP (
  Indexed(..),
  node2NLP
  ) where

import Prelude hiding (id)
import qualified Data.Text.Lazy as T      -- text
import qualified Control.Applicative as M -- base
import qualified Control.Monad as M       -- base
import qualified Parser.CCG as CCG
import Interface.Text
--import Interface.TeX
import Interface.HTML

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

initializeIndex :: Indexed a -> a
initializeIndex (Indexed m) = let (m',_,_,_,_) = m 0 0 [] [] in m'

loop :: Int -> Bool -> CCG.Node -> Indexed T.Text
loop i lexonly node = 
  case CCG.daughters node of
    [] -> do
          j <- terminalIndex
          let id = T.concat ["s", T.pack $ show i, "_", T.pack $ show j]
          pushTerminal id $ T.concat ["<token surf='", CCG.pf node, "' base='", CCG.pf node, "' category='", printCat $ CCG.cat node, "' id='", id, "' />"]
    _ -> do
         ids <- mapM (loop i lexonly) $ CCG.daughters node
         if lexonly 
            then return T.empty
            else do
             j <- childIndex
             let id = T.concat ["s", T.pack $ show i, "_sp", T.pack $ show j]
             pushChild id $ T.concat ["<span child='", T.unwords ids, "' rule='", toMathML $ CCG.rs node, "' category='", printCat $ CCG.cat node,"' id='", id, "' />"]

node2NLP :: Int -> Bool -> T.Text -> CCG.Node -> T.Text
node2NLP i lexonly sentence node = initializeIndex $ do
                    id <- loop i lexonly node
                    (cs,ts) <- popResults
                    return $ T.concat $ (header id) ++ (reverse ts) ++ (mediate id $ CCG.showScore node) ++ (reverse cs) ++ footer
  where header id = ["<sentence id='", id, "'>", sentence, "<tokens>"]
        mediate id score = ["</tokens><ccg score='", score, "' id='s0_ccg0' root='", id, "'>"]
        footer = ["</ccg></sentence>"]

printCat :: CCG.Cat -> T.Text
printCat category = case category of
    CCG.SL x y      -> T.concat [printCat x, "/",  printCat' y]
    CCG.BS x y      -> T.concat [printCat x, "\\", printCat' y]
    CCG.T True i _     -> T.concat ["T", T.pack $ show i]
    CCG.T False i c     -> T.concat [printCat c, (T.pack $ show i)]
    CCG.S (pos:(conj:pmf)) -> 
              T.concat [
                       "S[pos=",
                       toT pos,",conj=",
                       toT conj,",",
                       printPMFs pmf,"]"
                       ]
    CCG.NP [cas]    -> T.concat ["NP[case=", toText cas, "]"]
    CCG.Sbar [sf]   -> T.concat ["Sbar[form=", toText sf, "]"]
    CCG.N           -> "N"
    CCG.CONJ        -> "CONJ"
    CCG.LPAREN      -> "LPAREN"
    CCG.RPAREN      -> "RPAREN"
    _ -> "Error in Simpletext Cat"
    where -- A bracketed version of `toText'` function
    printCat' c = if CCG.isBaseCategory c
                  then printCat c
                  else T.concat ["(", printCat c, ")"]

toT :: CCG.Feature -> T.Text
toT (CCG.SF _ f) = T.intercalate "|" $ map (T.pack . show) f
toT (CCG.F f) = T.intercalate "|" $ map (T.pack . show) f

-- | prints a list of syntactic features each of which ranges over {P,M,PM}.
printPMFs :: [CCG.Feature] -> T.Text
printPMFs pmfs = T.intercalate "," $ printPMFsLoop ["t","p","n","N","T"] pmfs
 
printPMFsLoop :: [T.Text] -> [CCG.Feature] -> [T.Text]
printPMFsLoop labels pmfs = case (labels,pmfs) of
  ([],[])         -> []
  ((l:ls),(p:ps)) -> (printPMF l p):(printPMFsLoop ls ps)
  _ -> [T.concat ["Error: mismatch in ", T.pack (show labels), " and ", T.pack (show pmfs)]]

-- | prints a syntactic feature that ranges over {P,M,PM}.
printPMF :: T.Text -> CCG.Feature -> T.Text
printPMF label pmf = case (label,pmf) of
    (l,CCG.F [CCG.P])   -> T.append l "=+"
    (l,CCG.F [CCG.M])   -> T.append l "=-"
    (l,CCG.F [CCG.P,CCG.M]) -> T.append l "=±"
    (l,CCG.F [CCG.M,CCG.P]) -> T.append l "=±"
    (l,CCG.SF _ f)  -> printPMF l (CCG.F f)
    _ -> T.concat ["Error: printPMF", T.pack $ show pmf]
