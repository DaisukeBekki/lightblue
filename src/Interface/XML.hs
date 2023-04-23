{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

{-|
Module      : Interface.XML
Copyright   : (c) Daisuke Bekki, 2017
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

XML interface
-}
module Interface.XML (
  XML(..),
  node2XML
  ) where

import Prelude hiding (id)
import qualified Data.Text.Lazy as T      --text
import qualified Control.Applicative as M --base
import qualified Control.Monad as M       --base
import qualified Text.XML.Prettify as X   --xml-prettify-text
import qualified Protolude as X           --protolude
import Parser.CCG
import Interface.Text
import Interface.HTML

-- | A class with a 'toXML' method.
class XML a where
  toXML :: a -> T.Text

{- Initializing or Re-indexing -}

-- | Indexed monad controls indices to be attached to preterms.
newtype Indexed a = Indexed (Int -> Int -> [T.Text] -> [T.Text] -> (a,Int,Int,[T.Text],[T.Text]))

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
spanIndex :: Indexed Int
spanIndex = Indexed (\spanI tokenI spans tokens -> (spanI,spanI+1,tokenI,spans,tokens))

tokenIndex :: Indexed Int
tokenIndex = Indexed (\spanI tokenI spans tokens -> (tokenI,spanI,tokenI+1,spans,tokens))

pushSpan :: T.Text -> T.Text -> Indexed T.Text
pushSpan id text = Indexed (\spanI tokenI spans tokens -> (id,spanI,tokenI,text:spans,tokens))

pushToken :: T.Text -> T.Text -> Indexed T.Text
pushToken id text = Indexed (\spanI tokenI spans tokens -> (id,spanI,tokenI,spans,text:tokens))

popResults :: Indexed ([T.Text],[T.Text])
popResults = Indexed (\spanI tokenI spans tokens -> ((spans,tokens),spanI,tokenI,spans,tokens))

initializeIndex :: Indexed a -> a
initializeIndex (Indexed m) = let (m',_,_,_,_) = m 0 0 [] [] in m'

-- | prints a node as an XML tag <sentence>...</sentence>
node2XML :: Int         -- ^ an index to start
            -> Int      -- ^ n-th parse
            -> Bool     -- ^ if True, shows lexical items only
            -> Node -- ^ a node (=parse result)
            -> T.Text
node2XML i j iflexonly node =
  initializeIndex $ do
                    let sid = "s" ++ (show i)
                    id <- traverseNode sid iflexonly True node
                    (cs,ts) <- popResults
                    return $ prettifyXML $ T.concat $ header ++ (reverse ts) ++ (mediate sid id $ showScore node) ++ (reverse cs) ++ footer
  where header = ["<tokens>"]
        mediate sid id scor = ["</tokens><ccg score='", scor, "' id='", T.pack sid, "_ccg",T.pack $ show j,"' root='", id, "'>"]
        footer = ["</ccg>"]

prettifyXML :: T.Text -> T.Text
prettifyXML text =
  let opts = X.PrettifyOpts {X.indentStyle = X.SPACE 2, X.endOfLine = X.LF} in
  T.fromStrict $ X.prettyPrintXml opts $ X.toS text

traverseNode :: String            -- ^ Sentence ID
                -> Bool           -- ^ If True, only lexical items will be printed
                -> Bool           -- ^ True if the node is the root node
                -> Node           -- ^ A given node
                -> Indexed T.Text -- ^ The XML code
traverseNode sid iflexonly ifroot node =
  let rootmark = if ifroot
                    then T.pack "root='true' "
                    else T.empty in
  case daughters node of
    [] -> do
          j <- tokenIndex
          let id = T.pack $ sid ++ "_" ++ (show j);
              terminalcat = toXML $ cat node;
          tokenid <- pushToken id $ T.concat ["<token surf='", pf node, "' base='", pf node, "' category='", terminalcat, "' id='", id, "' />"]
          k <- spanIndex
          let id' = T.pack $ sid ++ "_sp" ++ (show k)
          pushSpan id' $ T.concat ["<span terminal='", tokenid, "' category='", terminalcat, "' ", rootmark, "id='", id', "' />"]
    _ -> do
         ids <- mapM (traverseNode sid iflexonly False) $ daughters node
         if iflexonly 
            then return T.empty
            else do
             j <- spanIndex
             let id = T.pack $ sid ++ "_sp" ++ (show j)
             pushSpan id $ T.concat ["<span child='", T.unwords ids, "' rule='", toMathML $ rs node, "' category='", toXML $ cat node,"' ", rootmark, "id='", id, "'/>"]

-- | gives the text representation of a syntactic category as an XML attribute

instance XML Cat where
  toXML category = case category of
    SL x y      -> T.concat [toXML x, "/",  toXML' y]
    BS x y      -> T.concat [toXML x, "\\", toXML' y]
    T True i _     -> T.concat ["T", T.pack $ show i]
    T False _ c     -> toXML c -- T.concat [toXML c, (T.pack $ show i)]
    S (pos:(conj:pmf)) -> 
              T.concat [
                       "S[pos=",
                       toXML pos,",conj=",
                       toXML conj,",",
                       toXML pmf,"]"
                       ]
    NP [cas]    -> T.concat ["NP[case=", toText cas, "]"]
    Sbar [sf]   -> T.concat ["Sbar[form=", toText sf, "]"]
    N           -> "N"
    CONJ        -> "CONJ"
    LPAREN      -> "LPAREN"
    RPAREN      -> "RPAREN"
    _ -> "Error in Simpletext Cat"
    where -- A bracketed version of `toText'` function
    toXML' c = if isBaseCategory c
                  then toXML c
                  else T.concat ["(", toXML c, ")"]

instance XML Feature where
  toXML (SF _ f) = T.intercalate "|" $ map (T.pack . show) f
  toXML (F f) = T.intercalate "|" $ map (T.pack . show) f

-- | prints a list of syntactic features each of which ranges over {P,M,PM}.
instance XML [Feature] where
  toXML pmfs = T.intercalate "," $ pmFeatures2XMLloop ["t","p","n","N","T"] pmfs

pmFeatures2XMLloop :: [T.Text] -> [Feature] -> [T.Text]
pmFeatures2XMLloop labels pmfs = case (labels,pmfs) of
      ([],[])         -> []
      ((l:ls),(p:ps)) -> (pmFeature2XML l p):(pmFeatures2XMLloop ls ps)
      _ -> [T.concat ["Error: mismatch in ", T.pack (show labels), " and ", T.pack (show pmfs)]]

-- | prints a syntactic feature that ranges over {P,M,PM}.
pmFeature2XML :: T.Text -> Feature -> T.Text
pmFeature2XML label pmf = case (label,pmf) of
    (l,F [P])   -> T.append l "=+"
    (l,F [M])   -> T.append l "=-"
    (l,F [P,M]) -> T.append l "=±"
    (l,F [M,P]) -> T.append l "=±"
    (l,SF _ f)  -> pmFeature2XML l (F f)
    _ -> T.concat ["Error: printPMF", T.pack $ show pmf]

