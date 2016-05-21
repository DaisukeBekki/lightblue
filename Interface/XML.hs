{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : XML interface
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}
module Interface.XML (
  render
  ) where

import qualified Parser.CCG as CCG
import qualified Interface.Text as T
import qualified Data.Text.Lazy as T   --text
import qualified Data.Map as M         --container
import qualified Text.XML as X         --xml-conduit
import qualified Data.Fixed as F       --base
import qualified System.IO as S        --base

--class XMLable a where
--  toXML :: a -> X.Node

myname :: T.Text -> X.Name
myname t = X.Name (T.toStrict t) Nothing Nothing 

-- | instance XMLable CCG.Node where
toXML :: CCG.Node -> X.Node
toXML node@(CCG.Node _ _ _ _ _ _ _ _) = 
    case CCG.daughters node of 
      [] -> X.NodeElement $ X.Element 
                              (myname $ T.pack $ show $ CCG.rs node)
                              (M.fromList [
                                ("pf", T.toStrict $ CCG.pf node),          
                                ("cat", T.toStrict $ T.toText $ CCG.cat node),
                                ("dts", T.toStrict $ T.toText $ CCG.sem node),
                                ("score", T.toStrict $ T.pack $ show ((fromRational $ CCG.score node)::F.Fixed F.E2)),
                                ("source", T.toStrict $ CCG.source node)
                                ]) 
                              []
      dtrs -> X.NodeElement $ X.Element 
                                (myname $ T.pack $ show $ CCG.rs node)
                                (M.fromList [
                                --("pf", T.toStrict $ CCG.pf (node)),          
                                 ("cat", T.toStrict $ T.toText $ CCG.cat node),
                                 ("dts", T.toStrict $ T.toText $ CCG.sem node),
                                 ("score", T.toStrict $ T.pack $ show ((fromRational $ CCG.score node)::F.Fixed F.E2))
                                 ]) 
                                (map toXML dtrs)

-- | combines a set of CCG nodes into an XML document.
toXMLDocument :: [CCG.Node] -> X.Document
toXMLDocument nodes = 
  X.Document
    (X.Prologue [X.MiscInstruction (X.Instruction "target" "Daisuke Bekki")] Nothing [])
    (X.Element (myname "parse-trees") (M.fromList []) $ reverse $ foldNodes (1::Int) $ map toXML nodes)
    []
  where foldNodes i ns = case ns of
          [] -> []
          (x:xs) -> (X.NodeElement $ X.Element (myname "parse-tree") (M.fromList [("id", T.toStrict $ T.pack $ show i)]) [x]):(foldNodes (i+1) xs)

-- | This function takes a handle (= stdin, stdout or stderr), a list of CCG nodes, converts it to XML format and prints it out to the handle.
render :: S.Handle -> [CCG.Node] -> IO()
render handle nodes = do
  let text = X.renderText X.def (toXMLDocument nodes)
  S.hPutStr handle $ T.unpack text

--renderSettings :: X.RenderSettings
--renderSettings = X.RenderSettings True [] (X.orderAttrs [("LEX",["pf","cat","dts","soruce","score"])]) (\c -> False)

{-
render2 :: S.Handle -> CCG.Node -> IO()
render2 handle node = do
-}