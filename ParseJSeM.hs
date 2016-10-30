{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile)
import qualified Data.Text.Lazy as T    -- text
import qualified Data.Text.Lazy.IO as T -- text
import qualified System.Process as S       -- process
import qualified System.Environment as E   -- base
import qualified Parser.ChartParser as CP  -- lightblue
import qualified DTS.UDTT as DTS -- lightblue
import qualified Interface.Text as T       -- lightblue
import qualified Interface.JSeM as J       -- lightblue

jSeMpath :: FilePath
jSeMpath = "../JSeM_beta/JSeM_beta_150415.xml"

main :: IO()
main = do
  args <- E.getArgs
  jsemdata <- J.parseJSeM $ head args
  mapM_ processJSeMData jsemdata

processJSeMData :: J.JSeMData -> IO()
processJSeMData jsemdata = do
  T.putStrLn $ T.concat ["id [", J.jsem_id (jsemdata), "]"]
  mapM_ (\p -> do {T.putStr $ T.concat ["P: ", p, "\n"]}) $ J.premise jsemdata
  T.putStr $ T.concat ["H: ", J.hypothesis jsemdata, "\n"]
  psems <- mapM parseText $ J.premise jsemdata
  hsem <- parseText $ J.hypothesis jsemdata
  let sem = DTS.betaReduce $ currying psems hsem
  T.putStrLn $ T.toText sem
  T.putStrLn ""

currying :: [DTS.Preterm] -> DTS.Preterm -> DTS.Preterm
currying [] preterm = preterm
currying (p:ps) preterm = DTS.Pi p (currying ps preterm)

parseText :: T.Text -> IO(DTS.Preterm)
parseText sentence = do
  nodes <- CP.simpleParse 16 sentence
  return $ CP.sem (head nodes)

callCoq :: T.Text -> IO()
callCoq _ = do
  let coqcommand = T.concat ["echo -e \"Extraction Language Scheme.\nParameter A:Prop.\nParameter B:Prop.\nTheorem id: A -> B -> A.\nExtraction id.\n\" | coqtop 2> /dev/null | awk '{if($0 != \"\") {print $0}}' | tail -n 2"]
  (_, stdout, _, _) <- S.runInteractiveCommand $ T.unpack coqcommand
  t <- T.hGetContents stdout
  T.putStrLn $ T.replace "\n" "" $ T.strip t