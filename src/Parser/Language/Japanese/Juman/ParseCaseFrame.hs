{-# OPTIONS -Wall #-}

{-|
河原・林部らの格フレーム辞書をパーズして動詞の辞書を生成するプログラム
To run: ~/.cabal/bin/parseHayashibe > KawaharaFrame.txt
（約７分）
KawaharaFrame.txt: ググる/ググる	ニ格,ガ格#ト格,ガ格#ヲ格,ガ格
-}

module Parser.Language.Japanese.Juman.ParseCaseFrame (
  buildCaseFrame
  ) where

import System.FilePath ((</>)) --filepath
import qualified Data.ByteString.Lazy as BSL
--import qualified Data.ByteString.Lazy.UTF8 as U
import Prelude as P
import Text.XML.Expat.SAX
import Data.List as L 
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import System.IO as S
import Data.Time as Time
import Parser.Language.Japanese.Juman.Config (Config(..),fetchConfig)

-- | The main function to generate the case frame data
-- |   from the Case Frame XML file (kyodaiCaseFrameData)
-- |   The output is written on moduleDir/kyodaiCaseFrameFileName
buildCaseFrame :: IO()
buildCaseFrame = do
  start <- Time.getCurrentTime
  config <- fetchConfig
  contents <- BSL.readFile $ kyodaiCaseFrameData config
  let entries = parse defaultParseOptions contents
      outputFilePath = (moduleDir config) </> (kyodaiCaseFrameFileName config)
  S.withFile outputFilePath S.WriteMode $ \h -> 
    mapM_ (T.hPutStrLn h. (\(midashi,caselistlist) -> T.concat [midashi, "\t", T.intercalate "#" $ map (T.intercalate ",") caselistlist])) $ findEntry entries
  stop <- Time.getCurrentTime
  let time = Time.diffUTCTime stop start
  S.hPutStrLn S.stderr $ "\nTotal Execution Time (buildCaseFrame): " ++ show time

type Event = SAXEvent T.Text T.Text
type CaseFrame = (T.Text,[[T.Text]])  -- ex. ("拡張",[[ヲ格,ガ格],[ニ格,ガ格]])

findEntry :: [Event] -> [CaseFrame]
findEntry events = case events of
  [] -> []
  ((StartElement "entry" attribs):es) ->
    let attrMap = M.fromList attribs in
    case (do
          headword <- M.lookup "headword" attrMap
          predtype <- M.lookup "predtype" attrMap
          return (headword,predtype)) of
      Just (headword,predtype) -> 
        -- ex：headword="気付く/きづく+れる/れる~テ形+しまう/しまう" predtype="動" 
        --     headword="くれる/くれる?刳れる/くれる?刳れる/くれる?暮れる/くれる?暮れる/くれる?繰れる/くれる?繰れる/くれる"
        let headwordlist = T.split (=='+') headword in
        case () of
          _ | predtype /= "動" -> findEntry es -- || "~" `T.isInfixOf` headword
            | length headwordlist == 1 -> findCaseframe es (head headwordlist) []
            -- | length headwordlist >= 2 && "する/" `T.isPrefixOf` (head $ tail headwordlist) -> findCaseframe es (head headwordlist) []
            | otherwise -> findEntry es
      Nothing -> findEntry es
  (_:es) -> findEntry es

findCaseframe :: [Event]       -- ^ The rest of the event list
                 -> T.Text     -- ^ headword:                            ex. "拡張/かくちょう"
                 -> [[T.Text]] -- ^ the case list, initially set as []:  ex. [[ヲ,ガ],[ニ,ガ]]
                 -> [CaseFrame]
findCaseframe events headwords caselistlist = case events of -- headword=気付く/きづく
  [] -> []
  ((StartElement "caseframe" _):es) -> findArgument es [[]] headwords caselistlist
  ((EndElement "entry"):es) -> L.foldl' (\l h -> (h,caselistlist):l) (findEntry es) $ filter (\h -> not ("い" `T.isSuffixOf` h || "だ" `T.isSuffixOf` h)) $ T.split (=='?') headwords
  (_:es) -> findCaseframe es headwords caselistlist

findArgument :: [Event]       -- ^ The rest of the event list
                -> [[T.Text]] -- ^ caselist:                         ex.
                -> T.Text     -- ^ headword:                         ex. "拡張/かくちょう"
                -> [[T.Text]] -- ^ The case frames obtained so far:  ex.
                -> [CaseFrame]
findArgument events caselist headwords caselistlist = case events of
  [] -> []
  ((StartElement "argument" attribs):es) ->
    let attrMap = M.fromList attribs in
    case (do
          freq <- M.lookup "frequency" attrMap
          cas <- M.lookup "case" attrMap
          return (freq,cas)) of
      Just (_,cas) ->
        case () of
          _ | ("ノ格" `T.isPrefixOf` cas || cas `elem` ["ガ２","デ格","ヘ格","カラ格","マデ格","ヨリ格","について","にもとづいて","にあわせて","にくわえて","にとって","において","未格","無格","外の関係","修飾","時間"]) -> findArgument es caselist headwords caselistlist
            | ("ト格" `T.isPrefixOf` cas) -> findComponent False False es caselist headwords caselistlist
            | otherwise -> findArgument es [cas:cl | cl<-caselist] headwords caselistlist
      Nothing -> findArgument es caselist headwords caselistlist
  ((EndElement "caseframe"):es) -> findCaseframe es headwords (caselist ++ caselistlist) --(updateList caselist caselistlist)
  (_:es) -> findArgument es caselist headwords caselistlist

findComponent :: Bool 
                 -> Bool 
                 -> [Event] 
                 -> [[T.Text]] 
                 -> T.Text 
                 -> [[T.Text]] 
                 -> [CaseFrame]
findComponent toSfound toNPfound events caselist headwords caselistlist = case events of
  [] -> []
  ((EndElement "argument"):es) -> case (toSfound,toNPfound) of
                                    -- (True,True) -> findArgument es [cc:cl | cl<-caselist, cc<- ["ト節","ト格"]] headwords caselistlist
                                    (True,True) -> findArgument es [cc:cl | cl<-caselist, cc<- ["ト節"]] headwords caselistlist
                                    (True,False) -> findArgument es ["ト節":cl | cl<-caselist] headwords caselistlist
                                    -- (False,True) -> findArgument es ["ト格":cl | cl<-caselist] headwords caselistlist
                                    (False,True) -> findArgument es caselist headwords caselistlist
                                    (False,False) -> findArgument es caselist headwords caselistlist
  ((CharacterData text):es) -> if "補文" `T.isInfixOf` text
                                 then findComponent True toNPfound es caselist headwords caselistlist
                                 else findComponent toSfound True es caselist headwords caselistlist
  (_:es) -> findComponent toSfound toNPfound es caselist headwords caselistlist

{-
import Prelude as P hiding (readFile) 
import Data.Text as T -- this is required in order to substitute "Data.Text.Internal"
                      -- that Text.XML(.Cursor) uses.
import Data.Text.IO as T
import Text.XML as X -- Need 'xml-conduit' package
import Text.XML.Cursor as X -- Need 'xml-conduit' package
import qualified Data.List as L
import Data.Map as M
--import System.Environment (getArgs)

main :: IO()
main = do
  doc <- X.readFile def "/home/bekki/bigdata/cf.20160114.xml"
  let cursor = fromDocument doc
  print "hey"    
-}

{-
import Prelude as P 
import Text.XML.Expat.SAX
import Data.Text.Lazy
import bytestring-0.9.2.1:Data.ByteString.Lazy as L

--import Data.Text as T -- this is required in order to substitute "Data.Text.Internal"
                      -- that Text.XML(.Cursor) uses.
--import Data.Text.IO as T
--import Text.XML as X -- Need 'xml-conduit' package
--import Text.XML.Cursor as X -- Need 'xml-conduit' package
----import qualified Data.List as L
----import Data.Map as M

parsexml txt = parse defaultParseOptions txt :: [SAXEvent String String]

main :: IO()
main = do
  xml <- L.readFile "/home/bekki/bigdata/cf.20160114.xml"
  return $ (parsexml xml)
--  where 
--    query (StartElement "caseframe" attrs) = True
--    query _ = False
-}

