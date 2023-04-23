{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative hiding (style) --optparse-applicative
import Data.Semigroup ((<>))              --semigroup
import Control.Monad (forM_)              --base
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
--import qualified Data.Text as StrictT     --text
import qualified Data.Text.IO as StrictT  --text
import Data.Ratio ((%))                   --base
import qualified Data.Char as C           --base
import qualified Data.List as L           --base
import qualified Data.Fixed as F          --base
import qualified System.IO as S           --base
import qualified System.Environment as E -- base
import qualified Data.Map as M            --container
import qualified Data.Time as Time        --time
import qualified Parser.ChartParser as CP
import qualified Parser.Language.Japanese.MyLexicon as LEX
import qualified Interface as I
import qualified Interface.Text as T
import qualified JSeM as J
import qualified JSeM.XML as J
import qualified DTS.UDTT as DTS
--import qualified DTS.Prover.TypeChecker as TC
import qualified DTS.Prover as Prover
import qualified DTS.DTStoProlog as D2P

data Options =
  Version
  | Stat
  | Test
  | Options Command ParseInput FilePath Int Int Bool
    deriving (Show, Eq)

data Command =
  Parse ParseOutput I.Style Bool
  | Infer ProverName
  | Debug Int Int
  | Demo
  | Treebank
  | JSeMParser
    deriving (Show, Eq)

--commandReader :: String -> a -> String -> [(a,String)]
--commandReader r command option = [(command,s) | (x,s) <- lex r, map C.toLower x == option]

data ParseInput = SENTENCES | JSEM deriving (Eq,Show)
instance Read ParseInput where
  readsPrec _ r =
    [(SENTENCES,s) | (x,s) <- lex r, map C.toLower x == "sentences"]
    ++ [(JSEM,s) | (x,s) <- lex r, map C.toLower x == "jsem"]

data ParseOutput = TREE | POSTAG | NUMERATION deriving (Eq,Show)
instance Read ParseOutput where
  readsPrec _ r =
    [(TREE,s) | (x,s) <- lex r, map C.toLower x == "tree"]
    ++ [(POSTAG,s) | (x,s) <- lex r, map C.toLower x == "postag"]
    ++ [(NUMERATION,s) | (x,s) <- lex r, map C.toLower x == "numeration"]

data ProverName = DTS | Coq deriving (Eq,Show)
instance Read ProverName where
  readsPrec _ r =
    [(DTS,s) | (x,s) <- lex r, map C.toLower x == "dts"]
    ++ [(Coq,s) | (x,s) <- lex r, map C.toLower x == "coq"]

-- | Main function.  Check README.md for the usage.
main :: IO()
main = customExecParser p opts >>= lightblueMain 
  where opts = info (helper <*> optionParser)
                 ( fullDesc
                 <> progDesc "Usage: lightblue COMMAND <local options> <global options>"
                 <> header "lightblue - a Japanese CCG parser with DTS representations (c) Daisuke Bekki and Bekki Laboratory" )
        p = prefs showHelpOnEmpty

-- <$> :: (a -> b) -> Parser a -> Parser b
-- <*> :: Parser (a -> b) -> Parser a -> Parser b

optionParser :: Parser Options
optionParser = 
  flag' Version ( long "version" 
                <> short 'v' 
                <> help "Print the lightblue version" )
  <|> 
  flag' Stat ( long "stat" 
             <> help "Print the lightblue statistics" )
  <|> 
  flag' Test ( long "test" 
             <> hidden 
             <> internal
             <> help "Execute the test code" )
  <|> 
  Options
    <$> subparser 
      (command "parse"
           (info parseOptionParser
                 (progDesc "Local options: [-o|--output tree|postag|numeration] [-s|--style html|text|tex|xml] [--typecheck] (The default values: -o tree -s html)" ))
      <> command "infer"
           (info inferOptionParser
                 (progDesc "Local options: [-p|--prover dts|coq] (The default values: -p dts)" ))
      <> command "debug"
           (info debugOptionParser
                 (progDesc "shows all the parsing results between the two pivots. Local options: INT INT (No default values)" ))
      <> command "demo"
           (info (pure Demo)
                 (progDesc "sequentially shows parsing results of a given corpus. No local options." ))
      <> command "treebank"
           (info (pure Treebank)
                 (progDesc "print a semantic treebank build from a given corpus. No local options" ))
      <> command "jsemparser"
           (info (pure JSeMParser)
                 (progDesc "parse a jsem file. No local options" ))
      <> metavar "COMMAND (=parse|infer|debug|demo)"
      <> commandGroup "Available COMMANDs and thier local options"
      <> help "specifies the task to execute.  See 'Available COMMANDs ...' below about local options for each command"
      )
    <*> option auto
      ( long "input"
        <> short 'i' 
        <> metavar "sentences|jsem"
        <> value SENTENCES
        <> help "Specify input type (default: sentences)" )
    <*> strOption 
      ( long "file"
      <> short 'f'
      <> metavar "FILEPATH"
      <> help "Reads input texts from FILEPATH (Specify '-' to use stdin)"
      <> showDefault
      <> value "-" )
    <*> option auto 
      ( long "nbest"
      <> short 'n'
      <> help "Show N-best results"
      <> showDefault
      <> value 1
      <> metavar "INT" )
    <*> option auto 
      ( long "beam"
      <> short 'b'
      <> help "Specify the beam width"
      <> showDefault
      <> value 24
      <> metavar "INT" )
    <*> switch 
      ( long "time"
      <> help "Show the execution time in stderr" )

inferOptionParser :: Parser Command
inferOptionParser = Infer
  <$> option auto
    ( long "prover"
      <> short 'p'
      <> metavar "DTS|Coq"
      <> showDefault
      <> value DTS
      <> help "Choose prover" )

debugOptionParser :: Parser Command
debugOptionParser = Debug
  <$> argument auto idm
  <*> argument auto idm

parseOptionParser :: Parser Command
parseOptionParser = Parse
  <$> option auto
    ( long "output"
    <> short 'o'
    <> metavar "tree|postag|numeration"
    <> help "Specify the output content"
    <> showDefault
    <> value TREE )
  <*> option auto
    ( long "style"
    <> short 's'
    <> metavar "text|tex|xml|html"
    <> help "Print results in the specified style"
    <> showDefault
    <> value I.HTML )
  <*> switch 
    ( long "typecheck"
    <> help "Show type-checking trees for SRs" )

lightblueMain :: Options -> IO()
lightblueMain Version = showVersion
lightblueMain Stat = showStat
lightblueMain Test = test
lightblueMain (Options commands input filepath nbest beamw iftime) = do
  start <- Time.getCurrentTime
  contents <- case filepath of
    "-" -> T.getContents
    _   -> T.readFile filepath
  -- Main routine
  lightblueMainLocal commands contents
  -- Show execution time
  stop <- Time.getCurrentTime
  let time = Time.diffUTCTime stop start
  if iftime
     then S.hPutStrLn S.stderr $ "Total Execution Time: " ++ show time
     else return ()
  where
    -- |
    -- | Parse
    -- |
    lightblueMainLocal (Parse output style iftypecheck) contents = do
      let handle = S.stdout;
      sentences <- case input of 
                     SENTENCES -> return $ T.lines contents
                     JSEM -> do
                             parsedJSeM <- J.xml2jsemData $ T.toStrict contents
                             return $ concat $ map (\j -> (map T.fromStrict $ J.premises j) ++ [T.fromStrict $ J.hypothesis j]) parsedJSeM
      S.hPutStrLn handle $ I.headerOf style
      mapM_
        (\(sid,sentence) -> do
          nodes <- CP.simpleParse beamw sentence
          let nbestnodes = take nbest nodes;
              len = length nodes;
          case output of
            TREE       -> I.printNodes      handle style sid sentence iftypecheck nbestnodes
            POSTAG     -> I.posTagger       handle style nbestnodes
            NUMERATION -> I.printNumeration handle style sentence
          S.hPutStrLn handle $ I.interimOf style $ "[" ++ show (min (length nbestnodes) len) ++ " parse result(s) shown out of " ++ show len ++ " for s" ++ (show $ sid) ++ "]"
          ) $ zip ([0..]::[Int]) sentences
      S.hPutStr handle $ I.footerOf style
    -- |
    -- | Infer
    -- |
    lightblueMainLocal (Infer prover) contents = do
      let handle = S.stdout;
          proverf = case prover of
           DTS -> Prover.checkEntailment beamw nbest
           Coq -> D2P.dts2prolog beamw nbest
      case prover of
        DTS -> S.hPutStrLn handle $ I.headerOf I.HTML
        Coq -> return ()
      case input of --  $ ligthblue infer -i jsem -f ../JSeM_beta/JSeM_beta_150415.xml
        SENTENCES -> do
          let sentences = T.lines contents;
              (premises,hypothesis) = if null sentences
                                         then ([],T.empty)
                                         else (init sentences, last sentences)
          proverf premises hypothesis
        JSEM -> do
                --S.hPutStrLn S.stdout $ I.headerOf I.HTML
                parsedJSeM <- J.xml2jsemData $ T.toStrict contents
                mapM_ (\j -> do
                          mapM_ T.putStr ["JSeM [", T.fromStrict $ J.jsem_id j, "] "]
                          proverf (map T.fromStrict $ J.premises j) (T.fromStrict $ J.hypothesis j)
                          ) parsedJSeM
                --S.hPutStrLn S.stdout $ I.footerOf I.HTML
      case prover of
        DTS -> S.hPutStrLn handle $ I.footerOf I.HTML
        Coq -> return ()
    -- |
    -- | Debug
    -- |
    --lightblueMainLocal (Debug i j) contents = do
    lightblueMainLocal (Debug _ _) contents = do
      parsedJSeM <- J.xml2jsemData $ T.toStrict contents
      let sentences = case input of 
            SENTENCES -> T.lines contents
            JSEM -> concat $ map (\jsem -> (map T.fromStrict $ J.premises jsem) ++ [T.fromStrict $ J.hypothesis jsem]) parsedJSeM
      mapM_
        --(\(sid,sentence) -> do
        (\(_,sentence) -> do
          chart <- CP.parse beamw True (\_ _ -> id) sentence
          --let filterednodes = concat $ map snd $ filter (\((x,y),_) -> i <= x && y <= j) $ M.toList chart
          --I.printNodes S.stdout I.HTML sid sentence False filterednodes
          mapM_ (\((x,y),node) -> do
                                  S.putStr $ "(" ++ (show x) ++ "," ++ (show y) ++ ") "
                                  if null node
                                     then S.putStrLn ""
                                     else T.putStrLn $ T.toText $ CP.cat $ head node
                                  ) $ M.toList chart
          ) $ zip ([0..]::[Int]) sentences
    -- |
    -- | Demo (sequential parsing of a given corpus)
    -- |
    lightblueMainLocal Demo contents = do
      processCorpus beamw $ T.lines contents
    --
    -- | Treebank Builder
    --
    lightblueMainLocal Treebank contents = do
      I.treebankBuilder beamw $ T.lines contents
    --
    -- | JSeM Parser
    -- 
    lightblueMainLocal JSeMParser contents = do
      parsedJSeM <- J.xml2jsemData $ T.toStrict contents
      forM_ parsedJSeM $ \jsem -> do
        putStr $ show $ J.answer jsem
        S.putChar '\t'
        mapM_ StrictT.putStr $ J.premises jsem
        S.putChar '\t' 
        StrictT.putStrLn $ J.hypothesis jsem

-- | lightblue --version
-- |
showVersion :: IO()
showVersion = do
  T.putStr "lightblue version: "
  lightbluepath <- E.getEnv "LIGHTBLUE"
  cabal <- T.readFile $ lightbluepath ++ "lightblue.cabal"
  T.putStrLn $ last $ T.words $ head $ filter (T.isPrefixOf "version:") $ T.lines cabal

-- | lightblue --status
-- |
showStat :: IO()
showStat = do
  putStrLn "lightblue: "
  putStr "  "
  putStr $ show $ length $ LEX.emptyCategories
  putStrLn " empty categories from CCG book"
  putStr "  "
  putStr $ show $ length $ LEX.myLexicon
  putStrLn " lexical entries for closed words from CCG book"
  jumandicpath <- E.getEnv "LIGHTBLUE"
  jumandic <- T.readFile $ jumandicpath ++ "src/Parser/Japanese/Juman.dic"
  putStr "  "
  putStr $ show $ length $ T.lines jumandic
  putStrLn " lexical entries for open words from JUMAN++"

-- | lightblue --test
-- | 
test :: IO()
test = do
  let context = [DTS.Con "hoge", DTS.Con "evt", DTS.Con "entity"]
  T.hPutStrLn S.stderr $ T.toText $ DTS.Judgment context (DTS.Var 0) DTS.Type
  T.hPutStrLn S.stderr $ T.toText $ DTS.Judgment context (DTS.Var 2) DTS.Type

-- | lightblue demo
-- |
processCorpus :: Int -> [T.Text] -> IO()
processCorpus beam contents = do
    start <- Time.getCurrentTime
    (i,j,k,total) <- L.foldl' (parseSentence beam) (return (0,0,0,0)) $ filter isSentence contents
    stop <- Time.getCurrentTime
    let totaltime = Time.diffUTCTime stop start
    mapM_ (S.hPutStr S.stdout) [
      "Results: Full:Partial:Error = ",
      show i,
      ":",
      show j,
      ":",
      show k,
      ", Full/Total = ",
      show i,
      "/",
      show total,
      " (",
      show ((fromRational ((toEnum i % toEnum total)*100))::F.Fixed F.E3),
      "%)\n",
      "Execution Time: ",
      show totaltime,
      " (average: ",
      show ((fromRational ((toEnum (fromEnum totaltime)) % toEnum (total*1000000000000)))::F.Fixed F.E3),
      "s/sentence)\n"
      ]
    where isSentence t = not (T.null t || "（" `T.isSuffixOf` t)

parseSentence :: Int                    -- ^ beam width
                 -> IO(Int,Int,Int,Int) -- ^ (The number of fully succeeded, partially succeeded, failed, and total parses)
                 -> T.Text           -- ^ A next sentence to parse
                 -> IO(Int,Int,Int,Int)
parseSentence beam score sentence = do
  (i,j,k,total) <- score
  S.putStr $ "[" ++ show (total+1) ++ "] "
  T.putStrLn sentence
  chart <- CP.parse beam True (\_ _ -> id) sentence
  case CP.extractParseResult beam chart of
    CP.Full nodes -> 
       do
       T.putStrLn $ T.toText $ head $ nodes
       T.putStr $ T.concat ["Fully parsed, Full:Partial:Failed = ", T.pack (show $ i+1), ":", T.pack (show j), ":", T.pack (show k), ", Full/Total = ", T.pack (show $ i+1), "/", T.pack (show $ total+1), " ("] 
       S.putStrLn $ percent (i+1,total+1) ++ "%)\n"
       return (i+1,j,k,total+1)
    CP.Partial nodes -> 
       do
       T.putStrLn $ T.toText $ head $ nodes
       T.putStr $ T.concat ["Partially parsed, Full:Partial:Failed = ", T.pack (show i), ":", T.pack (show $ j+1), ":", T.pack (show k), ", Full/Total = ", T.pack (show $ i+1), "/", T.pack (show $ total+1), " ("]
       S.putStrLn $ percent (i,total+1) ++ "%)\n"
       return (i,j+1,k,total+1)
    CP.Failed ->
       do
       T.putStr $ T.concat ["Failed, Full:Partial:Failed = ", T.pack (show i), ":", T.pack (show $ j), ":", T.pack (show $ k+1), ", Full/Total = ", T.pack (show $ i+1), "/", T.pack (show $ total+1), " ("]
       S.putStrLn $ percent (i,total+1) ++ "%)\n"
       return (i,j,k+1,total+1)

percent :: (Int,Int) -> String
percent (i,j) = if j == 0
                   then show (0::F.Fixed F.E2)
                   else show ((fromRational (toEnum i % toEnum j)::F.Fixed F.E2) * 100)

{-
unknownOptionError :: String -> IO()
unknownOptionError unknown = do
  S.hPutStr S.stderr "Not supported: "
  S.hPutStrLn S.stderr $ show $ parserUsage defaultPrefs optionParser unknown
-}

{-  
  let nbest = 3;
      bestNodes =  map (take nbest) [[1,2,3,4],[5],[7,8,9,10]]::[[Int]];
      doubledNodes = map (map (\node -> (node, show node))) bestNodes
      chozenNodes = Prover.choice doubledNodes; 
      zippedNodes = map unzip chozenNodes;
      tripledNodes = map (\(ns,ss) -> (ns,ss,[sum ns])) zippedNodes;
      --nodeSrPrList = dropWhile (\(_,_,ps) -> ps /= []) tripledNodes;
  print bestNodes
  print doubledNodes
  print chozenNodes
  print zippedNodes
  print tripledNodes
-}

{-
proofSearch :: [DTS.Signature] 
               -> [DTS.Preterm]  -- ^ hypothesis:premises
               -> [Ty.UTree Ty.UJudgement]
proofSearch sig nodes = 
  case nodes of
    [] -> []
    (t:ts) -> Ty.proofSearch ts (("evt",DTS.Type):("entity",DTS.Type):sig) t

-- | lightblue --fuman (hidden option)
-- | transforms an input (from stdin) each of whose line is a json entry
-- | into an output (to stdout) each of whose line is a paragraph.
-- | Usage:
-- | cat <file> | lightblue --fuman | head -n | Fuman/para2sentence > ...txt
-- |
fuman2text :: IO()
fuman2text = do
  jsonStrings <- T.getContents
  mapM_ T.putStrLn $ M.catMaybes $ map (\j -> j ^? key "fuman" . _String) $ T.lines jsonStrings

processJSeMData :: J.JSeMData -> IO()
processJSeMData jsemdata = do
  let sem = DTS.betaReduce $ currying psems hsem
  T.putStrLn $ T.toText sem

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
-}


