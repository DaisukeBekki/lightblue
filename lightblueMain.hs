{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative                         -- optparse-applicative
import Data.Semigroup ((<>))              --semigroup
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.Char as C           --base
import qualified Data.List as L           --base
import qualified Data.Ratio as R          --base
import qualified Data.Fixed as F          --base
import qualified System.IO as S           --base
import qualified System.Environment as E -- base
import qualified Data.Map as M            --container
import qualified Data.Time as Time        --time
import qualified Parser.ChartParser as CP
import qualified Parser.Japanese.MyLexicon as LEX
import qualified Interface as I
import qualified Interface.Text as T
import qualified Interface.JSeM as J
import qualified DTS.UDTT as DTS
import qualified DTS.Prover as Prover
import qualified DTStoProlog as D2P

data Options =
  Version
  | Stat
  | Test
  | Options Command FilePath Int Int Bool Bool
    deriving (Show, Eq)

data Command =
  Parse TaskName ParseInput I.Style
  | Infer ProverName InferInput
  | Debug Int Int
  | Demo
  | Treebank
    deriving (Show, Eq)

data TaskName = PARSE | POSTAG | NUMERATION deriving (Eq,Show)
instance Read TaskName where
  readsPrec _ r =
    [(PARSE,s) | (x,s) <- lex r, map C.toLower x == "parse"]
    ++ [(POSTAG,s) | (x,s) <- lex r, map C.toLower x == "postag"]
    ++ [(NUMERATION,s) | (x,s) <- lex r, map C.toLower x == "numeration"]

data ParseInput = SENTENCE | CORPUS deriving (Eq,Show)
instance Read ParseInput where
  readsPrec _ r =
    [(SENTENCE,s) | (x,s) <- lex r, map C.toLower x == "sentence"]
    ++ [(CORPUS,s) | (x,s) <- lex r, map C.toLower x == "corpus"]

data InferInput = PARAGRAPH | JSEM deriving (Eq,Show)
instance Read InferInput where
  readsPrec _ r =
    [(PARAGRAPH,s) | (x,s) <- lex r, map C.toLower x == "paragraph"]
    ++ [(JSEM,s) | (x,s) <- lex r, map C.toLower x == "jsem"]

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
                 <> progDesc "Usage: lightblue <global options> COMMAND <local options> <global options>"
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
                 (progDesc "Local options: [-t|--task parse|postag|numeration] [-i|--input sentence|corpus] [-s|--style html|text|tex|xml] (The default values: -t parse -i sentence -s html)" ))
      <> command "infer"
           (info inferOptionParser
                 (progDesc "Local options: [-p|--prover dts|coq] [-i|--input paragraph|jsem] (The default values: -p dts -i paragraph)" ))
      <> command "debug"
           (info debugOptionParser
                 (progDesc "shows all the parsing results between the two pivots. Local options: INT INT (No default values)" ))
      <> command "demo"
           (info (pure Demo)
                 (progDesc "sequentially shows parsing results of the corpus FILENAME.  No local options." ))
      <> command "treebank"
           (info (pure Treebank)
                 (progDesc "print a semantic treebank build from a given corpus. No local options" ))
      <> metavar "COMMAND (=parse|infer|debug|demo|treebank)"
      <> commandGroup "Available COMMANDs and thier local options"
      <> help "specifies the task to execute.  See 'Available COMMANDs ...' below about local options for each command"
      )
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
      ( long "typecheck"
      <> help "Show type-checking trees for SRs" )
    <*> switch 
      ( long "time"
      <> help "Show the execution time in stderr" )

debugOptionParser :: Parser Command
debugOptionParser = Debug
  <$> argument auto idm
  <*> argument auto idm

inferOptionParser :: Parser Command
inferOptionParser = Infer
  <$> option auto
    ( long "prover"
      <> short 'p'
      <> metavar "DTS|Coq" 
      <> showDefault
      <> value DTS
      <> help "Choose prover" )
  <*> option auto
    ( long "input"
      <> short 'i' 
      <> metavar "paragraph|jsem"
      <> showDefault
      <> value PARAGRAPH
      <> help "Specify input" )

parseOptionParser :: Parser Command
parseOptionParser = Parse
  <$> option auto
    ( long "task"
    <> short 't'
    <> metavar "parse|postag|numeration"
    <> help "Execute the specified task"
    <> showDefault
    <> value PARSE )
  <*> option auto
    ( long "input"
    <> short 'i' 
    <> metavar "sentence|corpus"
    <> help "Specify the style of input texts"
    <> showDefault
    <> value SENTENCE )
  <*> option auto
    ( long "style"
    <> short 's'
    <> metavar "text|tex|xml|html"
    <> help "Print results in the specified style"
    <> showDefault
    <> value I.HTML )

lightblueMain :: Options -> IO()
lightblueMain Version = showVersion
lightblueMain Stat = showStat
lightblueMain Test = test
lightblueMain (Options commands filepath nbest beamw iftypecheck iftime) = do
  start <- Time.getCurrentTime
  -- Main routine
  lightblueMainLocal commands
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
    lightblueMainLocal (Parse task input style) = do
      let handle = S.stdout
      sentences <- case filepath of
        "-" -> case input of
                 SENTENCE -> (\x -> [x]) <$> T.getLine
                 CORPUS   -> T.lines <$> T.getContents
        _   -> T.lines <$> T.readFile filepath
      S.hPutStrLn handle $ I.headerOf style
      mapM_
        (\sentence -> do
          nodes <- CP.simpleParse beamw sentence
          let nbestnodes = take nbest nodes;
              len = length nodes;
          case task of
            PARSE      -> I.printNodes      handle style iftypecheck nbestnodes
            POSTAG     -> I.posTagger       handle style nbestnodes
            NUMERATION -> I.printNumeration handle style sentence
          S.hPutStrLn S.stderr $ show (min (length nbestnodes) len) ++ " parse result(s) shown out of " ++ show len
          S.hPutStrLn handle $ I.interimOf style
          ) sentences
      S.hPutStrLn handle $ I.footerOf style
    -- |
    -- | Infer
    -- |
    lightblueMainLocal (Infer prover input) = do
      contents <- case filepath of
        "-" -> T.getContents
        _   -> T.readFile filepath
      let proverf = case prover of
           DTS -> Prover.checkEntailment beamw nbest
           Coq -> D2P.dts2prolog beamw nbest
      S.hPutStrLn S.stdout $ I.headerOf I.HTML
      case input of --  $ ligthblue infer -i jsem -f ../JSeM_beta/JSeM_beta_150415.xml
        PARAGRAPH -> do
          let sentences = T.lines contents;
              (premises,hypothesis) = if null sentences
                                         then ([],T.empty)
                                         else (L.init sentences,L.last sentences)
          proverf premises hypothesis
        JSEM -> mapM_ (\j -> do
                          mapM_ T.putStr ["JSeM [", J.jsem_id j, "] "]
                          proverf (J.premise j) (J.hypothesis j)
                          ) $ J.parseJSeM contents
      S.hPutStrLn S.stdout $ I.footerOf I.HTML
    -- |
    -- | Debug
    -- |
    lightblueMainLocal (Debug i j) = do
      sentence <- T.getLine
      chart <- CP.parse beamw sentence
      I.printNodes S.stdout I.HTML iftypecheck $ L.concat $ map (\(_,nodes) -> nodes) $ filter (\((x,y),_) -> i <= x && y <= j) $ M.toList chart
    -- |
    -- | Corpus (Parsing demo)
    -- |
    lightblueMainLocal Demo = do
      contents <- case filepath of
        "-" -> T.getContents
        _   -> T.readFile filepath
      processCorpus beamw $ T.lines contents
    --
    -- | Treebank Builder
    --
    lightblueMainLocal Treebank = do
      I.treebankBuilder beamw

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
  jumandic <- T.readFile $ jumandicpath ++ "Parser/Japanese/Juman.dic"
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
                             show ((fromRational ((toEnum i R.% toEnum total)*100))::F.Fixed F.E3),
                             "%)\n",
                             "Execution Time: ",
                             show totaltime,
                             " (average: ",
                             show ((fromRational ((toEnum (fromEnum totaltime)) R.% toEnum (total*1000000000000)))::F.Fixed F.E3),
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
  chart <- CP.parse beam sentence
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
                   else show ((fromRational (toEnum i R.% toEnum j)::F.Fixed F.E2) * 100)

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


