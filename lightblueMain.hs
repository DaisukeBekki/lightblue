{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative                         -- optparse-applicative
import Options.Applicative.Help.Core (parserUsage) -- optparse-applicative
import Data.Semigroup ((<>))              -- semigroup
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
import qualified Interface.HTML as HTML
import qualified Interface.JSeM as J
import qualified DTS.UDTT as DTS
import qualified DTS.Prover as Prover
import qualified DTStoProlog as D2P

data Options =
  Version
  | Stat
  | Treebank
  | Test
  | Options Command FilePath Int Int Bool Bool
    deriving (Show, Eq)

data Command =
  Parse String String I.Style
  | Infer ProverName String
  | Demo
  | Debug Int Int
    deriving (Show, Eq)

data ProverName = DTS | Coq deriving (Eq,Show)

instance Read ProverName where
  readsPrec _ r =
    [(DTS,s) | (x,s) <- lex r, map C.toLower x == "dts"]
    ++ [(Coq,s) | (x,s) <- lex r, map C.toLower x == "coq"]

-- | Main function.  Check README.md for the usage.
main :: IO()
main = execParser opts >>= lightblueMain 
  where opts = info (helper <*> optionParser)
                 ( fullDesc
                 <> progDesc "echo <sentence> | ./lightblue\n echo <sentence> | ./lightblue"
                 <> header "lightblue - a Japanese CCG parser with DTS representations (c) Bekki Laboratory" )

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
  flag' Stat ( long "treebank" 
             <> help "Print a semantic treebank build from a given corpus" )
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
                 (progDesc "parse --task parse|postag|numeration|sembank --input sentence|corpus --output html|text|tex|xml" ))
      <> command "infer"
           (info inferOptionParser
                 (progDesc "infer --prover dts|coq --input paragraph|jsem" ))
      <> command "demo"
           (info (pure Demo)
                 (progDesc "demo FILENAME: shows parsing results of the corpus FILENAME" ))
      <> command "debug"
           (info debugOptionParser
                 (progDesc "debug i j: shows all the parsing results between the pivots i and j." ))
      <> metavar "parse|infer|debug"
      <> help "Commands to execute.  See 'Available commands' below for options for each command"
      )
    <*> strOption 
      ( long "file"
      <> short 'f'
      <> metavar "FILEPATH"
      <> help "Read input texts from FILEPATH (Specify '-' to use stdin)"
      <> showDefault
      <> value "-" )
    <*> option auto 
      ( long "nbest"
      <> short 'n'
      <> help "Show N-best derivations"
      <> showDefault
      <> value 1
      <> metavar "N" )
    <*> option auto 
      ( long "beam"
      <> short 'b'
      <> help "Specify the beam width"
      <> showDefault
      <> value 24
      <> metavar "N" )
    <*> switch 
      ( long "typecheck"
      <> help "Show type-checking trees for the SR" )
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
  <*> strOption
    ( long "input"
      <> short 'i' 
      <> metavar "paragraph|jsem"
      <> showDefault
      <> value "paragraph"
      <> help "Specify input" )

parseOptionParser :: Parser Command
parseOptionParser = Parse
  <$> strOption
    ( long "task"
    <> short 't'
    <> metavar "parse|postag|numeration|sembank"
    <> help ("Execute the specified task"
             --"Usage for parse: cat <sentence> | lightblue -t parse > output.html "
             --"Usage for infer: cat <textfile> | lightblue -t infer > output.html "
             --"where <textfile> consists of premises and a coclusion" 
             --"(with one sentence per each line)"
            )
    <> showDefault
    <> value "parse" )
  <*> strOption 
    ( long "input"
    <> short 'i' 
    <> metavar "sentence|corpus"
    <> help "Specify the style of input texts"
    <> showDefault
    <> value "sentence" )
  <*> option auto
    ( long "output"
    <> short 'o'
    <> metavar "text|tex|xml|html"
    <> help "Print results in the specified format"
    <> showDefault
    <> value I.HTML )

unknownOptionError :: String -> IO()
unknownOptionError unknown = do
  S.hPutStr S.stderr "Unknown option: "
  S.hPutStrLn S.stderr $ show $ parserUsage defaultPrefs optionParser unknown

lightblueMain :: Options -> IO()
lightblueMain Version = showVersion
lightblueMain Stat = showStat
lightblueMain Treebank = treebankBuilder 24
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
    lightblueMainLocal (Parse task input output) = do
      let handle = S.stdout
      sentences <- case filepath of
        "-" -> case input of
                 "sentence" -> (\x -> [x]) <$> T.getLine
                 "corpus"   -> T.lines <$> T.getContents
                 _          -> do
                               unknownOptionError input
                               return []
        _   -> T.lines <$> T.readFile filepath
      S.hPutStrLn handle $ I.headerOf output
      mapM_
        (\sentence -> do
          nodes <- CP.simpleParse beamw sentence
          let nbestnodes = take nbest nodes
          case (task, output) of
            ("postag",_)     -> I.posTagger       handle output nbestnodes
            ("numeration",_) -> I.printNumeration handle output sentence
            ("parse",I.HTML) -> I.printNodesInHTML handle iftypecheck nbestnodes
            ("parse",I.TEXT) -> do
                                I.printNodesInText handle iftypecheck nbestnodes
                                let l = length nodes;
                                    b = length nbestnodes
                                S.hPutStrLn handle $ show (min b l) ++ " parse result(s) shown out of " ++ show l ++ "\n"
            ("parse",I.TEX)  -> I.printNodesInTeX  handle iftypecheck nbestnodes
            ("parse",I.XML)  -> I.printNodesInXML  handle sentence nbestnodes
            (t,f) -> unknownOptionError $ "task=" ++ t ++ ", output=" ++ (show f)
          ) sentences
      S.hPutStrLn handle $ I.footerOf output
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
      case input of --  $ ligthblue infer -i jsem -f ../JSeM_beta/JSeM_beta_150415.xml
        "paragraph" -> do
          let sentences = T.lines contents;
              (premises,hypothesis) = if null sentences
                                         then ([],T.empty)
                                         else (L.init sentences,L.last sentences)
          S.hPutStrLn S.stdout $ I.headerOf I.HTML
          proverf premises hypothesis
          S.hPutStrLn S.stdout $ I.footerOf I.HTML
        "jsem" -> mapM_ (\j -> do
                          mapM_ T.putStr ["JSeM [", J.jsem_id j, "] "]
                          proverf (J.premise j) (J.hypothesis j)
                          ) $ J.parseJSeM contents
        _ -> unknownOptionError input
    -- |
    -- | Corpus (Parsing demo)
    -- |
    lightblueMainLocal Demo = do
      contents <- case filepath of
        "-" -> T.getContents
        _   -> T.readFile filepath
      processCorpus beamw $ T.lines contents
    -- |
    -- | Debug
    -- |
    lightblueMainLocal (Debug i j) = do
      sentence <- T.getLine
      chart <- CP.parse beamw sentence
      I.printNodesInHTML S.stdout iftypecheck $ L.concat $ map (\(_,nodes) -> nodes) $ filter (\((x,y),_) -> i <= x && y <= j) $ M.toList chart

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

test :: IO()
test = do
  let context = [DTS.Con "hoge", DTS.Con "evt", DTS.Con "entity"]
  T.hPutStrLn S.stderr $ T.toText $ DTS.Judgment context (DTS.Var 0) DTS.Type
  T.hPutStrLn S.stderr $ T.toText $ DTS.Judgment context (DTS.Var 2) DTS.Type

-- | lightblue -t sembank
-- |
treebankBuilder :: Int -> IO()
treebankBuilder beam = do
  content <- T.getContents
  nodes <- mapM (CP.simpleParse beam) $ T.lines content
  S.putStrLn HTML.htmlHeader4MathML
  T.putStrLn HTML.startMathML
  T.putStrLn $ DTS.toVerticalMathML $ map (CP.sem . head) nodes
  T.putStrLn HTML.endMathML
  S.putStrLn HTML.htmlFooter4MathML

-- | lightblue --corpus filepath
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


