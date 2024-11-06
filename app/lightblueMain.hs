{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Options.Applicative hiding (style) --optparse-applicative
--import Data.Semigroup ((<>))              --semigroup
import Control.Monad (forM)               --base
import ListT (toList)                     --list-t
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
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
import qualified Parser.Language.Japanese.Lexicon as LEX
import qualified Parser.Language.Japanese.MyLexicon as LEX
import qualified Parser.Language.Japanese.Juman.CallJuman as Juman
import Parser.Language (jpOptions)
import qualified Interface as I
import qualified Interface.Text as T
import qualified JSeM as J
import qualified JSeM.XML as J
import qualified DTS.UDTTdeBruijn as UDTT
import qualified DTS.DTTdeBruijn as DTT
import DTS.TypeChecker (typeInfer,nullProver)
import qualified DTS.QueryTypes as QT
import qualified DTS.NaturalLanguageInference as NLI
import qualified JSeM as JSeM                         --jsem
import qualified ML.Exp.Classification.Bounded as NLP --nlp-tools

data Options =
  Version
  | Stat
  | Test
  | Options Command FilePath Juman.MorphAnalyzerName Int Int Int Int Bool Bool Bool Bool
    deriving (Show, Eq)

data Command =
  Parse I.ParseOutput I.Style NLI.ProverName
  | JSeM I.Style NLI.ProverName Int
  | Numeration I.Style
  -- | Debug Int Int
  | Demo
  -- | Treebank
    deriving (Show, Eq)

--commandReader :: String -> a -> String -> [(a,String)]
--commandReader r command option = [(command,s) | (x,s) <- lex r, map C.toLower x == option]

-- data ParseInput = SENTENCES | JSEM deriving (Eq,Show)
-- instance Read ParseInput where
--   readsPrec _ r =
--     [(SENTENCES,s) | (x,s) <- lex r, map C.toLower x == "sentences"]
--     ++ [(JSEM,s) | (x,s) <- lex r, map C.toLower x == "jsem"]

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
                 (progDesc "Local options: [-o|--output tree|postag] [-s|--style html|text|tex|xml] [--noTypeCheck] [--noInference] [-p|--prover wani|null] (The default values: -o tree -s html -p wani)" ))
      <> command "jsem"
           (info jsemOptionParser
                 (progDesc "Local options: [-s|--style html|text|tex|xml] [--noTypeCheck] [-p|--prover wani|null] (The default values: -s html -p wani)" ))
      -- <> command "infer"
      --      (info inferOptionParser
      --            (progDesc "Local options: [-p|--prover wani|null] [--nsample n] (The default values: -p wani --nsample 0)" ))
      <> command "numeration"
           (info numerationOptionParser
                 (progDesc "Local option: [-s|--style html|text|tex|xml] (shows all the lexical items in each of the numeration for the inupt sentences" ))
      -- <> command "debug"
      --      (info debugOptionParser
      --            (progDesc "shows all the parsing results between the two pivots. Local options: INT INT (No default values)" ))
      <> command "demo"
           (info (pure Demo)
                 (progDesc "sequentially shows parsing results of a given corpus. No local options." ))
      -- <> command "treebank"
      --      (info (pure Treebank)
      --            (progDesc "print a semantic treebank build from a given corpus. No local options" ))
      <> metavar "COMMAND (=parse|jsem|numeration|demo)"
      <> commandGroup "Available COMMANDs and thier local options"
      <> help "specifies the task to execute.  See 'Available COMMANDs ...' below about local options for each command"
      )
    -- <*> option auto
    --   ( long "input"
    --     <> short 'i' 
    --     <> metavar "sentences|jsem"
    --     <> value SENTENCES
    --     <> help "Specify input type (default: sentences)" )
    <*> strOption 
      ( long "file"
      <> short 'f'
      <> metavar "FILEPATH"
      <> help "Reads input texts from FILEPATH (Specify '-' to use stdin)"
      <> showDefault
      <> value "-" )
    <*> option auto
      ( long "ma"
        <> short 'm' 
        <> metavar "juman|jumanpp|kwja"
        <> value Juman.KWJA
        <> help "Specify morphological analyzer (default: KWJA)" )
    <*> option auto 
      ( long "beam"
      <> short 'b'
      <> help "Specify the beam width"
      <> showDefault
      <> value 32
      <> metavar "INT" )
    <*> option auto 
      ( long "nparse"
      -- <> short 'n'
      <> help "Show N-best parse trees for each sentence"
      <> showDefault
      <> value (-1)
      <> metavar "INT" )
    <*> option auto 
      ( long "ntypecheck"
      -- <> short 'n'
      <> help "Show N-best type check diagram for each logical form"
      <> showDefault
      <> value (-1)
      <> metavar "INT" )
    <*> option auto 
      ( long "nproof"
      -- <> short 'n'
      <> help "Show N-best proof diagram for each proof search"
      <> showDefault
      <> value (-1)
      <> metavar "INT" )
    <*> switch 
      ( long "noTypeCheck"
      <> help "If True, execute no type checking for LFs" )
    <*> switch 
      ( long "noInference"
      <> help "If true, execute no inference" )
    <*> switch 
      ( long "time"
      <> help "Show the execution time in stderr" )
    <*> switch 
      ( long "verbose"
      <> help "Show logs of type inferer and type checker" )

parseOptionParser :: Parser Command
parseOptionParser = Parse
  <$> option auto
    ( long "output"
    <> short 'o'
    <> metavar "tree|postag"
    <> help "Specify the output content"
    <> showDefault
    <> value I.TREE )
  <*> option auto
    ( long "style"
    <> short 's'
    <> metavar "text|tex|xml|html"
    <> help "Print results in the specified format"
    <> showDefault
    <> value I.HTML )
  <*> option auto
    ( long "prover"
      <> short 'p'
      <> metavar "Wani|Null"
      <> showDefault
      <> value NLI.Wani
      <> help "Choose prover" )

jsemOptionParser :: Parser Command
jsemOptionParser = JSeM
  <$> option auto
    ( long "style"
    <> short 's'
    <> metavar "text|tex|xml|html"
    <> help "Print results in the specified format"
    <> showDefault
    <> value I.HTML )
  <*> option auto
    ( long "prover"
      <> short 'p'
      <> metavar "Wani|Null"
      <> showDefault
      <> value NLI.Wani
      <> help "Choose prover" )
  <*> option auto
    ( long "nsample"
    <> metavar "text|tex|xml|html"
    <> help "How many data to process: 0 means all data"
    <> showDefault
    <> value (-1)
    <> metavar "INT" )  

numerationOptionParser :: Parser Command
numerationOptionParser = Numeration
  <$> option auto
    ( long "style"
    <> short 's'
    <> metavar "text|tex|xml|html"
    <> help "Print results in the specified format"
    <> showDefault
    <> value I.HTML )

-- debugOptionParser :: Parser Command
-- debugOptionParser = Debug
--   <$> argument auto idm
--   <*> argument auto idm

lightblueMain :: Options -> IO()
lightblueMain Version = showVersion
lightblueMain Stat = showStat
lightblueMain Test = test
lightblueMain (Options commands filepath morphaName beamW nParse nTypeCheck nProof noTypeCheck noInference iftime verbose) = do
  start <- Time.getCurrentTime
  contents <- case filepath of
    "-" -> T.getContents
    _   -> T.readFile filepath
  lexicalResource <- LEX.lexicalResourceBuilder morphaName
  -- | Main routine
  lightblueMainLocal commands lexicalResource contents
  -- | Show execution time
  stop <- Time.getCurrentTime
  let time = Time.diffUTCTime stop start
  if iftime
     then S.hPutStrLn S.stderr $ "Total Execution Time: " ++ show time
     else return ()
  where
    -- |
    -- | Parse
    -- |
    lightblueMainLocal (Parse output style proverName) lr contents = do
      let handle = S.stdout
          parseSetting = CP.ParseSetting jpOptions lr beamW nParse nTypeCheck nProof True Nothing Nothing noInference verbose
          prover = NLI.getProver proverName $ QT.ProofSearchSetting Nothing Nothing (Just QT.Classical)
          parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity)] [] $ T.lines contents
          posTagOnly = case output of 
                         I.TREE -> False
                         I.POSTAG -> True
      S.hPutStrLn handle $ I.headerOf style
      NLI.printParseResult handle style 1 noTypeCheck posTagOnly parseResult
      S.hPutStrLn handle $ I.footerOf style
    --
    -- | JSeM Parser
    -- 
    lightblueMainLocal (JSeM style proverName nSample) lr contents = do
      parsedJSeM <- J.xml2jsemData $ T.toStrict contents
      let handle = S.stdout
          parseSetting = CP.ParseSetting jpOptions lr beamW nParse nTypeCheck nProof True Nothing Nothing noInference verbose
          prover = NLI.getProver proverName $ QT.ProofSearchSetting Nothing Nothing (Just QT.Classical)
          parsedJSeM'
            | nSample < 0 = parsedJSeM
            | otherwise = take nSample parsedJSeM
      S.hPutStrLn handle $ I.headerOf style
      pairs <- forM parsedJSeM' $ \j -> do
        mapM_ T.putStr ["[JSeM id: ", T.fromStrict $ J.jsem_id j, "] "]
        mapM_ StrictT.putStr $ J.premises j
        S.putStr " ==> "
        StrictT.putStrLn $ J.hypothesis j
        S.putStr "\n"
        let sentences = reverse $ (T.fromStrict $ J.hypothesis j):(map T.fromStrict $ J.premises j)
            parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity)] [] sentences
        NLI.printParseResult handle style 1 noTypeCheck False parseResult
        inferenceLabels <- toList $ NLI.trawlParseResult parseResult
        let groundTruth = J.jsemLabel2YesNo $ J.answer j
            prediction = case inferenceLabels of
              [] -> J.Other
              (bestLabel:_) -> bestLabel
        S.putStrLn $ "\nPrediction: " ++ (show prediction) ++ "\nGround truth: " ++ (show groundTruth) ++ "\n"
        return (prediction, groundTruth)
      T.putStrLn $ T.fromStrict $ NLP.showClassificationReport pairs
      S.hPutStrLn handle $ I.footerOf style
    -- | 
    -- | Numeration
    -- | 
    lightblueMainLocal (Numeration style) lr contents = do
      let handle = S.stdout
          sentences = T.lines contents
      S.hPutStrLn handle $ I.headerOf style
      mapM_ (\(sid,sentence) -> do
        S.hPutStrLn handle $ I.interimOf style $ "[" ++ (show sid) ++ "]"
        I.printNumeration handle style lr sentence
        ) $ zip ([1..]::[Int]) sentences
      S.hPutStrLn handle $ I.footerOf style
    -- |
    -- | Demo (sequential parsing of a given corpus)
    -- |
    lightblueMainLocal Demo lr contents = processCorpus lr beamW $ T.lines contents
    -- -- |
    -- -- | Debug
    -- -- |
    -- --lightblueMainLocal (Debug i j) contents = do
    -- lightblueMainLocal (Debug _ _) contents = do
    --   parsedJSeM <- J.xml2jsemData $ T.toStrict contents
    --   let sentences = T.lines contents
    --   forM_ () $ 
    --     (\(_,sentence) -> do
    --       chart <- CP.parse (CP.ParseSetting jpOptions morphaName beamW nParse nTypeCheck nProof True Nothing Nothing False False) sentence
    --       --let filterednodes = concat $ map snd $ filter (\((x,y),_) -> i <= x && y <= j) $ M.toList chart
    --       --I.printNodes S.stdout I.HTML sid sentence False filterednodes
    --       mapM_ (\((x,y),node) -> do
    --                               S.putStr $ "(" ++ (show x) ++ "," ++ (show y) ++ ") "
    --                               if null node
    --                                  then S.putStrLn ""
    --                                  else T.putStrLn $ T.toText $ CP.cat $ head node
    --                               ) $ M.toList chart
    --       ) $ zip ([0..]::[Int]) sentences
    -- --
    -- -- | Treebank Builder
    -- --
    -- lightblueMainLocal Treebank contents = do
    --   I.treebankBuilder beamw $ T.lines contents

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
  let signature = [("entity", DTT.Type), ("evt",DTT.Type), ("f", DTT.Pi (DTT.Con "entity") DTT.Type)]
      context = [(DTT.Con "dog")]
      termA = UDTT.Sigma (UDTT.Con "entity") (UDTT.App (UDTT.Con "f") (UDTT.Var 0))
      -- typeA = DTS.Kind
      tcq = UDTT.TypeInferQuery signature context termA 
      pss = QT.ProofSearchSetting Nothing Nothing (Just QT.Classical)
  typeCheckResults <- toList $ typeInfer (nullProver pss) False tcq
  T.putStrLn $ T.toText $ head typeCheckResults
  --T.hPutStrLn S.stderr $ T.toText $ DTS.Judgment context (DTS.Var 0) DTS.Type
  --T.hPutStrLn S.stderr $ T.toText $ DTS.Judgment context (DTS.Var 2) DTS.Type

-- | lightblue demo
-- |
processCorpus :: LEX.LexicalResource -> Int -> [T.Text] -> IO()
processCorpus lr beamW contents = do
    start <- Time.getCurrentTime
    let parseSetting = CP.ParseSetting jpOptions lr beamW 1 1 1 True Nothing Nothing False False
    (i,j,k,total) <- L.foldl' (parseSentence parseSetting beamW) (return (0,0,0,0)) $ filter isSentence contents
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

parseSentence :: CP.ParseSetting
                 -> Int                 -- ^ beam width
                 -> IO(Int,Int,Int,Int) -- ^ (The number of fully succeeded, partially succeeded, failed, and total parses)
                 -> T.Text              -- ^ A next sentence to parse
                 -> IO(Int,Int,Int,Int)
parseSentence ps beamW score sentence = do
  (i,j,k,total) <- score
  S.putStr $ "[" ++ show (total+1) ++ "] "
  T.putStrLn sentence
  chart <- CP.parse ps sentence
  case CP.extractParseResult beamW chart of
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
