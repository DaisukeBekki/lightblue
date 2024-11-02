{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative hiding (style) --optparse-applicative
--import Data.Semigroup ((<>))              --semigroup
import Control.Monad (forM_)              --base
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
import qualified Parser.Language.Japanese.MyLexicon as LEX
import qualified Parser.Language.Japanese.Juman.CallJuman as Juman
import Parser.Language (LangOptions(..),jpOptions)
import qualified Interface as I
import qualified Interface.Text as T
import qualified JSeM as J
import qualified JSeM.XML as J
import qualified DTS.UDTTdeBruijn as UDTT
import qualified DTS.DTTdeBruijn as DTT
import DTS.TypeChecker (typeInfer,nullProver)
import qualified DTS.QueryTypes as QT
import qualified DTS.NaturalLanguageInference as NLI

data Options =
  Version
  | Stat
  | Test
  | Options Command ParseInput FilePath Juman.MorphAnalyzerName Int Int Int Int Int Bool Bool
    deriving (Show, Eq)

data Command =
  Parse I.ParseOutput I.Style Bool Bool NLI.ProverName
  -- | Infer NLI.ProverName
  | Numeration I.Style
  | Debug Int Int
  | Demo
  -- | Treebank
  | JSeMParser
    deriving (Show, Eq)

--commandReader :: String -> a -> String -> [(a,String)]
--commandReader r command option = [(command,s) | (x,s) <- lex r, map C.toLower x == option]

data ParseInput = SENTENCES | JSEM deriving (Eq,Show)
instance Read ParseInput where
  readsPrec _ r =
    [(SENTENCES,s) | (x,s) <- lex r, map C.toLower x == "sentences"]
    ++ [(JSEM,s) | (x,s) <- lex r, map C.toLower x == "jsem"]

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
      -- <> command "infer"
      --      (info inferOptionParser
      --            (progDesc "Local options: [-p|--prover wani|null] [--nsample n] (The default values: -p wani --nsample 0)" ))
      <> command "numeration"
           (info numerationOptionParser
                 (progDesc "Local option: [-s|--style html|text|tex|xml] (shows all the lexical items in each of the numeration for the inupt sentences" ))
      <> command "debug"
           (info debugOptionParser
                 (progDesc "shows all the parsing results between the two pivots. Local options: INT INT (No default values)" ))
      <> command "demo"
           (info (pure Demo)
                 (progDesc "sequentially shows parsing results of a given corpus. No local options." ))
      -- <> command "treebank"
      --      (info (pure Treebank)
      --            (progDesc "print a semantic treebank build from a given corpus. No local options" ))
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
      ( long "ma"
        <> short 'm' 
        <> metavar "juman|kwja"
        <> value Juman.KWJA
        <> help "Specify morphological analyzer (default: KWJA)" )
    <*> option auto 
      ( long "beam"
      <> short 'b'
      <> help "Specify the beam width"
      <> showDefault
      <> value 24
      <> metavar "INT" )
    <*> option auto 
      ( long "nparse"
      -- <> short 'n'
      <> help "Show N-best parse trees for each sentence"
      <> showDefault
      <> value 1
      <> metavar "INT" )
    <*> option auto 
      ( long "ntypecheck"
      -- <> short 'n'
      <> help "Show N-best type check diagram for each logical form"
      <> showDefault
      <> value 1
      <> metavar "INT" )
    <*> option auto 
      ( long "nproof"
      -- <> short 'n'
      <> help "Show N-best proof diagram for each proof search"
      <> showDefault
      <> value 1
      <> metavar "INT" )
    <*> option auto
      ( long "nsample"
      <> metavar "text|tex|xml|html"
      <> help "How many data to process: 0 means all data"
      <> showDefault
      <> value 0
      <> metavar "INT" )  
    <*> switch 
      ( long "time"
      <> help "Show the execution time in stderr" )
    <*> switch 
      ( long "verbose"
      <> help "Show logs of type inferer and type checker" )

-- inferOptionParser :: Parser Command
-- inferOptionParser = Infer
--   <$> option auto
--     ( long "prover"
--       <> short 'p'
--       <> metavar "Wani|Null"
--       <> showDefault
--       <> value NLI.Wani
--       <> help "Choose prover" )

numerationOptionParser :: Parser Command
numerationOptionParser = Numeration
  <$> option auto
    ( long "style"
    <> short 's'
    <> metavar "text|tex|xml|html"
    <> help "Print results in the specified format"
    <> showDefault
    <> value I.HTML )

debugOptionParser :: Parser Command
debugOptionParser = Debug
  <$> argument auto idm
  <*> argument auto idm

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
  <*> switch 
    ( long "noTypeCheck"
    <> help "Execute type checking for LFs" )
  <*> switch 
    ( long "noInference"
    <> help "True if it is not an inference (parse and type check only)" )
  <*> option auto
    ( long "prover"
      <> short 'p'
      <> metavar "Wani|Null"
      <> showDefault
      <> value NLI.Wani
      <> help "Choose prover" )

lightblueMain :: Options -> IO()
lightblueMain Version = showVersion
lightblueMain Stat = showStat
lightblueMain Test = test
lightblueMain (Options commands input filepath morphaName beamW nParse nTypeCheck nProof nSample iftime verbose) = do
  start <- Time.getCurrentTime
  contents <- case filepath of
    "-" -> T.getContents
    _   -> T.readFile filepath
  -- | Main routine
  lightblueMainLocal commands contents
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
    lightblueMainLocal (Parse output style noTypeCheck isInference proverName) contents = do
      let handle = S.stdout
          parseSetting = CP.ParseSetting jpOptions morphaName beamW nParse nTypeCheck nProof True Nothing Nothing isInference verbose
          prover = NLI.getProver proverName $ QT.ProofSearchSetting Nothing Nothing (Just QT.Intuitionistic)
      sentences <- case input of 
                     SENTENCES -> return $ T.lines contents
                     JSEM -> do
                             parsedJSeM <- J.xml2jsemData $ T.toStrict contents
                             let parsedJSeM' = if nSample == 0
                                                 then parsedJSeM
                                                 else take nSample parsedJSeM
                             return $ concat $ map (\j -> (map T.fromStrict $ J.premises j) ++ [T.fromStrict $ J.hypothesis j]) parsedJSeM'
      S.hPutStrLn handle $ I.headerOf style
      mapM_
        (\(sid,sentence) -> do
          let (NLI.MoreSentences parseResult) = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity)] [] [sentence]
          NLI.printSentenceAndParseTrees handle style noTypeCheck (case output of I.TREE -> False; I.POSTAG -> True) parseResult
            -- I.POSTAG     -> do
            --                 let (NLI.SentenceAndParseTrees _ parseTrees) = parseResult
            --                 parseTrees' <- toList parseTrees 
            --                 let nodes = map (\(NLI.ParseTreesAndFelicityCheck n _ _) -> n) parseTrees'
            --                 I.posTagger handle style $ take nParse nodes
          --let len = length nodes;
          S.hPutStrLn handle $ I.interimOf style $ "[" ++ (show sid) ++ "]" --"[" ++ show (min (length nbestnodes) len) ++ " parse result(s) shown out of " ++ show len ++ " for s" ++ (show $ sid) ++ "]"
          ) $ zip ([1..]::[Int]) sentences
      S.hPutStr handle $ I.footerOf style
    -- | 
    -- | Numeration
    -- | 
    lightblueMainLocal (Numeration style) contents = do
      let handle = S.stdout
          sentences = T.lines contents
      S.hPutStrLn handle $ I.headerOf style
      mapM_ (\(sid,sentence) -> do
        S.hPutStrLn handle $ I.interimOf style $ "[" ++ (show sid) ++ "]"
        I.printNumeration handle style morphaName sentence
        ) $ zip ([1..]::[Int]) sentences
      S.hPutStr handle $ I.footerOf style
    -- -- |
    -- -- | Infer
    -- -- |
    -- lightblueMainLocal (Infer proverName) contents = do
    --   let handle = S.stdout
    --       parseSetting = CP.ParseSetting jpOptions morphaName beamW True Nothing Nothing True
    --       inferenceSetting = NLI.InferenceSetting beamW nParse Nothing Nothing parseSetting typeCheck proverName
    --   S.hPutStrLn handle $ I.headerOf I.HTML
    --   case input of
    --     SENTENCES -> do -- lightblue infer -i sentence 
    --       let sentences = T.lines contents 
    --           inferencePair = if null sentences
    --             then NLI.InferencePair [] T.empty
    --             else NLI.InferencePair (init sentences) (last sentences)
    --       NLI.checkInference inferenceSetting inferencePair
    --     JSEM -> do  --  ligthblue infer -i jsem -f ../JSeM_beta/JSeM_beta_150415.xml
    --       parsedJSeM <- J.xml2jsemData $ T.toStrict contents
    --       let parsedJSeM' = if nsample == 0
    --                            then parsedJSeM
    --                            else take nsample parsedJSeM
    --       forM_ parsedJSeM' $ \j -> do
    --         mapM_ T.putStr ["JSeM [", T.fromStrict $ J.jsem_id j, "] "]
    --         let inferencePair = NLI.InferencePair (map T.fromStrict $ J.premises j) (T.fromStrict $ J.hypothesis j)
    --         --mapM_ (T.putStrLn . T.fromStrict) $ J.premises j
    --         --T.putStrLn $ T.fromStrict $ J.hypothesis j
    --         NLI.checkInference inferenceSetting inferencePair
    --   S.hPutStrLn handle $ I.footerOf I.HTML
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
          chart <- CP.parse (CP.ParseSetting jpOptions morphaName beamW nParse nTypeCheck nProof True Nothing Nothing False False) sentence
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
      processCorpus morphaName beamW $ T.lines contents
    -- --
    -- -- | Treebank Builder
    -- --
    -- lightblueMainLocal Treebank contents = do
    --   I.treebankBuilder beamw $ T.lines contents
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
  let signature = [("entity", DTT.Type), ("evt",DTT.Type), ("f", DTT.Pi (DTT.Con "entity") DTT.Type)]
      context = [(DTT.Con "dog")]
      termA = UDTT.Sigma (UDTT.Con "entity") (UDTT.App (UDTT.Con "f") (UDTT.Var 0))
      -- typeA = DTS.Kind
      tcq = UDTT.TypeInferQuery signature context termA 
      pss = QT.ProofSearchSetting Nothing Nothing (Just QT.Intuitionistic)
  typeCheckResults <- toList $ typeInfer (nullProver pss) False tcq
  T.putStrLn $ T.toText $ head typeCheckResults
  --T.hPutStrLn S.stderr $ T.toText $ DTS.Judgment context (DTS.Var 0) DTS.Type
  --T.hPutStrLn S.stderr $ T.toText $ DTS.Judgment context (DTS.Var 2) DTS.Type

-- | lightblue demo
-- |
processCorpus :: Juman.MorphAnalyzerName -> Int -> [T.Text] -> IO()
processCorpus morphaName beamW contents = do
    start <- Time.getCurrentTime
    (i,j,k,total) <- L.foldl' (parseSentence morphaName beamW) (return (0,0,0,0)) $ filter isSentence contents
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

parseSentence :: Juman.MorphAnalyzerName
                 -> Int                    -- ^ beam width
                 -> IO(Int,Int,Int,Int) -- ^ (The number of fully succeeded, partially succeeded, failed, and total parses)
                 -> T.Text           -- ^ A next sentence to parse
                 -> IO(Int,Int,Int,Int)
parseSentence morphaName beam score sentence = do
  (i,j,k,total) <- score
  S.putStr $ "[" ++ show (total+1) ++ "] "
  T.putStrLn sentence
  chart <- CP.parse (CP.ParseSetting jpOptions morphaName beam 1 1 1 True Nothing Nothing False False) sentence
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
