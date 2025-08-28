{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Options.Applicative hiding (style) --optparse-applicative
import Control.Applicative (optional)     --base
import Control.Monad (forM)               --base
import ListT (toList)                     --list-t
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.Text as StrictT     --text
import qualified Data.Text.IO as StrictT  --text
import Data.Ratio ((%))                   --base
import qualified Data.List as L           --base
import qualified Data.Fixed as F          --base
import qualified System.IO as S           --base
import qualified System.Environment as E -- base
import qualified Data.Map as M            --container
import qualified Data.Time as Time        --time
import qualified Parser.ChartParser as CP
import qualified Parser.PartialParsing as CP
import qualified Parser.Language.Japanese.Lexicon as JLEX
import qualified Parser.Language.Japanese.MyLexicon as JLEX
import qualified Parser.Language.Japanese.Juman.CallJuman as Juman
import qualified Parser.Language.Japanese.Filter as JFilter
import Parser.Language.Japanese.Filter.KNPFilter (knpFilter)    --lightblue
import Parser.Language.Japanese.Filter.KWJAFilter (kwjaFilter)  --lightblue
import qualified Parser.Language.English.Lexicon as ELEX
import Parser.Language (LangOptions(..))
import Parser.LangOptions (defaultJpOptions,defaultEnOptions)
import qualified Interface as I
import qualified Interface.Text as T
import qualified Interface.HTML as I
import qualified JSeM as J
import qualified JSeM.XML as J
import qualified DTS.UDTTdeBruijn as UDTT
import qualified DTS.DTTdeBruijn as DTT
import qualified DTS.DTTwithName as DTTwN
import DTS.TypeChecker (typeCheck,typeInfer,nullProver)
import qualified DTS.QueryTypes as QT
import qualified DTS.NaturalLanguageInference as NLI
import qualified JSeM as JSeM                         --jsem
import qualified ML.Exp.Classification.Bounded as NLP --nlp-tools

data Options = Options Lang Command I.Style NLI.ProverName FilePath Int Int Int Int Int Int Bool Bool Bool Bool

data Command =
  Parse I.ParseOutput
  | JSeM String Int
  | Numeration 
  | Demo
  | Version
  | Stat
  | Test
    deriving (Show, Eq)

data Lang = JP Juman.MorphAnalyzerName JFilter.FilterName | EN deriving (Show, Eq)

-- <$> :: (a -> b) -> Parser a -> Parser b
-- <*> :: Parser (a -> b) -> Parser a -> Parser b

jpParser :: Parser Lang
jpParser = JP
  <$> option auto
    ( long "ma"
      <> short 'm' 
      <> metavar "juman|jumanpp|kwja"
      <> value Juman.KWJA
      <> help "Specify morphological analyzer (default: KWJA)" )
  <*> option auto
    ( long "filter"
      <> metavar "knp|kwja|none"
      <> value JFilter.NONE
      <> help "Specify node filter (default: NONE)" )

enParser :: Parser Lang
enParser = pure EN

--commandReader :: String -> a -> String -> [(a,String)]
--commandReader r command option = [(command,s) | (x,s) <- lex r, map C.toLower x == option]

optionParser :: Parser Options
optionParser = 
  -- flag' Version ( long "version" 
  --               <> short 'v' 
  --               <> hidden
  --               <> help "Print the lightblue version" )
  -- <|> 
  -- flag' Stat ( long "stat"
  --            <> hidden 
  --            <> help "Print the lightblue statistics" )
  -- <|> 
  -- flag' Test ( long "test"
  --            <> hidden 
  --            <> internal
  --            <> help "Execute the test code" )
  -- <|> 
  Options
    <$> subparser
      (command "jp"
           (info jpParser
                 (progDesc "Local options: [-m juman|jumanpp|kwja] [--filter knp|kwja|none] (The default values: -m kwja --filter none" ))
      <> command "en"
           (info enParser
                 (progDesc "No local options" ))
      )
    <*> subparser 
      (command "parse"
           (info parseOptionParser
                 (progDesc "Local options: [-o|--output tree|postag] (The default values: -o tree)" ))
      <> command "jsem"
           (info jsemOptionParser
                 (progDesc "Local options: [--jsemid <int>] [--nSample <int>] (The default values:)" ))
      <> command "numeration"
           (info (pure Numeration)
                 (progDesc "Shows all the lexical items in each of the numeration for the inupt sentences." ))
      <> command "demo"
           (info (pure Demo)
                 (progDesc "Sequentially shows parsing results of a given corpus. No local options." ))
      <> command "version"
           (info (pure Version)
                 (progDesc "Print the lightblue version." ))
      <> command "stat"
           (info (pure Stat)
                 (progDesc "Print the lightblue statistics." ))
      <> command "test"
           (info (pure Test)
                 (progDesc "Execute the test code." ))
      -- <> command "debug"
      --      (info debugOptionParser
      --            (progDesc "shows all the parsing results between the two pivots. Local options: INT INT (No default values)" ))
      <> metavar "COMMAND (=parse|jsem|numeration|demo|version|stat)"
      <> commandGroup "Available COMMANDs and thier local options"
      <> help "specifies the task to execute.  See 'Available COMMANDs ...' below about local options for each command"
      )
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
    <*> strOption 
      ( long "file"
      <> short 'f'
      <> metavar "FILEPATH"
      <> help "Reads input texts from FILEPATH (Specify '-' to use stdin)"
      <> showDefault
      <> value "-" )
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
      <> value (-1)
      <> metavar "INT" )
    <*> option auto 
      ( long "maxdepth"
      <> help "Set the maximum search depth in proof search"
      <> showDefault
      <> value 5
      <> metavar "INT" )
    <*> option auto 
      ( long "maxtime"
      <> help "Set the maximum search time in proof search"
      <> showDefault
      <> value 100000
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

jsemOptionParser :: Parser Command
jsemOptionParser = JSeM
  <$> strOption
    ( long "jsemid"
      <> metavar "STRING"
      <> showDefault
      <> value "all"
      <> help "Skip JSeM data the JSeM ID of which is not equial to this value")
  <*> option auto
    ( long "nsample"
      <> showDefault
      <> value (-1)
      <> metavar "INT"
      <> help "How many data to process")

-- debugOptionParser :: Parser Command
-- debugOptionParser = Debug
--   <$> argument auto idm
--   <*> argument auto idm

-- | Main function.  Check README.md for the usage.
main :: IO ()
main = customExecParser p opts >>= lightblueMain 
  where opts = info (helper <*> optionParser)
                 ( fullDesc
                 <> progDesc "Usage: lightblue LANG COMMAND <local options> <global options>"
                 <> header "lightblue - a CCG parser with DTS (c) Daisuke Bekki and Bekki Laboratory" )
        p = prefs showHelpOnEmpty

lightblueMain :: Options -> IO ()
lightblueMain (Options lang commands style proverName filepath beamW nParse nTypeCheck nProof maxDepth maxTime noTypeCheck noInference ifTime verbose) = do
  start <- Time.getCurrentTime
  langOptions <- case lang of
                   JP morphaName filterName -> do
                        jpo <- defaultJpOptions
                        return $ jpo {
                          morphaName = morphaName,
                          nodeFilterBuilder = case filterName of
                                                JFilter.KNP  -> knpFilter
                                                JFilter.KWJA -> kwjaFilter
                                                JFilter.NONE -> \_ -> return (\_ _ -> id) 
                          }
                   EN -> return defaultEnOptions
  contents <- case filepath of
                "-" -> T.getContents
                _   -> T.readFile filepath
  let ifPurify = True
      ifDebug = Nothing
      parseSetting = CP.ParseSetting langOptions beamW nParse nTypeCheck nProof ifPurify ifDebug noInference verbose
  -- | Main routine
  lightblueMainLocal commands parseSetting contents
  -- | Show execution time
  stop <- Time.getCurrentTime
  if ifTime
     then S.hPutStrLn S.stderr $ "Total Execution Time: " ++ (show $ Time.diffUTCTime stop start)
     else return ()
  where
    -- |
    -- | Parse command
    -- |
    lightblueMainLocal (Parse output) parseSetting contents = do
      let handle = S.stdout
          prover = NLI.getProver proverName $ QT.ProofSearchSetting (Just maxDepth) Nothing (Just QT.Intuitionistic) False
          parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity)] [] $ T.lines contents
          posTagOnly = case output of 
                         I.TREE -> False
                         I.POSTAG -> True
      S.hPutStrLn handle $ I.headerOf style
      NLI.printParseResult handle style 1 noTypeCheck posTagOnly "input" parseResult
      S.hPutStrLn handle $ I.footerOf style
    --
    -- | JSeM command
    -- 
    lightblueMainLocal (JSeM jsemID nSample) parseSetting contents = do
      parsedJSeM <- J.xml2jsemData $ T.toStrict contents
      let parsedJSeM'
            | jsemID == "all" = parsedJSeM
            | otherwise = dropWhile (\j -> (J.jsem_id j) /= (StrictT.pack jsemID)) parsedJSeM
          parsedJSeM''
            | nSample < 0 = parsedJSeM'
            | otherwise = take nSample parsedJSeM'
          handle = S.stdout
          prover = NLI.getProver proverName $ QT.ProofSearchSetting (Just maxDepth) (Just maxTime) (Just QT.Intuitionistic) False
      S.hPutStrLn handle $ I.headerOf style
      pairs <- forM parsedJSeM'' $ \j -> do
        let title = "JSeM-ID " ++ (StrictT.unpack $ J.jsem_id j)
        S.putStr $ "[" ++ title ++ "] "
        mapM_ StrictT.putStr $ J.premises j
        S.putStr " ==> "
        StrictT.putStrLn $ J.hypothesis j
        S.putStr "\n"
        let sentences = postpend (map T.fromStrict $ J.premises j) (T.fromStrict $ J.hypothesis j)
            -- 公理を追加していない
            parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity)] [] sentences
            -- 727の公理(破くー破れる)
            -- parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity), ("yabuku",DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.App (DTT.App (DTT.App (DTT.Con "破く/やぶく/ガヲ") (DTT.Var 0)) (DTT.Var 1)) (DTT.Var 2)) (DTT.App (DTT.App (DTT.Con "破れる/やぶれる/ガ") (DTT.Var 1)) (DTT.Var 3))))))] [] sentences
            -- 727の公理2(上手くいっていないが要検討)(破くー破れる)
            -- parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity), ("yabuku", DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.App (DTT.Con "破く/やぶく/ガヲ") (DTT.Var 1)) (DTT.Var 2)) (DTT.Var 0))) (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.Con "破れる/やぶれる/ガ") (DTT.Var 2)) (DTT.Var 0))))))] [] sentences
            -- 728の公理(閉めるー閉まる)
            -- parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity), ("shimeru",DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.App (DTT.App (DTT.App (DTT.Con "閉める/しめる/ガヲ") (DTT.Var 0)) (DTT.Var 1)) (DTT.Var 2)) (DTT.App (DTT.App (DTT.Con "閉まる/しまる/ガ") (DTT.Var 1)) (DTT.Var 3))))))] [] sentences       
            -- 519の公理
            -- parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity), ("chiisana", DTT.Pi (DTT.Entity) (DTT.Pi (DTT.App (DTT.Con "小さな/ちいさな") (DTT.Var 0)) (DTT.Pi (DTT.App (DTT.Con "大きな/おおきな") (DTT.Var 1)) (DTT.Bot))))] [] sentences
            -- 520の公理
            -- parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity), ("ookina", DTT.Pi (DTT.Entity) (DTT.Pi (DTT.App (DTT.Con "大きな/おおきな") (DTT.Var 0)) (DTT.Pi (DTT.App (DTT.Con "小さな/ちいさな") (DTT.Var 1)) (DTT.Bot))))] [] sentences
            -- 523の公理
            -- parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity), ("hiraku",DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.App (DTT.App (DTT.Con "開く/ひらく/ガ") (DTT.Var 1)) (DTT.Var 0)) (DTT.Pi (DTT.App (DTT.App (DTT.Con "閉まる/しまる/ガ") (DTT.Var 2)) (DTT.Var 1)) (DTT.Bot)))))] [] sentences
            -- 524の公理
            -- parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity), ("shimaru",DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.App (DTT.App (DTT.Con "閉まる/しまる/ガ") (DTT.Var 1)) (DTT.Var 0)) (DTT.Pi (DTT.App (DTT.App (DTT.Con "開く/あく/ガ") (DTT.Var 2)) (DTT.Var 1)) (DTT.Bot)))))] [] sentences
            -- 開くの公理2
            -- parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity), ("hitraku2", DTT.Pi(DTT.Entity) (DTT.Pi (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.Con "開く/ひらく/ガ") (DTT.Var 1)) (DTT.Var 0))) (DTT.Pi (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.Con "閉まる/しまる/ガ") (DTT.Var 2)) (DTT.Var 0))) (DTT.Bot))))] [] sentences     
        NLI.printParseResult handle style 1 noTypeCheck False title parseResult
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
    -- | Numeration command
    -- | 
    lightblueMainLocal Numeration parseSetting@CP.ParseSetting{..} contents = do
      let handle = S.stdout
          sentences = T.lines contents
      S.hPutStrLn handle $ I.headerOf style
      case langOptions of
        JpOptions _ _ _ _ _ _ _ _ _ -> 
          mapM_ (\(sid,sentence) -> do
            (_,numeration) <- JLEX.setupLexicon langOptions sentence
            S.hPutStrLn handle $ I.interimOf style $ "[" ++ (show sid) ++ "]"
            mapM_ ((T.hPutStrLn handle) . (I.printLexicalItem style)) numeration
            ) $ zip ([1..]::[Int]) sentences
        EnOptions _ _ _ _ _ -> 
          putStrLn "English version of printNumeration function will be implemented soon."
      S.hPutStrLn handle $ I.footerOf style
    -- |
    -- | Demo command (sequential parsing of a given corpus)
    -- |
    lightblueMainLocal Demo parseSetting contents = processCorpus parseSetting $ T.lines contents
    -- |
    -- | Other commands
    -- |
    lightblueMainLocal Version _ _ = showVersion
    lightblueMainLocal Stat _ _ = showStat
    lightblueMainLocal Test _ _ = test
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

postpend :: [a] -> a -> [a]
postpend [] y = [y]
postpend (x:xs) y = x:(postpend xs y)

-- |
-- | lightblue --version
-- |
showVersion :: IO ()
showVersion = do
  T.putStr "lightblue version: "
  lightbluepath <- E.getEnv "LIGHTBLUE"
  cabal <- T.readFile $ lightbluepath ++ "lightblue.cabal"
  T.putStrLn $ last $ T.words $ head $ filter (T.isPrefixOf "version:") $ T.lines cabal

-- |
-- | lightblue --status
-- |
showStat :: IO ()
showStat = do
  putStrLn "lightblue: "
  putStr "  "
  putStr $ show $ length $ JLEX.emptyCategories
  putStrLn " empty categories from CCG book (Bekki 2010)"
  putStr "  "
  putStr $ show $ length $ JLEX.myLexicon
  putStrLn " lexical entries for closed words from CCG book (Bekki 2010)"
  jumandicpath <- E.getEnv "LIGHTBLUE"
  jumandic <- T.readFile $ jumandicpath ++ "src/Parser/Language/Japanese/Juman/Juman.dic"
  putStr "  "
  putStr $ show $ length $ T.lines jumandic
  putStrLn " lexical entries for open words from JUMAN++ dictionary + Kyoto case frame"

-- | lightblue --test
-- | 
test :: IO ()
test = do
  let signature = [("f", DTT.Pi DTT.Entity DTT.Type)]
      context = []
      termM = UDTT.Sigma UDTT.Entity (UDTT.App (UDTT.Con "f") (UDTT.Var 0))
      typeA = DTT.Type
      tcq = UDTT.Judgment signature context termM typeA
      prover = NLI.getProver NLI.Wani $ QT.ProofSearchSetting Nothing Nothing (Just QT.Intuitionistic) False
  typeCheckResults <- toList $ typeCheck prover False tcq
  T.putStrLn $ I.startMathML
  T.putStrLn $ I.toMathML $ DTTwN.fromDeBruijnSignature signature
  T.putStrLn $ I.endMathML
  putStrLn $ I.interimOf I.HTML ""
  T.putStrLn $ I.startMathML
  T.putStrLn $ I.toMathML $ fmap DTTwN.fromDeBruijnJudgment $ head typeCheckResults
  T.putStrLn $ I.endMathML

-- | lightblue demo
-- |
processCorpus :: CP.ParseSetting -> [T.Text] -> IO()
processCorpus ps@CP.ParseSetting{..} contents = do
    start <- Time.getCurrentTime
    --let parseSetting = CP.ParseSetting langOptions beamW 1 1 1 True Nothing Nothing False False
    (i,j,k,total) <- L.foldl' (parseSentence ps) (return (0,0,0,0)) $ filter isSentence contents
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
                 -> IO(Int,Int,Int,Int) -- ^ (The number of fully succeeded, partially succeeded, failed, and total parses)
                 -> T.Text              -- ^ A next sentence to parse
                 -> IO(Int,Int,Int,Int)
parseSentence ps@CP.ParseSetting{..} score sentence = do
  (i,j,k,total) <- score
  S.putStr $ "[" ++ show (total+1) ++ "] "
  T.putStrLn sentence
  chart <- CP.parse ps sentence
  case CP.extractParseResult beamWidth chart of
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
