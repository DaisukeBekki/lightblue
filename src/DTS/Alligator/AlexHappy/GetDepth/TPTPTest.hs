import System.Directory
import System.Timeout
import Control.Monad
-- import qualified DTS.Alligator.AlexHappy.GetDepth.Eval as E
import qualified DTS.Alligator.AlexHappy.GetDepth.FileParser as F
import qualified DTS.Alligator.AlexHappy.GetDepth.Syntax as S

import qualified DTS.Alligator.AlexHappy.TPTPInfo as TI
import qualified DTS.Alligator.ProofTree as APT
import qualified Data.Text.Lazy.IO as T
import qualified DTS.DTT as DT
import qualified Data.Maybe
import Data.List.Split
import Data.Default (Default(..))

csvHeader :: String
csvHeader="file\tpredicates\tvariables\tatoms\tclauses\tclauseSize\tfunctors\tmaximalDepth\n"

resultfname :: String
resultfname = "DTS/Alligator/AlexHappy/output/numlist.csv"

parseCSV :: String -> [[String]]
parseCSV = (map (splitOn "\t")  .lines)

isTestFile :: String -> Bool
isTestFile fname=
  any (\ex -> take (1 + length ex) (reverse fname) == reverse ('.':ex )) TI.testFileExtentions


writeResults :: S.Nums -> IO ()
writeResults info = do
  appendFile resultfname (S.filename info ++ "\t" ++(show $S.predicates info)++ "\t" ++ (show $ S.variables info) ++ "\t" ++ (show $ S.atoms info) ++ "\t" ++ (show $ S.clauses info) ++ "\t"  ++ (show $ S.clauseSize info) ++ "\t" ++ (show $ S.functors info) ++ "\t" ++ (show $ S.maximalDepth info) ++ "\n")
--
-- -- generateCsvRow :: TI.Info -> String
-- generateCsvRow info =
--   TI.filename info ++ "\t" ++
--   "" ++ "\t" ++
--   (case TI.status info of Just sta -> show sta ; _ -> "") ++ "\t" ++
--   show (TI.dneResult info) ++ "\t"++
--   TI.dneUrl info ++ "\t"++
--   show (TI.efqResult info) ++ "\t"++
--   TI.efqUrl info ++ "\t"++
--   (case TI.language info of Just lan -> show lan ; _ -> "") ++ "\t" ++
--   TI.strcontext info ++ "\t" ++
--   TI.strtarget info ++ "\t" ++
--   TI.note info ++ "\t" ++ "\t" ++"\n"


-- csvHeader="file\tassestment\tstatus\tdneresult\tefqresult\tlanguage\tcontext\ttarget\tprocessed\tnote\t\n"

testInfoFile :: String -> IO ()
testInfoFile fname = do
  base <- F.fileparseInfo fname
  let info = base {S.filename = fname}
  writeResults info
  -- return ((TI.dneResult info,TI.efqResult info),(case (TI.status info) of Just status -> Just $ TI.statusToResult status ; Nothing -> Nothing))

testInfoDir :: String -> IO ()
testInfoDir dir = do
  c <- getDirectoryContents dir
  let fnames = filter (`notElem` TI.exceptList) $ map (dir ++ ) $filter isTestFile c
  forM_ (zip [1..] fnames)
    $ \ (num,fname) ->
      print (show num ++"/"++ show (length fnames))
      >>(
        do
          testInfoFile fname
          appendFile resultfname "")


main = writeInfoCsv

-- parseTest dir = do
--   c <- getDirectoryContents dir
--   let fnames = filter (`notElem` TI.exceptList) $ map (dir ++ ) $filter isTestFile c
--   forM_
--       fnames
--       fileParseTest
--   putStrLn "end"
--
-- fileParseTest fname = do
--   base <- F.fileparseInfo fname
--   putStrLn "fileParseTest"
  -- if (TI.note base == "")
  -- then
  --   putStr ""
  -- else
  --   putStrLn $fname ++ "\n\t"  ++ (TI.note base)


writeInfoCsv = do
  writeFile resultfname csvHeader
  forM_ TI.dirs testInfoDir
  -- result <- map testInfoDir dirs
  -- print ""
