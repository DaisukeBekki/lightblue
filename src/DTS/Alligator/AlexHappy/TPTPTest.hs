import qualified DTS.Alligator.AlexHappy.FileParser as F

import System.Directory
import System.Timeout
import Control.Monad
import qualified DTS.Alligator.AlexHappy.Eval as E
import qualified DTS.Alligator.AlexHappy.TPTPInfo as TI
import qualified DTS.Alligator.ProofTree as APT
import qualified Data.Text.Lazy.IO as T
import qualified DTS.DTT as DT
import qualified Data.Maybe
import Data.List.Split
import Data.Default (Default(..))

csvHeader :: String
csvHeader="file\tassestment\tstatus\tdneresult\tdneurl\tefqresult\tefqurl\tlanguage\tcontext\ttarget\tnote\t\n"

parseCSV :: String -> [[String]]
parseCSV = (map (splitOn "\t")  .lines)

isTestFile :: String -> Bool
isTestFile fname=
  any (\ex -> take (1 + length ex) (reverse fname) == reverse ('.':ex )) TI.testFileExtentions

answerIs :: TI.Result -> APT.ProofMode -> DT.Preterm -> TI.Info -> IO TI.Info
answerIs yesOrNo proofMode conjecture base =
  let dne = APT.prove (TI.context base) [] conjecture APT.settingDef{APT.mode = proofMode}
      url = TI.outputdir++takeWhile (/= '.') ( reverse$ takeWhile (/='/')$ reverse $init$TI.filename base) ++ "_dts.html"
  in
    if not (null dne)
    then do
      contents <- APT.announce dne
      T.writeFile url contents
      case proofMode of
        APT.WithDNE -> return $base {TI.dneResult =  yesOrNo}{TI.dneUrl = url}
        APT.WithEFQ -> return $base {TI.efqResult =  yesOrNo}{TI.efqUrl = url}
        APT.Plain -> return base
    else
      return base

computeWith :: APT.ProofMode -> TI.Info -> IO TI.Info
computeWith proofMode base =
  case TI.target base  of
    Nothing -> return base
    Just conjecture -> do
      justYes <- timeout TI.timelimit $ answerIs TI.YES proofMode conjecture base
      let isYes =
            case justYes of
              Just yes ->TI.dneResult yes == TI.YES
              Nothing -> False
      if isYes
      then
        return $ Data.Maybe.fromMaybe base justYes
      else do
        justNo <- timeout TI.timelimit $ answerIs TI.NO proofMode (DT.Not conjecture) base
        return $ Data.Maybe.fromMaybe base justNo


writeResults :: TI.Info -> IO TI.Info
writeResults info = do
  appendFile TI.resultfname (  TI.filename info ++ "\t" ++"" ++ "\t" ++ (case TI.status info of Just sta -> show sta ; _ -> "") ++ "\t")
  dneBase <- computeWith APT.WithDNE info
  appendFile TI.resultfname (show (TI.dneResult dneBase) ++ "\t" ++ (TI.dneUrl dneBase) ++ "\t")
  efqBase <- return dneBase--computeWith APT.WithEFQ dneBase  --時間削減のためにEFQ省略
  appendFile TI.resultfname (show (TI.efqResult efqBase) ++ "\t" ++ (TI.efqUrl efqBase) ++ "\t")
  appendFile TI.resultfname $(case TI.language info of Just lan -> show lan ; _ -> "") ++ "\t" ++ TI.strcontext info ++ "\t" ++TI.strtarget info ++ "\t" ++  TI.note info ++ "\t" ++ "\t" ++"\n"
  return efqBase

generateCsvRow :: TI.Info -> String
generateCsvRow info =
  TI.filename info ++ "\t" ++
  "" ++ "\t" ++
  (case TI.status info of Just sta -> show sta ; _ -> "") ++ "\t" ++
  show (TI.dneResult info) ++ "\t"++
  TI.dneUrl info ++ "\t"++
  show (TI.efqResult info) ++ "\t"++
  TI.efqUrl info ++ "\t"++
  (case TI.language info of Just lan -> show lan ; _ -> "") ++ "\t" ++
  TI.strcontext info ++ "\t" ++
  TI.strtarget info ++ "\t" ++
  TI.note info ++ "\t" ++ "\t" ++"\n"


-- csvHeader="file\tassestment\tstatus\tdneresult\tefqresult\tlanguage\tcontext\ttarget\tprocessed\tnote\t\n"

testInfoFile :: String -> IO TI.Info
testInfoFile fname = do
  base <- F.fileparseInfo fname
  let info = base {TI.filename = fname}
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
          appendFile TI.resultfname "")


main = writeInfoCsv

parseTest dir = do
  c <- getDirectoryContents dir
  let fnames = filter (`notElem` TI.exceptList) $ map (dir ++ ) $filter isTestFile c
  forM_
      fnames
      fileParseTest
  putStrLn "end"

fileParseTest fname = do
  base <- F.fileparseInfo fname
  if (TI.note base == "")
  then
    putStr ""
  else
    putStrLn $fname ++ "\n\t"  ++ (TI.note base)


writeInfoCsv = do
  writeFile TI.resultfname csvHeader
  forM_ TI.dirs testInfoDir
  -- result <- map testInfoDir dirs
  -- print ""
