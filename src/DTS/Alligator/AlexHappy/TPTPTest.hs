import qualified DTS.Alligator.AlexHappy.FileParser as F
import System.Environment (getArgs)
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

answerIs :: TI.Result -> APT.ProofMode -> DT.Preterm -> TI.Info -> Int -> IO TI.Info
answerIs yesOrNo proofMode conjecture base depth=
  let dne = APT.prove (TI.context base) [] conjecture APT.settingDef{APT.mode = proofMode}{APT.maxdepth = depth}
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

computeWith :: APT.ProofMode -> TI.Info -> Int ->  IO TI.Info
computeWith proofMode base depth=
  case TI.target base  of
    Nothing -> return base
    -- Just conjecture -> do
    --   justYes <- timeout TI.timelimit $ answerIs TI.YES proofMode conjecture base depth
    --   let isYes =
    --         case justYes of
    --           Just yes ->TI.dneResult yes == TI.YES
    --           Nothing -> False
    --   if isYes
    --   then
    --     return $ Data.Maybe.fromMaybe base justYes
    --   else do
    --     justNo <- timeout TI.timelimit $ answerIs TI.NO proofMode (DT.Not conjecture) base depth
    --     return $ Data.Maybe.fromMaybe base justNo
    Just conjecture -> do
      yes <- answerIs TI.YES proofMode conjecture base depth
      let isYes =case proofMode of
            APT.WithDNE -> TI.dneResult yes == TI.YES
            APT.WithEFQ -> TI.efqResult yes == TI.YES
            APT.Plain -> undefined
      if isYes then
        return yes
      else
        answerIs TI.NO proofMode (DT.Not conjecture) base depth


writeResults :: TI.Info -> Int -> String -> IO TI.Info
writeResults info depth output = do
  appendFile output (  TI.filename info ++ "\t" ++"" ++ "\t" ++ (case TI.status info of Just sta -> show sta ; _ -> "") ++ "\t")
  dneBase <- computeWith APT.WithDNE info depth
  appendFile output (show (TI.dneResult dneBase) ++ "\t" ++ (TI.dneUrl dneBase) ++ "\t")
  efqBase <- return dneBase --computeWith APT.WithEFQ dneBase  --時間削減のためにEFQ省略
  appendFile output (show (TI.efqResult efqBase) ++ "\t" ++ (TI.efqUrl efqBase) ++ "\t")
  appendFile output $(case TI.language info of Just lan -> show lan ; _ -> "") ++ "\t" ++ TI.strcontext info ++ "\t" ++TI.strtarget info ++ "\t" ++  TI.note info ++ "\t" ++ "\t" ++"\n"
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

testInfoFile :: String -> Int -> String -> IO TI.Info
testInfoFile fname depth output= do
  base <- F.fileparseInfo fname
  let info = base {TI.filename = fname}
  writeResults info depth output
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
          testInfoFile fname 9 (TI.resultfname++".csv")
          appendFile TI.resultfname "")

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

--shでやること : csvHeaderをかく。ファイル名を指定する
main = do
  args <- getArgs
  let fnum = if null args then "" else (head args)
  let depth = if null args then 9 else ((read $args !! 1) :: Int)
  let outputfile = TI.resultfname++fnum++".csv"
  let fname = if null args then "../../TPTP-v7.3.0/Problems/SYN/dummy.p" else args !! 2
  let yesNoUnknown = if null args then TI.YES else ((read $args !! 3) :: TI.Result)
  -- writeFile outputfile csvHeader
  -- let dir = "../../TPTP-v7.3.0/Problems/SYN/"
  -- c <- getDirectoryContents dir
  let toBeTested= case yesNoUnknown of TI.YES -> fname `elem` TI.yesList ; TI.NO -> fname `elem` TI.noList ; TI.UNKNOWN -> fname `notElem` (TI.yesList++TI.noList++TI.exceptList) && isTestFile fname
  if toBeTested
  then do
    testInfoFile fname depth outputfile
    appendFile outputfile ""
  else appendFile outputfile ""


  -- let fnames = TI.yesList ++ TI.noList ++ (filter (\x -> x `notElem` (TI.yesList++TI.noList)) $ filter (`notElem` TI.exceptList) $ map (dir ++ ) $filter isTestFile c)
  -- forM_ (zip [1..] fnames)
  --   $ \ (num,fname) ->
  --     print (show num ++"/"++ show (length fnames))
  --     >>(
  --       do
  --         testInfoFile fname depth outputfile
  --         appendFile outputfile "")


writeInfoCsv fnum= do
  writeFile (TI.resultfname++fnum++".csv") csvHeader
  forM_ TI.dirs testInfoDir
  -- result <- map testInfoDir dirs
  -- print ""
