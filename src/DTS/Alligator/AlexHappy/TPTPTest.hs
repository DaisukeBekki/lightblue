module DTS.Alligator.AlexHappy.TPTPTest (
  writeInfoCsv
) where

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

-- map (\lst -> lst !! 2) $parseCSV csv
testFileExtentions :: [String]
testFileExtentions = ["test","p"]

exceptList :: [String]
exceptList =
  [
    "../../TPTP-v7.3.0/Problems/SYN/SYN407^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN732^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN055^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000^3.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN071+1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN989^2.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN057^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN731^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN056^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN377^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN988^2.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN551+3.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN367^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN001^4.002.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000+2.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN007^4.014.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN393^4.002.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN416^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN051^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN355^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN382^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN417+1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN036^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN356^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN381^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN388^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN988^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN989^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN389^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN357^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN999^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN998^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN001^4.001.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN978^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN416^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN386^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN374^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN360^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN996^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN983^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN997^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN375^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN361^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN977^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN377^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN995^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN994^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN390^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN984^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN990^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN052^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN991^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN391^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN985^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN365^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000-1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN387^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN993^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN987^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN001^4.004.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN393^4.004.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000_1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN045^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN392^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN992^1.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN358^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN364^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN915^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN040^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN397^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN049^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN036^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN916^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000^2.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN041^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN357^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN989^3.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN046^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN001^4.003.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000-2.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN393^4.003.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN047^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN045^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN058^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN064^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN741^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN059^5.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN987^2.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN000_2.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN387^7.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN044^4.p",
    "../../TPTP-v7.3.0/Problems/SYN/SYN551+2.p"
  ]

dirs :: [String]
-- dirs = ["DTS/Alligator/Test/"]
dirs = ["DTS/Alligator/Test/","../../TPTP-v7.3.0/Problems/SYN/"]

isTestFile :: String -> Bool
isTestFile fname=
  any (\ex -> take (1 + length ex) (reverse fname) == reverse ('.':ex )) testFileExtentions

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
  efqBase <- computeWith APT.WithEFQ dneBase
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
  let fnames = filter (`notElem` exceptList) $ map (dir ++ ) $filter isTestFile c
  -- p/rint ""
  -- map
  --   (\(fname,num) -> do
  --       print (show num ++"/"++ show (length fnames))
  --       print  fname
  --       testInfoFile fname)
  --   (zip fnames [1..])
  forM_ (zip [1..] fnames)
    $ \ (num,fname) ->
      print (show num ++"/"++ show (length fnames))
      >>(
        do
          -- print fname
          -- base <- F.fileparseInfo fname
          -- let info = base {TI.filename = fname}
          -- _ <- writeResults info
          testInfoFile fname
          appendFile TI.resultfname "")


main = writeInfoCsv

writeInfoCsv = do
  writeFile TI.resultfname csvHeader
  forM_ dirs testInfoDir
  -- result <- map testInfoDir dirs
  -- print ""
