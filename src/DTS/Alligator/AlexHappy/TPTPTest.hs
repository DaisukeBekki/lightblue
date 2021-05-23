module DTS.Alligator.AlexHappy.TPTPTest where

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
      justYes <- timeout TI.timelimit $ answerIs TI.YES proofMode conjecture base depth
      let yes = Data.Maybe.fromMaybe base justYes
      -- yes <- answerIs TI.YES proofMode conjecture base depth
      let isYes =case proofMode of
            APT.WithDNE -> TI.dneResult yes == TI.YES
            APT.WithEFQ -> TI.efqResult yes == TI.YES
            APT.Plain -> undefined
      if isYes then do
        return yes
      else do
        justNo <- timeout TI.timelimit $ answerIs TI.NO proofMode (DT.Not conjecture) base depth
        return $ Data.Maybe.fromMaybe base justNo
        -- answerIs TI.NO proofMode (DT.Not conjecture) base depth


getResults :: TI.Info -> Int -> IO TI.Info
getResults info depth = do
  -- appendFile output (  TI.filename info ++ "\t" ++"" ++ "\t" ++ (case TI.status info of Just sta -> show sta ; _ -> "") ++ "\t")
  -- putStr (  TI.filename info ++ "\t" ++"" ++ "\t" ++ (case TI.status info of Just sta -> show sta ; _ -> "") ++ "\t")
  dneBase <- computeWith APT.WithDNE info depth
  -- appendFile output (show (TI.dneResult dneBase) ++ "\t" ++ (TI.dneUrl dneBase) ++ "\t")
  -- putStr (show (TI.dneResult dneBase) ++ "\t" ++ (TI.dneUrl dneBase) ++ "\t")
  efqBase <- return dneBase --computeWith APT.WithEFQ dneBase  --時間削減のためにEFQ省略
  -- appendFile output (show (TI.efqResult efqBase) ++ "\t" ++ (TI.efqUrl efqBase) ++ "\t")
  -- putStr (show (TI.efqResult efqBase) ++ "\t" ++ (TI.efqUrl efqBase) ++ "\t")
  -- appendFile output $(case TI.language info of Just lan -> show lan ; _ -> "") ++ "\t" ++ TI.strcontext info ++ "\t" ++TI.strtarget info ++ "\t" ++  TI.note info ++ "\t" ++ "\t" ++"\n"
  -- putStr $(case TI.language info of Just lan -> show lan ; _ -> "") ++ "\t" ++ TI.strcontext info ++ "\t" ++TI.strtarget info ++ "\t" ++  TI.note info ++ "\t" ++ "\t" ++"\n"
  return efqBase


generateCsvRow :: TI.Info -> TI.Result -> String
generateCsvRow info yesNoUnknown=
  TI.filename info ++ "\\t" ++
  (if (null $TI.dneUrl info) then "UNKNOWN" else show yesNoUnknown) ++ "\\t" ++
  (case TI.status info of Just sta -> show sta ; _ -> "") ++ "\\t" ++
  show (TI.dneResult info) ++ "\\t"++
  TI.dneUrl info ++ "\\t"++
  show (TI.efqResult info) ++ "\\t"++
  TI.efqUrl info ++ "\\t"++
  (case TI.language info of Just lan -> show lan ; _ -> "") ++ "\\t" ++
  TI.strcontext info ++ "\\t" ++
  TI.strtarget info ++ "\\t" ++
  TI.note info  ++"\\n"


-- csvHeader="file\tassestment\tstatus\tdneresult\tefqresult\tlanguage\tcontext\ttarget\tprocessed\tnote\t\n"

testInfoFile :: String -> Int -> IO TI.Info
testInfoFile fname depth= do
  base <- F.fileparseInfo fname
  let info = base {TI.filename = fname}
  getResults info depth
  -- return ((TI.dneResult info,TI.efqResult info),(case (TI.status info) of Just status -> Just $ TI.statusToResult status ; Nothing -> Nothing))

--shでやること : csvHeaderをかく。ファイル名を指定する
--stack run tptp 8 ../../TPTP-v7.3.0/Problems/SYN/SYN403+1.p YES

hojo depth fname yesNoUnknown= do
  let toBeTested= case yesNoUnknown of TI.YES -> fname `elem` TI.yesList ; TI.NO -> fname `elem` TI.noList ; TI.UNKNOWN -> fname `notElem` (TI.yesList++TI.noList++TI.exceptList) && isTestFile fname
  if toBeTested
  then do
    outputInfo <- testInfoFile fname depth
    -- print outputInfo
    let output = generateCsvRow outputInfo yesNoUnknown
    return output
  else do
    return ""

main = do
  args <- getArgs
  let depth = if null args then 9 else ((read $args !! 0) :: Int)
  let fname = if length args < 1 then "../../TPTP-v7.3.0/Problems/SYN/dummy.p" else args !! 1
  let yesNoUnknown = if length args < 2 then TI.YES else ((read $args !! 2) :: TI.Result)
  output <- hojo depth fname yesNoUnknown
  putStrLn output
