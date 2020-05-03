module DTS.Alligator.AlexHappy.TPTPTest (
  writeResultCsv
) where

import System.Directory
import System.Timeout          --
import Data.List
import Control.Monad
import DTS.Alligator.AlexHappy.FileParser

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

resultfname :: String
resultfname = "DTS/Alligator/AlexHappy/result.csv"

isTestFile :: String -> Bool
isTestFile fname=
  any (\ex -> take (1 + length ex) (reverse fname) == reverse ('.':ex )) testFileExtentions

timelimit :: Int
timelimit =  1000000

announce :: (String,Maybe (Either String (Bool,String,String,String))) -> String
announce (f,Just (Right (b,s1,s2,s3))) = f ++ "\t" ++show b ++"\t" ++s1++"\t"++s2++"\t"++ s3 ++ "\t\t\n"
announce (f,Just (Left s)) = f ++ "\t\t\t\t\t"++ s ++"\t\n"
announce (f,Nothing) = f ++ "\t\t\t\t\ttimeout\t\n"

test :: String ->IO [Maybe Bool]
test dir= do
  c <- getDirectoryContents dir
  let fnames = filter (`notElem` exceptList) $ map (dir ++ ) $filter isTestFile c
  results <- forM (zip [1..] fnames)
    $ \ (num,fname) ->
      print (show num ++"/"++ show (length fnames)) >>print fname
       >>(do result <- timeout timelimit (fileparse fname);appendFile resultfname $announce (fname,result); return $ (\r -> case r of Just (Right (b,_,_,_)) -> Just b ; _ -> Nothing) result)
  return results

testexceptList :: IO [Maybe Bool]
testexceptList= do
  results <- forM (zip [1..] exceptList)
    $ \ (num,fname) ->
      print (show num ++"/"++ show (length exceptList)) >>print fname
       >>(do result <- timeout timelimit (fileparse fname);appendFile resultfname $announce (fname,result); return $ (\r -> case r of Just (Right (b,_,_,_)) -> Just b ; _ -> Nothing) result)
  return results

numOf :: Bool -> [[Maybe Bool]] -> Int
numOf bo result =  length $ filter (\r -> case r of Just b -> b== bo ; _ -> False) $  concat result

writeResultCsv = do
  writeFile resultfname $"file\tresult\taxioms\tconjencture\tprocessed\tnote\tlimit:" ++ show timelimit++"s\n"
  result <- forM dirs test
  putStrLn "count True"
  appendFile resultfname $"\n\t"++"True : "++ (show $ numOf True result) ++ " False : "++(show $ numOf False result)++"\t\t\t\t\t"
