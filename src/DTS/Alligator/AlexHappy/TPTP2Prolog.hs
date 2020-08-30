-- module DTS.Alligator.AlexHappy.TPTP2Prolog where
import           System.Environment (getArgs)
import qualified DTS.Alligator.AlexHappy.Parser as P (parseExpr)
import qualified DTS.Alligator.AlexHappy.Syntax as S
import qualified  DTS.Alligator.AlexHappy.Eval as E (evalInfo)
import qualified DTS.Alligator.AlexHappy.TPTPInfo as TI

import qualified DTS.Alligator.AlexHappy.Syntaxf as F
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.List as L
import Data.Char
import System.Directory
import System.Timeout
import Control.Monad
import Data.List.Split
import Data.Default (Default(..))
import Debug.Trace as D

plFileHead :: String
plFileHead =
  ":-use_module(swi_alligator,[prove/2]).\n:- op(500,xfx,:). \n:- op(300,yfx,-). \n:- op(400,xfy,=>). \n:- op(400,xfy,&). \n:- op(300,fy,~). \n:- dynamic checktheorem/6.\n"

plFileTail :: String
plFileTail =
  "writeResult(Logic,ID,Prediction,Gold,Fname) :-\nformat('~w&~w&~w&~w&~w~n', [Logic,ID,Prediction,Gold,Fname]).\nevalyes(Logic,ID) :-\n  checkTheorem(Logic,ID,Context,Theorem,Gold,Fname),\n  ( prove(Context,_ : Theorem) -> writeResult(Logic,ID,yes,Gold,Fname)).\nevalno(Logic,ID) :-\n  checkTheorem(Logic,ID,Context,Theorem,Gold,Fname),\n  ( prove(Context,_ : ( ~ (Theorem) ) ) -> writeResult(Logic,ID,no,Gold,Fname) ).\n"

plFileName :: String
plFileName = "DTS/Alligator/AlexHappy/fortest.pl"

dirLogic :: String -> String
dirLogic dir=
  M.fromMaybe "" $lookup dir $zip TI.dirs ["test","syn"]

isTestFile :: String -> Bool
isTestFile fname=
  any (\ex -> take (1 + length ex) (reverse fname) == reverse ('.':ex )) TI.testFileExtentions

plContectGen :: [F.Expr] -> [(String,Int)] -> String
plContectGen context prelst' =
  let prelst = filter ((`notElem` ["false","top"]). fst) prelst'
      contextLst = map
        (\(conName,contextId) ->
          let contextElement = context !! contextId
              f = t2pl contextElement
          in
            (if isUpper $head conName then "big"++conName else conName) ++ " : ("++f ++ ")"
        )
        prelst
  in
    (if null contextLst then "[ type:set " else init $ init $ foldr (\a b -> b ++ a ++ " , " ) "[ type:set ," contextLst) ++ "]"

--(((p & q) -> r) -> (p -> q -> r))
t2pl :: F.Expr -> String
t2pl f =
  case f of
    (F.Tletter con) -> "( "++(if isUpper $head con then "big"++con else con)++" )"
    F.Ttrue -> "( ~false )"
    F.Tfalse -> "( false )"
    (F.Tneg formula) ->
      let s1 = t2pl formula in
      "~( "++s1++" )"
    F.Tbinary biop f1 f2 ->
      let
          s1'  = t2pl f1
          s2'  = t2pl f2
          s1 = "( "++s1'++" )"
          s2 = "( "++s2'++" )" in
      case biop of
        F.Tand ->  s1 ++ " & " ++ s2
        F.Tor -> s1 ++ " \\/ " ++ s2
        F.Timp -> s1 ++ " -> " ++ s2
        F.Tequiv -> "( " ++ s1 ++ " -> " ++ s2 ++" ) & ( "++s2 ++ " -> "++ s1++" )"
    F.TApp f args->
      let s' = t2pl f
          s = "( "++s'++" )"
          plargs = map (\var -> case var of F.TFormula f' -> let f1 = t2pl f' in "( "++ f1 ++" )" ; F.TDef _ _ -> "") args
      in foldl (\a b -> a ++ if b == "" then "" else "-"++b) s plargs
    F.Tall [] f -> t2pl f
    F.Tall (var:r) f->
      -- let s' = t2pl f
      --     s = "( "++s'++" )"
      --     replaceTarget var=
      --       case var of
      --         F.TDef _ _ -> D.trace "TDef in all" Nothing
      --         F.TFormula (F.Tletter var) ->
      --           Just (var,map toUpper var)
      --         F.TFormula _ -> D.trace "Tformula in all" Nothing
      --     replaceTargets = L.sort $ M.catMaybes $map replaceTarget vars
      -- in
      --     foldl (\b (a1,a2) -> T.unpack $ T.replace (T.pack a1) (T.pack a2) (T.pack b)) s replaceTargets
      case var of
        F.TDef _ _ -> D.trace "TDef in all" ""
        F.TFormula (F.Tletter var) ->
          let s=  t2pl (F.Tall r f)
              varR = map toUpper var
              f' = T.unpack $T.replace (T.pack var) (T.pack varR) (T.pack s)
          in
          "pi("++varR++":type"++","++f'++ ")"
        F.TFormula _ -> D.trace "Tformula in all" ""
    F.Texist [] f ->
      t2pl f
    F.Texist (var:r) f->
      -- let s = "( "++t2pl f++" )"
      --     replaceTarget var=
      --       case var of
      --         F.TDef _ _ -> D.trace "TDef in all" Nothing
      --         F.TFormula (F.Tletter var) ->
      --           Just (var,map toUpper var)
      --         F.TFormula _ -> D.trace "Tformula in all" Nothing
      --     replaceTargets = L.sort $ M.catMaybes $map replaceTarget vars
      -- in
      --     foldl (\b (a1,a2) -> T.unpack $ T.replace (T.pack a1) (T.pack a2) (T.pack b)) s replaceTargets
      case var of
        F.TDef _ _ -> D.trace "TDef in all" ""
        F.TFormula (F.Tletter var) ->
          let s=  t2pl (F.Texist r f)
              varR = map toUpper var
              f' = T.unpack $T.replace (T.pack var) (T.pack varR) (T.pack s)
          in
          "sigma("++varR++":type"++","++f'++ ")"
        F.TFormula _ -> D.trace "Tformula in all" ""


evalPl :: [S.Expr] -> String -> IO (Maybe (String,String,String))
evalPl ast fname = do
  info <- E.evalInfo ast fname
  let contextWithTexpr = TI.contextWithTexpr info
  {-contextWithTexpr <- D.trace (show $TI.contextWithTexpr info) $return $TI.contextWithTexpr info-}
  let prelst = TI.prelst info
  -- prelst <- {-D.trace (show $TI.prelst info) $-}return $TI.prelst info
  case TI.targetWithTexpr info of
    Just targetWithTexpr ->
      let pltarget = t2pl targetWithTexpr
          plcontext = plContectGen contextWithTexpr prelst
          status' = TI.status info
      in
        case status' of
          Just status ->
            {-D.trace ("target"++show targetWithTexpr) $-}return $ Just (plcontext,pltarget,map toLower $show(TI.statusToResult status))
          Nothing ->
            {-D.trace ("target"++show targetWithTexpr) $-}return $ Just (plcontext,pltarget,"unknown")
    Nothing ->
      return $ Just ("[]","(~(false))","unknown")

parseContextConjPl :: String -> IO (Maybe (String,String,String))
parseContextConjPl  fname  = do
  input <- readFile fname
  let ast' = P.parseExpr input
  case ast' of
    Right ast -> do
      evalPl ast fname
    Left err -> return Nothing

checkTheoremGen :: String -> String -> Int -> IO String
checkTheoremGen dir fname num = do
  concon <- parseContextConjPl  fname
  case concon of
    Just (context,conjecture,status) -> do
      let str = "checkTheorem("++dirLogic dir++" , "++show num++" , "++context++" , "++conjecture++" , "++status++" , \'"++fname++"\').\n"
      return str
    Nothing -> return ""



writePlInDir :: String -> IO ()
writePlInDir dir = do
  c <- getDirectoryContents dir
  let fnames = filter (`notElem` TI.exceptList) $ map (dir ++ ) $filter isTestFile c
  forM_ (zip [1..] fnames)
    $ \ (num,fname) ->
      print (show num ++"/"++ show (length fnames))
      >> (do
        str <- checkTheoremGen dir fname num
        appendFile plFileName str
        ) --(plInFile fname) ++ "\n")

writePlFile = do
  writeFile plFileName plFileHead
  forM_ TI.dirs writePlInDir
  appendFile plFileName plFileTail

-- main = writePlFile
main = do
  args <- getArgs
  let dir = head args
  let fname = args !! 1
  let num = read (args !! 2) :: Int
  let outputfname = args !! 3
  if isTestFile fname && fname `notElem` TI.exceptList
  then do
    str <- checkTheoremGen dir fname num
    appendFile outputfname str
    print $num+1
  else
    print num
