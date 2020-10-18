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

plContectGen :: [F.Expr] -> [(String,Int)] -> Int -> String
plContectGen context prelst' cnum =
  let prelst = filter ((`notElem` ["false","top"]). fst) prelst'
      -- contextLst = map
      --   (\(conName,contextId) ->
      --     let contextElement = context !! contextId
      --         f = t2pl contextElement
      --     in
      --       (if isUpper $head conName then "big"++conName else conName) ++ " : ("++f ++ ")"
      --   )
      --
      --   prelst
-- in foldl (\(a,n) -> \ b -> let b' = case var of F.TFormula f' -> let (f1,n1) = t2pl f' n' in "( "++ f1 ++" )" ; F.TDef _ _ -> "" in let a' = a ++ if b' == "" then "" else "-" ++ b' in (a',n+1)) (s,n') args
  in
    let (lsthead,num) =
            if null prelst
              then ("[ type:set   ",cnum)
              else
                foldr
                  (
                    \(conName,contextId) (a,n) ->
                        let contextElement = context !! contextId
                            (f,n') = t2pl contextElement n
                        in
                          let b' =  (if isUpper $head conName then "big"++conName else conName) ++ " : ("++f ++ ")"
                          in
                            (b' ++ a ++ " , ",n')
                  )
                  ("[ type:set ,",cnum)
                  prelst
    in init $ init $ lsthead++"]"

--(((p & q) -> r) -> (p -> q -> r))
t2pl :: F.Expr -> Int -> (String,Int)
t2pl f cnum=
  case f of
    (F.Tletter con) -> ("( "++(if isUpper $head con then "big"++con else con)++" )",cnum)
    F.Ttrue -> ("( pi( False : false,false) )",cnum)
    F.Tfalse -> ("( false )",cnum)
    (F.Tneg formula) ->
      let (s1,n1) = t2pl formula cnum  in
      ("pi( C"++show n1++" : "++s1++" , false )",n1+1)
    F.Tbinary biop f1 f2 ->
      let
          (s1',n1')  = t2pl f1 cnum
          (s2',n2')  = t2pl f2 n1'
          s1 = "( "++s1'++" )"
          s2 = "( "++s2'++" )" in
      case biop of
        F.Tand ->  ("( sigma( C"++show n2'++" : "++s1 ++ " , " ++ s2 ++" )",n2'+1)
        F.Tor -> ("( pi( C"++show n2'++" : (sigma( C"++(show $n2'+1)++" : pi( C"++(show $n2'+2)++":"++s1++" , false) , pi( C"++(show $n2'+3)++" : "++s2++" , false)) , false))",n2'+4)
        F.Timp -> ("( pi( C"++show n2'++" : "++s1++" , "++s2++" ))",n2'+1)
        F.Tequiv -> ("(sigma( C"++show n2'++" : pi( C"++(show $n2'+1)++":"++s1++","++s2++") , pi( C"++(show $n2'+2)++" : "++s2++" , "++s1++") )",n2'+3)
    F.TApp f args->
      let (s',n') = t2pl f cnum
          s = "( "++s'++" )"
          -- plargs = map (\var -> case var of F.TFormula f' -> let (f1,n1) = t2pl f' n' in "( "++ f1 ++" )" ; F.TDef _ _ -> "") args
      in foldl (\(a,n) -> \ b -> let b' = case b of F.TFormula f' -> let (f1,n1) = t2pl f' n' in "( "++ f1 ++" )" ; F.TDef _ _ -> "" in let a' = a ++ if b' == "" then "" else "-" ++ b' in (a',n+1)) (s,n') args
      -- in foldl (\a b -> a ++ if b == "" then "" else "-"++b) s plargs
    F.Tall [] f -> let (s1,n1) = t2pl f cnum in (s1,n1)
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
        F.TDef _ _ -> D.trace "TDef in all" ("",cnum)
        F.TFormula (F.Tletter var) ->
          let (s,n)=  t2pl (F.Tall r f) cnum
              varR = map toUpper var
              f' = T.unpack $T.replace (T.pack var) (T.pack varR) (T.pack s)
          in
          ("pi( "++varR++" : type"++" , "++f'++ " )",n)
        F.TFormula _ -> D.trace "Tformula in all" ("",cnum)
    F.Texist [] f ->
      let (s,n) = t2pl f cnum in (s,n)
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
        F.TDef _ _ -> D.trace "TDef in all" ("",cnum)
        F.TFormula (F.Tletter var) ->
          let (s,n)=  t2pl (F.Texist r f) cnum
              varR = map toUpper var
              f' = T.unpack $T.replace (T.pack var) (T.pack varR) (T.pack s)
          in
          ("sigma( "++varR++" : type"++" , "++f'++ " )",n)
        F.TFormula _ -> D.trace "Tformula in all" ("",cnum)


evalPl :: [S.Expr] -> String -> IO (Maybe (String,String,String))
evalPl ast fname = do
  info <- E.evalInfo ast fname
  let contextWithTexpr = TI.contextWithTexpr info
  {-contextWithTexpr <- D.trace (show $TI.contextWithTexpr info) $return $TI.contextWithTexpr info-}
  let prelst = TI.prelst info
  -- prelst <- {-D.trace (show $TI.prelst info) $-}return $TI.prelst info
  case TI.targetWithTexpr info of
    Just targetWithTexpr ->
      let (pltarget,cnum) = t2pl targetWithTexpr 0
          plcontext = plContectGen contextWithTexpr prelst cnum
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
    --appendFile outputfname str
    -- print $num+1
    putStrLn str
  else
    -- error "no parse"
    -- print num
    putStrLn ""
