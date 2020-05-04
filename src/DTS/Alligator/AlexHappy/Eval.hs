module DTS.Alligator.AlexHappy.Eval (evalInfo) where

import DTS.Alligator.AlexHappy.Syntax as S
import DTS.Alligator.AlexHappy.Syntaxf as F
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.List as L
import DTS.Alligator.AlexHappy.Parser as P
import DTS.Alligator.AlexHappy.Parserf as PF
import qualified Data.Text.Lazy as Te
import qualified Data.Text.IO as T
import qualified DTS.DTT as DT
import qualified DTS.Prover.Judgement as J
import qualified DTS.Alligator.Arrowterm as A
import qualified DTS.Alligator.Prover as AP

import Data.Maybe

import DTS.Alligator.AlexHappy.TPTPInfo
import Data.Default (Default(..))

import Debug.Trace as D
import System.Timeout


testexpr = [Status "Theorem",PreNum 3,Formula "fof" "" "axiom" "p",Formula "fof" "" "axiom" "p=>q",Formula "fof" "" "axiom" "q=>r",Formula "fof" "" "conjecture" "r"]

updateConLst :: String -> [(String,Int)] -> Either String [(String,Int)]
updateConLst con conlst =
  case lookup con conlst of
    Just varnum ->Right conlst
    Nothing ->
      case lookup "" conlst of
        Just varnum ->  Right $ (con,varnum) : filter ((/= varnum) . snd) conlst
        Nothing -> Left "Error at updateConLst"

updateConLst' :: String -> [(String,Int)] -> [(String,Int)]
updateConLst' con conlst =
  case updateConLst con conlst of
    Right a -> a
    Left _ -> []

dnPr :: DT.Preterm
dnPr = DT.Pi DT.Type (DT.Pi (DT.Pi (DT.Pi (DT.Var 0) DT.Bot) DT.Bot) (DT.Var 1))

classic = [dnPr]

importAxiom :: String -> Info -> Info
importAxiom fname info = info

importAxiomio :: String -> IO Info -> IO Info
importAxiomio fname infoio = do
  input <- readFile $ tptpdir ++ (tail $ init fname)
  info <- infoio
  let ast' = P.parseExpr input
  case ast' of
    Right ast -> do
      info' <- setInfo ast
      return $ info  {context = (context info') ++ (context info)} {strcontext =  ", load " ++ tptpdir ++ (tail $ init fname) ++ strcontext info}
    Left err ->
      return $ info {note = "axioim Parser Error" ++ show err}

updateInfo :: IO Info -> S.Expr -> IO Info
updateInfo baseio expr = do
  base <- baseio
  case expr of
    Sout a
      -> return $ base
    File a b
      -> return $ base {filename = a}
    Status a
      -> return $ base {status = a}
    PreNum a
      -> return $ base {prelst = L.zip (L.replicate a "") [1..a] ++ prelst base} {context = L.replicate a DT.Type  ++ context base}
    Include a
      -> importAxiomio a (return base)
    Formula lan name sort f
      -> do
        let either_pre = processf f (filter (/= "") $ map fst $prelst base)
            base' = base {language = lan}
        case either_pre of
          Right (conlst,term) ->
            let prelst' = foldr updateConLst' (prelst base) conlst
                term' = foldr (\(con,varnum) preterm -> A.subst preterm (DT.Var varnum) (DT.Con $ Te.pack con)) term prelst'
            in case sort of
              "conjecture" -> return $ base' { target = Just term'} {strtarget = f}
              "negated_conjecture" -> return $ base' { negated_conjecture = f : (negated_conjecture base')}
              "plain" -> undefined
              "type" -> undefined
              "unknown" -> undefined
              _ ->  --axiom-like formulae (axiom, hypothesis, definition, assumption, lemma, theorem, and corollary).
                {- They are accepted, without proof, as a basis for proving conjectures in THF, TFF, and FOF problems. In CNF problems the axiom-like formulae are accepted as part of the set whose satisfiability has to be established. -}
                return $ base' { prelst = map (\(str,int) -> (str,int + 1)) prelst'} {context = term' : context base} {strcontext = strcontext base ++ "," ++ f}
          Left err ->
            return $ base' {note = "error in process" ++ f}
    _ -> return base

settarget :: Info -> Info
settarget base =
  case target base of
    Just y -> base
    Nothing ->
      case negated_conjecture base of
        (fs:r) ->do
          let f = case r of [] -> fs ; _ -> init $foldr (\a b -> a ++ "&"++ b) "" (fs:r)
          case processf f (filter (/= "") $ map fst $prelst base) of
            Right (conlst,term) ->
              let prelst' = foldr updateConLst' (prelst base) conlst
                  term' = foldr (\(con,varnum) preterm -> A.subst preterm (DT.Var varnum) (DT.Con $ Te.pack con)) term prelst' in
              base {target = Just term'} {strtarget = f}
            Left err -> base {note = "error in process" ++ f}
        [] -> base {note = "found nothing to prove"}

gettarget :: Info -> DT.Preterm
gettarget info =  fromMaybe DT.Bot $ target info

setInfo :: [S.Expr] -> IO Info
setInfo expr= do
    x <- foldl updateInfo (return $ def{prelst = [("",0)]}{context = [DT.Type,DT.Top]}) expr
    let info = settarget x
    return $ info {strcontext = if strcontext x == "" then "" else tail $ strcontext x} {strprocessed = if length (context x) <=  100 then show $ A.Arrow (map A.Conclusion (context x)) (A.Conclusion  $ gettarget info) else ""}

evalInfo ::[S.Expr] -> IO Info
evalInfo expr = do
  base <- setInfo expr
  case target base  of
    Nothing -> return base --undefined --negated_conjectureでどうにもならなかったらfalse
    Just conjecture ->
      return $ base {result = [] /= AP.prove (context base) classic conjecture} {context = []} {target = Nothing} {prelst = []}

processf :: String -> [String] -> Either String ([String] , DT.Preterm)
processf input conlst = do
  let ast' = PF.parseExpr input
  case ast' of
    Right ast ->Right $ t2dt ast conlst
    Left err -> Left $ "Error in processf @" ++ input

t2dt :: F.Expr
  -> [String]  -- ^ for bound variables in (Tall,Texist)
  -> ([String],DT.Preterm)  -- ^ (bound variables,result)
t2dt (Tletter con) s =
  let s' = if con `elem` s then s else con :s
  in (s' , DT.Con $ Te.pack con)
t2dt Ttrue s = (s, DT.Top)
t2dt Tfalse s= (s, DT.Bot)
t2dt (Tneg formula) s=
  let (s' , arg1) = t2dt formula s in
    (s' , DT.Not arg1)
t2dt (Tbinary biop f1 f2) s =
  let (s1 , arg1) = t2dt f1 s
      (s' , arg2) = t2dt f2 s1 in
    case biop of
      Tand -> (s' , DT.Sigma arg1 arg2)
      Tor -> (s' , DT.Not $ DT.Sigma (DT.Not arg1) (DT.Not arg2))
      Timp -> (s' , DT.Pi arg1 arg2)
      Tequiv -> (s' , DT.Sigma (DT.Pi arg1 arg2) (DT.Pi arg2 arg1))
t2dt (TApp f []) s =t2dt f s
t2dt (TApp f (a1:r)) s =
  let (s1,alast) = t2dt a1 s
      (s',args) = t2dt (TApp f r) s1 in
    (s' , DT.App args alast)
t2dt (Tall [] f ) s = t2dt f s
t2dt (Tall vars f ) s =
  let Tvar var = head vars
      (s' , arg2) = t2dt (Tall (tail vars) f) (var : s )
  in (filter (/= var) s' , DT.Pi DT.Type (A.subst arg2 (DT.Var 0) (DT.Con $ Te.pack var)))
t2dt (Texist [] f ) s = t2dt f s
t2dt (Texist vars f ) s =
  let Tvar var = head vars
      (s' , arg2) = t2dt (Texist (tail vars) f) (var : s )
  in (filter (/= var) s' , DT.Sigma DT.Type (A.subst arg2 (DT.Var 0) (DT.Con $ Te.pack var)))
