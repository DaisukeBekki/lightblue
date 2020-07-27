module DTS.Alligator.AlexHappy.Eval (evalInfo) where

import qualified DTS.Alligator.AlexHappy.Syntax as S
import qualified DTS.Alligator.AlexHappy.Syntaxf as F
import qualified Data.Map as Map
import qualified Data.List as L
import qualified DTS.Alligator.AlexHappy.Parser as P
import qualified DTS.Alligator.AlexHappy.Parserf as PF
import qualified Data.Text.Lazy as Te
-- import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as T
import qualified DTS.DTT as DT
import qualified DTS.Prover.Judgement as J
import qualified DTS.Alligator.Arrowterm as A

--import qualified DTS.Alligator.Prover as AP
import qualified DTS.Alligator.AlexHappy.TPTPInfo as TI

import Data.Maybe
import Control.Monad.State

import Data.Default (Default(..))

import Debug.Trace as D
import System.Timeout


testexpr = [S.Status "Theorem",S.PreNum 3,S.Formula "fof" "" "axiom" "p",S.Formula "fof" "" "axiom" "p=>q",S.Formula "fof" "" "axiom" "q=>r",S.Formula "fof" "" "conjecture" "r"]

updateConLst :: String -> [(String,Int)] -> Either String [(String,Int)]
updateConLst con conlst =
  case lookup con conlst of
    Just varnum ->Right conlst
    Nothing ->
      let lst' = dropWhile ((/="") .fst) conlst
      in
        if null lst'
        then
          Left "Error at updateConLst"
        else
          let (_,varnum) = head lst'
          in Right $(takeWhile ((/="") .fst) conlst) ++ ((con,varnum):tail lst')


updateConLst' :: String -> [(String,Int)] -> [(String,Int)]
updateConLst' con conlst =
  case updateConLst con conlst of
    Right a -> a
    Left _ -> []

importAxiomio :: String -> IO TI.Info -> IO TI.Info
importAxiomio fname infoio = do
  input <- readFile $ TI.tptpdir ++ tail (init fname)
  info <- infoio
  let ast' = P.parseExpr input
  case ast' of
    Right ast -> do
      info' <- foldl updateInfo infoio ast
      return $ info  {TI.context = TI.context info' ++ TI.context info} {TI.strcontext =  ", load " ++ TI.tptpdir ++ tail ( init fname) ++ TI.strcontext info}
    Left err ->
      return $ info {TI.note = "axioim Parser Error" ++ show err}

generateType :: Int -> DT.Preterm
generateType 0 = DT.Type
generateType num = DT.Pi DT.Type (generateType (num-1))

generateExpr :: Int -> F.Expr
generateExpr 0 = F.Tletter "prop"
generateExpr num = F.Tbinary F.Timp (F.Tletter "type") (generateExpr (num - 1))


contextUpdate :: [(String,Int)] -> TI.Info ->  TI.Info
contextUpdate conlst' base =
  let conlst = filter ((/= 0).snd) conlst'
  in
    foldr
      (
        \(var,argnum) info ->
          case lookup var $ TI.prelst info of
            Nothing ->
              let prelst2 = {-D.trace ("beforecontextUpdate context : "++ (show $ (TI.context info)) ++ "\nprelst : "++ (show $(TI.prelst info))++"\nconlst"++(show $ conlst')++"\n")
                    $-}dropWhile ((/= "").fst) $ TI.prelst info
                  targetNum =  (snd . head)  prelst2
                  prelst' =  takeWhile ((/="").fst) (TI.prelst info) ++ ((var,targetNum) : tail prelst2)
                  context' = take targetNum (TI.context info) ++ [generateType argnum] ++ drop (targetNum+ 1) (TI.context info)
                  contextWithTexpr' = take targetNum (TI.contextWithTexpr info) ++ [generateExpr argnum] ++ drop (targetNum+ 1) (TI.contextWithTexpr info)
              in
                 info {TI.context = context'} {TI.prelst = prelst'}{TI.contextWithTexpr = contextWithTexpr'}
            Just num ->
              let context' = take num (TI.context info) ++ [generateType argnum] ++ drop (num+1) (TI.context info)
                  contextWithTexpr' = take num (TI.contextWithTexpr info) ++ [generateExpr argnum] ++ drop (num+1) (TI.contextWithTexpr info)
              in
                 info {TI.context = context'}{TI.contextWithTexpr = contextWithTexpr'}
      )
      base
      conlst

updateInfo :: IO TI.Info -> S.Expr -> IO TI.Info
updateInfo baseio expr = do
  base <- baseio
  case expr of
    S.Sout a
      -> return  base
    S.File a b
      -> return base
    S.Status a
      -> return $ base {TI.status = Just (read a :: TI.Status)}
    S.PreNum a
      ->  return $ base  {TI.prelst = L.zip (L.replicate a "") [0..(a-1)]  ++ map (\(str,int) -> (str,int + a)) (TI.prelst base)} {TI.context = L.replicate a DT.Type  ++ TI.context base}{TI.contextWithTexpr = L.replicate a (F.Tletter "type")  ++ TI.contextWithTexpr base}
    S.Include a
      -> importAxiomio a baseio
    S.Formula lan name sort f
      -> do
        let either_pre = processf  f (filter (/= "") $ map fst $TI.prelst base)
            base2 = base {TI.language = Just (read lan ::TI.Language)}
        case either_pre of
          Right (ast,(conlst',term)) ->
            let conlst = map fst conlst'
                prelst' = foldr updateConLst' (TI.prelst base) conlst
                base' = contextUpdate conlst' base2
                term' = foldr (\(con,varnum) preterm -> A.subst preterm (DT.Var varnum) (DT.Con $ Te.pack con)) term prelst'
            in case read sort :: TI.Role of
              TI.Conjecture -> return $ base' { TI.prelst = prelst'}{ TI.target = Just term'} {TI.strtarget = f}
              TI.NegatedConjecture -> return $ base'  { TI.prelst = prelst'}{ TI.negated_conjecture = Just $ case TI.negated_conjecture base' of Just term1 -> DT.Sigma (DT.Not term1) term' ; Nothing -> DT.Not term' } {TI.strnegated =  "(~("++TI.strnegated base'++"))& " ++ f }
              TI.Plain -> undefined
              TI.Type -> undefined
              TI.RUnknown -> undefined
              sort' ->
                if TI.isAxiomLike sort'
                then
                  return  $base' { TI.prelst = ("axiom",0):map (\(str,int) -> (str,int + 1)) prelst'} {TI.context = term' : TI.context base} {TI.contextWithTexpr = ast : TI.contextWithTexpr base}{TI.strcontext =  TI.strcontext base ++ "," ++ f}
                else undefined
          Left err ->
            return $ base2 {TI.note = "error in process" ++ f}
    _ -> return base

updateInfoAboutFormulae :: IO TI.Info -> S.Expr -> IO TI.Info
updateInfoAboutFormulae baseio expr = do
  base <- baseio
  case expr of
    S.Sout a
      -> return  base
    S.File a b
      -> return base
    S.Status a
      -> return base
    S.PreNum a
      -> return base
    S.Include a
      -> importAxiomio a baseio
    S.Formula lan name sort f
      -> do
        let either_pre = processf  f (filter (/= "") $ map fst $TI.prelst base)
            base2 = base {TI.language = Just (read lan ::TI.Language)}
        case either_pre of
          Right (ast,(conlst',term)) ->
            let conlst = map fst conlst'
                prelst' = foldr updateConLst' (TI.prelst base) conlst
                base' =contextUpdate conlst' base2
                term' = foldr (\(con,varnum) preterm -> A.subst preterm (DT.Var varnum) (DT.Con $ Te.pack con)) term prelst'
            in case read sort :: TI.Role of
              TI.Conjecture -> return $ base' { TI.target = Just term'} {TI.strtarget = f}{TI.targetWithTexpr=Just ast}
              TI.NegatedConjecture -> return $ base' { TI.negated_conjecture = Just $ case TI.negated_conjecture base' of Just term1 -> DT.Sigma (DT.Not term1) term' ; Nothing -> DT.Not term' }{TI.negatedWithTexpr= Just $ case TI.negatedWithTexpr base' of Just term1 -> F.Tbinary F.Tand (F.Tneg term1) ast ; Nothing -> F.Tneg ast} {TI.strnegated =  "(~("++TI.strnegated base'++"))"++"& " ++ f }
              TI.Plain -> undefined
              TI.Type -> undefined
              TI.RUnknown -> undefined
              sort' ->
                if TI.isAxiomLike sort'
                then
                  return $ base' { TI.prelst = ("axiom",0):map (\(str,int) -> (str,int + 1)) prelst'} {TI.context = term' : TI.context base}{TI.contextWithTexpr = ast : TI.contextWithTexpr base} {TI.strcontext =  TI.strcontext base ++ "," ++ f}{TI.contextWithTexpr = ast : TI.contextWithTexpr base}
                else undefined
          Left err ->
            return $ base2 {TI.note = "error in process" ++ f}
    _ -> return base

settarget :: TI.Info -> TI.Info
settarget base =
  case TI.target base of
    Just y -> base
    Nothing ->
      case TI.negated_conjecture base of
        Just y -> base {TI.target = Just y} {TI.strtarget = if TI.strtarget base == "" then "" else tail $ TI.strnegated base}
        Nothing -> base {TI.note = "found nothing to prove"}

gettarget :: TI.Info -> DT.Preterm
gettarget info =  fromMaybe DT.Bot $ TI.target info

setInfo :: [S.Expr] -> String -> IO TI.Info
setInfo expr fname= do
    x2 <- foldl updateInfo (return $ def{TI.context = [DT.Type,DT.Top]}{TI.contextWithTexpr = [F.Tletter "prop",F.Tneg $ F.Tletter "false"]}{TI.prelst=[("false",0),("top",1)]}) expr--(return $ def{prelst = [("",0)]}{context = [DT.Type,DT.Top]}) expr
    let (prelst',contextcontextWithTexpr) = unzip $filter (\((str,_),_) -> (str /= "") && (str /= "axiom")) $zip (TI.prelst x2) $zip (TI.context x2) (TI.contextWithTexpr x2)
        (context,contextWithTexpr) = unzip contextcontextWithTexpr
        prelst = zip (map fst prelst') [0..]
    x <- foldl updateInfoAboutFormulae (return$ x2{TI.prelst=prelst}{TI.context=context}{TI.contextWithTexpr = contextWithTexpr}) expr
    let info = settarget x
        prelstWithoutAxiomname = map (\(str,num) -> if str == "axiom" then ("axiom"++(show num),num) else (str,num) ) $TI.prelst info
    return $
      info {TI.filename = fname}{TI.strcontext = if TI.strcontext x == "" then "" else tail $ TI.strcontext x} {TI.prelst = prelstWithoutAxiomname}--{TI.strprocessed = if length (TI.context x) <=  100 then show $ A.Arrow (map A.arrowNotat {-[DT.Not DT.Top]-}(TI.context x)) (A.arrowNotat  $ {-DT.Top-}gettarget info) else ""}


evalInfo ::[S.Expr] -> String -> IO TI.Info
evalInfo expr fname = do
  base <- setInfo expr fname
  return base

processf  :: String -> [String] -> Either String (F.Expr,([(String,Int)] , DT.Preterm))
processf  input conlst = do
  let ast' = PF.parseExpr input
  case ast' of
    -- Right ast ->D.trace input Right $ t2dt  ast $map (\x -> (x,0)) conlst
    Right ast -> Right (ast, t2dt  ast $map (\x -> (x,0)) conlst)
    Left err -> Left $ "Error in processf @" ++ input



t2dt  :: F.Expr
  -> [(String,Int)]  -- ^ for bound variables in (F.Tall,F.Texist)
  -> ([(String,Int)],DT.Preterm)  -- ^ (bound variables,result,functions)
t2dt  (F.Tletter con) s =
  let s' = if con `elem` map fst s then s else (con,0) :s
  in  (s' , DT.Con $ Te.pack con )
t2dt  F.Ttrue s = (s, DT.Top)
t2dt  F.Tfalse s=  (s, DT.Bot)
t2dt  (F.Tneg formula) s=
  let (s' , arg1) = t2dt  formula s in
    (s' , DT.Not arg1)
t2dt  (F.Tbinary biop f1 f2) s =
  let (s1 , arg1) = t2dt  f1 s
      (s' , arg2) = t2dt  f2 s1 in
    case biop of
      F.Tand -> (s' , DT.Sigma arg1 arg2)
      F.Tor -> (s' , DT.Not $ DT.Sigma (DT.Not arg1) (DT.Not arg2))
      F.Timp -> (s' , DT.Pi arg1 arg2)
      F.Tequiv -> (s' , DT.Sigma (DT.Pi arg1 arg2) (DT.Pi arg2 arg1))
t2dt  (F.TApp f []) s =t2dt  f s
t2dt  (F.TApp f (a:r)) s =
  case a of
    F.TFormula a1 ->
      let (s1,alast) = t2dt a1 s
          (s2,args) = t2dt  (F.TApp f r) s1
          f' = case f of
            F.Tletter fstr -> fstr
            _ -> " "
          s' = case lookup f' s2 of
            Nothing -> (f',length (a:r)):s2
            Just num -> if num < length (a:r)
              then (f',length (a:r)) : L.delete (f',num) s2
              else s2
      in
        (s' , DT.App args alast)
    _ -> D.trace "TDef in app" undefined
t2dt  (F.Tall [] f ) s = t2dt  f s
t2dt  (F.Tall vars f ) s =
  case head vars of
    F.TDef _ _ -> D.trace "TDef in all" undefined
    F.TFormula (F.Tletter var) ->
      let (s' , arg2) = t2dt  (F.Tall (tail vars) f) ((var,0) : s )
      in (filter ((/= var).fst) s' , DT.Pi DT.Type (A.subst arg2 (DT.Var 0) (DT.Con $ Te.pack var)))
    F.TFormula _ -> D.trace "Tformula in all" undefined
t2dt  (F.Texist [] f ) s = t2dt  f s
t2dt  (F.Texist vars f ) s =
  case head vars of
    F.TDef _ _ -> D.trace "TDef in exist" undefined
    F.TFormula (F.Tletter var) ->
      let (s' , arg2) = t2dt  (F.Texist (tail vars) f) ((var,0) : s )
      in (filter ((/= var).fst) s' , DT.Sigma DT.Type (A.subst arg2 (DT.Var 0) (DT.Con $ Te.pack var)))
    F.TFormula _ -> D.trace "Tformula in exist" undefined
