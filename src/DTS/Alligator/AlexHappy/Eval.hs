module DTS.Alligator.AlexHappy.Eval (eval,eval1) where

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

import Debug.Trace as D
import System.Timeout


data Value
  = VBool Bool String String String
  | VUnit
  deriving (Eq)

instance Show Value where
  show (VBool b s1 s2 s3) = show b ++ " "++s1++" "++s2++" "++s3
  show _ = ""

type Eval = StateT Env' IO
type Env = [(String, Value)]
type Env' = ([(String,Int)],[DT.Preterm],Int,String)

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

-- ^ modify :: (s -> s) -> State s (): 関数で状態を更新します。
-- ^ lookup 2 [(1,"a"),(2,"b")]  : b
eval1 :: S.Expr -> Eval Value
eval1 expr = case expr of
  Sout a ->
    return VUnit
  File a b->
    return VUnit
  Status a-> do
    liftIO $ print a
    return VUnit
  PreNum a -> do
    modify $ \(prelst,pretermlst,clanum,claid) -> ( (L.zip (L.replicate a "") [1..a] ++ prelst )++ [("",0)], L.replicate a DT.Type ++ [DT.Type] ++ pretermlst,clanum,claid)
    return VUnit
  ClaNum a -> do
    modify $ \(prelst,pretermlst,_,claid) -> ( prelst, pretermlst,a,claid)
    return VUnit
  Formula "cnf"  name sort f -> do
    (prelst,pretermlst,clanum,negcons) <- get
    let negcons' = negcons ++ "&" ++ f
        clanum' = clanum - 1
    -- liftIO $ print ("cnf" ++ (show clanum'))
    if sort == "negated_conjecture"
      then
        if clanum' == 0
          then do
            let either_pre = processf (tail negcons') (filter (/= "") $ map fst prelst)
            case either_pre of
              Right (conlst,term) ->
                let prelst' = foldr updateConLst' prelst conlst
                    term' = foldr (\(con,varnum) preterm -> A.subst preterm (DT.Var varnum) (DT.Con $ Te.pack con)) term prelst'
                in do
                  test <- liftIO $ timeout 1000000 $ return $[] /= AP.prove pretermlst classic term'
                  case test of
                    Just True -> return $ VBool True "" f (show $ A.Arrow (map (\p -> A.Conclusion p) pretermlst) (A.Conclusion term'))
                    _ -> return $ VBool False "" f (show $ A.Arrow (map (\p -> A.Conclusion p) pretermlst) (A.Conclusion term'))
              Left err -> return $ VBool False "" "" (err)
          else do
            modify $ \(_,_,_,_) -> (prelst,pretermlst,clanum',negcons')
            return VUnit
      else
        if sort == "axiom"
        then do
          let either_pre = processf f (filter (/= "") $ map fst prelst)
          case either_pre of
            Right (conlst,term) -> do
              let prelst' = foldr updateConLst' prelst conlst
                  term' = foldr (\(con,varnum) preterm -> A.subst preterm (DT.Var varnum) (DT.Con $ Te.pack con)) term prelst'
              modify $ \(_,_,_,_) -> (map (\(str,int) -> (str,int + 1)) prelst',term' : pretermlst,clanum - 1,negcons)
              return $VBool True f "" ""
            Left err -> do
              return $ VBool False "" "" ("error in process "++ err)
        else return VUnit
  Formula "fof"  name sort f -> do
    (prelst,pretermlst',clanum,claid) <- get
    let either_pre = processf f (filter (/= "") $ map fst prelst)
    case either_pre of
      Right (conlst,term) ->
        let prelst' = foldr updateConLst' prelst conlst
            term' = foldr (\(con,varnum) preterm -> A.subst preterm (DT.Var varnum) (DT.Con $ Te.pack con)) term prelst'
        in
        if sort == "axiom"
          then do
            modify $ \(_,_,_,_) -> (map (\(str,int) -> (str,int + 1)) prelst',term' : pretermlst',clanum ,claid)
            return $VBool True f "" ""
          else do
            test <- liftIO $ timeout 1000000 $ return $[] /= AP.prove pretermlst' classic term'
            case test of
              Just True -> return $ VBool True "" f (show $ A.Arrow (map (\p -> A.Conclusion p) pretermlst') (A.Conclusion term'))
              _ -> return $ VBool False "" f (show $ A.Arrow (map (\p -> A.Conclusion p) pretermlst') (A.Conclusion term'))
      Left err -> do
        return $ VBool False "" "" ("error in process "++ err)
{-
:l DTS.Alligator.AlexHappy.Eval
import DTS.Alligator.AlexHappy.Parserf as PF
import qualified DTS.Alligator.Prover as AP
import DTS.Alligator.AlexHappy.Lexer
import DTS.Alligator.AlexHappy.Lexerf
f = "(~q1(a,c,a))"
prelst = []
processf f (filter (/= "") $ map fst prelst)
ast = PF.parseExpr f
ast
tokenStream = DTS.Alligator.AlexHappy.Lexerf.scanTokens f

:l DTS.Alligator.AlexHappy.FileParser
import DTS.Alligator.AlexHappy.Parser as P
import DTS.Alligator.AlexHappy.Lexer as Lx
import Control.Monad.State
import DTS.Alligator.AlexHappy.Syntax
import DTS.DTT as DT
fname = "DTS/Alligator/TPTP-v7.3.0/Problems/SYN/SYN284-1.p"
input <- readFile fname
let ast' = P.parseExpr input
tokenStream = Lx.scanTokens input
Right ast = ast'
test <- evalStateT (mapM eval1 ast) ([],[DT.Top],0,[])
-}

and' :: [Value] -> Maybe (Bool,String,String,String)
and' lst=  case filter (\v -> case v of VBool b "" "" _ -> False ;  VUnit -> False ; _ -> True) lst of
            [] -> Nothing
            lst' ->  Just $ (\l -> (all (\(VBool b _ _ _)-> b) l
              ,init $ concat $ " " :
                  map
                    (\(VBool _ s _ _) ->  if s== "" then "" else s ++ ",")
                    l
              ,concatMap (\(VBool b _ s _) -> s) l
              ,concatMap (\(VBool b _ _ s) -> s) l))
                lst'


eval :: [S.Expr] -> IO (Maybe (Bool,String,String,String))
eval ast = evalStateT (mapM eval1 ast) ([],[DT.Top],0,[]) >>= \result -> return $ and' result

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
--
-- t2dtstr :: P.Tformula -> String
-- t2dtstr (P.Tletter con) = "DT.Con $ T.pack \""++[con] ++"\""
-- t2dtstr P.Ttrue = "DT.Top"
-- t2dtstr P.Tfalse = "DT.Bot"
-- t2dtstr (P.Tneg formula) = "DT.Not ("++t2dtstr formula++")"
-- t2dtstr (P.Tbinary (P.Tand) f1 f2) = "DT.Sigma ("++t2dtstr f1++") ("++t2dtstr f2++")"
-- t2dtstr (P.Tbinary (P.Tor) f1 f2 )= "DT.Not $ DT.Sigma (DT.Not $"++ t2dtstr f1++") (DT.Not $"++ t2dtstr f2++")"
-- t2dtstr (P.Tbinary (P.Timp) f1 f2) = "DT.Pi (" ++ t2dtstr f1 ++ ") (" ++ t2dtstr f2 ++ ")"
-- t2dtstr (P.Tbinary (P.Tequiv) f1 f2) = "DT.Sigma (DT.Pi ("++t2dtstr f1++") (" ++t2dtstr f2++")) (DT.Pi (" ++t2dtstr f2 ++") (" ++t2dtstr f1++"))"
-- t2dtstr (P.Tall [P.Tvar con] f )= "DT.Pi (DT.Con $ T.pack \"" ++ [con]++ "\") (" ++t2dtstr f++")"
-- t2dtstr (P.Texist [P.Tvar con] f) = "DT.Sigma (DT.Con $ T.pack \"" ++ [con] ++"\" ) (" ++t2dtstr f++")"
