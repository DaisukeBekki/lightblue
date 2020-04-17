module DTS.Alligator.AlexHappy.Evalf (t2dt) where

import DTS.Alligator.AlexHappy.Syntaxf
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.List as L
import DTS.Alligator.AlexHappy.Parser as P
import qualified Data.Text.Lazy as Te
import qualified Data.Text.IO as T
import qualified DTS.DTT as DT
import qualified DTS.Prover.Judgement as J
import qualified DTS.Alligator.Arrowterm as A
import qualified DTS.Alligator.Prover as AP


data Value
  = VInt Int
  | VUnit

instance Show Value where
  show (VInt x) = show x

type Eval = StateT Env' IO
type Env = [(String, Value)]
type Env' = ([(String,Int)],[DT.Preterm])

-- ^ modify :: (s -> s) -> State s (): 関数で状態を更新します。
-- ^ lookup 2 [(1,"a"),(2,"b")]  : b
-- eval1 :: Expr -> Eval Value
-- eval1 expr = case expr of

  -- Num a -> return (VInt a)
  -- Var a -> do
  --   env <- get
  --   case lookup a env of
  --     Just val -> return val
  --     Nothing -> error "Not in scope"
  -- Print a -> do
  --   a' <- eval1 a
  --   liftIO $ print a'
  --   return VUnit
  -- Sout a ->
  --   if a == ""
  --     then
  --       return VUnit
  --     else
  --       do
  --         liftIO $ print $ "about " ++ show a
  --         return VUnit
  -- File a b-> do
  --   liftIO $ print $ "file :" ++ a ++ " and " ++ b
  --   return VUnit
  -- PreNum a -> do
  --   modify $ \(prelst,pretermlst) -> (L.zip (L.replicate a "") [1..a] ++ prelst ,( L.replicate a DT.Type) ++ pretermlst) --Data.Bifunctor.bimap ((++) L.zip (L.replicate a "") [1..a] )((++) (L.replicate a DT.Type)
  --   liftIO $ print $ "number of predicates :" ++ (show a)
  --   return VUnit
  -- Formula level  name sort f -> do
  --   (prelst,pretermlst) <- get
  --   liftIO $ print $ sort ++ " " ++ f ++ (show $ L.length pretermlst)
  --   return VUnit
-- eval :: Expr -> IO ()
-- eval xs =  undefined
-- evalStateT (mapM_ eval1 xs) ([],[])


t2dt :: Expr -> [String] -> ([String],DT.Preterm)
t2dt (Tletter con) s =
  let s' = if con `elem` s then s else con :s
  in (s' , DT.Con $ Te.pack con)
t2dt Ttrue s = (s, DT.Top)
t2dt Tfalse s= (s, DT.Top)
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
t2dt (Tall [] f ) s = t2dt f s
t2dt (Tall vars f ) s =
  let Tvar var = head vars
      (s' , arg2) = t2dt (Tall (tail vars) f) (var : s )
  in (filter (/= var) s' , DT.Pi (DT.Con $ Te.pack var) arg2)
t2dt (Texist [] f ) s = t2dt f s
t2dt (Texist vars f ) s =
  let Tvar var = head vars
      (s' , arg2) = t2dt (Texist (tail vars) f) (var : s )
  in (filter (/= var) s' , DT.Sigma (DT.Con $ Te.pack var) arg2)
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
--
--
-- readTFormula :: String -> IO DT.Preterm
-- readTFormula filename= do
--   text <- T.readFile $"DTS/Alligator/TPTP/"++filename
--   f' <- P.parseFormula $ P.cleanse text
--   return $t2dt f'
--
-- main1 :: IO Bool
-- main1 =
--   let
--     sig_env = []
--     var_env = [DT.Type,DT.Type,DT.Type]
--   in do
--     pre_type <- readTFormula "SYN001+1.p"
--     return $ [] /= (AP.prove var_env sig_env (A.subst (A.subst (A.subst pre_type (DT.Var 2) (DT.Con $ Te.pack "p")) (DT.Var 1) (DT.Con $ Te.pack "q") ) (DT.Var 0) (DT.Con $ Te.pack "r") ) )
--
--
-- test :: String -> IO Bool
-- test file=
--   let
--     sig_env = []
--     var_env = [DT.Type,DT.Type,DT.Type]
--   in do
--     pre_type <- readTFormula file
--     return $ [] /= (AP.prove var_env sig_env (A.subst (A.subst (A.subst pre_type (DT.Var 2) (DT.Con $ Te.pack "p")) (DT.Var 1) (DT.Con $ Te.pack "q") ) (DT.Var 0) (DT.Con $ Te.pack "r") ) )
