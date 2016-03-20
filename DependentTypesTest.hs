{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

--import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import DependentTypes

main :: IO()
main = do
  ---1.3---
  let p = Lam (App (Var 0) (Lam (Var 0)))
  T.putStrLn $ toText $ p
  T.putStrLn $ toTextWithVN [] $ p
  let f = Var 0
  T.putStrLn $ toText $ f
  T.putStrLn $ toTextWithVN ["f"] $ f
  let pf = subst (App (Var 0) (Lam (Var 0))) f 0 
  T.putStrLn $ toText $ pf    
  T.putStrLn $ toTextWithVN ["f"] $ pf    
  ---1.4---
  let p2 = Lam (App (Var 1) (Var 0))
  T.putStrLn $ toText p2
  T.putStrLn $ toTextWithVN ["p"] p2
  let gy = App (Var 0) (Var 1)
  T.putStrLn $ toText gy
  T.putStrLn $ toTextWithVN ["g","y"] gy
  let g = subst (Lam (App (Var 1) (Var 0))) (App (Var 0) (Var 1)) 0
  T.putStrLn $ toText g
  T.putStrLn $ toTextWithVN ["g","y"] g
  --- toTextWithVN
  T.putStrLn $ toTextWithVN [] Type
  T.putStrLn $ toTextWithVN [] Kind
  T.putStrLn $ toTextWithVN [] (Pi (Con "entity") (App (Con "run") (Var 0)))
  T.putStrLn $ toTextWithVN [] (Pi (Sigma (Con "entity") (Sigma (Con "man") (App (Con "enter") (Var 1)))) (Not (App (Con "whistle") (Proj Fst (Var 0)))))
  T.putStrLn $ toTextWithVN [] (Pi (Sigma (Con "entity") (Sigma (Con "man") (App (Con "enter") (Var 1)))) (Not (App (Con "whistle") (Asp 1 (Con "entity")))))
  --- beta reduction
  let verb = Lam (Sigma (Con "entity") (App (Var 1) (Var 0))) 
  let arg = Proj Fst (Asp 1 (Sigma (Con "entity") (App (Con "fatherOf") (Pair (Var 0) (Var 1)))))  
  T.putStrLn $ toTextWithVN ["x"] (betaReduce (App verb arg))
  ---
  let g = Lam (Lam (App (Con "F") (Pair (Var 0) (Var 1))))
  let f = Lam (Var 0)   
  T.putStrLn $ toText $ betaReduce (App (App g (Var 1)) (Var 0)) 
   

