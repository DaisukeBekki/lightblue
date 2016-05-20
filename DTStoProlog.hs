{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as T --text
import qualified Data.Text.Lazy.IO as T --text
import qualified DTS.DependentTypes as DTS
import qualified DTS.DependentTypesWVN as D
import qualified Parser.ChartParser as CP
import qualified System.Process as S

-- function: f
-- a function which converts DTS preterm with variable name to Prolog input formula
f :: D.Preterm -> T.Text
f preterm = case preterm of
  D.Var v -> v
  D.Con c -> T.replace "[MCN]" "" c
  D.Type  -> "type"
  D.Kind  -> "kind"
  D.Pi vname a b    -> T.concat ["forall(", vname, ",", (f $ a), ",", (f $ b), ")"]
  D.Sigma vname a b -> T.concat ["exists(", vname, ",", (f $ a), ",", (f $ b), ")"]
  D.Not a           -> T.concat ["not(", (f $ a), ")"]
  D.Lam vname b     -> T.concat ["lam(", vname, ",", (f $ b), ")"]
  D.Asp i a         -> T.concat ["@(", (T.pack (show i)), ",", (f $ a), ")"]
  D.Pair t u        -> T.concat ["[", (f $ t), ",", (f $ u), "]"]
  D.Proj D.Fst t    -> T.concat ["pi1(", (f $ t), ")"]
  D.Proj D.Snd t    -> T.concat ["pi2(", (f $ t), ")"]
  D.Eq _ m n        -> T.concat ["eq(", (f $ m), ",", (f $ n), ")"]
  D.Top -> "true"
  D.Bot -> "false"
  D.App (D.App (D.App (D.App g x4) x3) x2) x1
                        -> T.concat [(f $ g), "(", (f $ x1), ",", (f $ x2), ",", (f $ x3), ",", (f $ x4), ")"]
  D.App (D.App (D.App g x3) x2) x1
                        -> T.concat [(f $ g), "(", (f $ x1), ",", (f $ x2), ",", (f $ x3), ")"]
  D.App (D.App g x2) x1 -> T.concat [(f $ g), "(", (f $ x1), ",", (f $ x2), ")"]
  D.App g x1            -> T.concat [(f $ g), "(", (f $ x1), ")"]
--  D.Lamvec vname m -> T.concat ["lamvec(", vname, ",", f $ m, ")"]
--  D.Appvec vname m -> T.concat ["appvec(", vname, ",", f $ m, ")"]
  D.Lamvec _ m -> f $ m
  D.Appvec _ m -> f $ m
  D.Unit         -> "true"
  D.Nat          -> "true"
  D.Zero         -> "true"
  D.Succ _       -> "true"
  D.Natrec _ _ _ -> "true"
  D.Refl _ _     -> "true"
  D.Idpeel _ _   -> "true"


-- function convcoq

convcoq :: D.Preterm -> T.Text
convcoq preterm = case preterm of
  D.Var v -> v
  D.Con c -> case c of
             "entity" -> "Entity"
             "event"  -> "Event"
             "state"  -> "State"
             _        -> T.concat ["_", c]
  D.Type  -> "Prop"
  D.Kind  -> "kind"
  D.Pi _ a b    -> T.concat [(convcoq $ a), " -> ", (convcoq $ b)]
  D.Sigma vname a b -> T.concat ["exists(", vname, ",", (convcoq $ a), ",", (convcoq $ b), ")"]
  D.Not a           -> T.concat ["not(", (convcoq $ a), ")"]
  D.Lam vname b     -> T.concat ["lam(", vname, ",", (convcoq $ b), ")"]
  D.Asp i a         -> T.concat ["@(", (T.pack (show i)), ",", (convcoq $ a), ")"]
  D.Pair t u        -> T.concat ["[", (convcoq $ t), ",", (convcoq $ u), "]"]
  D.Proj D.Fst t    -> T.concat ["pi1(", (convcoq $ t), ")"]
  D.Proj D.Snd t    -> T.concat ["pi2(", (convcoq $ t), ")"]
  D.Eq _ m n        -> T.concat ["eq(", (convcoq $ m), ",", (convcoq $ n), ")"]
  D.Top -> "true"
  D.Bot -> "false"
  D.App (D.App (D.App (D.App g x4) x3) x2) x1
                        -> T.concat [(convcoq $ g), "(", (convcoq $ x1), ",", (convcoq $ x2), ",", (convcoq $ x3), ",", (convcoq $ x4), ")"]
  D.App (D.App (D.App g x3) x2) x1
                        -> T.concat [(convcoq $ g), "(", (convcoq $ x1), ",", (convcoq $ x2), ",", (convcoq $ x3), ")"]
  D.App (D.App g x2) x1 -> T.concat [(convcoq $ g), "(", (convcoq $ x1), ",", (convcoq $ x2), ")"]
  D.App g x1            -> T.concat [(convcoq $ g), "(", (convcoq $ x1), ")"]
--  D.Lamvec vname m -> T.concat ["lamvec(", vname, ",", f $ m, ")"]
--  D.Appvec vname m -> T.concat ["appvec(", vname, ",", f $ m, ")"]
  D.Lamvec _ m -> convcoq $ m
  D.Appvec _ m -> convcoq $ m
  D.Unit         -> "true"
  D.Nat          -> "true"
  D.Zero         -> "true"
  D.Succ _       -> "true"
  D.Natrec _ _ _ -> "true"
  D.Refl _ _     -> "true"
  D.Idpeel _ _   -> "true"

-- sigToCoq :: DTS.Signature -> T.Text
-- sigToCoq (text, preterm) = T.concat ["Parameter _", text, " : ", (convcoq $ DTS.fromDeBruijn $ preterm), ". \n"]

makeCoqSigList :: [DTS.Signature] -> T.Text
makeCoqSigList siglist = (T.concat (map (\ (text, preterm) -> T.concat ["Parameter _", text, " : ", (convcoq $ DTS.fromDeBruijn $ preterm), ". \n"]) siglist))


main :: IO()
main = do
  sentence <- T.getLine
  (chart,_) <- CP.parse 24 sentence
  let top = CP.topBox chart;
      sonly = filter CP.isS top;
      top' = if sonly == [] then top else sonly
      representative = head $ CP.bestOnly $ top'
  let formula = DTS.fromDeBruijn $ CP.sem $ representative
  T.putStrLn "-- Preterm ---------"
  T.putStrLn $ CP.toText $ formula
  let proformula = f $ formula ;
  T.putStrLn "-- Prolog input ---------"
  T.putStrLn $ proformula
-- Call Prolog (@-elimination)
  let command1 = T.concat ["swipl -s Prolog/presupposition.pl -g main -t halt --quiet -- \"", proformula, "\""]
  (_, stdout1, _, _) <- S.runInteractiveCommand $ T.unpack command1
  t1 <- T.hGetContents stdout1
  T.putStrLn $ "-- After resolving @ --------"
  T.putStrLn $ t1
-- Call Prolog (Sigma-elimination)
  let command2 = T.concat ["swipl -s Prolog/elimSigma.pl -g main -t halt --quiet -- \"", t1, "\""]
  (_, stdout2, _, _) <- S.runInteractiveCommand $ T.unpack command2
  t2 <- T.hGetContents stdout2
  T.putStrLn $ "-- After elimSigma --------"
  T.putStrLn $ t2
-- List Signature in Coq format
  let signature_list = CP.sig $ representative
  T.putStrLn "-- Coq signature --------"
  T.putStrLn $ makeCoqSigList $ signature_list
