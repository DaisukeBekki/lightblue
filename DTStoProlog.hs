{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as T     -- text
import qualified Data.Text.Lazy.IO as T  -- text
import qualified Data.List as L          -- base
import qualified Data.Time as Time       -- time
import qualified System.Process as S     -- process
import qualified System.Environment as E -- base
import qualified Control.Monad as M      -- base
import qualified DTS.DependentTypes as DTS
import qualified DTS.DTSwithVarName as D
import qualified Parser.ChartParser as CP
import qualified Interface.Text as T

-- function: cname
-- normalize the given constant text
cname_f :: T.Text -> T.Text
cname_f cname = T.concat ["c_",  (T.replace "ー" "xmdashx" $
                                  T.replace "（" "lpar" $
                                  T.replace "）" "rpar" $
                                  T.replace "・" "middot" $
                                  T.replace "々" "ono" $
                                  T.replace "£"  "pound" $
                                  T.replace "~" "" $
                                  T.replace "]" "_" $
                                  T.replace "[" "_" $
                                  T.replace "/" "_" $
                                  head $ T.split (==';') cname)]

-- function: f
-- a function which converts DTS preterm with variable name to Prolog input formula
f :: D.Preterm -> T.Text
f preterm = case preterm of
  D.Var v -> T.toText v
  D.Con c -> case c of
             "entity" -> "entity"
             "event"  -> "event"
             "state"  -> "state"
             cname    -> cname_f $ cname
  D.Type  -> "type"
  D.Kind  -> "kind"
  D.Pi vname a b    -> T.concat ["forall(", T.toText vname, ",", (f $ a), ",", (f $ b), ")"]
  D.Sigma vname a b -> T.concat ["exists(", T.toText vname, ",", (f $ a), ",", (f $ b), ")"]
  D.Not a           -> T.concat ["not(", (f $ a), ")"]
  D.Lam vname b     -> T.concat ["lam(", T.toText vname, ",", (f $ b), ")"]
  D.Asp i a         -> T.concat ["@(", (T.pack (show i)), ",", (f $ a), ")"]
  D.Pair t u        -> T.concat ["[", (f $ t), ",", (f $ u), "]"]
  D.Proj D.Fst t    -> T.concat ["pi1(", (f $ t), ")"]
  D.Proj D.Snd t    -> T.concat ["pi2(", (f $ t), ")"]
  D.Eq _ m n        -> T.concat ["eq(", (f $ m), ",", (f $ n), ")"]
  D.Top -> "true"
  D.Bot -> "false"
  D.App (D.App (D.Con c) x1) x2 -> if (T.isInfixOf "DRel" c) then "true"
                                   else T.concat [(f $ (D.Con c)), "(", (f $ x1), ",", (f $ x2), ")"]
  D.App (D.App (D.App (D.App g x1) x2) x3) x4
                        -> T.concat [(f $ g), "(", (f $ x1), ",", (f $ x2), ",", (f $ x3), ",", (f $ x4), ")"]
  D.App (D.App (D.App g x1) x2) x3
                        -> T.concat [(f $ g), "(", (f $ x1), ",", (f $ x2), ",", (f $ x3), ")"]
  D.App (D.App g x1) x2 -> T.concat [(f $ g), "(", (f $ x1), ",", (f $ x2), ")"]
  D.App g x1            -> T.concat [(f $ g), "(", (f $ x1), ")"]
  D.Lamvec _ m -> f $ m
  D.Appvec _ m -> f $ m
  D.Unit         -> "unit"
  D.Nat          -> "nat"
  D.Zero         -> "zero"
  D.Succ n       -> T.concat ["succ(", (f $ n), ")"]
  D.Natrec n e f' -> T.concat ["natrec(", (f $ n), ",", (f $ e), ",", (f $ f'), ")"]
  D.Refl a m     -> T.concat ["refl(", (f $ a), ",", (f $ m), ")"]
  D.Idpeel m n   -> T.concat ["idpeel(", (f $ m), ",", (f $ n), ")"]
  D.DRel _ _ _ _ -> "true"


-- function convcoq
convcoq :: D.Preterm -> T.Text
convcoq preterm = case preterm of
  D.Var v -> T.toText v
  D.Con c -> case c of
             "entity" -> "Entity"
             "event"  -> "Event"
             "state"  -> "State"
             cname        -> "_" `T.append` (cname_f cname)
  D.Type  -> "Prop"
  D.Kind  -> "Kind"
  D.Pi vname a b    -> T.concat ["forall ", T.toText vname, ":", (convcoq $ a), ", ", (convcoq $ b)]
  D.Sigma vname a b -> T.concat ["exists ", T.toText vname, ":", (convcoq $ a), ", ", (convcoq $ b)]
  D.Not a           -> T.concat ["not ", (convcoq $ a)]
  D.Lam vname b     -> T.concat ["lam(", T.toText vname, ",", (convcoq $ b), ")"]
  D.Asp i a         -> T.concat ["asp ", (T.pack (show i)), " ", (convcoq $ a)]
  D.Pair t u        -> T.concat ["(", (convcoq $ t), ",", (convcoq $ u), ")"]
  D.Proj D.Fst t    -> T.concat ["projT1", (convcoq $ t)]
  D.Proj D.Snd t    -> T.concat ["projT2", (convcoq $ t)]
  D.Eq _ m n        -> T.concat ["eq ", (convcoq $ m), " ", (convcoq $ n)]
  D.Top -> "True"
  D.Bot -> "False"
  D.App (D.App (D.App (D.App g x1) x2) x3) x4
                        -> T.concat ["_", (convcoq $ g), " ", (convcoq $ x1), " ", (convcoq $ x2), " ", (convcoq $ x3), " ", (convcoq $ x4)]
  D.App (D.App (D.App g x1) x2) x3
                        -> T.concat ["_", (convcoq $ g), " ", (convcoq $ x1), " ", (convcoq $ x2), " ", (convcoq $ x3)]
  D.App (D.App g x1) x2 -> T.concat ["_", (convcoq $ g), " ", (convcoq $ x1), " ", (convcoq $ x2)]
  D.App g x1            -> T.concat ["_", (convcoq $ g), " ", (convcoq $ x1)]
  D.Lamvec _ m -> convcoq $ m
  D.Appvec _ m -> convcoq $ m
  D.Unit         -> "Unit"
  D.Nat          -> "Nat"
  D.Zero         -> "Zero"
  D.Succ n       -> T.concat ["Succ ", (convcoq $ n)]
  D.Natrec n e f' -> T.concat ["Natec ", (convcoq $ n), " ", (convcoq $ e), " ", (convcoq $ f')]
  D.Refl a m     -> T.concat ["Refl ", (convcoq $ a), " ", (convcoq $ m)]
  D.Idpeel m n   -> T.concat ["Idpeel ", (convcoq $ m), " ", (convcoq $ n)]
  D.DRel _ _ _ _ -> "True"

makeCoqSigList :: [DTS.Signature] -> T.Text
makeCoqSigList siglist = T.concat (L.nub (map (\ (text, preterm) -> T.concat ["Parameter _", (cname_f text), " : ", (convcoq $ DTS.initializeIndex $ DTS.fromDeBruijn $ preterm), ". \n"]) siglist))

pairsList2listsPair :: [(a, b)] -> ([a], [b])
pairsList2listsPair [] = ([], [])
pairsList2listsPair (p:plist) = ((fst $ p):(fst $ resultPair), (snd $ p):(snd $ resultPair))
  where resultPair = pairsList2listsPair $ plist

conv2CoqTheorem :: D.Preterm -> IO T.Text
conv2CoqTheorem formula = do
  lightbluepath <- M.liftM T.pack $ E.getEnv "LIGHTBLUE"
  T.putStrLn "-- Preterm ---------"
  T.putStrLn $ T.toText $ formula
  let proformula = f $ formula ;
  T.putStrLn "-- Prolog input ---------"
  T.putStrLn $ proformula
-- Call Prolog (@-elimination)
  let command1 = T.concat ["swipl -s ", lightbluepath, "/Prolog/presupposition.pl -g main -t halt --quiet -- \"", proformula, "\""]
  (_, stdout1, _, procHandle1) <- S.runInteractiveCommand $ T.unpack command1
  _ <- S.waitForProcess procHandle1
  t1 <- T.hGetContents stdout1
  T.putStrLn $ "-- After resolving @ --------"
  T.putStrLn $ t1
-- Call Prolog (Sigma-elimination)
  let command2 = T.concat ["swipl -s ", lightbluepath, "/Prolog/elimSigma.pl -g main -t halt --quiet -- \"", t1, "\""]
  (_, stdout2, _, procHandle2) <- S.runInteractiveCommand $ T.unpack command2
  _ <- S.waitForProcess procHandle2
  t2 <- T.hGetContents stdout2
  T.putStrLn $ "-- After elimSigma --------"
  T.putStrLn $ t2
-- Call Prolog (Prolog to Coq)
  let command3 = T.concat ["swipl -s ", lightbluepath, "/Prolog/prolog2coq.pl -g main -t halt --quiet -- \"", t2, "\" ; cat interpretation.txt"]
  (_, stdout3, _, procHandle3) <- S.runInteractiveCommand $ T.unpack command3
  _ <- S.waitForProcess procHandle3
  -- file "interpretation.txt" is created in the current directory
  t3 <- T.hGetContents stdout3
  T.putStrLn $ "-- Coq formula --------"
  T.putStrLn $ t3
  return t3


proveEntailment :: T.Text -> D.Preterm -> T.Text -> IO Bool
proveEntailment flag formula coqsig = do
  if flag == "e" then T.putStrLn "*** Result: Entailment"
                 else T.putStrLn "*** Result: Contradiction"
  lightbluepath <- M.liftM T.pack $ E.getEnv "LIGHTBLUE"
  t3 <- conv2CoqTheorem formula
  let coqcode = T.concat ["Add LoadPath \\\"", lightbluepath, "\\\".\nRequire Export coqlib.\n",
                          coqsig,
                          "Theorem trm : ", t3, ".\n",
                          "Proof. firstorder. Qed. Print trm.\n"]
  T.putStrLn "-- Coq code --------"
  T.putStrLn coqcode
  let command4 = T.concat ["echo \"", coqcode, "\" | coqtop"]
--  T.putStrLn "-- Coq command --------"
--  T.putStrLn command4
  (_, stdout4, _, procHandle4) <- S.runInteractiveCommand $ T.unpack command4
  _ <- S.waitForProcess procHandle4
  t4 <- T.hGetContents stdout4
--  T.putStrLn "-- Result --------"
  --T.putStrLn t4
  --if (T.isInfixOf "trm is defined" t4) then return True else return False
  return (T.isInfixOf "trm is defined" t4)


currying :: [DTS.Preterm] -> DTS.Preterm -> DTS.Preterm
currying [] preterm = preterm
currying (p:ps) preterm = DTS.Pi p (currying ps preterm)

neg_currying :: [DTS.Preterm] -> DTS.Preterm -> DTS.Preterm
neg_currying [] preterm = DTS.Not preterm
neg_currying (p:ps) preterm = DTS.Pi p (neg_currying ps preterm)


main :: IO()
main = do
  sentences <- T.getContents
  parse_start <- Time.getCurrentTime
  pairsList <- mapM (\t -> do
                           nodes <- CP.simpleParse 24 t
                           let representative = head nodes
                           return ((CP.sem $ representative), (CP.sig $ representative))) (T.lines sentences)
  parse_stop <- Time.getCurrentTime
  let listsPair = pairsList2listsPair $ pairsList
      srlist = fst $ listsPair
      siglists = snd $ listsPair
      formula = DTS.initializeIndex (DTS.fromDeBruijn (DTS.betaReduce $ currying (L.init $ srlist) (L.last $ srlist)))
      neg_formula = DTS.initializeIndex (DTS.fromDeBruijn (DTS.betaReduce $ neg_currying (L.init $ srlist) (L.last $ srlist)))
      coqsig = makeCoqSigList (L.concat siglists)
  proof_start <- Time.getCurrentTime
  (proveEntailment "e" formula coqsig) >>=
   (\entails -> if entails then T.putStrLn "-- Answer --------\nyes"
                else (proveEntailment "c" neg_formula coqsig) >>=
                     (\contradicts -> if contradicts then T.putStrLn "-- Answer --------\nno"
                                      else T.putStrLn "-- Answer --------\nunknown"))
  proof_stop <- Time.getCurrentTime
  let parse_time = Time.diffUTCTime parse_stop parse_start
      proof_time = Time.diffUTCTime proof_stop proof_start
  T.putStrLn "-- Time --------"
  T.putStrLn $ T.concat ["Parsing Time: ", T.pack (show parse_time)]
  T.putStrLn $ T.concat ["Proving Time: ", T.pack (show proof_time)]
