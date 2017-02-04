{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : DTStoProlog
Copyright   : (c) Ribeka Tanaka, 2016
Licence     : All right reserved
Maintainer  : Ribeka Tanaka
Stability   : beta
-}

module DTStoProlog (
  dts2prolog
  ) where

import qualified Data.Text.Lazy as T     -- text
import qualified Data.Text.Lazy.IO as T  -- text
import qualified Data.List as L          -- base
import qualified Data.Time as Time       -- time
import qualified System.Process as S     -- process
import qualified System.Environment as E -- base
import qualified Control.Monad as M      -- base
import qualified DTS.UDTT as DTS
import qualified DTS.UDTTwithName as D
import qualified Parser.ChartParser as CP
import qualified Interface.Text as T

-- function: cname_f
norm_cname :: T.Text -> T.Text
norm_cname cname = case T.uncons cname of
  Nothing -> ""
  Just (char, rest) -> T.concat [char', norm_cname rest]
    where
      char' = case char of
        'ー' -> "xmdashx"
        '）' -> "rpar"
        '・' -> "middot"
        '々' -> "ono"
        '£'  -> "pound"
        '~'  -> ""
        '＃' -> "MCN"
        ']'  -> "_"
        '['  -> "_"
        '/'  -> "_"
        c    -> T.singleton c

cname_f :: T.Text -> T.Text
cname_f cname = T.concat ["c_", norm_cname (head $ T.split (==';') cname)]

-- function: f
f :: D.Preterm -> T.Text
f preterm = case preterm of
  D.Var v -> T.toText v
  D.Con c -> case c of
             "entity" -> "entity"
             "evt" -> "evt"
--             "event"  -> "event"
--             "state"  -> "state"
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


-- function: convcoq
convcoq :: D.Preterm -> T.Text
convcoq preterm = case preterm of
  D.Var v -> T.toText v
  D.Con c -> case c of
             "entity" -> "Entity"
             "evt" -> "Evt"
--             "event"  -> "Event"
--             "state"  -> "State"
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
makeCoqSigList siglist = T.concat (L.nub (map (\ (text, preterm) -> T.concat ["Parameter _", (cname_f text), " : ", (convcoq $ DTS.initializeIndex $ DTS.fromDeBruijn [] $ preterm), ". \n"]) siglist))

runMyCommand :: T.Text -> IO T.Text
runMyCommand command = do
  (_, stdout, _, procHandle) <- S.runInteractiveCommand $ T.unpack command
  _ <- S.waitForProcess procHandle
  T.hGetContents stdout

proveEntailment :: T.Text -> D.Preterm -> T.Text -> IO (Bool, [(T.Text, Time.NominalDiffTime)])
proveEntailment flag formula coqsig = do
  if flag == "e" then T.putStrLn "*** Result: Entailment"
                 else T.putStrLn "*** Result: Contradiction"
  lightbluepath <- M.liftM T.pack $ E.getEnv "LIGHTBLUE"
  conv_start <- Time.getCurrentTime
  T.putStrLn "-- Preterm ---------"
  T.putStrLn $ T.toText formula
  T.putStrLn "-- Prolog input ---------"
  let proformula = f formula
  T.putStrLn $ proformula
  conv_stop <- Time.getCurrentTime
  elimasp_start <- Time.getCurrentTime
  T.putStrLn $ "-- After resolving @ --------"
  result1 <- runMyCommand(T.concat ["swipl -s ", lightbluepath, "/Prolog/presupposition.pl -g main -t halt --quiet -- \"", proformula, "\" []"])
  T.putStrLn $ result1
  elimasp_stop <- Time.getCurrentTime
  elimsigma_start <- Time.getCurrentTime
  T.putStrLn $ "-- After elimSigma --------"
  result2 <- runMyCommand(T.concat ["swipl -s ", lightbluepath, "/Prolog/elimSigma.pl -g main -t halt --quiet -- \"", result1, "\""])
  T.putStrLn $ result2
  elimsigma_stop <- Time.getCurrentTime
  coq_start <- Time.getCurrentTime
  T.putStrLn $ "-- Coq formula --------"
  theorem <- runMyCommand(T.concat ["swipl -s ", lightbluepath, "/Prolog/prolog2coq.pl -g main -t halt --quiet -- \"", result2, "\" ; cat interpretation.txt"])
  T.putStrLn $ theorem
  let coqcode = T.concat [--"Add LoadPath \\\"", lightbluepath,
                          --"\\\".\nRequire Export coqlib.\n",
                          "Parameters Entity Evt : Type.\n",
                          coqsig,
                          "Theorem trm : ", theorem, ".\n",
                          "Proof. repeat (eexists; firstorder; eauto). Qed. Print trm.\n"]
  T.putStrLn "-- Coq code --------"
  T.putStrLn coqcode
  coqresult <- runMyCommand(T.concat ["echo \"", coqcode, "\" | coqtop"])
  coq_stop <- Time.getCurrentTime
  let conv_time = Time.diffUTCTime conv_stop conv_start
      elimasp_time = Time.diffUTCTime elimasp_stop elimasp_start
      elimsigma_time = Time.diffUTCTime elimsigma_stop elimsigma_start
      coq_time = Time.diffUTCTime coq_stop coq_start
      timelist = [("Parsing Time: ", conv_time),
                  ("Presupposition-Resolution Time: ", elimasp_time),
                  ("Sigma-Elimination Time: ", elimsigma_time),
                  ("Coq Proving Time: ", coq_time)]
  return ((T.isInfixOf "trm is defined" coqresult), timelist)

printTimes :: [(T.Text, Time.NominalDiffTime)] -> T.Text
printTimes [] = ""
printTimes ((t, time):plist) = T.concat [t, T.pack (show time), "\n", printTimes plist]

addTimesList :: [(T.Text, Time.NominalDiffTime)] -> [(T.Text, Time.NominalDiffTime)] -> [(T.Text, Time.NominalDiffTime)]
addTimesList [] [] = []
addTimesList ((t1, time1):plist1) ((t2, time2):plist2) =
  if t1==t2 then (t1, time1+time2):(addTimesList plist1 plist2)
  else [("error", 0)]
addTimesList [] (_:_) = [("error", 0)]
addTimesList (_:_) [] = [("error", 0)]

currying :: [DTS.Preterm] -> DTS.Preterm -> DTS.Preterm
currying [] preterm = preterm
currying (p:ps) preterm = DTS.Pi p (currying ps preterm)

neg_currying :: [DTS.Preterm] -> DTS.Preterm -> DTS.Preterm
neg_currying [] preterm = DTS.Not preterm
neg_currying (p:ps) preterm = DTS.Pi p (neg_currying ps preterm)

dts2prolog :: IO()
dts2prolog = do
  sentences <- T.getContents
  parse_start <- Time.getCurrentTime
  parseresult <- mapM (\t -> do
                              nodes <- CP.simpleParse 24 t
                              let representative = head nodes
                              return ((CP.sem $ representative), (CP.sig $ representative))
                       ) (T.lines sentences)
  let (srlist, siglists) = unzip parseresult
      premises = L.init $ srlist
      conclusion = L.last $ srlist
      formula = DTS.initializeIndex . (DTS.fromDeBruijn []) . DTS.betaReduce $ (currying premises conclusion)
      neg_formula = DTS.initializeIndex . (DTS.fromDeBruijn []) . DTS.betaReduce $ (neg_currying premises conclusion)
      coqsig = makeCoqSigList (L.concat siglists)
  parse_stop <- Time.getCurrentTime
  let parse_time = Time.diffUTCTime parse_stop parse_start
  (proveEntailment "e" formula coqsig) >>=
   (\(e_result, e_timeslist) -> if e_result
     then let e_conv_time = snd $ head e_timeslist in
              T.putStrLn $ T.concat ["-- Answer --------\nyes\n",
                                     "-- Time --------\n",
                                     "Parsing Time: ",
                                     T.pack (show (parse_time+e_conv_time)), "\n",
                                 (printTimes $ tail e_timeslist)]
     else (proveEntailment "c" neg_formula coqsig) >>=
           (\(c_result, c_timeslist) -> if c_result
             then let e_conv_time = snd $ head e_timeslist
                      c_conv_time = snd $ head c_timeslist in
                  T.putStrLn $ T.concat ["-- Answer --------\nno\n",
                                         "-- Time --------\n",
                                         "Parsing Time: ",
                                         T.pack (show (parse_time+e_conv_time+c_conv_time)), "\n",
                                     (printTimes $ tail (addTimesList e_timeslist c_timeslist))]
             else let e_conv_time = snd $ head e_timeslist
                      c_conv_time = snd $ head c_timeslist in
                  T.putStrLn $ T.concat ["-- Answer --------\nunknown\n",
                                      "-- Time --------\n",
                                      "Parsing Time: ",
                                      T.pack (show (parse_time+e_conv_time+c_conv_time)), "\n",
                                      (printTimes $ (addTimesList e_timeslist c_timeslist))]))

