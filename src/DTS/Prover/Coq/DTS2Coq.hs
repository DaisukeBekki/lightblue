{-|
Module      : DTS.Prover.Coq.DTS2Coq
Description : 
Copyright   : 
Licence     : 
Maintainer  : Ribeka Tanaka and Mai Matsubara
Stability   : beta
-}

module DTS.Prover.Coq.DTS2Coq (
  coqProver
  ) where

import qualified System.Environment as E --base
import qualified Control.Monad as M      --base
import qualified Data.List as L          --base
import qualified Data.Text.Lazy as T     --text
import qualified Data.Text.Lazy.IO as T  --text
import qualified Data.Text as StrictT    --text
import qualified Data.Text.IO as StrictT --text
import qualified Shelly as S             --shelly
import DTS.UDTTdeBruijn as DTSd          --lightblue
import DTS.UDTTvarName as D              --lightblue
import DTS.TypeQuery as TQ               --lightblue
import Interface.Text                    --lightblue

type DTTpreterm = D.Preterm DTSd.DTT

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
f :: DTTpreterm -> T.Text
f preterm = case preterm of
  D.Var v -> toText v
  D.Con c -> case c of
             "entity" -> "entity"
             "evt" -> "evt"
--             "event"  -> "event"
--             "state"  -> "state"
             cname    -> cname_f cname
  D.Type  -> "type"
  D.Kind  -> "kind"
  D.Pi vname a b    -> T.concat ["forall(", toText vname, ",", (f $ a), ",", (f $ b), ")"]
  D.Sigma vname a b -> T.concat ["exists(", toText vname, ",", (f $ a), ",", (f $ b), ")"]
  D.Not a           -> T.concat ["not(", (f $ a), ")"]
  D.Lam vname b     -> T.concat ["lam(", toText vname, ",", (f $ b), ")"]
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
  D.Unit         -> "unit"
  D.Nat          -> "nat"
  D.Zero         -> "zero"
  D.Succ n       -> T.concat ["succ(", (f $ n), ")"]
  D.Natrec n e f' -> T.concat ["natrec(", (f $ n), ",", (f $ e), ",", (f $ f'), ")"]
  D.Refl a m     -> T.concat ["refl(", (f $ a), ",", (f $ m), ")"]
  D.Idpeel m n   -> T.concat ["idpeel(", (f $ m), ",", (f $ n), ")"]

-- function: convcoq
convcoq :: DTTpreterm -> T.Text
convcoq preterm = case preterm of
  D.Var v -> toText v
  D.Con c -> case c of
             "entity" -> "Entity"
             "evt" -> "Evt"
--             "event"  -> "Event"
--             "state"  -> "State"
             cname        -> "_" `T.append` (cname_f cname)
  D.Type  -> "Prop"
  D.Kind  -> "Kind"
  D.Pi vname a b    -> T.concat ["forall ", toText vname, ":", (convcoq $ a), ", ", (convcoq $ b)]
  D.Sigma vname a b -> T.concat ["exists ", toText vname, ":", (convcoq $ a), ", ", (convcoq $ b)]
  D.Not a           -> T.concat ["not ", (convcoq $ a)]
  D.Lam vname b     -> T.concat ["lam(", toText vname, ",", (convcoq $ b), ")"]
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
  D.Unit         -> "Unit"
  D.Nat          -> "Nat"
  D.Zero         -> "Zero"
  D.Succ n       -> T.concat ["Succ ", (convcoq $ n)]
  D.Natrec n e f' -> T.concat ["Natec ", (convcoq $ n), " ", (convcoq $ e), " ", (convcoq $ f')]
  D.Refl a m     -> T.concat ["Refl ", (convcoq $ a), " ", (convcoq $ m)]
  D.Idpeel m n   -> T.concat ["Idpeel ", (convcoq $ m), " ", (convcoq $ n)]

makeCoqSigList :: D.Signature -> T.Text
makeCoqSigList siglist = T.concat (L.nub (map (\ (text, preterm) -> T.concat ["Parameter _", (cname_f text), " : ", (convcoq preterm), ". \n"]) siglist))

coqProver :: TQ.ProofSearchSetting -> TQ.ProofSearchQuery -> IO ()
coqProver _ (ProofSearchQuery coqSig coqCtx coqTyp) = do
  lightbluepath <- M.liftM T.pack $ E.getEnv "LIGHTBLUE"
  let coqsig = T.toStrict $ makeCoqSigList $ D.fromDeBruijnSignature coqSig
      coqcode = StrictT.concat [
                  --"Add LoadPath \\\"", lightbluepath,
                  --"\\\".\nRequire Export coqlib.\n",
                  "Parameters Entity Evt : Type.\n",
                  coqsig,
                  "Theorem trm : ", mkTheorem (D.fromDeBruijnList coqCtx) (D.fromDeBruijn coqTyp), ".\n",
                  "Proof. repeat (eexists; firstorder; eauto). Qed. Print trm.\n"
                  ]
  output <- S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ StrictT.concat ["echo \"", coqcode, "\" | coqtop"]
  StrictT.putStrLn output

mkTheorem :: [(D.VarName,DTTpreterm)] -> DTTpreterm -> StrictT.Text
mkTheorem ctx typ = ""

