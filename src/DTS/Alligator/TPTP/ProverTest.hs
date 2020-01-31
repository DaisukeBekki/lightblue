{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module DTS.Alligator.TPTP.ProverTest (

  ) where

import qualified DTS.Alligator.TPTP.Parser as P
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as Te
import qualified DTS.DTT as DT

t2dt :: P.Tformula -> DT.Preterm
t2dt (P.Tletter con) = DT.Con $ Te.singleton con
t2dt P.Ttrue = DT.Top
t2dt P.Tfalse = DT.Bot
t2dt (P.Tneg formula) = DT.Not (t2dt formula)
t2dt (P.Tbinary (P.Tand) f1 f2) = DT.Sigma (t2dt f1) (t2dt f2)
t2dt (P.Tbinary (P.Tor) f1 f2 )= DT.Not $ DT.Sigma (DT.Not $ t2dt f1) (DT.Not $ t2dt f2)
t2dt (P.Tbinary (P.Timp) f1 f2) = DT.Pi (t2dt f1) (t2dt f2)
t2dt (P.Tbinary (P.Tequiv) f1 f2) = DT.Sigma (DT.Pi (t2dt f1) (t2dt f2)) (DT.Pi (t2dt f2) (t2dt f1))
t2dt (P.Tall [P.Tvar con] f )= DT.Pi (DT.Con $ Te.singleton con) (t2dt f)
t2dt (P.Texist [P.Tvar con] f) = DT.Sigma (DT.Con $ Te.singleton con) (t2dt f)

t2dtstr :: P.Tformula -> String
t2dtstr (P.Tletter con) = "DT.Con $ T.pack \""++[con] ++"\""
t2dtstr P.Ttrue = "DT.Top"
t2dtstr P.Tfalse = "DT.Bot"
t2dtstr (P.Tneg formula) = "DT.Not ("++t2dtstr formula++")"
t2dtstr (P.Tbinary (P.Tand) f1 f2) = "DT.Sigma ("++t2dtstr f1++") ("++t2dtstr f2++")"
t2dtstr (P.Tbinary (P.Tor) f1 f2 )= "DT.Not $ DT.Sigma (DT.Not $"++ t2dtstr f1++") (DT.Not $"++ t2dtstr f2++")"
t2dtstr (P.Tbinary (P.Timp) f1 f2) = "DT.Pi (" ++ t2dtstr f1 ++ ") (" ++ t2dtstr f2 ++ ")"
t2dtstr (P.Tbinary (P.Tequiv) f1 f2) = "DT.Sigma (DT.Pi ("++t2dtstr f1++") (" ++t2dtstr f2++")) (DT.Pi (" ++t2dtstr f2 ++") (" ++t2dtstr f1++"))"
t2dtstr (P.Tall [P.Tvar con] f )= "DT.Pi (DT.Con $ T.pack \"" ++ [con]++ "\") (" ++t2dtstr f++")"
t2dtstr (P.Texist [P.Tvar con] f) = "DT.Sigma (DT.Con $ T.pack \"" ++ [con] ++"\" ) (" ++t2dtstr f++")"


dtmain :: String -> IO DT.Preterm
dtmain filename= do
  text <- T.readFile $"DTS/Alligator/TPTP/"++filename
  f' <- P.parseFormula $ P.cleanse text
  return $t2dt f'
