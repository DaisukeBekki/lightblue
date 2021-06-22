module DTS.Alligator.TestTree
(
  dni
) where
import DTS.Alligator.ProofTree
import qualified DTS.DTT as DT
import qualified DTS.UDTT as UD
import qualified Data.Text.Lazy as T
import qualified DTS.Prover_daido.Judgement as J
import qualified System.Timeout as ST
import qualified DTS.Alligator.Arrowterm as A

import qualified Interface.HTML as HTML
import qualified Data.Text.Lazy.IO as T


p :: DT.Preterm
p = DT.Con $ T.pack "p"

q :: DT.Preterm
q = DT.Con $ T.pack "q"

r :: DT.Preterm
r = DT.Con $ T.pack "r"

dn_pr :: DT.Preterm
dn_pr = DT.Pi (DT.Type) (DT.Pi (DT.Pi (DT.Pi (DT.Var 0) DT.Bot) DT.Bot) (DT.Var 1))

classic = []

transitive_7_18 :: [J.Tree J.Judgement]
transitive_7_18 =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 2) (DT.Var 2)) (DT.Pi (DT.Pi (DT.Var 2) (DT.Var 2)) (DT.Pi (DT.Var 4) (DT.Var 3)))
  in prove var_env sig_env pre_type settingDef

-- transitive_7_18 :: [J.Tree J.Judgement]
transitive_7_18j =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 2) (DT.Var 2)) (DT.Pi (DT.Pi (DT.Var 2) (DT.Var 2)) (DT.Pi (DT.Var 4) (DT.Var 3)))
  in map A.jTreeToaTree $prove var_env sig_env pre_type settingDef

transitive :: [J.Tree J.Judgement]
transitive =
  let
    sig_env = classic
    var_env = [(DT.Var 4),DT.Pi (DT.Var 3) (DT.Var 3),DT.Pi (DT.Var 1) (DT.Var 1),DT.Type,DT.Type,DT.Type]
    pre_type =  DT.Var 3
  in prove var_env sig_env pre_type settingDef

transitive_2 ::[J.Tree J.Judgement]
transitive_2 =
  let
    sig_env = classic
    var_env = [(DT.Pi (DT.Var 2) (DT.Var 2)),(DT.Pi (DT.Var 2) (DT.Var 2)),DT.Type,DT.Type,DT.Type]
    pre_type =  DT.Pi (DT.Var 4) (DT.Var 3)
  in prove var_env sig_env pre_type settingDef

dni :: [J.Tree J.Judgement]
dni =
  let sig_env = classic
      var_env = [DT.Type]
      pre_type =  DT.Pi (DT.Var 0) (DT.Not (DT.Not (DT.Var 1)))
  in prove var_env sig_env pre_type settingDef

dni_2 :: [J.Tree J.Judgement]
dni_2 =
  let sig_env = classic
      var_env = [r,q,p]
      pre_type =  DT.Pi p (DT.Pi (DT.Pi p DT.Bot) DT.Bot)
  in prove var_env sig_env pre_type settingDef

dni_3 :: [J.Tree J.Judgement]
dni_3 =
  let sig_env = classic
      var_env = [r,q,p]
      pre_type = DT.Pi (DT.Pi p DT.Bot) DT.Bot
  in prove var_env sig_env pre_type settingDef

dni_4 :: [J.Tree J.Judgement]
dni_4 =
  let sig_env = classic
      var_env = []
      pre_type = DT.Pi (DT.Type) (DT.Pi (DT.Not $ DT.Not $ DT.Var 0) (DT.Var 1))
  in prove var_env sig_env pre_type settingDef

dni_5 :: [J.Tree J.Judgement]
dni_5 =
  let sig_env = classic
      var_env = [DT.Not $ DT.Not $ DT.Var 0,DT.Type]
      pre_type = DT.Type
  in prove var_env sig_env pre_type settingDef

efq :: [J.Tree J.Judgement]
efq =
  let sig_env = classic
      var_env = [DT.Type, DT.Bot]
      pre_type = (DT.Var 0)
  in prove var_env sig_env pre_type settingDNE

dne :: [J.Tree J.Judgement]
dne =
  let
    sig_env = classic
    var_env = [DT.Var 3,DT.Pi (DT.Var 3) (DT.Var 2),DT.Pi (DT.Var 1) (DT.Pi (DT.Pi (DT.Var 3) (DT.Bot)) (DT.Bot)),DT.Type,DT.Type,DT.Type]
    pre_type = DT.Var 3
  in prove var_env sig_env pre_type settingDNE
lem :: [J.Tree J.Judgement]
lem =
  let
    sig_env = classic
    var_env = [DT.Type]
    pre_type = DT.Not (DT.Sigma (DT.Not $ DT.Var 0) (DT.Not (DT.Not (DT.Var 1))))
  in prove var_env sig_env pre_type settingDNE

tnd :: [J.Tree J.Judgement]
tnd =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 1) (DT.Var 1)) (DT.Pi (DT.Pi (DT.Not (DT.Var 2)) (DT.Var 2)) (DT.Var 2))
  in prove var_env sig_env pre_type settingDNE

pars :: [J.Tree J.Judgement] -- depth 9以上
pars =
  let
    sig_env = []
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Pi (DT.Var 1) (DT.Var 1)) (DT.Var 2)) (DT.Var 2)
  in prove var_env sig_env pre_type settingDNE

gem :: [J.Tree J.Judgement]
gem =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Not (DT.Sigma (DT.Not (DT.Pi (DT.Var 1) (DT.Var 1))) (DT.Not (DT.Var 2)))
  in prove var_env sig_env pre_type settingDNE

--prove([e:set,f:e->prop,g:e->prop,a:sigma(A0:e,sigma(A1:f-A0,g-A0))],P:sigma(X:sigma(Y:e,f-Y),g-pi1(X))).
--prove([e:prop],P:[P1:sigma(P2:[P3:e]=>false,[P4:[P5:[P6:e]=>false]=>false]=>false)]=>false).


pred8 :: [J.Tree J.Judgement]
pred8 =
  let
    sig_env = classic
    var_env = [DT.Pi (DT.Var 1) (DT.Type),DT.Pi (DT.Var 0) (DT.Type),DT.Type]
    pre_type = DT.Pi (DT.Sigma (DT.Var 2) (DT.Sigma (DT.App (DT.Var 2) (DT.Var 0)) (DT.App (DT.Var 2) (DT.Var 1)))) (DT.Sigma (DT.Sigma (DT.Var 3) (DT.App (DT.Var 3) (DT.Var 0))) (DT.App (DT.Var 2) (DT.Proj DT.Fst (DT.Var 0))))
  in prove var_env sig_env pre_type settingDef

dne_2 :: [J.Tree J.Judgement]
dne_2 =
  let
    sig_env = classic
    var_env = [DT.Type,r,q]
    pre_type = DT.Pi (DT.Pi (DT.Pi (DT.Var 0) DT.Bot) DT.Bot) (DT.Var 1)
  in prove var_env sig_env pre_type settingDNE

dne' :: [J.Tree J.Judgement]
dne' =
  let
    sig_env = classic
    var_env = [DT.Type]
    pre_type = DT.Pi (DT.Not (DT.Not(DT.Not (DT.Var 0)))) (DT.Not (DT.Var 1))
  in prove var_env sig_env pre_type settingDef

cm :: [J.Tree J.Judgement]
cm =
  let
    sig_env = classic
    var_env = [DT.Type]
    pre_type =  DT.Pi (DT.Pi (DT.Var 0) (DT.Pi (DT.Var 1) DT.Bot)) (DT.Pi (DT.Var 1) DT.Bot)
  in prove var_env sig_env pre_type settingDef

cm' :: [J.Tree J.Judgement]
cm' =
  let
    sig_env = classic
    var_env = [DT.Type]
    pre_type =  DT.Pi (DT.Pi (DT.Pi (DT.Var 0) DT.Bot) (DT.Var 1)) (DT.Var 1)
  in prove var_env sig_env pre_type settingDNE

raa :: [J.Tree J.Judgement]
raa =
  let
    sig_env = classic
    var_env = [DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 0) DT.Bot) (DT.Not (DT.Var 1))
  in     prove var_env sig_env pre_type settingDef

raa' :: [J.Tree J.Judgement]
raa' =
  let
    sig_env = classic
    var_env = [DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Not (DT.Var 0)) DT.Bot) (DT.Var 1)
  in   prove var_env sig_env pre_type settingDNE

con1 :: [J.Tree J.Judgement]
con1 =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 1) (DT.Var 1)) (DT.Pi (DT.Not (DT.Var 1)) (DT.Not (DT.Var 3)))
  in   prove var_env sig_env pre_type settingDef

con2 :: [J.Tree J.Judgement]
con2 =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 1) (DT.Not (DT.Var 1))) (DT.Pi (DT.Var 1) (DT.Not (DT.Var 3)))
  in   prove var_env sig_env pre_type settingDef

con3 :: [J.Tree J.Judgement]
con3 =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Not (DT.Var 1))  (DT.Var 1)) (DT.Pi (DT.Not (DT.Var 1)) (DT.Var 3))
  in    prove var_env sig_env pre_type settingDNE

con4 :: [J.Tree J.Judgement]
con4 =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Not (DT.Var 1))  (DT.Not (DT.Var 1))) (DT.Pi (DT.Var 1) (DT.Var 3))
  in   prove var_env sig_env pre_type settingDNE

false :: [J.Tree J.Judgement]
false =
  let
    sig_env = []
    var_env = [DT.Type]
    pre_type =  DT.Bot
  in    prove var_env sig_env pre_type settingDef

formedness :: [J.Tree J.Judgement]
formedness =
  let
    sig_env = classic
    var_env = [DT.Pi p q,DT.Pi q r]
    pre_type = DT.Pi p r
  in   prove var_env sig_env pre_type settingDef

membership_test :: [J.Tree J.Judgement]
membership_test =
  let
    var_env = [DT.Sigma (DT.Var 0)  (DT.Var 2),DT.Con $ T.pack "p",DT.Con $ T.pack "q"]
    sig_env = []
    pre_type = DT.Con $ T.pack "p"
  in    prove var_env sig_env pre_type settingDef

pi_intro_test :: [J.Tree J.Judgement]
pi_intro_test =
  let
    sig_env = []
    var_env = [DT.Con $ T.pack "r",DT.Con $ T.pack "q",DT.Con $ T.pack "p"]
    pre_type = DT.Pi (DT.Con $ T.pack "p") (DT.Con $ T.pack "p")
  in   prove var_env sig_env pre_type settingDef

pi_elim_test :: [J.Tree J.Judgement]
pi_elim_test =
  let
    sig_env = classic
    var_env = [r,q,p,DT.Pi p DT.Bot]
    pre_type = DT.Bot
  in   prove var_env sig_env pre_type settingDef

p_not_p :: [J.Tree J.Judgement]
p_not_p =
  let
    sig_env = classic
    var_env = [r,q,p]
    pre_type =  (DT.Pi p DT.Bot)
  in    prove var_env sig_env pre_type settingDef

--ok [p,q,r,¬¬pー＞¬r]->⊥
ex1 :: [J.Tree J.Judgement]
ex1 =
  let
    sig_env = classic
    var_env = [DT.Pi (DT.Pi (DT.Pi p DT.Bot) DT.Bot) (DT.Pi r DT.Bot),r,q,p]
    pre_type = DT.Bot
  in   prove var_env sig_env pre_type settingDef

importation :: [J.Tree J.Judgement]
importation =
  let
    sig_env = classic
    var_env = [r,q,p]
    pre_type = DT.Pi (DT.Pi (DT.Sigma p q) r) (DT.Pi p (DT.Pi q r))
  in   prove var_env sig_env pre_type settingDef

mp :: [J.Tree J.Judgement]
mp =
  let
    sig_env = classic
    var_env = [DT.Pi (DT.Var 2) (DT.Var 2), (DT.Var 1),DT.Type,DT.Type]
    pre_type = DT.Var 2
  in   prove var_env sig_env pre_type settingDef

s :: [J.Tree J.Judgement]
s =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 2) (DT.Pi (DT.Var 2) (DT.Var 2))) (DT.Pi (DT.Pi (DT.Var 3) (DT.Var 3)) (DT.Pi (DT.Var 4) (DT.Var 3)))
  in   prove var_env sig_env pre_type settingDef

k :: [J.Tree J.Judgement]
k =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type =DT.Pi (DT.Var 1) (DT.Pi (DT.Var 1) (DT.Var 3))
  in   prove var_env sig_env pre_type settingDef

i :: [J.Tree J.Judgement]
i =
  let
    sig_env = classic
    var_env = [DT.Type]
    pre_type = DT.Pi (DT.Var 0) (DT.Var 1)
  in   prove var_env sig_env pre_type settingDef

b :: [J.Tree J.Judgement]
b =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 1) (DT.Var 1)) (DT.Pi (DT.Pi (DT.Var 3) (DT.Var 3)) (DT.Pi (DT.Var 4) (DT.Var 3)))
  in   prove var_env sig_env pre_type settingDef

c :: [J.Tree J.Judgement]
c =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 2) (DT.Pi (DT.Var 2) (DT.Var 2))) (DT.Pi (DT.Var 2) (DT.Pi (DT.Var 4) (DT.Var 3)))
  in   prove var_env sig_env pre_type settingDef

w :: [J.Tree J.Judgement]
w =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 1) (DT.Pi (DT.Var 2) (DT.Var 2))) (DT.Pi (DT.Var 2) (DT.Var 2))
  in   prove var_env sig_env pre_type settingDef

b' :: [J.Tree J.Judgement]
b' =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type,DT.Type]
    pre_type =DT.Pi (DT.Pi (DT.Var 2) (DT.Var 2)) (DT.Pi (DT.Pi (DT.Var 2) (DT.Var 2)) (DT.Pi (DT.Var 4) (DT.Var 3)))
  in   prove var_env sig_env pre_type settingDef

c_star :: [J.Tree J.Judgement]
c_star =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Var 1) (DT.Pi (DT.Pi (DT.Var 2) (DT.Var 2)) (DT.Var 2))
  in   prove var_env sig_env pre_type settingDef

ex2 :: [J.Tree J.Judgement] --Alligatorでできなかった
ex2 =
  let
    sig_env = classic
    var_env = [DT.Sigma (DT.Var 2) (DT.Sigma (DT.App (DT.Var 2) (DT.Var 0)) (DT.App (DT.Var 2) (DT.Var 1))),DT.Pi (DT.Var 1) DT.Type,DT.Pi (DT.Var 0) (DT.Type),DT.Type]
    pre_type = DT.Sigma (DT.Sigma (DT.Var 3) (DT.App (DT.Var 3) (DT.Var 0))) (DT.App (DT.Var 2) (DT.Proj DT.Fst (DT.Var 0)))
  in   prove var_env sig_env pre_type settingDef

-- ex3 :: [J.Tree J.Judgement] --Alligatorでできなかった
ex3 =
  let
    sig_env = classic
    var_env = [DT.Sigma (DT.Var 2) (DT.App (DT.Var 1) (DT.Var 0)),DT.Pi (DT.Var 1) (DT.Var 1),DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Var 3) (DT.Var 3)
  in   prove var_env sig_env pre_type settingDef

all1 :: [J.Tree J.Judgement]
all1 =
  let
    sig_env = classic
    var_env = [DT.Pi (DT.Type) (DT.App (DT.Var 1) (DT.Var 0)),DT.Pi (DT.Type) (DT.Type),DT.Type]
    pre_type = DT.App (DT.Var 1) (DT.Var 2)
  in   prove var_env sig_env pre_type settingDef

-- all2 :: [J.Tree J.Judgement]
all2 =
  let
    sig_env = classic
    var_env = [DT.Pi (DT.Type) (DT.App (DT.Var 1) (DT.Var 3)),DT.Pi (DT.Type) (DT.Var 1),DT.Var 0,DT.Type,DT.Type]
    pre_type = DT.App (DT.Var 1) (DT.Var 3)
  in   prove var_env sig_env pre_type settingDef

all3 :: [J.Tree J.Judgement]
all3 =
  let
    sig_env = classic
    var_env = [DT.Pi (DT.Type) (DT.App (DT.Var 1) (DT.Var 0)),DT.Pi (DT.Type) (DT.Var 1),DT.Var 0,DT.Type]
    pre_type = DT.App (DT.Var 1) (DT.Var 3)
  in   prove var_env sig_env pre_type settingDef

all4 :: [J.Tree J.Judgement]
all4 =
  let
    sig_env = classic
    var_env = [DT.Var 0,DT.Type]
    pre_type = DT.Pi (DT.Type) (DT.Pi (DT.Var 0) (DT.Var 1))
  in   prove var_env sig_env pre_type settingDef
--
-- all4 :: [J.Tree J.Judgement]
-- all4 =
--   let
--     sig_env = classic
--     var_env = [DT.Var 0,DT.Type]
--     pre_type = DT.Pi (DT.Var 1) (DT.Var 1)
--   in   prove var_env sig_env pre_type settingDef

shiftIndices :: DT.Preterm -> Int -> Int-> DT.Preterm
shiftIndices term d i = DT.toDTT $ UD.shiftIndices  (DT.toUDTT term) d i

entity :: DT.Preterm
entity = DT.Type

girl :: DT.Preterm -> DT.Preterm
girl girl_en= DT.Pi girl_en DT.Type

thesis :: DT.Preterm -> DT.Preterm
thesis thesis_en = DT.Pi thesis_en DT.Type

write :: DT.Preterm ->  DT.Preterm
write write_en = DT.Pi (write_en) (DT.Pi (shiftIndices write_en 1 0) (DT.Type))

there_is_a_girl :: DT.Preterm -> DT.Preterm -> DT.Preterm
there_is_a_girl en gi =
  DT.Sigma (en) (DT.App (shiftIndices gi 1 0) (DT.Var 0))

a_girl_writes_a_thesis :: DT.Preterm ->  DT.Preterm -> DT.Preterm ->  DT.Preterm ->  DT.Preterm
a_girl_writes_a_thesis en gi th wr =
  DT.Sigma
    (DT.Sigma
      (there_is_a_girl en gi)
      (DT.Sigma (shiftIndices en 1 0) (DT.App (shiftIndices th 2 0) (DT.Var 0)) ))
    (DT.App
      (DT.App (shiftIndices wr 1 0) (DT.Proj DT.Fst $ DT.Proj DT.Fst $ DT.Var 0))
      (DT.Proj DT.Fst $ DT.Proj DT.Snd $ DT.Var 0)
    )

is_there_a_girl :: [J.Tree J.Judgement]
is_there_a_girl =
  let sig_env = classic
      var_env = [a_girl_writes_a_thesis (DT.Var 3) (DT.Var 2) (DT.Var 1) (DT.Var 0),write (DT.Var 2),thesis (DT.Var 1),girl (DT.Var 0),entity]
      pre_type = there_is_a_girl (DT.Var 4) (DT.Var 3)
  in prove var_env sig_env pre_type settingDef

not_false :: [J.Tree J.Judgement]
not_false =
  let
    sig_env = classic
    var_env = []
    pre_type =DT.Bot
  in   prove var_env sig_env pre_type settingDef

-- is_there_a_girl :: [AJudgement]
-- is_there_a_girl =
--   let sig_env = []
--       var_env = [a_girl_writes_a_thesis (DT.Var 3) (DT.Var 2) (DT.Var 1) (DT.Var 0),write (DT.Var 2),thesis (DT.Var 1),girl (DT.Var 0),entity]
--       pre_type = there_is_a_girl (DT.Var 4) (DT.Var 3)
--   in prove var_env sig_env pre_type settingDef

{-
is_there_a_girl :: [AJudgement]
is_there_a_girl =
  let words = [write 2,thesis 1,girl 0,entity]
      sentences = [a_girl_writes_a_thesis 3 2 1 0]
      prop = there_is_a_girl 4 3
  in prove sentences words prop
-}
lnc :: [J.Tree J.Judgement]
lnc =
  let sig_env = classic
      var_env = [DT.Type]
      pre_type = DT.Not (DT.Sigma (DT.Var 0) (DT.Not (DT.Var 1)))
  in   prove var_env sig_env pre_type settingDef

pel10 :: [J.Tree J.Judgement]
pel10 =
  let sig_env = classic
      var_env = [DT.Pi (DT.Var 4) (DT.Not (DT.Sigma (DT.Not (DT.Var 4)) (DT.Not (DT.Var 4))) ),DT.Pi (DT.Var 1) (DT.Sigma (DT.Var 4) (DT.Var 4)),DT.Pi (DT.Var 1) (DT.Var 1),DT.Type,DT.Type,DT.Type]
      -- pre_type = DT.Sigma (DT.Pi (DT.Var 5) (DT.Var 5)) (DT.Pi (DT.Var 5) (DT.Var 7))
      pre_type = DT.Pi (DT.Var 5) (DT.Var 5)
  in
    -- A.AJudgement (map (A.Conclusion) var_env) (A.Conclusion DT.Bot) (A.Conclusion pre_type)
    prove var_env sig_env pre_type settingDef

-- extest :: [J.Tree J.Judgement]
-- extest =
--   let sigEnv = classic
--       varEnv = [DT.App (DT.Var 0) (DT.Var 2),DT.Pi (DT.Type) (DT.Type),DT.Type]
--       preType = DT.Sigma (DT.Type) (DT.App (DT.Var 2) (DT.Var 0))
--   in
--     prove varEnv sigEnv preType

-- extest :: [J.Tree J.Judgement]
extest =
  let sigEnv = classic
      varEnv = [DT.App (DT.Var 0) (DT.Var 1),DT.Pi (DT.Var 1) (DT.Type),DT.Var 0,DT.Type]
      preType = DT.Sigma (DT.Var 3) (DT.App (DT.Var 2) (DT.Var 0))
  in
    prove varEnv sigEnv preType settingDef

extest2 =
  let sigEnv = classic
      varEnv = [DT.App (DT.Var 0) (DT.Var 1),DT.Pi (DT.Type) (DT.Type),DT.Type]
      preType = DT.Sigma (DT.Type) (DT.App (DT.Var 2) (DT.Var 0))
  in
    prove varEnv sigEnv preType settingDef

eq1 :: [J.Tree J.Judgement]
eq1 =
  let
    sigEnv = classic
    varEnv = [DT.Type]
    preType =  DT.Eq DT.Type (DT.Var 0) (DT.Var 0)
  in prove varEnv sigEnv preType settingDef

eq2 :: [J.Tree J.Judgement]
eq2 =
  let
    sigEnv = classic
    varEnv = [DT.Type]
    preType =  DT.Eq (DT.Type) (DT.Pi (DT.Var 0) (DT.Pi (DT.Var 1) DT.Bot)) (DT.Pi (DT.Var 0) (DT.Pi (DT.Var 1) DT.Bot))
  in prove varEnv sigEnv preType settingDef

eq2' :: [J.Tree J.Judgement]
eq2' =
  let
    sigEnv = classic
    varEnv = [DT.Type]
    preType =  DT.Eq (DT.Type) (DT.Pi (DT.Var 0) (DT.Pi (DT.Var 1) DT.Bot)) (DT.Pi (DT.Var 0) DT.Bot)
  in prove varEnv sigEnv preType settingDef

eq3 :: [J.Tree J.Judgement]
eq3 =
  let
    sigEnv = classic
    varEnv = [DT.Var 2,DT.Eq DT.Type (DT.Var 0) (DT.Var 1),DT.Type,DT.Type,DT.Type]
    preType =  DT.Var 2
  in prove varEnv sigEnv preType settingDef

eq3' :: [J.Tree J.Judgement]
eq3' =
  let
    sigEnv = classic
    varEnv = [DT.Var 3,DT.Eq DT.Type (DT.Var 0) (DT.Var 1),DT.Type,DT.Type,DT.Type]
    preType =  DT.Var 2
  in prove varEnv sigEnv preType settingDef

eqPi1 :: [J.Tree J.Judgement]
eqPi1 =
  let
    sigEnv = classic
    varEnv = [DT.Pi (DT.Var 3) (DT.Var 2),DT.Eq (DT.Var 1) (DT.Var 2) (DT.Type),DT.Type,DT.Type,DT.Type]
    preType =  DT.Pi (DT.Var 3) (DT.Var 3)
  in prove varEnv sigEnv preType settingDef

eqPi1' :: [J.Tree J.Judgement]
eqPi1' =
  let
    sigEnv = classic
    varEnv = [DT.Pi (DT.Var 3) (DT.Var 2),DT.Eq DT.Type (DT.Var 1) (DT.Var 2),DT.Type,DT.Type,DT.Type]
    preType =  DT.Pi (DT.Var 2) (DT.Var 4)
  in prove varEnv sigEnv preType settingDef

eqPi2 :: [J.Tree J.Judgement]
eqPi2 =
  let
    sigEnv = classic
    varEnv = [DT.Var 3,DT.Pi (DT.Var 3) (DT.Var 2),DT.Eq DT.Type (DT.Var 1) (DT.Var 2),DT.Type,DT.Type,DT.Type]
    preType =  DT.Var 3
  in prove varEnv sigEnv preType settingDef

eqPiI :: [J.Tree J.Judgement]
eqPiI =
  let
    sigEnv = classic
    varEnv = [DT.Pi (DT.Var 3) (DT.Var 2),DT.Eq DT.Type (DT.Var 1) (DT.Var 2),DT.Type,DT.Type,DT.Type]
    preType =  DT.Pi (DT.Var 4) (DT.Var 4)
  in prove varEnv sigEnv preType settingDef

eqSigma1 :: [J.Tree J.Judgement]
eqSigma1 =
  let
    sigEnv = classic
    varEnv = [DT.Var 3,DT.Eq DT.Type (DT.Var 1) (DT.Var 2),DT.Type,DT.Type,DT.Type]
    preType =  DT.Sigma (DT.Var 4) (DT.Var 4)
  in prove varEnv sigEnv preType settingDef

eqSigma2 :: [J.Tree J.Judgement]
eqSigma2 =
  let
    sigEnv = classic
    varEnv = [DT.Sigma (DT.Var 3) (DT.Var 2),DT.Eq DT.Type (DT.Var 1) (DT.Var 2),DT.Type,DT.Type,DT.Type]
    preType =  DT.Var 3
  in prove varEnv sigEnv preType settingDef

hasProof :: [J.Tree J.Judgement]  -> Bool
hasProof = ([] /=)

sk_lst = map hasProof [i,b,c,w,b',c_star,transitive_7_18,dni,dni_2,dni_3,dne',cm,raa,con1,con2]
hm_lst = sk_lst ++ map hasProof [lnc]
not_sk_lst = map hasProof [gem,{-pars,-}tnd,lem,efq,dne_2,cm',raa',con3,con4]
tautology = map hasProof [transitive,transitive_2,importation]
contradiction = map hasProof [not_false,p_not_p]
truelst = map hasProof [membership_test,pi_intro_test,pi_elim_test,ex1]
falselst = map hasProof [false,formedness]
eqTrueLst = map hasProof [eq1,eq2,eq3,eqPi1,eqPi2,eqPiI,eqSigma1,eqSigma2]
eqFalseLst = map hasProof [eq2',eq3',eqPi1']

test =
  and sk_lst
  && and not_sk_lst
  && and tautology
  && not (or contradiction)
  && and truelst
  && not (or falselst)
  && and eqTrueLst
  && not (or eqFalseLst)


-- searchProoftest1 = searchProof [A.Conclusion $ DT.Con (T.pack "p")] (A.Conclusion $ DT.Type) 1
-- searchProoftest2 = searchProof [A.Conclusion $ DT.Con (T.pack "p")] (A.Conclusion $ DT.Con (T.pack "p")) 1
-- searchProoftest3 = searchProof
--   [A.Conclusion $ DT.Pi (DT.Con (T.pack "p")) (DT.Con (T.pack "q")),A.Conclusion $ DT.Con (T.pack "p")]
--   (A.Conclusion $ DT.Con (T.pack "q"))
--   1
-- searchProoftest4 = searchProof
--   [A.Conclusion $ DT.Pi (DT.Con (T.pack "p")) (DT.Con (T.pack "q")),A.Conclusion $ DT.Con (T.pack "p")]
--   (A.Conclusion $ DT.Pi (DT.Pi (DT.Con (T.pack "p")) (DT.Con (T.pack "q"))) (DT.Con (T.pack "p")))
--   1
-- searchProoftest5 = searchProof
--   [A.Conclusion $ DT.Var 1,A.Conclusion DT.Type,A.Conclusion DT.Type]
--   (A.Arrow [A.Conclusion$ DT.Var 1] (A.Conclusion $DT.Var 3))
--   1
-- searchProoftest6 = searchProof
--   [A.Conclusion $ DT.Var 1,A.Conclusion DT.Type,A.Conclusion DT.Type]
--   (A.Arrow [A.Conclusion$ DT.Var 2,A.Conclusion$ DT.Var 2] (A.Conclusion $DT.Var 4))
--   1
--
-- searchProoftest7 = searchProof
--   [A.Arrow [A.Conclusion $ DT.Con $T.pack "p",A.Conclusion $ DT.Con $T.pack "q"] $ A.Arrow_Sigma (A.Conclusion $ DT.Var 4) (A.Conclusion $ DT.Con $T.pack "a"),A.Conclusion $ DT.Con $T.pack "p",A.Conclusion $ DT.Con $T.pack "q",A.Conclusion DT.Type]
--   (A.Arrow [A.Conclusion $ DT.Con $T.pack "p",A.Conclusion $ DT.Con $T.pack "q"] (A.Conclusion $ DT.Var 5))
--   1
--
-- searchProoftest8 = searchProof
--   [A.Arrow [A.Conclusion $ DT.Con $T.pack "p",A.Conclusion $ DT.Con $T.pack "q"] $ A.Arrow_Sigma (A.Conclusion $ DT.Var 4) (A.Conclusion $ DT.Con $T.pack "a"),A.Conclusion $ DT.Con $T.pack "p",A.Conclusion $ DT.Con $T.pack "q",A.Conclusion DT.Type]
--   (A.Conclusion $ DT.Var 3)
--   1
--
-- searchProoftest9 = searchProof
--   [A.Conclusion (DT.Var 1),A.Conclusion (DT.Var 1),A.Conclusion DT.Type,A.Conclusion DT.Type]
--   (A.Arrow_Sigma (A.Conclusion $DT.Var 3) (A.Conclusion $DT.Var 3))
--   1
--
-- testcon =[A.Arrow [A.Arrow [A.Conclusion $ DT.Var 0] (A.Conclusion DT.Bot)] (A.Conclusion DT.Bot) ,A.Conclusion DT.Type]
-- testtype = A.Conclusion (DT.Var 1)
-- searchProoftestlatest = searchProof
--   testcon
--   testtype
--   1
