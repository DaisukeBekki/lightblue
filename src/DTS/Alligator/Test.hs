module DTS.Alligator.Test
(
  dni
) where
import DTS.Alligator.Prover0204
import qualified DTS.DTT as DT
import qualified DTS.UDTT as UD
import qualified Data.Text.Lazy as T
import qualified DTS.Prover.Judgement as J
import qualified System.Timeout as ST

-- tabletimeout :: [Char]
-- tabletimeout =
--   justtimeout >>= (\x -> x)



p :: DT.Preterm
p = DT.Con $ T.pack "p"

q :: DT.Preterm
q = DT.Con $ T.pack "q"

r :: DT.Preterm
r = DT.Con $ T.pack "r"

dn_pr :: DT.Preterm
dn_pr = DT.Pi (DT.Type) (DT.Pi (DT.Pi (DT.Pi (DT.Var 0) DT.Bot) DT.Bot) (DT.Var 1))

classic = [dn_pr]

transitive_7_18 :: [J.Judgement]
transitive_7_18 =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 2) (DT.Var 2)) (DT.Pi (DT.Pi (DT.Var 2) (DT.Var 2)) (DT.Pi (DT.Var 4) (DT.Var 3)))
  in prove var_env sig_env pre_type

transitive :: [J.Judgement]
transitive =
  let
    sig_env = classic
    var_env = [DT.Pi p q,DT.Pi q r,p]
    pre_type =  r
  in prove var_env sig_env pre_type

transitive_2 ::[J.Judgement]
transitive_2 =
  let
    sig_env = classic
    var_env = [(DT.Pi (DT.Var 2) (DT.Var 2)),(DT.Pi (DT.Var 2) (DT.Var 2)),DT.Type,DT.Type,DT.Type]
    pre_type =  DT.Pi (DT.Var 4) (DT.Var 3)
  in prove var_env sig_env pre_type

dni :: [J.Judgement]
dni =
  let sig_env = classic
      var_env = [DT.Type]
      pre_type =  DT.Pi (DT.Var 0) (DT.Not (DT.Not (DT.Var 1)))
  in prove var_env sig_env pre_type

dni_2 :: [J.Judgement]
dni_2 =
  let sig_env = classic
      var_env = [r,q,p]
      pre_type =  DT.Pi p (DT.Pi (DT.Pi p DT.Bot) DT.Bot)
  in prove var_env sig_env pre_type

dni_3 :: [J.Judgement]
dni_3 =
  let sig_env = classic
      var_env = [r,q,p]
      pre_type = DT.Pi (DT.Pi p DT.Bot) DT.Bot
  in prove var_env sig_env pre_type

dni_4 :: [J.Judgement]
dni_4 =
  let sig_env = classic
      var_env = []
      pre_type = DT.Pi (DT.Type) (DT.Pi (DT.Not $ DT.Not $ DT.Var 0) (DT.Var 1))
  in prove var_env sig_env pre_type

dni_5 :: [J.Judgement]
dni_5 =
  let sig_env = classic
      var_env = [DT.Not $ DT.Not $ DT.Var 0,DT.Type]
      pre_type = DT.Type
  in prove var_env sig_env pre_type

efq :: [J.Judgement]
efq =
  let sig_env = classic
      var_env = [DT.Type, DT.Bot]
      pre_type = (DT.Var 0)
  in prove var_env sig_env pre_type

dne :: [J.Judgement]
dne =
  let
    sig_env = classic
    var_env = [DT.Var 3,DT.Pi (DT.Var 3) (DT.Var 2),DT.Pi (DT.Var 1) (DT.Pi (DT.Pi (DT.Var 3) (DT.Bot)) (DT.Bot)),DT.Type,DT.Type,DT.Type]
    pre_type = DT.Var 3
  in prove var_env sig_env pre_type
lem :: [J.Judgement]
lem =
  let
    sig_env = classic
    var_env = [DT.Type]
    pre_type = DT.Not (DT.Sigma (DT.Not $ DT.Var 0) (DT.Not (DT.Not (DT.Var 1))))
  in prove var_env sig_env pre_type

tnd :: [J.Judgement]
tnd =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 1) (DT.Var 1)) (DT.Pi (DT.Pi (DT.Not (DT.Var 2)) (DT.Var 2)) (DT.Var 2))
  in prove var_env sig_env pre_type

pars :: [J.Judgement]
pars =
  let
    sig_env = []
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Pi (DT.Var 1) (DT.Var 1)) (DT.Var 2)) (DT.Var 2)
  in prove var_env sig_env pre_type

gem :: [J.Judgement]
gem =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Not (DT.Sigma (DT.Not (DT.Pi (DT.Var 1) (DT.Var 1))) (DT.Not (DT.Var 2)))
  in prove var_env sig_env pre_type

--prove([e:set,f:e->prop,g:e->prop,a:sigma(A0:e,sigma(A1:f-A0,g-A0))],P:sigma(X:sigma(Y:e,f-Y),g-pi1(X))).
--prove([e:prop],P:[P1:sigma(P2:[P3:e]=>false,[P4:[P5:[P6:e]=>false]=>false]=>false)]=>false).


pred8 :: [J.Judgement]
pred8 =
  let
    sig_env = classic
    var_env = [DT.Pi (DT.Var 1) (DT.Type),DT.Pi (DT.Var 0) (DT.Type),DT.Type]
    pre_type = DT.Pi (DT.Sigma (DT.Var 2) (DT.Sigma (DT.App (DT.Var 2) (DT.Var 0)) (DT.App (DT.Var 2) (DT.Var 1)))) (DT.Sigma (DT.Sigma (DT.Var 3) (DT.App (DT.Var 3) (DT.Var 0))) (DT.App (DT.Var 2) (DT.Proj DT.Fst (DT.Var 0))))
  in prove var_env sig_env pre_type

dne_2 :: [J.Judgement]
dne_2 =
  let
    sig_env = classic
    var_env = [DT.Type,r,q]
    pre_type = DT.Pi (DT.Pi (DT.Pi (DT.Var 0) DT.Bot) DT.Bot) (DT.Var 1)
  in prove var_env sig_env pre_type

dne' :: [J.Judgement]
dne' =
  let
    sig_env = classic
    var_env = [DT.Type]
    pre_type = DT.Pi (DT.Not (DT.Not(DT.Not (DT.Var 0)))) (DT.Not (DT.Var 1))
  in prove var_env sig_env pre_type

cm :: [J.Judgement]
cm =
  let
    sig_env = classic
    var_env = [DT.Type]
    pre_type =  DT.Pi (DT.Pi (DT.Var 0) (DT.Pi (DT.Var 1) DT.Bot)) (DT.Pi (DT.Var 1) DT.Bot)
  in prove var_env sig_env pre_type

cm' :: [J.Judgement]
cm' =
  let
    sig_env = classic
    var_env = [DT.Type]
    pre_type =  DT.Pi (DT.Pi (DT.Pi (DT.Var 0) DT.Bot) (DT.Var 1)) (DT.Var 1)
  in prove var_env sig_env pre_type

raa :: [J.Judgement]
raa =
  let
    sig_env = classic
    var_env = [DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 0) DT.Bot) (DT.Not (DT.Var 1))
  in     prove var_env sig_env pre_type

raa' :: [J.Judgement]
raa' =
  let
    sig_env = classic
    var_env = [DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Not (DT.Var 0)) DT.Bot) (DT.Var 1)
  in   prove var_env sig_env pre_type

con1 :: [J.Judgement]
con1 =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 1) (DT.Var 1)) (DT.Pi (DT.Not (DT.Var 1)) (DT.Not (DT.Var 3)))
  in   prove var_env sig_env pre_type

con2 :: [J.Judgement]
con2 =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 1) (DT.Not (DT.Var 1))) (DT.Pi (DT.Var 1) (DT.Not (DT.Var 3)))
  in   prove var_env sig_env pre_type

con3 :: [J.Judgement]
con3 =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Not (DT.Var 1))  (DT.Var 1)) (DT.Pi (DT.Not (DT.Var 1)) (DT.Var 3))
  in    prove var_env sig_env pre_type

con4 :: [J.Judgement]
con4 =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Not (DT.Var 1))  (DT.Not (DT.Var 1))) (DT.Pi (DT.Var 1) (DT.Var 3))
  in   prove var_env sig_env pre_type

false :: [J.Judgement]
false =
  let
    sig_env = classic
    var_env = [DT.Type]
    pre_type =  DT.Bot
  in    prove var_env sig_env pre_type

formedness :: [J.Judgement]
formedness =
  let
    sig_env = classic
    var_env = [DT.Pi p q,DT.Pi q r]
    pre_type = DT.Pi p r
  in   prove var_env sig_env pre_type

membership_test :: [J.Judgement]
membership_test =
  let
    var_env = [DT.Sigma (DT.Var 0)  (DT.Var 2),DT.Con $ T.pack "p",DT.Con $ T.pack "q"]
    sig_env = []
    pre_type = DT.Con $ T.pack "p"
  in    prove var_env sig_env pre_type

pi_intro_test :: [J.Judgement]
pi_intro_test =
  let
    sig_env = []
    var_env = [DT.Con $ T.pack "r",DT.Con $ T.pack "q",DT.Con $ T.pack "p"]
    pre_type = DT.Pi (DT.Con $ T.pack "p") (DT.Con $ T.pack "p")
  in   prove var_env sig_env pre_type

pi_elim_test :: [J.Judgement]
pi_elim_test =
  let
    sig_env = classic
    var_env = [r,q,p,DT.Pi p DT.Bot]
    pre_type = DT.Bot
  in   prove var_env sig_env pre_type

p_not_p :: [J.Judgement]
p_not_p =
  let
    sig_env = classic
    var_env = [r,q,p]
    pre_type =  (DT.Pi p DT.Bot)
  in    prove var_env sig_env pre_type

--ok [p,q,r,¬¬pー＞¬r]->⊥
ex1 :: [J.Judgement]
ex1 =
  let
    sig_env = classic
    var_env = [DT.Pi (DT.Pi (DT.Pi p DT.Bot) DT.Bot) (DT.Pi r DT.Bot),r,q,p]
    pre_type = DT.Bot
  in   prove var_env sig_env pre_type

importation :: [J.Judgement]
importation =
  let
    sig_env = classic
    var_env = [r,q,p]
    pre_type = DT.Pi (DT.Pi (DT.Sigma p q) r) (DT.Pi p (DT.Pi q r))
  in   prove var_env sig_env pre_type

mp :: [J.Judgement]
mp =
  let
    sig_env = classic
    var_env = [DT.Pi (DT.Var 2) (DT.Var 2), (DT.Var 1),DT.Type,DT.Type]
    pre_type = DT.Var 2
  in   prove var_env sig_env pre_type

s :: [J.Judgement]
s =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 2) (DT.Pi (DT.Var 2) (DT.Var 2))) (DT.Pi (DT.Pi (DT.Var 3) (DT.Var 3)) (DT.Pi (DT.Var 4) (DT.Var 3)))
  in   prove var_env sig_env pre_type

k :: [J.Judgement]
k =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type =DT.Pi (DT.Var 1) (DT.Pi (DT.Var 1) (DT.Var 3))
  in   prove var_env sig_env pre_type

i :: [J.Judgement]
i =
  let
    sig_env = classic
    var_env = [DT.Type]
    pre_type = DT.Pi (DT.Var 0) (DT.Var 1)
  in   prove var_env sig_env pre_type

b :: [J.Judgement]
b =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 1) (DT.Var 1)) (DT.Pi (DT.Pi (DT.Var 3) (DT.Var 3)) (DT.Pi (DT.Var 4) (DT.Var 3)))
  in   prove var_env sig_env pre_type

c :: [J.Judgement]
c =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 2) (DT.Pi (DT.Var 2) (DT.Var 2))) (DT.Pi (DT.Var 2) (DT.Pi (DT.Var 4) (DT.Var 3)))
  in   prove var_env sig_env pre_type

w :: [J.Judgement]
w =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Pi (DT.Var 1) (DT.Pi (DT.Var 2) (DT.Var 2))) (DT.Pi (DT.Var 2) (DT.Var 2))
  in   prove var_env sig_env pre_type

b' :: [J.Judgement]
b' =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type,DT.Type]
    pre_type =DT.Pi (DT.Pi (DT.Var 2) (DT.Var 2)) (DT.Pi (DT.Pi (DT.Var 2) (DT.Var 2)) (DT.Pi (DT.Var 4) (DT.Var 3)))
  in   prove var_env sig_env pre_type

c_star :: [J.Judgement]
c_star =
  let
    sig_env = classic
    var_env = [DT.Type,DT.Type]
    pre_type = DT.Pi (DT.Var 1) (DT.Pi (DT.Pi (DT.Var 2) (DT.Var 2)) (DT.Var 2))
  in   prove var_env sig_env pre_type

ex2 :: [J.Judgement] --Alligatorでできなかった
ex2 =
  let
    sig_env = classic
    var_env = [DT.Sigma (DT.Var 2) (DT.Sigma (DT.App (DT.Var 2) (DT.Var 0)) (DT.App (DT.Var 2) (DT.Var 1))),DT.Pi (DT.Var 1) DT.Type,DT.Pi (DT.Var 0) (DT.Type),DT.Type]
    pre_type = DT.Sigma (DT.Sigma (DT.Var 3) (DT.App (DT.Var 3) (DT.Var 0))) (DT.App (DT.Var 2) (DT.Proj DT.Fst (DT.Var 0)))
  in   prove var_env sig_env pre_type


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

-- is_there_a_girl :: [J.Judgement]
-- is_there_a_girl =
--   let sig_env = classic
--       var_env = [a_girl_writes_a_thesis (DT.Var 3) (DT.Var 2) (DT.Var 1) (DT.Var 0),write (DT.Var 2),thesis (DT.Var 1),girl (DT.Var 0),entity]
--       pre_type = there_is_a_girl (DT.Var 4) (DT.Var 3)
--   in prove var_env sig_env pre_type
not_false :: [J.Judgement]
not_false =
  let
    sig_env = classic
    var_env = []
    pre_type =DT.Bot
  in   prove var_env sig_env pre_type

-- is_there_a_girl :: [AJudgement]
-- is_there_a_girl =
--   let sig_env = []
--       var_env = [a_girl_writes_a_thesis (DT.Var 3) (DT.Var 2) (DT.Var 1) (DT.Var 0),write (DT.Var 2),thesis (DT.Var 1),girl (DT.Var 0),entity]
--       pre_type = there_is_a_girl (DT.Var 4) (DT.Var 3)
--   in prove' var_env sig_env pre_type

{-
is_there_a_girl :: [AJudgement]
is_there_a_girl =
  let words = [write 2,thesis 1,girl 0,entity]
      sentences = [a_girl_writes_a_thesis 3 2 1 0]
      prop = there_is_a_girl 4 3
  in prove sentences words prop
-}
lnc :: [J.Judgement]
lnc =
  let sig_env = classic
      var_env = [DT.Type]
      pre_type = DT.Not (DT.Sigma (DT.Var 0) (DT.Not (DT.Var 1)))
  in   prove var_env sig_env pre_type


hasProof :: [J.Judgement]  -> Bool
hasProof = ([] /=)

sk_lst = map hasProof [i,b,c,w,b',c_star,transitive_7_18,dni,dni_2,dni_3,dne',cm,raa,con1,con2]
hm_lst = sk_lst ++ map hasProof [lnc]
not_sk_lst = map hasProof [gem,pars,tnd,lem,efq,dne_2,cm',raa',con3,con4]
tautology = map hasProof [transitive,transitive_2,importation]
contradiction = map hasProof [not_false,p_not_p]
truelst = map hasProof [membership_test,pi_intro_test,pi_elim_test,ex1]
falselst = map hasProof [false,formedness]

test =
  (and sk_lst)
  && (and not_sk_lst)
  && (and tautology)
  && not (or contradiction)
  && (and truelst)
  && not (or falselst)


{-
con = [Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Con $ T.pack "p"),(Conclusion $ DT.Con $ T.pack "q")]
depth = 1
arrow_type = (Conclusion $ DT.Con $ T.pack  "p")
deduce con arrow_type depth

arrow_type = (Conclusion $ DT.Var 2)
deduce con arrow_type depth

arrow_type = (Conclusion $ DT.Var 1)
deduce con arrow_type depth

arrow_type = (Conclusion $ DT.Var 0)
deduce con arrow_type depth

arrow_type = Arrow_Sigma (Conclusion $ DT.Var 1) (Conclusion $ DT.Var 3)
deduce con arrow_type depth


con = [Arrow [Conclusion $ DT.Var 2] (Conclusion $ DT.Var 2),Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Var 0),(Conclusion $ DT.Con $ T.pack "q")]
depth =  1
arrow_type = Arrow [Conclusion $ DT.Var 2] (Conclusion $ DT.Var 4)
deduce con arrow_type depth

con = [(Conclusion $ DT.Var 2),Arrow [Conclusion $ DT.Var 2] (Conclusion $ DT.Var 2),Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Var 0),(Conclusion $ DT.Con $ T.pack "q")]
depth =  1
arrow_type = Arrow [Conclusion $ DT.Var 3] (Conclusion $ DT.Var 5)
deduce con arrow_type depth


arrow_type = Arrow [Conclusion $ DT.Var 1] (Conclusion $ DT.Con $ T.pack "q")
pi_intro con arrow_type depth

arrow_type = Arrow [Conclusion $ DT.Var 1] (Conclusion $ DT.Var 2)
pi_intro con arrow_type depth


con = [Arrow [Conclusion $ DT.Var 0] (Conclusion $ DT.Var 2),(Conclusion $DT.Var 0),(Conclusion $ DT.Con $ T.pack "q")]
arrow_type =Conclusion $ DT.Var 2
deduce con arrow_type depth


test = Arrow [Conclusion $ DT.Con $ T.pack "q"] (Arrow [Conclusion $ DT.Con $ T.pack "q"] (Arrow_Sigma (Conclusion $ DT.Con $T.pack "s") (Conclusion $ DT.Con $T.pack "r")))
con =  [(Arrow [Conclusion $ DT.Con $ T.pack "p"] (Arrow_Sigma (Conclusion $ DT.Con $T.pack "s") (Conclusion $ DT.Con $T.pack "r"))),(Arrow [Conclusion $ DT.Con $ T.pack "q"] (Arrow_Sigma (Conclusion $ DT.Con $T.pack "s") (Conclusion $ DT.Con $T.pack "r"))),to1Arrow test,Conclusion $ DT.Con $ T.pack "q"]
b1 = (Arrow_Sigma (Conclusion $ DT.Con $T.pack "s") (Conclusion $ DT.Con $T.pack "r"))
deduce con b1 depth


con = [Arrow [Conclusion $ DT.Con $ T.pack "q"] (Conclusion $ DT.Var 3),Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Var 0),(Conclusion $ DT.Con $ T.pack "q")]
depth = 1
arrow_type = Arrow_Sigma (Conclusion $ DT.Var 2) (Conclusion $ DT.Var 3)
deduce con arrow_type depth
-}

{-
con = [Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Con $ T.pack "p"),(Conclusion $ DT.Con $ T.pack "q")]
depth = 1
arrow_type = (Conclusion $ DT.Con $ T.pack  "p")
deduce con arrow_type depth

arrow_type = (Conclusion $ DT.Var 2)
deduce con arrow_type depth

arrow_type = (Conclusion $ DT.Var 1)
deduce con arrow_type depth

arrow_type = (Conclusion $ DT.Var 0)
deduce con arrow_type depth

arrow_type = Arrow_Sigma (Conclusion $ DT.Var 1) (Conclusion $ DT.Var 3)
deduce con arrow_type depth


con = [Arrow [Conclusion $ DT.Var 2] (Conclusion $ DT.Var 2),Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Var 0),(Conclusion $ DT.Con $ T.pack "q")]
depth =  1
arrow_type = Arrow [Conclusion $ DT.Var 2] (Conclusion $ DT.Var 4)
deduce con arrow_type depth

con = [(Conclusion $ DT.Var 2),Arrow [Conclusion $ DT.Var 2] (Conclusion $ DT.Var 2),Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Var 0),(Conclusion $ DT.Con $ T.pack "q")]
depth =  1
arrow_type = Arrow [Conclusion $ DT.Var 3] (Conclusion $ DT.Var 5)
deduce con arrow_type depth


arrow_type = Arrow [Conclusion $ DT.Var 1] (Conclusion $ DT.Con $ T.pack "q")
pi_intro con arrow_type depth

arrow_type = Arrow [Conclusion $ DT.Var 1] (Conclusion $ DT.Var 2)
pi_intro con arrow_type depth


con = [Arrow [Conclusion $ DT.Var 0] (Conclusion $ DT.Var 2),(Conclusion $DT.Var 0),(Conclusion $ DT.Con $ T.pack "q")]
arrow_type =Conclusion $ DT.Var 2
deduce con arrow_type depth


test = Arrow [Conclusion $ DT.Con $ T.pack "q"] (Arrow [Conclusion $ DT.Con $ T.pack "q"] (Arrow_Sigma (Conclusion $ DT.Con $T.pack "s") (Conclusion $ DT.Con $T.pack "r")))
con =  [(Arrow [Conclusion $ DT.Con $ T.pack "p"] (Arrow_Sigma (Conclusion $ DT.Con $T.pack "s") (Conclusion $ DT.Con $T.pack "r"))),(Arrow [Conclusion $ DT.Con $ T.pack "q"] (Arrow_Sigma (Conclusion $ DT.Con $T.pack "s") (Conclusion $ DT.Con $T.pack "r"))),to1Arrow test,Conclusion $ DT.Con $ T.pack "q"]
b1 = (Arrow_Sigma (Conclusion $ DT.Con $T.pack "s") (Conclusion $ DT.Con $T.pack "r"))
deduce con b1 depth


con = [Arrow [Conclusion $ DT.Con $ T.pack "q"] (Conclusion $ DT.Var 3),Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Var 0),(Conclusion $ DT.Con $ T.pack "q")]
depth = 1
arrow_type = Arrow_Sigma (Conclusion $ DT.Var 2) (Conclusion $ DT.Var 3)
deduce con arrow_type depth
-}

{-
con = [Arrow_Sigma (Conclusion $ DT.Var 0) (Conclusion $ DT.Var 2),(Conclusion $DT.Var 0),(Conclusion $ DT.Con $ T.pack "q")]
arrow_type = (Conclusion DT.Type)
depth = 1
arrow_term = Arrow [Conclusion $DT.Con $T.pack "q"] (Conclusion $ DT.Var 3)
pi_form con arrow_term arrow_type depth
-}
