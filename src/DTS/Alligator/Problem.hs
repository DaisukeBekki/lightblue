module DTS.Alligator.Problem
(
  context,
  conclusion
) where
import DTS.Alligator.ProofTree
import qualified DTS.DTT as DT
import qualified DTS.UDTT as UD
import qualified Data.Text.Lazy as T
import qualified DTS.Prover.Judgement as J
import qualified System.Timeout as ST
import qualified DTS.Alligator.Arrowterm as A

import qualified Interface.HTML as HTML
import qualified Data.Text.Lazy.IO as T

shiftIndices :: DT.Preterm -> Int -> Int-> DT.Preterm
shiftIndices term d i = DT.toDTT $ UD.shiftIndices  (DT.toUDTT term) d i

entity :: DT.Preterm
entity = DT.Type

man :: DT.Preterm -> DT.Preterm
man man_en= DT.Pi man_en DT.Type

enter:: DT.Preterm ->  DT.Preterm
enter enter_en = DT.Pi enter_en (DT.Pi (shiftIndices enter_en 1 0) DT.Type)

thereIsAman :: DT.Preterm -> DT.Preterm -> DT.Preterm
thereIsAman en man =
  DT.Sigma en (DT.App (shiftIndices man 1 0) (DT.Var 0))

aManEnters:: DT.Preterm -> DT.Preterm -> DT.Preterm -> DT.Preterm
aManEnters entity man enter =
  DT.Sigma
    (thereIsAman entity man)
    (DT.App (shiftIndices enter 1 0) (DT.Proj DT.Fst $ DT.Var 0))

-- a_girl_writes_a_thesis :: DT.Preterm ->  DT.Preterm -> DT.Preterm ->  DT.Preterm ->  DT.Preterm
-- a_girl_writes_a_thesis en gi th wr =
--   DT.Sigma
--     (DT.Sigma
--       (there_is_a_girl en gi)
--       (DT.Sigma (shiftIndices en 1 0) (DT.App (shiftIndices th 2 0) (DT.Var 0)) ))
--     (DT.App
--       (DT.App (shiftIndices wr 1 0) (DT.Proj DT.Fst $ DT.Proj DT.Fst $ DT.Var 0))
--       (DT.Proj DT.Fst $ DT.Proj DT.Snd $ DT.Var 0)
--     )


context :: [DT.Preterm]
context = [aManEnters (DT.Var 2) (DT.Var 1) (DT.Var 0),enter (DT.Var 1),man (DT.Var 0),entity]

conclusion :: DT.Preterm
conclusion = thereIsAman (DT.Var 3) (DT.Var 2)
