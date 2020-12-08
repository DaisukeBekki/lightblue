import qualified DTS.Alligator.ProofTree as APT
import qualified DTS.DTT as DT
import qualified DTS.UDTT as UD
import qualified Data.Text.Lazy as T
import qualified DTS.Prover.Judgement as J
import qualified System.Timeout as ST
import qualified DTS.Alligator.Arrowterm as A
import qualified DTS.Alligator.Problem as P

import qualified Interface.HTML as HTML
import qualified Data.Text.Lazy.IO as T

main = do
  let url="forTest.html"
  let result = APT.prove P.context [] P.conclusion APT.settingDef
  html <- APT.announce result
  T.writeFile url html
