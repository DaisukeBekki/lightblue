module Parser.Language.Japanese.JsemLexicon (
  verbLexicon,
  ) where

import qualified Data.Text.Lazy as T
import Parser.CCG
import Parser.Language.Japanese.Templates
import qualified DTS.DTTdeBruijn as DTT --lightblue
import DTS.UDTTdeBruijn as UDTT --lightblue

type Signature = DTT.Signature

terminator :: UDTT.Preterm
terminator = UDTT.Ann (UDTT.Lam UDTT.Top) (DTT.Pi DTT.Entity DTT.Type)

mylex :: [T.Text] -> T.Text -> Cat -> (UDTT.Preterm, Signature) -> [Node]
mylex wds num cat' (sem',sig') = [(lexicalitem wd num 100 cat' (sem',sig')) | wd <- wds ]

verbLexicon :: [Node]
verbLexicon = concat $ [
  -- for 695
  mylex ["最中"] "new" (N) (predSR 1 "最中/さいちゅう"),
  -- for 697
  mylex ["つつ"] "new" (((T False 1 modifiableS `BS` NP[F[Ga]]) `SL` (T False 1 modifiableS `BS` NP[F[Ga]])) `BS` (S [F verb, F[Cont], F[M],F[M],F[M],F[M],F[M]] `BS` NP[F[Ga]])) 
        ((Lam (Lam (Lam (Lam (Sigma (App (App (Var 3) (Var 1)) terminator) (App (App (Var 3) (Var 2)) (Var 1))))))), []),
  -- for 699
  mylex ["打ち合わせ"] "new" (N) (predSR 1 "打ち合わせ/うちあわせ")
  ]