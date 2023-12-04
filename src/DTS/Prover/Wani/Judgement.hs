{-|
  Module      : DTS.Prover_daido.Judgement
  Maintainer  : Daido Hinari <daido.hinari@is.ocha.ac.jp>
  DTS.Prover.Judgement in different notation

  = When adding new rules, edit ...
  * `Label`
  * show Label
  * labelToMathML
  * labelToTex
-}
module DTS.Prover.Wani.Judgement
( -- * Types
  TEnv,
  TUEnv,
  SUEnv,
  -- * Judgements
  Judgement(..),
  UJudgement(..),
  -- * Trees
  -- ** Labels
  Label(..),
  ULabel(..),
  -- ** Trees
  -- $judgements
  Tree(..),
  UTree (..),
  -- * Functions
  getTerm,
  getType,
  getTypeU,
  getTermU,
  getList,
  utreeToTeX,
  utreeToMathML,
  treeToMathML,
  treeToTeX
) where

import qualified DTS.UDTTdeBruijn as UDdB -- UDTT
import DTS.Labels (UDTT,DTT)              -- UDTT
import qualified Data.Text.Lazy as T      -- text
import qualified Data.List as L           -- base
import qualified Data.Maybe as M
import qualified Control.Applicative as M -- base
import qualified Control.Monad as M       -- base
import Interface.Text
import Interface.TeX
import Interface.HTML

-- | Type Environment for __UDTT__
--
-- Newer variables come former.
type TUEnv = UDdB.Context

-- | Type Environment for __DTT__
--
-- Newer variables come former.
  --
-- For examples, following environment is notated as [c,b,a]
-- \[
-- \Gamma \equiv a,b,c
-- \]
type TEnv = [(UDdB.Preterm DTT)]

-- | Signature Environment Type for __UDTT__
--  
-- Newer variables come former.
--
--  When Signature "fuga" has value "@UD.Var 0@", Signature is notated as @(fuga,UD.Var 0)@
type SUEnv = [(T.Text,(UDdB.Preterm UDTT))]

-- | getList : get all velues for @var@ in @env@
getList :: 
     (Eq k, Show k) 
  => [(k, v)] -- ^ env
  -> k -- ^ var
  -> [v]
getList [] key = []
getList ((k,v):xs) key
  | key == k  = v:(getList xs key)
  | otherwise = getList xs key


-- | UJudgement : Judgement for __UDTT__
data UJudgement =
  UJudgement TUEnv (UDdB.Preterm UDTT) (UDdB.Preterm UDTT)
    deriving (Eq, Show)

toUJudgment :: UJudgement  -> UDdB.Judgment UDTT
toUJudgment (UJudgement env preM preA) = UDdB.Judgment {UDdB.context = env, UDdB.term = preM, UDdB.typ = (M.fromMaybe UDdB.Type (UDdB.toDTT preA))}

instance Typeset UJudgement where
  toTeX = undefined

instance MathML UJudgement where
  toMathML = undefined

{--
-- | translates a UDTT Judgement into a tex source code.
instance Typeset UJudgement where
  toTeX = toTeX . toUJudgment

-- | translates a UDTT Judgement into a MathML notation.
instance MathML UJudgement where
  toMathML = toMathML . toUJudgment
--}

-- | Judgement : Judgement for __DTT__
data Judgement =
  Judgement TEnv (UDdB.Preterm DTT) (UDdB.Preterm DTT)
    deriving (Eq, Show)

toJudgment :: Judgement  -> UDdB.Judgment UDTT
toJudgment (Judgement env preM preA) = 
    let preM' = UDdB.toUDTT preM in
    UDdB.Judgment {UDdB.context = env, UDdB.term = preM', UDdB.typ = preA}


  -- | translates a DTT Judgement into a tex source code.
instance Typeset Judgement where
  toTeX  = undefined

-- | translates a DTT Judgement into a MathML notation.
instance MathML Judgement where
  toMathML  = undefined

{--
  -- | translates a DTT Judgement into a tex source code.
instance Typeset Judgement where
  toTeX  = toTeX . toJudgment

-- | translates a DTT Judgement into a MathML notation.
instance MathML Judgement where
  toMathML  = toMathML . toJudgment
--}

-- Most important change from DTS.Prover.Judgement


-- | rule names
data Label =
   CHK   -- ^(CHK) rule
 | CON   -- ^(CON) rule
 | VAR   -- ^(VAR) rule
 | TypeF  -- ^(typeF) rule
 | PiF    -- ^(Pi F) rule
 | PiI    -- ^(Pi I) rule
 | PiE    -- ^(Pi E) rule
 | SigF   -- ^(Sig F) rule
 | SigI   -- ^(Sig I) rule
 | SigE   -- ^(Sig E) rule
 | NotF   -- ^(Not F) rule
 | NotI   -- ^(Not I) rule
 | NotE   -- ^(Not E) rule
 | TopF   -- ^(TF) rule
 | TopI   -- ^(TI) rule
 | BotF   -- ^(Bot F) rule
 | DREL   -- ^(DRel) rule
 | EqE    -- ^(EqE) rule
 | EqF    -- ^(EqI) rule
 deriving (Eq)

instance Show Label where
  show label =
    case label of
     CHK   -> "CHK"
     CON   -> "CON"
     VAR   -> "VAR"
     TypeF  -> "typeF"
     PiF    -> "Pi F"
     PiI    -> "Pi I"
     PiE    -> "Pi E"
     SigF   -> "Sig F"
     SigI   -> "Sig I"
     SigE   -> "Sig E"
     NotF   -> "Not F"
     NotI   -> "Not I"
     NotE   -> "Not E"
     TopF   -> "TF"
     TopI   -> "TI"
     BotF   -> "Bot F"
     DREL   -> "DRel"
     EqE    -> "Eq E"
     EqF    -> "Eq F"

-- | translate label into a MathML notation
labelToMathML :: Label -> String
labelToMathML label =
    case label of
     PiF    -> "&Pi;F"
     PiI    -> "&Pi;I"
     PiE    -> "&Pi;E"
     SigF   -> "&Sigma;F"
     SigI   -> "&Sigma;I"
     SigE   -> "&Sigma;E"
     NotF   -> "<not/>F"
     NotI   -> "<not/>I"
     NotE   -> "<not/>E"
     TopF   -> "&top;F"
     TopI   -> "&top;I"
     BotF   -> "&bot;F"
     EqE    -> "= E"
     EqF    -> "= F"
     _ -> show label

-- | translate label into a tex source code
labelToTeX :: Label -> String
labelToTeX label =
    case label of
     PiF    -> "\\Pi F"
     PiI    -> "\\Pi I"
     PiE    -> "\\Pi E"
     SigF   -> "\\Sigma F"
     SigI   -> "\\Sigma I"
     SigE   -> "\\Sigma E"
     NotF   -> "\\neg F"
     NotI   -> "\\neg I"
     NotE   -> "\\neg E"
     TopF   -> "\\top F"
     TopI   -> "\\top I"
     BotF   -> "\\bot F"
     EqE    -> "= E"
     EqE    -> "= F"
     _ -> show label

data ULabel = 
   ASP'  -- ^ underspecified type
  | L Label -- ^ otherwise
  deriving (Eq)

instance Show ULabel where
  show ulabel =
    case ulabel of
     ASP'     -> "@"
     L label    -> show label

-- | translate ulabel into a MathML notation
ulabelToMathML :: ULabel -> String
ulabelToMathML ulabel =
    case ulabel of
     L label    -> labelToMathML label
     ASP' -> show ulabel

-- | translate ulabel into a tex source code
ulabelToTeX :: ULabel -> String
ulabelToTeX ulabel =
    case ulabel of
     L label    -> labelToTeX label
     ASP' -> show ulabel

-- | UTree  : Tree for __UDTT__
data UTree  a = 
   UT ULabel a [UTree  a] -- ^ tree
  | UError' a T.Text -- ^ error
  deriving (Eq, Show) 

-- | translate uTree into a tex source code
utreeToTeX  :: (UTree  UJudgement) -> T.Text
utreeToTeX = undefined
{--
utreeToTeX (UT label downside upside) =
  T.pack ("\\nd[(\\underline{"++ ulabelToTeX label ++ "})]{") `T.append` (toTeX downside) `T.append` T.pack "}{" `T.append`  (if null upside then T.pack "" else T.init $ T.concat$ map (\tree -> utreeToTeX tree `T.append`  T.pack "&") upside) `T.append` T.pack "}"
utreebaseToTex (UError' judgement msg) =
  T.pack "\\nd[(Error)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` msg `T.append` T.pack "}"
--}

-- | translate uTree into a MathML notation
utreeToMathML :: (UTree  UJudgement) -> T.Text
utreeToMathML = undefined
{--
utreeToMathML (UT label downside upside) =
  case length upside of
    0 ->
      T.concat [
        T.pack ("<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>("++ulabelToMathML label++")</p></mi><mfrac linethickness='2px'>"),
        T.pack "<mi color='White'> Empty </mi>",
        toMathML downside,
        T.pack "</mfrac></mrow>"
        ]
    1 ->
        T.concat [
        T.pack ("<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>("++ulabelToMathML label++")</p></mi><mfrac linethickness='2px'>"),
        utreeToMathML $head upside,
        toMathML downside,
        T.pack "</mi></mrow>"
        ]
    num ->
        T.append (T.concat [
          T.pack ("<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>("++ ulabelToMathML label ++")</p></mi><mfrac linethickness='2px'>"),
          T.pack "<mrow>"])
        (T.append (T.concat $
          foldr (\n lst -> (utreeToMathML $upside !! n):((T.pack " "):lst)) [] [0..(num-1)] 
          {-[utreeToMathML $head upside,
          T.pack " ",
          utreeToMathML $upside !! 1 ]-})
          (T.concat [T.pack "</mrow>",
          toMathML downside,
          T.pack "</mfrac></mrow>"
          ]))
utreeToMathML (UError' judgement msg) =
   T.concat [
     T.pack "<mrow><mi fontsize='0.8'>(Error)</mi><mfrac linethickness='2px'>",
     msg,
     toMathML judgement,
     T.pack "</mfrac></mrow>"
     ]
--}

-- | Tree for __DTT__
--
-- Second input comes bottom of the tree and third come upside.
--
-- For example, following tree is notated as @T SigI a&b [ tree to b,tree to a]@
--
-- Dash in @Error'@ is to avoid conflict with DTS.Prover.Judgement.Error
--
-- a  \   b
--
-- \------
--
-- a & b
data Tree a = 
   T Label a [Tree a] -- ^ tree
  | Error' a T.Text -- ^ error
  deriving (Eq, Show)

-- | translate uTree into a tex source code
treeToTeX  :: (Tree Judgement) -> T.Text
treeToTeX = undefined
{--
treeToTeX (T label downside upside) =
  T.pack ("\\nd[("++ labelToTeX label ++ ")]{") `T.append` (toTeX downside) `T.append` T.pack "}{" `T.append`  (if null upside then T.pack "" else T.init $ T.concat$ map (\tree -> treeToTeX tree `T.append`  T.pack "&") upside) `T.append` T.pack "}"
treebaseToTex (Error' judgement msg) =
  T.pack "\\nd[(Error)]{" `T.append` (toTeX judgement) `T.append` T.pack "}{" `T.append` msg `T.append` T.pack "}"
--}

-- | translate uTree into a MathML notation
treeToMathML :: (Tree Judgement) -> T.Text
treeToMathML = undefined
{--
treeToMathML (T label downside upside) =
  case length upside of
    0 ->
      T.concat [
        T.pack ("<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>("++labelToMathML label++")</p></mi><mfrac linethickness='2px'>"),
        T.pack "<mi color='White'> Empty </mi>",
        toMathML downside,
        T.pack "</mfrac></mrow>"
        ]
    1 ->
        T.concat [
        T.pack ("<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>("++labelToMathML label++")</p></mi><mfrac linethickness='2px'>"),
        treeToMathML $head upside,
        toMathML downside,
        T.pack "</mi></mrow>"
        ]
    num ->
        T.append (T.concat [
          T.pack ("<mrow><mi fontsize='0.8'><p style='text-decoration: underline;'>("++ labelToMathML label ++")</p></mi><mfrac linethickness='2px'>"),
          T.pack "<mrow>"])
        (T.append (T.concat $
          foldr (\n lst -> (treeToMathML $upside !! n):((T.pack " "):lst)) [] [0..(num-1)] )
          (T.concat [T.pack "</mrow>",
          toMathML downside,
          T.pack "</mfrac></mrow>"
          ]))
treeToMathML (Error' judgement msg) =
   T.concat [
     T.pack "<mrow><mi fontsize='0.8'>(Error)</mi><mfrac linethickness='2px'>",
     msg,
     toMathML judgement,
     T.pack "</mfrac></mrow>"
     ]
--}

-- | get bottom type from UTree
getTypeU :: (UTree  UJudgement) -> [(UDdB.Preterm UDTT)]
getTypeU (UT ulabel (UJudgement env preM preA) upside) = [preA]
getTypeU (UError' (UJudgement env preM preA) text) = [preA]

-- | get bottom term from UTree
getTermU :: (UTree  UJudgement) -> [(UDdB.Preterm UDTT)]
getTermU (UT ulabel (UJudgement env preM preA) upside) = [preM]
getTermU (UError' (UJudgement env preM preA) text) = [preM]

-- | get bottom type from Tree]
getType :: (Tree Judgement) -> [(UDdB.Preterm DTT)]
getType (T label (Judgement env preM preA) upside) =  [preA]
getType (Error' (Judgement env preM preA) text) = [preA]

-- | get bottom term from Tree
getTerm :: (Tree Judgement) -> [(UDdB.Preterm DTT)]
getTerm (T label (Judgement env preM preA) upside) =  [preM]
getTerm (Error' (Judgement env preM preA) text) = [preM]

-- | print UDTT type environment
printGammaU :: TUEnv -> T.Text
printGammaU = undefined
{--
printGammaU [] = T.pack ""
printGammaU [x] = toTeX x
printGammaU (x:xs) = (toTeX x) `T.append` T.pack ", " `T.append` (printGammaU xs)
--}

-- | print DTT type environment
printGamma :: TEnv -> T.Text
printGamma x = undefined
{--
printGamma [] = T.pack ""
printGamma [x] = toTeX x
printGamma (x:xs) = (toTeX x) `T.append` T.pack ", " `T.append` (printGamma xs)
--}
