module DTS.Alligator.Arrowterm
(
  Arrowterm(..),
  AJudgement(..),
  arrow_notat,
  arrow2DT,
  a2dtJudgement,
  dt2aJudgement,
  TEnv,
  envfromAJudgement,
  termfromAJudgement,
  typefromAJudgement,
  shiftIndices,
  arrow_subst,
  ArrowSelector(..),
  reduce,
  subst
) where

import qualified DTS.DTT as DT            -- DTT
import qualified DTS.UDTT as UD            -- DTT
import qualified Data.Text.Lazy as T      -- text
import qualified Data.List as L           -- base
import qualified DTS.Prover.Judgement as J


data ArrowSelector = Arrow_Fst | Arrow_Snd deriving (Eq, Show)

-- | Arrowterm
data Arrowterm =
  Conclusion DT.Preterm -- ^ 末尾の部分
  | Arrow_Sigma Arrowterm Arrowterm -- ^ Sigma型
  | Arrow_App Arrowterm Arrowterm -- ^ App型
  | Arrow_Pair Arrowterm Arrowterm -- ^ Pair型
  | Arrow_Proj ArrowSelector Arrowterm -- ^ Proj型
  | Arrow_Lam Arrowterm -- ^ Lam型
  | Arrow [Arrowterm]  Arrowterm -- ^ Pi型
  deriving (Eq)

-- | DTT.Preterm型に変換して得たテキストを加工している
instance Show Arrowterm where
  show term  = prestr2arrowstr ((filter (/= ' ') . show . arrow2DT) term) term

prestr2arrowstr :: String -> Arrowterm -> String
prestr2arrowstr prestr (Conclusion p) = (dropWhile (not . (`elem` (['0'..'z']++['(',')','\8869']))) prestr) --if (head prestr `elem` (['0'..'z']++['(',')'])) then prestr else tail prestr
prestr2arrowstr prestr (Arrow env r) =
  if length env == 0
  then
    (prestr2arrowstr  prestr r)
  else
    let parentheses =  take (length env) (treatParentheses prestr) --[(2,"q0")]
    in "[ " ++
        (tail
          (foldr
            (\a -> \b -> ", "  ++ a  ++ b)
            ""
            (map
              (\z ->
                let str = (init $ tail $ snd $ snd z)
                in (takeWhile (/= ':')  str) ++":" ++  (prestr2arrowstr (tail $ dropWhile (/= ':') str) $ fst z))
                $ zip (reverse env) parentheses))) ++
        " ] =>" ++ (prestr2arrowstr  (tail $ drop ((fst . last) parentheses) prestr) r)
prestr2arrowstr prestr (Arrow_Sigma h t) =
  let parentheses = head (treatParentheses prestr)
  in "(" ++ let str  = (tail $ init $ snd  parentheses) in ((takeWhile (/= ':')  str) ++":"++ (prestr2arrowstr (tail $ dropWhile (/= ':') str)  h)) ++ [' ',')','\215',' '] ++ (prestr2arrowstr  (drop ((fst  parentheses) + 1) prestr) t)
prestr2arrowstr prestr (Arrow_App h t) =
  let f_len = (length prestr) - (fst $ head $ treatParentheses (reverse prestr))
  in let f  = take f_len prestr in  (prestr2arrowstr f  h) ++ " " ++ (prestr2arrowstr  (init$ tail$drop (f_len) prestr) t)
prestr2arrowstr prestr (Arrow_Proj s t) =
  let parentheses = head (treatParentheses prestr)
  in (snd parentheses) ++"(" ++ (prestr2arrowstr (init $ tail (drop (fst parentheses) prestr)) t) ++ ")"
prestr2arrowstr prestr (Arrow_Lam t) =
  takeWhile (/= '.') prestr ++ ".(" ++ (prestr2arrowstr  (tail $ dropWhile (/= '.') prestr) t) ++")"
prestr2arrowstr prestr (Arrow_Pair h t) =
  let contents = init $ tail prestr
      index = fst $ head $ filter ((==",") . snd) $ treatParentheses contents
  in
     "("++(prestr2arrowstr (take (index - 1) contents) h)++","++(prestr2arrowstr (drop index contents) t)++")"

treatParentheses :: String -> [(Int,String)]
treatParentheses str =
  let str'= (takeWhile (\x -> (x /='(') && (x /=')' && (x/=',')) ) str)
      lst =  (indexParentheses (drop (length str') str))
  in
    filter ((/=0) . fst) $ (length str' , str') : (map (\x -> ( ((snd $ fst x) + length str'),snd x)) $ filter (\x ->((fst $ fst x) /= 1) || ((snd x) == ",") || ((snd x) == ['\8869']) ) $ zip (zip lst $ map sum  $ tail  $ L.inits lst) $  devideParentheses $ filter (`elem` (['0'..'z']++['(',')','\8594','\215','\8869','\960','\955','.',',',' '])) (drop (length str')  str))

indexParentheses :: String -> [Int]
indexParentheses str =
  if length str > 0
  then
    let f = fst ( last (fst (head (filter (\x -> snd x == 0) (tail (map (\z -> (z,(foldr (\x -> \y -> if (snd x)=='(' then (y + 1) else (if (snd x)==')' then (y-1) else y) ) 0 z)))(L.inits (zip [1..] str))  ))))))
    in (f : (indexParentheses (drop f str)))
  else []

devideParentheses :: String -> [String]
devideParentheses str =
  if length str > 0
  then
    let f = fst (head (filter (\x -> snd x == 0) (tail (map (\z -> (z,(foldr (\x -> \y -> if x=='(' then (y + 1) else (if x==')' then (y-1) else y) ) 0 z)))(L.inits str)  ))))
    in (f : (devideParentheses (drop (length f)str)))
  else []

arrow_notat_selector :: DT.Selector -> ArrowSelector
arrow_notat_selector DT.Fst = Arrow_Fst
arrow_notat_selector DT.Snd = Arrow_Snd

dt_notat_selector :: ArrowSelector -> DT.Selector
dt_notat_selector Arrow_Fst = DT.Fst
dt_notat_selector Arrow_Snd = DT.Snd

arrow2DT :: Arrowterm -> DT.Preterm
arrow2DT (Conclusion a) = a
arrow2DT (Arrow_Sigma h t)= DT.Sigma (arrow2DT h) (arrow2DT t)
arrow2DT (Arrow_Pair h t)= DT.Pair (arrow2DT h) (arrow2DT t)
arrow2DT (Arrow_App a b) = DT.App (arrow2DT a) (arrow2DT b)
arrow2DT (Arrow_Proj s p) = DT.Proj (dt_notat_selector s) (arrow2DT p)
arrow2DT (Arrow_Lam p) = DT.Lam (arrow2DT p)
arrow2DT (Arrow [] t) = arrow2DT t
arrow2DT (Arrow (f:r) t) = arrow2DT (Arrow r (Conclusion (DT.Pi (arrow2DT f)  (arrow2DT t))))

-- | 入力されたDT.PretermをArrowTermに変換する
arrow_notat :: DT.Preterm -> Arrowterm
arrow_notat (DT.Type) = Conclusion $DT.Type
arrow_notat (DT.Var i) = Conclusion $ DT.Var i
arrow_notat (DT.Con i) = Conclusion $ DT.Con i
arrow_notat (DT.Not i) =
  Arrow [arrow_notat i] $Conclusion DT.Bot
arrow_notat (DT.Pi h t) =
  Arrow [arrow_notat h] (arrow_notat t)
arrow_notat (DT.Sigma h t) =
  Arrow_Sigma (arrow_notat h) (arrow_notat t)
arrow_notat (DT.App a b) =
  Arrow_App (arrow_notat a) (arrow_notat b)
arrow_notat (DT.Pair a b) =
  Arrow_Pair (arrow_notat a) (arrow_notat b)
arrow_notat (DT.Proj selector p) =
  Arrow_Proj (arrow_notat_selector selector) (arrow_notat p)
arrow_notat (DT.Lam p) =
  Arrow_Lam (arrow_notat p)
arrow_notat dt= Conclusion dt

shiftIndices :: Arrowterm -> Int -> Int -> Arrowterm
shiftIndices term d i= ((arrow_notat . DT.toDTT) $ UD.shiftIndices ((DT.toUDTT . arrow2DT) term) d i)

reduce :: Arrowterm -> Arrowterm
reduce = arrow_notat . DT.toDTT . UD.betaReduce . DT.toUDTT . arrow2DT

type TEnv = [DT.Preterm]
type SUEnv = [(T.Text,DT.Preterm)]
type AEnv = [Arrowterm]

data AJudgement =
  AJudgement
  AEnv -- ^ context
  Arrowterm -- ^ term
  Arrowterm -- ^ type
    deriving (Eq)

instance Show AJudgement where
  show (AJudgement env a_term a_type ) =
    let str = show $ Arrow env a_term
        f = fst $ last $ fst $ head $ filter (\x -> snd x == 0) $ tail $ map (\z -> (z,(foldr (\x -> \y -> if (snd x)=='[' then y + 1 else (if (snd x)==']' then y-1 else y) ) 0 z))) $ L.inits (zip [1..] str)
    in
      if  length env == 0
      then
        " ト " ++ str ++ " : " ++  (show $ Arrow env a_type)
      else
        (init $ tail $take f str)++" ト " ++ (drop (f + (length " =>")) str) ++ " : " ++ (drop (f + (length " =>")) $ show $ Arrow env a_type)

a2dtJudgement :: AJudgement -> J.Judgement
a2dtJudgement (AJudgement env aterm atype) = J.Judgement (map arrow2DT env) (arrow2DT aterm) (arrow2DT atype)

dt2aJudgement ::  J.Judgement -> AJudgement
dt2aJudgement (J.Judgement env dtterm dttype) = AJudgement (map arrow_notat env) (arrow_notat dtterm) (arrow_notat dttype)

typefromAJudgement :: AJudgement -> Arrowterm
typefromAJudgement ( AJudgement env aterm atype) = atype

termfromAJudgement :: AJudgement -> Arrowterm
termfromAJudgement ( AJudgement env aterm atype) = aterm

envfromAJudgement :: AJudgement -> [Arrowterm]
envfromAJudgement ( AJudgement env aterm atype) = env

-- dne =   DT.Lam  (DT.Pi (DT.Pi (DT.Pi (DT.Var 1)  DT.Bot) (DT.Bot)) (DT.Var 2))
--efq = DT.Lam (DT.Pi DT.Bot (DT.Var 1))
dn_pr = DT.Pi (DT.Type) (DT.Pi (DT.Pi (DT.Pi (DT.Var 0) DT.Bot) DT.Bot) (DT.Var 1))
--dne = DT.Pi (DT.Type) (DT.Pi DT.Bot (DT.Var 1))

classic  :: TEnv
classic = [dn_pr]

subst :: DT.Preterm -> DT.Preterm -> DT.Preterm -> DT.Preterm
subst preterm l i =
  if preterm == i then
    l
  else
    case preterm of
      DT.Pi a b -> DT.Pi (subst a l i) (subst b (DT.toDTT (UD.shiftIndices (DT.toUDTT l) 1 0))  (DT.toDTT (UD.shiftIndices (DT.toUDTT i) 1 0)))
      DT.Not m -> DT.Not (subst m l i)
      DT.Lam m -> DT.Lam (subst m (DT.toDTT (UD.shiftIndices (DT.toUDTT l) 1 0)) (DT.toDTT (UD.shiftIndices (DT.toUDTT i) 1 0)))
      DT.App m n    -> DT.App (subst m l i) (subst n l i)
      DT.Sigma a b  -> DT.Sigma (subst a l i) (subst b (DT.toDTT (UD.shiftIndices (DT.toUDTT l) 1 0)) (DT.toDTT (UD.shiftIndices (DT.toUDTT i) 1 0)))
      DT.Pair m n   -> DT.Pair (subst m l i) (subst n l i)
      DT.Proj s m   -> DT.Proj s (subst m l i)
      DT.Eq a m n   -> DT.Eq (subst a l i) (subst m l i) (subst n l i)
      others -> others

arrow_subst :: Arrowterm -- ^ origin
  -> Arrowterm -- ^ 代入内容
  -> Arrowterm -- ^ 代入先
  -> Arrowterm
arrow_subst term i m= (arrow_notat) $ subst (arrow2DT term) (arrow2DT i) (arrow2DT m)
