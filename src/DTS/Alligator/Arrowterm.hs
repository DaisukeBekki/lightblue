module DTS.Alligator.Arrowterm
(
  Arrowterm(..),
  AJudgement(..),
  dtToArrow,
  arrow2DT,
  arrowNotat,
  a2dtJudgement,
  dt2aJudgement,
  aTreeTojTree,
  jTreeToaTree,
  TEnv,
  envfromAJudgement,
  termfromAJudgement,
  typefromAJudgement,
  shiftIndices,
  arrowSubst,
  ArrowSelector(..),
  upSide,
  downSide,
  reduce,
  subst,
  changeDownSide,
  -- Normterm(..),
  -- arrow2Normterm,
  genFreeCon,
  isFreeCon,
  canBeSame,
  aj
) where

import qualified DTS.DTT as DT            -- DTT
import qualified DTS.UDTT as UD            -- DTT
import qualified Data.Text.Lazy as T      -- text
import qualified Data.List as L           -- base
import qualified DTS.Prover_daido.Judgement as J


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
  | Arrow_Eq Arrowterm Arrowterm Arrowterm -- ^ 項1 項2 型
  deriving (Eq)

-- | DTT.Preterm型に変換して得たテキストを加工している
instance Show Arrowterm where
  show term  = prestr2arrowstr ((filter (/= ' ') . show . arrow2DT) term) term

-- -- | Arrowterm+定数の名前のリスト 例) Arrow [Conclusion $DT.Con "u"] (Conclusion $DT.Con "v")
-- data Normterm
--   = Normterm{
--       arrowterm :: Arrowterm, -- 例 Arrow [Conclusion $DT.Con "a"] (Conclusion $DT.Con "b")
--       corrTable :: [(Arrowterm,Arrowterm)] -- (origin,normalized)
--     } deriving (Show,Eq)
--
-- --
-- -- | dtToArrow後
-- arrow2Normterm :: String ->Arrowterm -> [(Arrowterm,Arrowterm)] -> Normterm
-- arrow2Normterm counter (Conclusion (DT.Con c)) coTable
--   | (Conclusion (DT.Con c)) `elem` (fst $unzip coTable) =
--     let con = snd $head $ filter (\(a,b) -> a==(Conclusion (DT.Con c))) coTable in
--       Normterm{arrowterm = con,corrTable=coTable}
--   | otherwise =
--     let con = genFreeCon (Conclusion (DT.Con c)) {-}("con"++ counter) -}"con" in
--       Normterm{arrowterm = con,corrTable=((Conclusion (DT.Con c)),con):coTable}
-- arrow2Normterm counter (Conclusion dtterm) _= Normterm{arrowterm = Conclusion dtterm,corrTable=[]}
-- arrow2Normterm counter (Arrow_Sigma aterm1 aterm2) coTable=
--   let normterm1 = arrow2Normterm (counter++"pi1") aterm1 coTable
--       arrowterm1 = arrowterm normterm1
--       corrTable1 = corrTable normterm1
--       normterm2 = arrow2Normterm (counter++"pi2") aterm2 corrTable1
--       arrowterm2 = arrowterm normterm2
--   in normterm2{arrowterm= Arrow_Sigma arrowterm1 arrowterm2}
-- arrow2Normterm counter (Arrow_App aterm1 aterm2) coTable = undefined
-- arrow2Normterm counter (Arrow_Pair aterm1 aterm2) coTable = undefined
-- arrow2Normterm counter (Arrow_Proj aterm1 aterm2) coTable = undefined
-- arrow2Normterm counter (Arrow_Lam aterm) coTable = undefined
-- arrow2Normterm counter (Arrow alst aterm) coTable = undefined



-- | DTT.Preterm型に変換して得たテキストとArrowtermを比較して加工している
prestr2arrowstr :: String -> Arrowterm -> String
prestr2arrowstr prestr (Conclusion p) = dropWhile (not . (`elem` (['0'..'z']++['(',')','\8869']))) prestr --if (head prestr `elem` (['0'..'z']++['(',')'])) then prestr else tail prestr
prestr2arrowstr prestr (Arrow env r) =
  if null env
  then
    prestr2arrowstr  prestr r
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
prestr2arrowstr prestr (Arrow_Eq a b t) = prestr

treatParentheses :: String -> [(Int,String)]
treatParentheses str =
  let str'= takeWhile (\x -> (x /='(') && (x /=')' && (x/=',')) ) str
      lst =  indexParentheses (drop (length str') str)
  in
    filter ((/=0) . fst) $
      (length str' , str') :
        (map
          (\x -> ( ((snd $ fst x) + length str'),snd x))
          $ filter
            (\x ->((fst $ fst x) /= 1) || ((snd x) == ",") || ((snd x) == ['\8869']) )
            $ zip
              (zip lst $ map sum  $ tail  $ L.inits lst)
              $  devideParentheses
                  $ filter
                    (`elem` (['0'..'z']++['(',')','\8594','\215','\8869','\960','\955','.',',',' ']))
                    (drop (length str')  str))

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

dtToArrow_selector :: DT.Selector -> ArrowSelector
dtToArrow_selector DT.Fst = Arrow_Fst
dtToArrow_selector DT.Snd = Arrow_Snd

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
arrow2DT (Arrow_Eq a b t) = DT.Eq (arrow2DT a) (arrow2DT b) (arrow2DT t)

-- | 入力されたDT.PretermをArrowTermに変換する
dtToArrow :: DT.Preterm -> Arrowterm
dtToArrow (DT.Type) = Conclusion $DT.Type
dtToArrow (DT.Var i) = Conclusion $ DT.Var i
dtToArrow (DT.Con i) = Conclusion $ DT.Con i
dtToArrow (DT.Not i) =
  Arrow [dtToArrow i] $Conclusion DT.Bot
dtToArrow (DT.Pi h t) =
  case dtToArrow t of
    Arrow env t' -> Arrow (env ++ [dtToArrow h]) t'
    t' -> Arrow [dtToArrow h] t'
dtToArrow (DT.Sigma h t) =
  Arrow_Sigma (dtToArrow h) (dtToArrow t)
dtToArrow (DT.App a b) =
  Arrow_App (dtToArrow a) (dtToArrow b)
dtToArrow (DT.Pair a b) =
  Arrow_Pair (dtToArrow a) (dtToArrow b)
dtToArrow (DT.Proj selector p) =
  Arrow_Proj (dtToArrow_selector selector) (dtToArrow p)
dtToArrow (DT.Lam p) =
  Arrow_Lam (dtToArrow p)
dtToArrow (DT.Eq a b t) =
  Arrow_Eq (dtToArrow a) (dtToArrow b) (dtToArrow t)
dtToArrow dt= Conclusion dt

arrowNotat :: Arrowterm -> Arrowterm
arrowNotat (Arrow [] a) = a
arrowNotat (Arrow a (Arrow b c)) = Arrow (b ++ a) c
arrowNotat a = a

shiftIndices :: Arrowterm -> Int -> Int -> Arrowterm
shiftIndices term d i= ((dtToArrow . DT.toDTT) $ UD.shiftIndices ((DT.toUDTT . arrow2DT) term) d i)

reduce :: Arrowterm -> Arrowterm
reduce = dtToArrow . DT.toDTT . UD.betaReduce . DT.toUDTT . arrow2DT

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
        con = foldl (\str ch-> if ('[' `elem`  str) && (length (filter ('[' ==) str) == length (filter (']' ==) str )) then str else str ++[ch]  ) "" str
        f = length con
    in
      if  null env
      then
        " ト " ++ str ++ " : " ++  show (Arrow env a_type)
      else
        (init $ tail $take f str)++" ト " ++ (drop (f + (length (" =>"::String))) str) ++ " : " ++ (drop (f + (length (" =>"::String))) $ show $ Arrow env a_type)

a2dtJudgement :: AJudgement -> J.Judgement
a2dtJudgement (AJudgement env aterm atype) = J.Judgement (map arrow2DT env) (arrow2DT aterm) (arrow2DT atype)

dt2aJudgement ::  J.Judgement -> AJudgement
dt2aJudgement (J.Judgement env dtterm dttype) = AJudgement (map dtToArrow env) (dtToArrow dtterm) (dtToArrow dttype)

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

aj :: TEnv -> DT.Preterm -> DT.Preterm -> AJudgement
aj env aterm atype =
  let arrow_env = map (dtToArrow . DT.toDTT . UD.betaReduce . DT.toUDTT) env
      arrow_term = (dtToArrow . DT.toDTT . UD.betaReduce . DT.toUDTT) aterm
      arrow_type = (dtToArrow . DT.toDTT . UD.betaReduce . DT.toUDTT) atype
  in AJudgement arrow_env arrow_term arrow_type


arrowSubst :: Arrowterm -- ^ origin
  -> Arrowterm -- ^ 代入内容
  -> Arrowterm -- ^ 代入先
  -> Arrowterm
arrowSubst term i m= dtToArrow $ subst (arrow2DT term) (arrow2DT i) (arrow2DT m)

downSide ::  J.Tree AJudgement -> AJudgement
downSide (J.T _ downside _)=  downside
downSide (J.Error' judgement _) = judgement

changeDownSide :: J.Tree AJudgement -> AJudgement -> J.Tree AJudgement
changeDownSide (J.T label downside upside) newdownside = J.T label newdownside upside
changeDownSide (J.Error' a t) newdownside =  J.Error' newdownside t

upSide ::  J.Tree AJudgement -> [J.Tree AJudgement]
upSide (J.T _ _ upside)=  upside
upSide (J.Error' _ _) = []

aTreeTojTree :: J.Tree AJudgement -> J.Tree J.Judgement
aTreeTojTree (J.T label downside upside)=J.T label (a2dtJudgement downside) (map aTreeTojTree upside)
aTreeTojTree (J.Error' judgement text) = J.Error' (a2dtJudgement judgement) text

jTreeToaTree :: J.Tree J.Judgement -> J.Tree AJudgement
jTreeToaTree (J.T label downside upside)=J.T label (dt2aJudgement downside) (map jTreeToaTree upside)
jTreeToaTree (J.Error' judgement text) = J.Error' (dt2aJudgement judgement) text

-- | generate free constraint from given word
genFreeCon ::  Arrowterm -- ^ term
  -> String -- ^ "hoge"
  ->  Arrowterm
genFreeCon term hoge =
  if isFreeCon term ( Conclusion $ DT.Con $ T.pack hoge)
    then
       Conclusion $ DT.Con $ T.pack hoge
    else
      genFreeCon term $hoge++hoge

-- | whether DT.Con "hoge" is free or not
isFreeCon ::  Arrowterm  -- ^ term
  ->  Arrowterm  -- ^ DT.Con "hoge"
  -> Bool
isFreeCon term con=
  let term' =  arrowSubst term con con
      term'' =  arrowSubst term ( Conclusion $ DT.Var 0) con
  in term' == term''

--形だけ比較
canBeSame :: Int ->   Arrowterm ->   Arrowterm -> Bool
canBeSame lim (  Conclusion (DT.Top)) (  Conclusion (DT.Top)) = True
canBeSame lim (  Conclusion (DT.Bot)) (  Conclusion (DT.Bot)) = True
canBeSame lim (  Conclusion (DT.Type)) (  Conclusion (DT.Type)) = True
canBeSame lim (  Conclusion (DT.Var anum)) (  Conclusion (DT.Var anum')) =
  anum == anum' || anum <= lim
canBeSame lim (  Conclusion (DT.Var anum)) _ =
  anum <= lim
canBeSame lim (  Arrow_Sigma a1 a2) (  Arrow_Sigma a1' a2')=
  canBeSame lim a1 a1' && canBeSame (lim+1) a2 a2'
canBeSame lim (  Arrow_App a1 a2) (  Arrow_App a1' a2')=
  canBeSame lim a1 a1' && canBeSame lim a2 a2'
canBeSame lim (  Arrow_Proj s a) (  Arrow_Proj s' a') =
  s == s' && canBeSame lim a a'
canBeSame lim (  Arrow_Pair a1 a2) (  Arrow_Pair a1' a2')=
  canBeSame lim a1 a1' && canBeSame lim a2 a2'
canBeSame lim (  Arrow_Lam a) (  Arrow_Lam a') = canBeSame lim a a'
canBeSame lim (  Arrow con a) (  Arrow con' a') =
  and $ map (\(num,(s,t)) -> canBeSame (lim+num) s t) $zip [0..] (zip (a:con) (a':con'))
  -- case length con - length con' of
  --   0 -> foldr (\a b -> b ) (canBeSame a a') []
  --   d -> []
canBeSame lim (  Arrow_Eq a b t) (  Arrow_Eq a' b' t') =
  canBeSame lim a a' && canBeSame lim b b' && canBeSame lim t t'
canBeSame lim other other' = False
