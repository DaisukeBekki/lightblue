
import DTS.Prover.TypeChecker as Ty
import qualified Data.Text.Lazy as T      -- text
import DTS.UDTT as UD
main = do
  putStrLn $ show $ Ty.proofSearch [UD.Unit] [((T.pack "taro"),UD.Con (T.pack "Entity"))] $ UD.Con (T.pack "Entity")
   putStrLn ""
  putStrLn $ show $ Ty.proofSearch [UD.Unit] [((T.pack "taro"),UD.Con (T.pack "Entity")),((T.pack "hanako"),UD.Con (T.pack "Entity"))] $ UD.Pi (UD.Con (T.pack "Entity")) (UD.Con (T.pack "Entity"))
  
