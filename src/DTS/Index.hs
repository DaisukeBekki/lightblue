
{-|
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module DTS.Index (
  Indexed(..)
  , sIndex
  , uIndex
  , xIndex
  , eIndex
  , aspIndex
  , initializeIndex
  ) where

import qualified Control.Applicative as M -- base
import qualified Control.Monad as M       -- base


{- Initializing or Re-indexing of vars and @s -}

-- | Indexed monad controls indices to be attached to preterms.  Arguments correspond to:
-- |   s for variables for xxx
-- |   u for variables for propositions
-- |   x for variables for entities
-- |   e for variables for eventualities
newtype Indexed a = Indexed { indexing :: Int -> Int -> Int -> Int -> Int -> (a,Int,Int,Int,Int,Int) }

instance Monad Indexed where
  return m = Indexed (\s u x e a -> (m,s,u,x,e,a))
  (Indexed m) >>= f = Indexed (\s u x e a -> let (m',s',u',x',e',a') = m s u x e a;
                                                   (Indexed n) = f m';
                                               in
                                               n s' u' x' e' a')

instance Functor Indexed where
  fmap = M.liftM

instance M.Applicative Indexed where
  pure = return
  (<*>) = M.ap

-- | A sequential number for variable names (i.e. x_1, x_2, ...) in a context
sIndex :: Indexed Int
sIndex = Indexed (\s u x e a -> (s,s+1,u,x,e,a))

uIndex :: Indexed Int
uIndex = Indexed (\s u x e a -> (u,s,u+1,x,e,a))

xIndex :: Indexed Int
xIndex = Indexed (\s u x e a -> (x,s,u,x+1,e,a))

eIndex :: Indexed Int
eIndex = Indexed (\s u x e a -> (e,s,u,x,e+1,a))

aspIndex :: Indexed Int
aspIndex = Indexed (\s u x e a -> (a,s,u,x,e,a+1))

-- | re-assigns sequential indices to all asperands that appear in a given preterm.
initializeIndex :: Indexed a -> a
initializeIndex (Indexed m) = let (m',_,_,_,_,_) = m 0 0 0 0 0 in m'
