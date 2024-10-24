{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}

{-|
Module      : DTS.UDTTwithName
Copyright   : (c) Daisuke Bekki, 2024
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Indexed Monad for sequential renaming of variiables.
-}

module DTS.Index (
  Indexed(..)
  , sIndex
  , uIndex
  , xIndex
  , eIndex
  , initializeIndex
  ) where

import qualified Control.Applicative as M --base
import qualified Control.Monad as M       --base

{- Initializing or Re-indexing of vars -}

-- | Indexed monad controls indices to be attached to preterms.  Arguments correspond to:
-- |   u for variables for propositions
-- |   x for variables for entities
-- |   e for variables for eventualities
newtype Indexed a = Indexed { indexing :: Int -> Int -> Int -> Int -> (a,Int,Int,Int,Int) }

instance Monad Indexed where
  return m = Indexed (\s u x e -> (m,s,u,x,e))
  (Indexed m) >>= f = Indexed (\s u x e -> let (m',s',u',x',e') = m s u x e;
                                                   (Indexed n) = f m';
                                               in
                                               n s' u' x' e')

instance Functor Indexed where
  fmap = M.liftM

instance M.Applicative Indexed where
  pure = return
  (<*>) = M.ap

-- | A sequential number for variable names (i.e. x_1, x_2, ...) in a context
sIndex :: Indexed Int
sIndex = Indexed (\s u x e -> (s,s+1,u,x,e))

uIndex :: Indexed Int
uIndex = Indexed (\s u x e -> (u,s,u+1,x,e))

xIndex :: Indexed Int
xIndex = Indexed (\s u x e -> (x,s,u,x+1,e))

eIndex :: Indexed Int
eIndex = Indexed (\s u x e -> (e,s,u,x,e+1))

-- | re-assigns sequential indices to all asperands that appear in a given preterm.
initializeIndex :: Indexed a -> a
initializeIndex (Indexed m) = let (m',_,_,_,_) = m 0 0 0 0 in m'
