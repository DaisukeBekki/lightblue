{-|
Description : Data.Text.Lazy interface
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module Interface.Text (
  SimpleText(..)
  ) where

import qualified Data.Text.Lazy as T

-- | A class with a 'toText' method that translates a SimpleText term into a simple text notation.
class SimpleText a where
  toText :: a -> T.Text
