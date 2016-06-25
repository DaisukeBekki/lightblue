{-|
Description : TeX interface
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}
module Interface.TeX (
  Typeset(..)
  ) where

import qualified Data.Text.Lazy as T

-- | `Typeset` is a class of types whose terms can be translated into a TeX source (in Data.Text.Lazy). 
-- `toTeX` method translates a Typeset term into a TeX source (in Data.Text.Lazy).
class Typeset a where
  toTeX :: a -> T.Text

