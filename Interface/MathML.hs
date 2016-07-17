{-|
Description : MathML interface
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}
module Interface.MathML (
  MathML(..)
  ) where

import qualified Data.Text.Lazy as T

-- | `Math` is a class of types whose terms can be translated into a MathML source (in Data.Text.Lazy). 
-- `toMathML` method translates a Typeset term into a TeX source (in Data.Text.Lazy).
class MathML a where
  toMathML :: a -> T.Text

