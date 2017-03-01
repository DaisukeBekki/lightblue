{-|
Description : TeX interface
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}
module Interface.TeX (
  Typeset(..),
  scaleboxsize
  ) where

import qualified Data.Text.Lazy as T
import qualified Data.Ratio as R
import qualified Data.Fixed as F

-- | `Typeset` is a class of types whose terms can be translated into a TeX source (in Data.Text.Lazy). 
-- `toTeX` method translates a Typeset term into a TeX source (in Data.Text.Lazy).
class Typeset a where
  toTeX :: a -> T.Text

-- | determines a scale of a parsing result (a syntactic derivation or an SR) given to x in \scalebox{x}{...}, according to the length of the sentence to be parsed.
scaleboxsize :: T.Text -> T.Text
scaleboxsize sentence = 
  let len = (fromIntegral $ T.length sentence) in
  T.pack $ show (fromRational (toEnum (max (1000-(len*7)) 300) R.% toEnum 1000)::F.Fixed F.E2)
