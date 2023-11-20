{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{-|
Copyright   : (c) Daisuke Bekki, 2023
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Implementation of Underspecified Dependent Type Theory (Bekki forthcoming).
-}
module DTS.UDTT (
  Preterm(..)
  ) where

import qualified Data.Text.Lazy as T      -- text
import qualified Data.List as L           -- base
import qualified DTS.UDTTdeBruijn as DB   -- lightblue
import qualified DTS.UDTTvarName as VN    -- lightblue

type Preterm = DB.Preterm DB.UDTT

