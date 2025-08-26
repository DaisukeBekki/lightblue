{-# LANGUAGE OverloadedStrings #-}
module DTS.Prover.Oracle (oracle) where

import qualified Data.List as L 

import qualified DTS.DTTdeBruijn as DdB
import qualified DTS.Prover.Wani.Arrowterm as A
import qualified Interface.Tree as UDT
import qualified DTS.QueryTypes as QT

import qualified DTS.Prover.Wani.WaniBase as B 

import qualified Debug.Trace as D

oracle :: (DdB.ConName,DdB.ConName) -> IO Bool
oracle ("udon","noodle") = return True
oracle (small,big) = return False