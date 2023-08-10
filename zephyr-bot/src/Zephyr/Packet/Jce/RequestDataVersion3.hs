{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Jce.RequestDataVersion3 where

import Zephyr.Utils.Jce
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Jce.JceMap

newtype RequestDataVersion3 = RequestDataVersion3 {
    _map :: JceField (JceMap String B.ByteString) 0
} deriving (Show, Eq, Generic)
instance Jce RequestDataVersion3