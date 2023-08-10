{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Jce.RequestDataVersion2 where

import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Jce.JceMap
import Zephyr.Utils.Jce
import GHC.Generics

newtype RequestDataVersion2 = RequestDataVersion2 {
    _map :: JceField (JceMap String (JceMap String B.ByteString)) 0
} deriving (Show, Eq, Generic)
instance Jce RequestDataVersion2