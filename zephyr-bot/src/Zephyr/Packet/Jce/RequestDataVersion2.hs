{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Zephyr.Packet.Jce.RequestDataVersion2 where

import Zephyr.Packet.Jce.JceCommon
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Jce.JceMap

newtype RequestDataVersion2 = RequestDataVersion2 {
    _map :: JceField (JceMap String (JceMap String B.ByteString)) 0
} deriving (Show, Eq, Generic)
instance Jce RequestDataVersion2