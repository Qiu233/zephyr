{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Jce.RequestDataVersion3 where

import Zephyr.Packet.Jce.JceCommon
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Jce.JceMap

newtype RequestDataVersion3 = RequestDataVersion3 {
    _map :: JceField (JceMap String B.ByteString) 0
} deriving (Show, Eq, Generic)
instance Jce RequestDataVersion3