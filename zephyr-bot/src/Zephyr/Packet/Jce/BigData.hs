{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Zephyr.Packet.Jce.BigData where
import Zephyr.Utils.Jce.Generic
import Data.Int
import GHC.Generics
import qualified Data.ByteString.Lazy as B


data BigDataIPInfo = BigDataIPInfo {
    _type :: JceField Int64 0,
    _server :: JceField String 1,
    _port :: JceField Int64 2
} deriving (Show, Eq, Generic)
instance Jce BigDataIPInfo

data BigDataIPList = BigDataIPList {
    _service_type :: JceField Int64 0,
    _ip_list :: JceField [BigDataIPInfo] 1,
    _fragment_size :: JceField Int64 3
} deriving (Show, Eq, Generic)
instance Jce BigDataIPList

data BigDataChannel = BigDataChannel {
    _ip_lists :: JceField [BigDataIPList] 0,
    _sig_session :: JceField B.ByteString 1,
    _key_session :: JceField B.ByteString 2,
    _sig_uin :: JceField Int64 3,
    _connect_flag :: JceField Int32 4,
    _pb_buf :: JceField B.ByteString 5
} deriving (Show, Eq, Generic)
instance Jce BigDataChannel
