{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Jce.DelMsgInfo where
import Zephyr.Utils.Jce.Generic
import Data.Int
import qualified Data.ByteString.Lazy as B
import GHC.Generics

data DelMsgInfo = DelMsgInfo {
    _from_uin    :: JceField Int64 0,
    _msg_time    :: JceField Int64 1,
    _msg_seq     :: JceField Int16 2,
    _msg_cookies :: JceField B.ByteString 3,
    _cmd         :: JceField Int16 4,
    _msg_type    :: JceField Int64 5,
    _app_id      :: JceField Int64 6,
    _send_time   :: JceField Int64 7,
    _sso_seq     :: JceField Int32 8,
    _sso_ip      :: JceField Int32 9,
    _client_ip   :: JceField Int32 10
} deriving (Show, Eq, Generic)
instance Jce DelMsgInfo