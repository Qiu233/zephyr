{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Jce.PushMessageInfo where
import Zephyr.Utils.Jce
import Data.Int
import GHC.Generics
import qualified Data.ByteString.Lazy as B

data PushMessageInfo = PushMessageInfo {
    _from_uin         :: JceField Int64 0,
    _msg_time         :: JceField Int64 1,
    _msg_type         :: JceField Int16 2,
    _msg_seq          :: JceField Int16 3,
    _msg              :: JceField String 4,
    _real_msg_time    :: JceField Int32 5,
    _v_msg            :: JceField B.ByteString 6,
    _app_share_id     :: JceField Int64 7,
    _msg_cookies      :: JceField B.ByteString 8,
    _app_share_cookie :: JceField B.ByteString 9,
    _msg_uid          :: JceField Int64 10,
    _last_change_time :: JceField Int64 11,
    _from_inst_id     :: JceField Int64 14,
    _remark_of_sender :: JceField B.ByteString 15,
    _from_mobile      :: JceField String 16,
    _from_name        :: JceField String 17
} deriving (Show, Eq, Generic)
instance Jce PushMessageInfo