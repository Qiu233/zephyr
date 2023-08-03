{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Zephyr.Packet.Jce.SvcRespRegister where

import Zephyr.Packet.Jce.JceCommon
import qualified Data.ByteString.Lazy as B

data SvcRespRegister = SvcRespRegister {
    _uin                            :: JceField Int64 0,
    _bid                            :: JceField Int64 1,
    _reply_code                     :: JceField Word8 2,
    _result                         :: JceField String 3,
    _server_time                    :: JceField Int64 4,
    _log_qq                         :: JceField Word8 5,
    _need_kik                       :: JceField Word8 6,
    _update_flag                    :: JceField Word8 7,
    _timestamp                      :: JceField Int64 8,
    _crash_flag                     :: JceField Word8 9,
    _client_ip                      :: JceField String 10,
    _client_port                    :: JceField Int32 11,
    _hello_interval                 :: JceField Int32 12,
    _large_seq                      :: JceField Int32 13,
    _large_seq_update               :: JceField Word8 14,
    _d769_rsp_body                  :: JceField B.ByteString 15,
    _status                         :: JceField Int32 16,
    _ext_online_status              :: JceField Int64 17,
    _client_battery_get_interval    :: JceField Int64 18,
    _client_auto_status_interval    :: JceField Int64 19

} deriving (Show, Eq, Generic)
instance Jce SvcRespRegister
