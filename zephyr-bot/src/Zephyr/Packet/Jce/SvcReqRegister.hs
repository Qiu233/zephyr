{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Jce.SvcReqRegister where
import Zephyr.Utils.Jce
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Data.Int
import Data.Word


data SvcReqRegister = SvcReqRegister {
    _uin            :: JceField Int64 0,
    _bid            :: JceField Int64 1,
    _conn_type      :: JceField Word8 2,
    _other          :: JceField String 3,
    _status         :: JceField Int32 4,
    _online_push    :: JceField Word8 5,
    _is_online      :: JceField Word8 6,
    _is_show_online :: JceField Word8 7,
    _kick_pc        :: JceField Word8 8,
    _kick_weak      :: JceField Word8 9,
    _timestamp      :: JceField Int64 10,
    _ios_version    :: JceField Int64 11,
    _net_type       :: JceField Word8 12,
    _build_ver      :: JceField String 13,
    _reg_type       :: JceField Word8 14,
    _dev_param      :: JceField B.ByteString 15,
    _guid           :: JceField B.ByteString 16,
    _locale_id      :: JceField Int32 17,
    _silent_push    :: JceField Word8 18,
    _dev_name       :: JceField String 19,
    _dev_type       :: JceField String 20,
    _os_ver         :: JceField String 21,
    _open_push      :: JceField Word8 22,
    _large_seq      :: JceField Int64 23,
    _last_watch_start_time :: JceField Int64 24,
    _old_sso_ip     :: JceField Int64 26,
    _new_sso_ip     :: JceField Int64 27,
    _channel_no     :: JceField String 28,
    _cp_id          :: JceField Int64 29,
    _vendor_name    :: JceField String 30,
    _vendor_os_name :: JceField String 31,
    _ios_idfa       :: JceField String 32,
    _b769           :: JceField B.ByteString 33,
    _is_set_status  :: JceField Word8 34,
    _server_buf     :: JceField B.ByteString 35,
    _set_mute       :: JceField Word8 36,
    _ext_online_status :: JceField Int64 38,
    _battery_status :: JceField Int32 39
} deriving (Show, Eq, Generic)
instance Jce SvcReqRegister