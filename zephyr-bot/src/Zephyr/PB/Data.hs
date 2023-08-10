{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.PB.Data where
import ProtoLite
import ProtoLite.Aliases
import GHC.Generics


data SSOReserveField = SSOReserveField {
    _flag           :: ProtoFieldOptional VInt32 9,
    _qimei          :: ProtoFieldOptional String 12,
    _newconn_flag   :: ProtoFieldOptional VInt32 14,
    _uid            :: ProtoFieldOptional String 16,
    _imsi           :: ProtoFieldOptional VInt32 18,
    _network_type   :: ProtoFieldOptional VInt32 19,
    _ip_stack_type  :: ProtoFieldOptional VInt32 20,
    _message_type   :: ProtoFieldOptional VInt32 21,
    _sec_info       :: ProtoFieldOptional SSOSecureInfo 24,
    _sso_ip_origin  :: ProtoFieldOptional VInt32 28
} deriving (Generic, Show)
instance ProtoBuf SSOReserveField

data SSOSecureInfo = SSOSecureInfo {
    _sec_sig            :: ProtoFieldOptional Bytes 1,
    _sec_device_token   :: ProtoFieldOptional Bytes 2,
    _sec_extra          :: ProtoFieldOptional Bytes 3
} deriving (Generic, Show)
instance ProtoBuf SSOSecureInfo

data DeviceInfo = DeviceInfo {
    _bootloader     :: ProtoFieldOptional String 1,
    _proc_version   :: ProtoFieldOptional String 2,
    _code_name      :: ProtoFieldOptional String 3,
    _incremental    :: ProtoFieldOptional VUInt32 4,
    _fingerprint    :: ProtoFieldOptional String 5,
    _boot_id        :: ProtoFieldOptional String 6,
    _android_id     :: ProtoFieldOptional String 7,
    _base_band      :: ProtoFieldOptional String 8,
    _incremental2   :: ProtoFieldOptional VUInt32 9
} deriving (Generic)
instance ProtoBuf DeviceInfo