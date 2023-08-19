{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.PB.CMD0x6FF.SubCMD0x501 where
import Zephyr.ProtoLite
import Zephyr.ProtoLite.Aliases
import GHC.Generics

newtype C501ReqBody = C501ReqBody {
    _req_body :: ProtoField (Optional SubCmd0X501ReqBody) 1281
} deriving (Show, Generic)
instance ProtoBuf C501ReqBody

newtype C501RspBody = C501RspBody {
    _rsp_body :: ProtoField (Optional SubCmd0X501RspBody) 1281
} deriving (Show, Generic)
instance ProtoBuf C501RspBody

data SubCmd0X501ReqBody = SubCmd0X501ReqBody {
    _uin               :: ProtoFieldOptional VUInt64 1,
    _idc_id            :: ProtoFieldOptional VUInt32 2,
    _appid             :: ProtoFieldOptional VUInt32 3,
    _login_sig_type    :: ProtoFieldOptional VUInt32 4,
    _login_sig_ticket  :: ProtoFieldOptional Bytes   5,
    _request_flag      :: ProtoFieldOptional VUInt32 6,
    _service_types     :: ProtoFieldRepeated VUInt32 7,
    _bid               :: ProtoFieldOptional VUInt32 8
} deriving (Show, Generic)
instance ProtoBuf SubCmd0X501ReqBody

data SubCmd0X501RspBody = SubCmd0X501RspBody {
    _c501_sig_session :: ProtoFieldOptional Bytes 1,
    _c501_session_key :: ProtoFieldOptional Bytes 2,
    _c501_addrs       :: ProtoFieldRepeated SrvAddrs 3
} deriving (Show, Generic)
instance ProtoBuf SubCmd0X501RspBody

data SrvAddrs = SrvAddrs {
    _service_type  :: ProtoFieldOptional VUInt32   1,
    _addrs :: ProtoFieldRepeated SrvIPAddr 2
} deriving (Show, Generic)
instance ProtoBuf SrvAddrs

data SrvIPAddr = SrvIPAddr {
    _type :: ProtoFieldOptional VUInt32 1,
    _ip   :: ProtoFieldOptional Fixed32 2,
    _port :: ProtoFieldOptional VUInt32 3,
    _area :: ProtoFieldOptional VUInt32 4
} deriving (Show, Generic)
instance ProtoBuf SrvIPAddr