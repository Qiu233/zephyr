{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.PB.Data where
import Zephyr.ProtoLite
import Zephyr.ProtoLite.Aliases
import GHC.Generics


data SSOReserveField = SSOReserveField {
    _flag           :: ProtoFieldOptional Int32 9,
    _qimei          :: ProtoFieldOptional String 12,
    _newconn_flag   :: ProtoFieldOptional Int32 14,
    _uid            :: ProtoFieldOptional String 16,
    _imsi           :: ProtoFieldOptional Int32 18,
    _network_type   :: ProtoFieldOptional Int32 19,
    _ip_stack_type  :: ProtoFieldOptional Int32 20,
    _message_type   :: ProtoFieldOptional Int32 21,
    _sec_info       :: ProtoFieldOptional SSOSecureInfo 24,
    _sso_ip_origin  :: ProtoFieldOptional Int32 28
} deriving (Eq, Generic, Show)
instance ProtoBuf SSOReserveField

data SSOSecureInfo = SSOSecureInfo {
    _sec_sig            :: ProtoFieldOptional Bytes 1,
    _sec_device_token   :: ProtoFieldOptional Bytes 2,
    _sec_extra          :: ProtoFieldOptional Bytes 3
} deriving (Eq, Generic, Show)
instance ProtoBuf SSOSecureInfo

data DeviceInfo = DeviceInfo {
    _bootloader     :: ProtoFieldOptional String 1,
    _proc_version   :: ProtoFieldOptional String 2,
    _code_name      :: ProtoFieldOptional String 3,
    _incremental    :: ProtoFieldOptional UInt32 4,
    _fingerprint    :: ProtoFieldOptional String 5,
    _boot_id        :: ProtoFieldOptional String 6,
    _android_id     :: ProtoFieldOptional String 7,
    _base_band      :: ProtoFieldOptional String 8,
    _incremental2   :: ProtoFieldOptional UInt32 9
} deriving (Eq, Show, Generic)
instance ProtoBuf DeviceInfo

data Sub8A = Sub8A {
    _msg_info           :: ProtoFieldRepeated Sub8AMsgInfo 1,
    _app_id             :: ProtoFieldOptional Int32 2,
    _inst_id            :: ProtoFieldOptional Int32 3,
    _long_message_flag  :: ProtoFieldOptional Int32 4,
    _reserved           :: ProtoFieldOptional Bytes 5
} deriving (Eq, Show, Generic)
instance ProtoBuf Sub8A

data Sub8AMsgInfo = Sub8AMsgInfo {
    _from_uin   :: ProtoFieldOptional Int64 1,
    _to_uin     :: ProtoFieldOptional Int64 2,
    _msg_seq    :: ProtoFieldOptional Int32 3,
    _msg_uid    :: ProtoFieldOptional Int64 4,
    _msg_time   :: ProtoFieldOptional Int64 5,
    _msg_random :: ProtoFieldOptional Int32 6,
    _pkg_num    :: ProtoFieldOptional Int32 7,
    _pkg_index  :: ProtoFieldOptional Int32 8,
    _dev_seq    :: ProtoFieldOptional Int32 9
} deriving (Eq, Show, Generic)
instance ProtoBuf Sub8AMsgInfo

{-message SubB3 {
  int32 type = 1;
  SubB3AddFrdNotify msgAddFrdNotify = 2;
}-}

data SubB3 = SubB3 {
    _type               :: ProtoFieldOptional Int32 1,
    _msg_add_frd_notify :: ProtoFieldOptional SubB3AddFrdNotify 2
} deriving (Eq, Show, Generic)
instance ProtoBuf SubB3

{-message SubB3AddFrdNotify {
  int64 uin = 1;
  string nick = 5;
}-}

data SubB3AddFrdNotify = SubB3AddFrdNotify {
    _uin    :: ProtoFieldOptional Int64 1,
    _nick   :: ProtoFieldOptional String 5
} deriving (Eq, Show, Generic)
instance ProtoBuf SubB3AddFrdNotify

{-
message Sub44 {
  Sub44FriendSyncMsg friendSyncMsg = 1;
  Sub44GroupSyncMsg groupSyncMsg = 2;
}-}

data Sub44 = Sub44 {
    _friend_sync_msg    :: ProtoFieldOptional Sub44FriendSyncMsg 1,
    _group_sync_msg     :: ProtoFieldOptional Sub44GroupSyncMsg 2
} deriving (Eq, Show, Generic)
instance ProtoBuf Sub44

{-message Sub44FriendSyncMsg {
  int64 uin = 1;
  int64 fUin = 2;
  int32 processType = 3;
  int32 time = 4;
  int32 processFlag = 5;
  int32 sourceId = 6;
  int32 sourceSubId = 7;
  repeated string strWording = 8;
}-}

data Sub44FriendSyncMsg = Sub44FriendSyncMsg {
    _uin            :: ProtoFieldOptional Int64 1,
    _f_uin          :: ProtoFieldOptional Int64 2,
    _process_type   :: ProtoFieldOptional Int32 3,
    _time           :: ProtoFieldOptional Int32 4,
    _process_flag   :: ProtoFieldOptional Int32 5,
    _source_id      :: ProtoFieldOptional Int32 6,
    _source_sub_id  :: ProtoFieldOptional Int32 7,
    _str_wording    :: ProtoFieldRepeated String 8
} deriving (Eq, Show, Generic)
instance ProtoBuf Sub44FriendSyncMsg

{-message Sub44GroupSyncMsg {
  int32 msgType = 1;
  int64 msgSeq = 2;
  int64 grpCode = 3;
  int64 gaCode = 4;
  int64 optUin1 = 5;
  int64 optUin2 = 6;
  bytes msgBuf = 7;
  bytes authKey = 8;
  int32 msgStatus = 9;
  int64 actionUin = 10;
  int64 actionTime = 11;
  int32 curMaxMemCount = 12;
  int32 nextMaxMemCount = 13;
  int32 curMemCount = 14;
  int32 reqSrcId = 15;
  int32 reqSrcSubId = 16;
  int32 inviterRole = 17;
  int32 extAdminNum = 18;
  int32 processFlag = 19;
}-}

data Sub44GroupSyncMsg = Sub44GroupSyncMsg {
    _msg_type   :: ProtoFieldOptional Int32 1,
    _msg_seq    :: ProtoFieldOptional Int64 2,
    _grp_code   :: ProtoFieldOptional Int64 3,
    _ga_code    :: ProtoFieldOptional Int64 4,
    _opt_uin1   :: ProtoFieldOptional Int64 5,
    _opt_uin2   :: ProtoFieldOptional Int64 6,
    _msg_buf    :: ProtoFieldOptional Bytes 7,
    _auth_key   :: ProtoFieldOptional Bytes 8,
    _msg_status :: ProtoFieldOptional Int32 9,
    _action_uin :: ProtoFieldOptional Int64 10,
    _action_time:: ProtoFieldOptional Int64 11,
    _cur_max_mem_count  :: ProtoFieldOptional Int32 12,
    _next_max_mem_count :: ProtoFieldOptional Int32 13,
    _cur_mem_count      :: ProtoFieldOptional Int32 14,
    _req_src_id         :: ProtoFieldOptional Int32 15,
    _req_src_sub_id     :: ProtoFieldOptional Int32 16,
    _inviter_role       :: ProtoFieldOptional Int32 17,
    _ext_admin_num      :: ProtoFieldOptional Int32 18,
    _process_flag       :: ProtoFieldOptional Int32 19
} deriving (Eq, Show, Generic)
instance ProtoBuf Sub44GroupSyncMsg

newtype SubD4 = SubD4 {
    _uin :: ProtoFieldOptional Int64 1
} deriving (Eq, Show, Generic)
instance ProtoBuf SubD4
