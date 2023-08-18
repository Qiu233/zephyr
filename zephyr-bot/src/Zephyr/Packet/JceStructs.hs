{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.JceStructs where
import Zephyr.Jce
import Data.Int
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Zephyr.Jce.JceMap
import Data.Word

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

data FileStorageServerInfo = FileStorageServerInfo {
    _server :: JceField String 1,
    _port :: JceField Int32 2
} deriving (Show, Eq, Generic)
instance Jce FileStorageServerInfo


data FileStoragePushFSSvcList = FileStoragePushFSSvcList {
    _upload_list :: JceField [FileStorageServerInfo] 0,
    _pic_download_list :: JceField [FileStorageServerInfo] 1,
    _gpic_download_list :: JceField [FileStorageServerInfo] 2,
    _qzone_proxy_service_list :: JceField [FileStorageServerInfo] 3,
    _url_encode_service_list :: JceField [FileStorageServerInfo] 4,
    _big_data_channel :: JceField BigDataChannel 5,
    _vip_emotion_list :: JceField [FileStorageServerInfo] 6,
    _c2c_pic_down_list :: JceField [FileStorageServerInfo] 7,
    _ptt_list :: JceField B.ByteString 10
} deriving (Show, Eq, Generic)
instance Jce FileStoragePushFSSvcList

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

newtype RequestDataVersion2 = RequestDataVersion2 {
    _map :: JceField (JceMap String (JceMap String B.ByteString)) 0
} deriving (Show, Eq, Generic)
instance Jce RequestDataVersion2

newtype RequestDataVersion3 = RequestDataVersion3 {
    _map :: JceField (JceMap String B.ByteString) 0
} deriving (Show, Eq, Generic)
instance Jce RequestDataVersion3

data RequestPacket = RequestPacket{
    _i_version :: JceField Int16 1,
    _c_packet_type :: JceField Word8 2,
    _i_message_type :: JceField Int32 3,
    _i_request_id :: JceField Int32 4,
    _s_servant_name :: JceField String 5,
    _s_func_name :: JceField String 6,
    _s_buffer :: JceField B.ByteString 7,
    _i_timeout :: JceField Int32 8,
    _context :: JceField (JceMap String String) 9,
    _status :: JceField (JceMap String String) 10
} deriving (Show, Eq, Generic)
instance Jce RequestPacket

data SsoServerInfo = SsoServerInfo {
    _server :: JceField String 1,
    _port :: JceField Int32 2,
    _location :: JceField String 8
} deriving (Show, Eq, Generic)
instance Jce SsoServerInfo

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

data SvcRespPushMsg a = SvcRespPushMsg {
    _uin          :: JceField Int64 0,
    _del_infos    :: JceField [a] 1,
    _svr_ip       :: JceField Int32 2,
    _push_token   :: JceField B.ByteString 3,
    _service_type :: JceField Int32 4
} deriving (Show, Eq, Generic)
instance Jce a => Jce (SvcRespPushMsg a)

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

data TroopListRequest = TroopListRequest {
    _uin :: JceField Int64 0,
    _get_msf_msg_flag :: JceField Word8 1,
    _cookies :: JceField B.ByteString 2,
    _group_info :: JceField [Int64] 3,
    _group_flag_ext :: JceField Word8 4,
    _version :: JceField Int32 5,
    _company_id :: JceField Int64 6,
    _version_num :: JceField Int64 7,
    _get_long_group_name :: JceField Word8 8
} deriving (Show, Generic)
instance Jce TroopListRequest


data TroopNumber = TroopNumber {
    _group_uin :: JceField Int64 0,
    _group_code :: JceField Int64 1,
    _flag :: JceField Word8 2,
    _group_info_seq :: JceField Int64 3,
    _group_name :: JceField String 4,
    _group_memo :: JceField String 5,
    _group_flag_ext :: JceField Int64 6,
    _group_rank_seq :: JceField Int64 7,
    _certification_type :: JceField Int64 8,
    _shut_up_timestamp :: JceField Int64 9,
    _my_shut_up_timestamp :: JceField Int64 10,
    _cmd_uin_uin_flag :: JceField Int64 11,
    _additional_flag :: JceField Int64 12,
    _group_type_flag :: JceField Int64 13,
    _group_sec_type :: JceField Int64 14,
    _group_sec_type_info :: JceField Int64 15,
    _group_class_ext :: JceField Int64 16,
    _app_privilege_flag :: JceField Int64 17,
    _subscription_uin :: JceField Int64 18,
    _member_num :: JceField Int64 19,
    _member_num_seq :: JceField Int64 20,
    _member_card_seq :: JceField Int64 21,
    _group_flag_ext3 :: JceField Int64 22,
    _group_owner_uin :: JceField Int64 23,
    _is_conf_group :: JceField Word8 24,
    _is_modify_conf_group_face :: JceField Word8 25,
    _is_modify_conf_group_name :: JceField Word8 26,
    _cmd_uin_join_time :: JceField Int64 27,
    _company_id :: JceField Int64 28,
    _max_group_member_num :: JceField Int64 29,
    _cmd_uin_group_mask :: JceField Int64 30,
    _guild_app_id :: JceField Int64 31,
    _guild_sub_type :: JceField Int64 32,
    _cmd_uin_ringtone_id :: JceField Int64 33,
    _cmd_uin_flag_ex2 :: JceField Int64 34
} deriving (Show, Generic)
instance Jce TroopNumber
