{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.PB.OIDB.OIDB0x88D where

import Zephyr.ProtoLite
import Zephyr.ProtoLite.Aliases
import GHC.Generics

data D88DRspBody = D88DRspBody {
    _rsp_group_info   :: ProtoFieldRepeated RspGroupInfo 1,
    _str_error_info   :: ProtoFieldOptional String 2
} deriving (Eq, Show, Generic)
instance ProtoBuf D88DRspBody

data RspGroupInfo = RspGroupInfo {
    _group_code   :: ProtoFieldOptional UInt64 1,
    _result       :: ProtoFieldOptional UInt32 2,
    _group_info   :: ProtoFieldOptional D88DGroupInfo 3
} deriving (Eq, Show, Generic)
instance ProtoBuf RspGroupInfo

data D88DGroupInfo = D88DGroupInfo {
    _group_owner                :: ProtoFieldOptional UInt64 1,
    _group_create_time          :: ProtoFieldOptional UInt32 2,
    _group_flag                 :: ProtoFieldOptional UInt32 3,
    _group_flag_ext             :: ProtoFieldOptional UInt32 4,
    _group_member_max_num       :: ProtoFieldOptional UInt32 5,
    _group_member_num           :: ProtoFieldOptional UInt32 6,
    _group_option               :: ProtoFieldOptional UInt32 7,
    _group_class_ext            :: ProtoFieldOptional UInt32 8,
    _group_special_class        :: ProtoFieldOptional UInt32 9,
    _group_level                :: ProtoFieldOptional UInt32 10,
    _group_face                 :: ProtoFieldOptional UInt32 11,
    _group_default_page         :: ProtoFieldOptional UInt32 12,
    _group_info_seq             :: ProtoFieldOptional UInt32 13,
    _group_roaming_time         :: ProtoFieldOptional UInt32 14,
    _group_name                 :: ProtoFieldOptional String 15,
    _group_memo                 :: ProtoFieldOptional Bytes 16,
    _group_finger_memo          :: ProtoFieldOptional Bytes 17,
    _group_class_text           :: ProtoFieldOptional Bytes 18,
    _group_alliance_code        :: ProtoFieldRepeated UInt32 19,
    _group_extra_aadm_num       :: ProtoFieldOptional UInt32 20,
    _group_uin                  :: ProtoFieldOptional UInt64 21,
    _group_cur_msg_seq          :: ProtoFieldOptional UInt32 22,
    _group_last_msg_time        :: ProtoFieldOptional UInt32 23,
    _group_question             :: ProtoFieldOptional Bytes 24,
    _group_answer               :: ProtoFieldOptional Bytes 25,
    _group_visitor_max_num      :: ProtoFieldOptional UInt32 26,
    _group_visitor_cur_num      :: ProtoFieldOptional UInt32 27,
    _level_name_seq             :: ProtoFieldOptional UInt32 28,
    _group_admin_max_num        :: ProtoFieldOptional UInt32 29,
    _group_aio_skin_timestamp   :: ProtoFieldOptional UInt32 30,
    _group_board_skin_timestamp :: ProtoFieldOptional UInt32 31,
    _group_aio_skin_url         :: ProtoFieldOptional Bytes 32,
    _group_board_skin_url       :: ProtoFieldOptional Bytes 33,
    _group_cover_skin_timestamp :: ProtoFieldOptional UInt32 34,
    _group_cover_skin_url       :: ProtoFieldOptional Bytes 35,
    _group_grade                :: ProtoFieldOptional UInt32 36,
    _active_member_num          :: ProtoFieldOptional UInt32 37,
    _certification_type         :: ProtoFieldOptional UInt32 38,
    _certification_text         :: ProtoFieldOptional Bytes 39,
    _group_rich_finger_memo     :: ProtoFieldOptional Bytes 40,
    -- _tag_record                 :: ProtoFieldRepeated D88DTagRecord 41,
    -- _group_geo_info             :: ProtoFieldOptional D88DGroupGeoInfo 42,
    _head_portrait_seq          :: ProtoFieldOptional UInt32 43,
    -- _msg_head_portrait          :: ProtoFieldOptional D88DGroupHeadPortrait 44,
    _shutup_timestamp           :: ProtoFieldOptional UInt32 45,
    _shutup_timestamp_me        :: ProtoFieldOptional UInt32 46,
    _create_source_flag         :: ProtoFieldOptional UInt32 47,
    _cmduin_msg_seq             :: ProtoFieldOptional UInt32 48,
    _cmduin_join_time           :: ProtoFieldOptional UInt32 49,
    _cmduin_uin_flag            :: ProtoFieldOptional UInt32 50,
    _cmduin_flag_ex             :: ProtoFieldOptional UInt32 51,
    _cmduin_new_mobile_flag     :: ProtoFieldOptional UInt32 52,
    _cmduin_read_msg_seq        :: ProtoFieldOptional UInt32 53,
    _cmduin_last_msg_time       :: ProtoFieldOptional UInt32 54,
    _group_type_flag            :: ProtoFieldOptional UInt32 55,
    _app_privilege_flag         :: ProtoFieldOptional UInt32 56,
    -- _st_group_ex_info           :: ProtoFieldOptional D88DGroupExInfoOnly 57,
    _group_sec_level            :: ProtoFieldOptional UInt32 58,
    _group_sec_level_info       :: ProtoFieldOptional UInt32 59,
    _cmduin_privilege           :: ProtoFieldOptional UInt32 60,
    _poid_info                  :: ProtoFieldOptional Bytes 61,
    _cmduin_flag_ex2            :: ProtoFieldOptional UInt32 62,
    _conf_uin                   :: ProtoFieldOptional UInt64 63,
    _conf_max_msg_seq           :: ProtoFieldOptional UInt32 64,
    _conf_to_group_time         :: ProtoFieldOptional UInt32 65,
    _password_redbag_time       :: ProtoFieldOptional UInt32 66,
    _subscription_uin           :: ProtoFieldOptional UInt64 67,
    _member_list_change_seq     :: ProtoFieldOptional UInt32 68,
    _member_card_seq            :: ProtoFieldOptional UInt32 69,
    _root_id                    :: ProtoFieldOptional UInt64 70,
    _parent_id                  :: ProtoFieldOptional UInt64 71,
    _team_seq                   :: ProtoFieldOptional UInt32 72,
    _history_msg_begin_time     :: ProtoFieldOptional UInt64 73,
    _invite_no_auth_num_limit   :: ProtoFieldOptional UInt64 74,
    _cmduin_history_msg_seq     :: ProtoFieldOptional UInt32 75,
    _cmduin_join_msg_seq        :: ProtoFieldOptional UInt32 76,
    _group_flag_ext3            :: ProtoFieldOptional UInt32 77,
    _group_open_appid           :: ProtoFieldOptional UInt32 78,
    _is_conf_group              :: ProtoFieldOptional UInt32 79,
    _is_modify_conf_group_face  :: ProtoFieldOptional UInt32 80,
    _is_modify_conf_group_name  :: ProtoFieldOptional UInt32 81,
    _no_finger_open_flag        :: ProtoFieldOptional UInt32 82,
    _no_code_finger_open_flag   :: ProtoFieldOptional UInt32 83
} deriving (Eq, Show, Generic)
instance ProtoBuf D88DGroupInfo

data D88DReqBody = D88DReqBody {
    _app_id              :: ProtoFieldOptional UInt32 1,
    _req_group_info     :: ProtoFieldRepeated ReqGroupInfo 2,
    _pc_client_version  :: ProtoFieldOptional UInt32 3
} deriving (Eq, Show, Generic)
instance ProtoBuf D88DReqBody


data ReqGroupInfo = ReqGroupInfo {
  _group_code               :: ProtoFieldOptional UInt64 1,
  _st_group_info            :: ProtoFieldOptional D88DGroupInfo 2,
  _last_get_group_name_time :: ProtoFieldOptional UInt32 3
} deriving (Eq, Show, Generic)
instance ProtoBuf ReqGroupInfo