{-# LANGUAGE DataKinds #-}
module Zephyr.Packet.Jce.TroopNumber where
import Data.Int
import Zephyr.Utils.Jce
import Data.Word
import GHC.Generics

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
