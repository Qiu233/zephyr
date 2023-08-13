{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Zephyr.Core.Entity.Group where
import Data.Int
import Data.Word

data MemberPermission =
    MP_Unknown
    | MP_Owner
    | MP_Admin
    | MP_Member
    deriving (Eq, Show, Enum)

data GroupInfo = GroupInfo {
    _uin :: Int64,
    _code :: Int64,
    _name :: String,
    _owner_uin :: Int64,
    _member_count :: Word16,
    _max_member_count :: Word16
} deriving (Eq, Show)

data GroupInfoDetailed = GroupInfoDetailed {
    _basic_info :: GroupInfo,
    _create_time :: Word32,
    _group_level :: Word32,
    _last_msg_seq :: Int64
} deriving (Eq, Show)

data GroupMemberInfo = GroupMemberInfo {
    _uin :: Int64,
    _nickname :: String,
    _cardname :: String,
    _join_time :: Int64,
    _last_speak_time :: Int64,
    _special_title :: String,
    _shutup_timestamp :: Int64,
    _permission :: MemberPermission,
    _level :: Word16,
    _gender :: Word8
} deriving (Eq, Show)