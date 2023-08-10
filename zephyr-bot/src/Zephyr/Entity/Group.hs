{-# LANGUAGE DuplicateRecordFields #-}
module Zephyr.Entity.Group where
import Data.Int
import Data.Word

data MemberPermission =
    Unknown
    | Owner
    | Admin
    | Member
    deriving (Eq, Show, Enum)

data GroupInfo = GroupInfo {
    _uin :: Int64,
    _code :: Int64,
    _name :: String,
    _owner_uin :: Int64,
    _member_count :: Word16,
    _max_member_count :: Word16
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