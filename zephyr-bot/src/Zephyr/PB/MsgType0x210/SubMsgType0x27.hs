{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.PB.MsgType0x210.SubMsgType0x27 where
import Zephyr.ProtoLite.Aliases
import Zephyr.ProtoLite
import GHC.Generics

newtype SubMsg0X27Body = SubMsg0X27Body {
    _mod_infos :: ProtoFieldRepeated ForwardBody 1
} deriving (Eq, Show, Generic)
instance ProtoBuf SubMsg0X27Body

data ForwardBody = ForwardBody {
    _mod_group_profile :: ProtoFieldOptional ModGroupProfile 12,
    _del_friend :: ProtoFieldOptional DelFriend 14
} deriving (Eq, Show, Generic)
instance ProtoBuf ForwardBody

{-message ModGroupProfile {
  optional uint64 groupUin = 1;
  repeated GroupProfileInfo groupProfileInfos = 2;
  optional uint64 groupCode = 3;
  optional uint64 cmdUin = 4;
}-}

data ModGroupProfile = ModGroupProfile {
    _group_uin :: ProtoFieldOptional UInt64 1,
    _group_profile_infos :: ProtoFieldRepeated GroupProfileInfo 2,
    _group_code :: ProtoFieldOptional UInt64 3,
    _cmd_uin :: ProtoFieldOptional UInt64 4
} deriving (Eq, Show, Generic)
instance ProtoBuf ModGroupProfile

{-message GroupProfileInfo {
  optional uint32 field = 1;
  optional bytes value = 2;
}
-}

data GroupProfileInfo = GroupProfileInfo {
    _field :: ProtoFieldOptional UInt32 1,
    _value :: ProtoFieldOptional String 2
} deriving (Eq, Show, Generic)
instance ProtoBuf GroupProfileInfo

{-message DelFriend {
  repeated uint64 uins = 1;
}-}

newtype DelFriend = DelFriend {
    _uins :: ProtoFieldRepeated UInt64 1
} deriving (Eq, Show, Generic)
instance ProtoBuf DelFriend