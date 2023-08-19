{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.PB.Msg.TextMsgExt where
import Zephyr.ProtoLite
import Zephyr.ProtoLite.Aliases
import GHC.Generics

data ExtChannelInfo = ExtChannelInfo {
    _guild_id              :: ProtoFieldOptional UInt64 1,
    _channel_id            :: ProtoFieldOptional UInt64 2
} deriving (Eq, Show, Generic)
instance ProtoBuf ExtChannelInfo

data TextResvAttr = TextResvAttr {
    _wording               :: ProtoFieldOptional Bytes 1,
    _text_analysis_result  :: ProtoFieldOptional UInt32 2,
    _at_type               :: ProtoFieldOptional UInt32 3,
    _at_member_uin         :: ProtoFieldOptional UInt64 4,
    _at_member_tiny_id     :: ProtoFieldOptional UInt64 5,
    _at_channel_info       :: ProtoFieldOptional ExtChannelInfo 8
} deriving (Eq, Show, Generic)
instance ProtoBuf TextResvAttr

data ExtRoleInfo = ExtRoleInfo {
    _id                    :: ProtoFieldOptional UInt64 1,
    _info                  :: ProtoFieldOptional Bytes 2,
    _flag                  :: ProtoFieldOptional UInt32 3
} deriving (Eq, Show, Generic)
instance ProtoBuf ExtRoleInfo