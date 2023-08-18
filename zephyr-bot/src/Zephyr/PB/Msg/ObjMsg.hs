{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.PB.Msg.ObjMsg where

import ProtoLite
import ProtoLite.Aliases
import GHC.Generics (Generic)

data MsgPic = MsgPic {
    _small_pic_url          :: ProtoFieldOptional Bytes 1,
    _original_pic_url       :: ProtoFieldOptional Bytes 2,
    _local_pic_id           :: ProtoFieldOptional VInt32 3
} deriving (Eq, Show, Generic)
instance ProtoBuf MsgPic


data ObjMsg = ObjMsg {
    _msg_type               :: ProtoFieldOptional VInt32 1,
    _title                  :: ProtoFieldOptional Bytes 2,
    _bytes_abstact          :: ProtoFieldOptional Bytes 3,
    _title_ext              :: ProtoFieldOptional Bytes 5,
    _msg_pic                :: ProtoFieldRepeated MsgPic 6,
    _msg_content_info       :: ProtoFieldRepeated MsgContentInfo 7,
    _report_id_show         :: ProtoFieldOptional VInt32 8
} deriving (Eq, Show, Generic)
instance ProtoBuf ObjMsg


data MsgContentInfo = MsgContentInfo {
    _content_info_id        :: ProtoFieldOptional Bytes 1,
    _msg_file               :: ProtoFieldOptional MsgFile 2
} deriving (Eq, Show, Generic)
instance ProtoBuf MsgContentInfo


data MsgFile = MsgFile {
    _bus_id                 :: ProtoFieldOptional VInt32 1,
    _file_path              :: ProtoFieldOptional String 2,
    _file_size              :: ProtoFieldOptional VInt64 3,
    _file_name              :: ProtoFieldOptional String 4,
    _int64_dead_time        :: ProtoFieldOptional VInt64 5,
    _file_sha1              :: ProtoFieldOptional Bytes 6,
    _ext                    :: ProtoFieldOptional Bytes 7
} deriving (Eq, Show, Generic)
instance ProtoBuf MsgFile