{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.PB.Notify.Group0x857 where
import Zephyr.ProtoLite.Aliases
import Zephyr.ProtoLite
import GHC.Generics

{-message AIOGrayTipsInfo{
  uint32 showLatest = 1;
  bytes content = 2;
  uint32 remind = 3;
  bytes brief = 4;
  uint64 receiverUin = 5;
  uint32 reliaoAdminOpt = 6;
}-}

data AIOGrayTipsInfo = AIOGrayTipsInfo {
    _show_latest      :: ProtoFieldOptional UInt32 1,
    _content          :: ProtoFieldOptional Bytes 2,
    _remind           :: ProtoFieldOptional UInt32 3,
    _brief            :: ProtoFieldOptional Bytes 4,
    _receiver_uin     :: ProtoFieldOptional UInt64 5,
    _reliao_admin_opt :: ProtoFieldOptional UInt32 6
} deriving (Show, Eq, Generic)
instance ProtoBuf AIOGrayTipsInfo

{-message GeneralGrayTipInfo {
  uint64 busiType = 1;
  uint64 busiId = 2;
  uint32 ctrlFlag = 3;
  uint32 c2cType = 4;
  uint32 serviceType = 5;
  uint64 templId = 6;
  repeated TemplParam msgTemplParam = 7;
  string content = 8;
}-}

data GeneralGrayTipInfo = GeneralGrayTipInfo {
    _busi_type        :: ProtoFieldOptional UInt64 1,
    _busi_id          :: ProtoFieldOptional UInt64 2,
    _ctrl_flag        :: ProtoFieldOptional UInt32 3,
    _c2c_type         :: ProtoFieldOptional UInt32 4,
    _service_type     :: ProtoFieldOptional UInt32 5,
    _templ_id         :: ProtoFieldOptional UInt64 6,
    _msg_templ_param  :: ProtoFieldRepeated TemplParam 7,
    _content          :: ProtoFieldOptional String 8
} deriving (Show, Eq, Generic)
instance ProtoBuf GeneralGrayTipInfo

{-message TemplParam {
  string name = 1;
  string value = 2;
}-}

data TemplParam = TemplParam {
    _name  :: ProtoFieldOptional String 1,
    _value :: ProtoFieldOptional String 2
} deriving (Show, Eq, Generic)
instance ProtoBuf TemplParam

{-message MessageRecallReminder {
  int64 uin = 1;
  bytes nickname = 2;
  repeated RecalledMessageMeta recalledMsgList = 3;
  bytes reminderContent = 4;
  bytes userdef = 5;
  int32 groupType = 6;
  int32 opType = 7;
}-}

data MessageRecallReminder = MessageRecallReminder {
    _uin               :: ProtoFieldOptional Int64 1,
    _nickname          :: ProtoFieldOptional Bytes 2,
    _recalled_msg_list :: ProtoFieldRepeated RecalledMessageMeta 3,
    _reminder_content  :: ProtoFieldOptional Bytes 4,
    _userdef           :: ProtoFieldOptional Bytes 5,
    _group_type        :: ProtoFieldOptional Int32 6,
    _op_type           :: ProtoFieldOptional Int32 7
} deriving (Show, Eq, Generic)
instance ProtoBuf MessageRecallReminder

{-message RecalledMessageMeta {
  int32 seq = 1;
  int32 time = 2;
  int32 msgRandom = 3;
  int32 msgType = 4;
  int32 msgFlag = 5;
  int64 authorUin = 6;
}-}

data RecalledMessageMeta = RecalledMessageMeta {
    _seq        :: ProtoFieldOptional Int32 1,
    _time       :: ProtoFieldOptional Int32 2,
    _msg_random :: ProtoFieldOptional Int32 3,
    _msg_type   :: ProtoFieldOptional Int32 4,
    _msg_flag   :: ProtoFieldOptional Int32 5,
    _author_uin :: ProtoFieldOptional Int64 6
} deriving (Show, Eq, Generic)
instance ProtoBuf RecalledMessageMeta

{-message RedGrayTipsInfo {
  uint32 showLatest = 1;
  uint64 senderUin = 2;
  uint64 receiverUin = 3;
  string senderRichContent = 4;
  string receiverRichContent = 5;
  bytes authKey = 6;
  sint32 msgType = 7;
  uint32 luckyFlag = 8;
  uint32 hideFlag = 9;
  uint64 luckyUin = 12;
}-}

data RedGrayTipsInfo = RedGrayTipsInfo {
    _show_latest          :: ProtoFieldOptional UInt32 1,
    _sender_uin           :: ProtoFieldOptional UInt64 2,
    _receiver_uin         :: ProtoFieldOptional UInt64 3,
    _sender_rich_content  :: ProtoFieldOptional String 4,
    _receiver_rich_content:: ProtoFieldOptional String 5,
    _auth_key             :: ProtoFieldOptional Bytes 6,
    _msg_type             :: ProtoFieldOptional Int32 7,
    _lucky_flag           :: ProtoFieldOptional UInt32 8,
    _hide_flag            :: ProtoFieldOptional UInt32 9,
    _lucky_uin            :: ProtoFieldOptional UInt64 12
} deriving (Show, Eq, Generic)
instance ProtoBuf RedGrayTipsInfo

{-message QQGroupDigestMsg {
  uint64 groupCode = 1;
  uint32 seq = 2;
  uint32 random = 3;
  int32 opType = 4;
  uint64 sender = 5;
  uint64 digestOper = 6;
  uint32 opTime = 7;
  uint32 lastestMsgSeq = 8;
  bytes operNick = 9;
  bytes senderNick = 10;
  int32 extInfo = 11;
}
-}

data QQGroupDigestMsg = QQGroupDigestMsg {
    _group_code     :: ProtoFieldOptional UInt64 1,
    _seq            :: ProtoFieldOptional UInt32 2,
    _random         :: ProtoFieldOptional UInt32 3,
    _op_type        :: ProtoFieldOptional Int32 4,
    _sender         :: ProtoFieldOptional UInt64 5,
    _digest_oper    :: ProtoFieldOptional UInt64 6,
    _op_time        :: ProtoFieldOptional UInt32 7,
    _lastest_msg_seq:: ProtoFieldOptional UInt32 8,
    _oper_nick      :: ProtoFieldOptional String 9,
    _sender_nick    :: ProtoFieldOptional String 10,
    _ext_info       :: ProtoFieldOptional Int32 11
} deriving (Show, Eq, Generic)
instance ProtoBuf QQGroupDigestMsg

{-message NotifyMsgBody {
  AIOGrayTipsInfo optMsgGrayTips = 5;
  RedGrayTipsInfo optMsgRedTips = 9;
  MessageRecallReminder optMsgRecall = 11;
  GeneralGrayTipInfo optGeneralGrayTip = 26;
  QQGroupDigestMsg qqGroupDigestMsg = 33;
  int32 serviceType = 13;
}-}

data NotifyMsgBody = NotifyMsgBody {
    _opt_msg_gray_tips   :: ProtoFieldOptional AIOGrayTipsInfo 5,
    _opt_msg_red_tips    :: ProtoFieldOptional RedGrayTipsInfo 9,
    _opt_msg_recall      :: ProtoFieldOptional MessageRecallReminder 11,
    _opt_general_gray_tip:: ProtoFieldOptional GeneralGrayTipInfo 26,
    _qq_group_digest_msg :: ProtoFieldOptional QQGroupDigestMsg 33,
    _service_type        :: ProtoFieldOptional Int32 13
} deriving (Show, Eq, Generic)
instance ProtoBuf NotifyMsgBody