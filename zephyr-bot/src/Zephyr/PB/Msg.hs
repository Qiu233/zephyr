{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.PB.Msg where
import ProtoLite
import Data.Int
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Word

-- Don't touch this file unless necessary.
-- As it takes too long to compile.

data C2CTempMessageHead = C2CTempMessageHead {
    _c2c_type :: ProtoField (Optional (Variant Int32)) 1,
    _service_type :: ProtoField (Optional (Variant Int32)) 2,
    _group_uin :: ProtoField (Optional (Variant Int64)) 3,
    _group_code :: ProtoField (Optional (Variant Int64)) 4,
    _sig :: ProtoField (Optional B.ByteString) 5,
    _sig_type :: ProtoField (Optional (Variant Int32)) 6,
    _from_phone :: ProtoField (Optional String) 7,
    _to_phone :: ProtoField (Optional String) 8,
    _lock_display :: ProtoField (Optional (Variant Int32)) 9,
    _direction_flag :: ProtoField (Optional (Variant Int32)) 10,
    _reserved :: ProtoField (Optional B.ByteString) 11
} deriving (Eq, Show, Generic)
instance ProtoBuf C2CTempMessageHead

data GroupInfo = GroupInfo {
    _group_code :: ProtoField (Optional (Variant Int64)) 1,
    _group_type :: ProtoField (Optional (Variant Int32)) 2,
    _group_info_seq :: ProtoField (Optional (Variant Int64)) 3,
    _group_card :: ProtoField (Optional String) 4,
    _group_rank :: ProtoField (Optional B.ByteString) 5,
    _group_level :: ProtoField (Optional (Variant Int32)) 6,
    _group_card_type :: ProtoField (Optional (Variant Int32)) 7,
    _group_name :: ProtoField (Optional B.ByteString) 8
} deriving (Eq, Show, Generic)
instance ProtoBuf GroupInfo

data MutilTransHead = MutilTransHead {
    _status :: ProtoField (Optional (Variant Int32)) 1,
    _msg_id :: ProtoField (Optional (Variant Int32)) 2
} deriving (Eq, Show, Generic)
instance ProtoBuf MutilTransHead

data MessageHeaad = MessageHeaad {
    _from_uin :: ProtoField (Optional (Variant Int64)) 1,
    _to_uin :: ProtoField (Optional (Variant Int64)) 2,
    _msg_type :: ProtoField (Optional (Variant Int32)) 3,
    _c2c_cmd :: ProtoField (Optional (Variant Int32)) 4,
    _msg_seq :: ProtoField (Optional (Variant Int32)) 5,
    _msg_time :: ProtoField (Optional (Variant Int32)) 6,
    _msg_uid :: ProtoField (Optional (Variant Int64)) 7,
    _c2c_tmp_msg_head :: ProtoField (Optional C2CTempMessageHead) 8,
    _group_info :: ProtoField (Optional GroupInfo) 9,
    _from_appid :: ProtoField (Optional (Variant Int32)) 10,
    _from_instid :: ProtoField (Optional (Variant Int32)) 11,
    _user_active :: ProtoField (Optional (Variant Int32)) 12,
    _from_nick :: ProtoField (Optional String) 14,
    _auth_uin :: ProtoField (Optional (Variant Int64)) 15,
    _auth_nick :: ProtoField (Optional String) 16,
    _msg_flag :: ProtoField (Optional (Variant Int32)) 17,
    _auth_remark :: ProtoField (Optional String) 18,
    _group_name :: ProtoField (Optional String) 19,
    _multi_trans_head :: ProtoField (Optional MutilTransHead) 20,
    _public_account_group_send_flag :: ProtoField (Optional (Variant Int32)) 22,
    _wseq_in_c2c_msg_head :: ProtoField (Optional (Variant Int32)) 23,
    _cpid :: ProtoField (Optional (Variant Int64)) 24,
    _multi_compatible_text :: ProtoField (Optional String) 26,
    _auth_sex :: ProtoField (Optional (Variant Int32)) 27,
    _is_src_msg :: ProtoField (Optional (Variant Bool)) 28
} deriving (Eq, Show, Generic)
instance ProtoBuf MessageHeaad

data ContentHead = ContentHead {
    _pkg_num :: ProtoField (Optional (Variant Int32)) 1,
    _pkg_index :: ProtoField (Optional (Variant Int32)) 2,
    _div_seq :: ProtoField (Optional (Variant Int32)) 3,
    _auto_reply :: ProtoField (Optional (Variant Int32)) 4
} deriving (Eq, Show, Generic)
instance ProtoBuf ContentHead

data Attr = Attr {
    _code_page :: ProtoField (Optional (Variant Int32)) 1,
    _time :: ProtoField (Optional (Variant Int32)) 2,
    _random :: ProtoField (Optional (Variant Int32)) 3,
    _color :: ProtoField (Optional (Variant Int32)) 4,
    _size :: ProtoField (Optional (Variant Int32)) 5,
    _effect :: ProtoField (Optional (Variant Int32)) 6,
    _char_set :: ProtoField (Optional (Variant Int32)) 7,
    _pitch_and_family :: ProtoField (Optional (Variant Int32)) 8,
    _font_name :: ProtoField (Optional String) 9,
    _reserve_data :: ProtoField (Optional B.ByteString) 10
} deriving (Eq, Show, Generic)
instance ProtoBuf Attr

data Text = Text {
    _str :: ProtoField (Optional String) 1,
    _link :: ProtoField (Optional String) 2,
    _attr6_buf :: ProtoField (Optional B.ByteString) 3,
    _attr7_buf :: ProtoField (Optional B.ByteString) 4,
    _buf :: ProtoField (Optional B.ByteString) 11,
    _pb_reserve :: ProtoField (Optional B.ByteString) 12
} deriving (Eq, Show, Generic)
instance ProtoBuf Text

data Face = Face {
    _index :: ProtoField (Optional (Variant Int32)) 1,
    _old :: ProtoField (Optional B.ByteString) 2,
    _buf :: ProtoField (Optional B.ByteString) 11
} deriving (Eq, Show, Generic)
instance ProtoBuf Face

data OnlineImage = OnlineImage {
    _guid :: ProtoField (Optional B.ByteString) 1,
    _file_path :: ProtoField (Optional B.ByteString) 2,
    _old_ver_send_file :: ProtoField (Optional B.ByteString) 3
} deriving (Eq, Show, Generic)
instance ProtoBuf OnlineImage

newtype PbReserve = PbReserve {
    _url :: ProtoField (Optional String) 30
} deriving (Eq, Show, Generic)
instance ProtoBuf PbReserve

data NotOnlineImage = NotOnlineImage {
    _file_path :: ProtoField (Optional String) 1,
    _file_len :: ProtoField (Optional (Variant Int32)) 2,
    _download_path :: ProtoField (Optional String) 3,
    _old_ver_send_file :: ProtoField (Optional B.ByteString) 4,
    _img_type :: ProtoField (Optional (Variant Int32)) 5,
    _previews_image :: ProtoField (Optional B.ByteString) 6,
    _pic_md5 :: ProtoField (Optional B.ByteString) 7,
    _pic_height :: ProtoField (Optional (Variant Int32)) 8,
    _pic_width :: ProtoField (Optional (Variant Int32)) 9,
    _res_id :: ProtoField (Optional String) 10,
    _flag :: ProtoField (Optional B.ByteString) 11,
    _thumb_url :: ProtoField (Optional String) 12,
    _original :: ProtoField (Optional (Variant Int32)) 13,
    _big_url :: ProtoField (Optional String) 14,
    _orig_url :: ProtoField (Optional String) 15,
    _biz_type :: ProtoField (Optional (Variant Int32)) 16,
    _result :: ProtoField (Optional (Variant Int32)) 17,
    _index :: ProtoField (Optional (Variant Int32)) 18,
    _op_face_buf :: ProtoField (Optional B.ByteString) 19,
    _old_pic_md5 :: ProtoField (Optional (Variant Bool)) 20,
    _thumb_width :: ProtoField (Optional (Variant Int32)) 21,
    _thumb_height :: ProtoField (Optional (Variant Int32)) 22,
    _file_id :: ProtoField (Optional (Variant Int32)) 23,
    _show_len :: ProtoField (Optional (Variant Int32)) 24,
    _download_len :: ProtoField (Optional (Variant Int32)) 25,
    _pb_reserve :: ProtoField (Optional PbReserve) 29
} deriving (Eq, Show, Generic)
instance ProtoBuf NotOnlineImage

data TransElem = TransElem {
    _elem_type :: ProtoField (Optional (Variant Int32)) 1,
    _elem_value :: ProtoField (Optional B.ByteString) 2
} deriving (Eq, Show, Generic)
instance ProtoBuf TransElem

data MarketFace = MarketFace {
    _face_name :: ProtoField (Optional B.ByteString) 1,
    _item_type :: ProtoField (Optional (Variant Int32)) 2,
    _face_info :: ProtoField (Optional (Variant Int32)) 3,
    _face_id :: ProtoField (Optional B.ByteString) 4,
    _tab_id :: ProtoField (Optional (Variant Int32)) 5,
    _sub_type :: ProtoField (Optional (Variant Int32)) 6,
    _key :: ProtoField (Optional B.ByteString) 7,
    _param :: ProtoField (Optional B.ByteString) 8,
    _media_type :: ProtoField (Optional (Variant Int32)) 9,
    _image_width :: ProtoField (Optional (Variant Int32)) 10,
    _image_height :: ProtoField (Optional (Variant Int32)) 11,
    _mobile_param :: ProtoField (Optional B.ByteString) 12,
    _pb_reserve :: ProtoField (Optional B.ByteString) 13
} deriving (Eq, Show, Generic)
instance ProtoBuf MarketFace

data CustomFace = CustomFace {
    _guid :: ProtoField (Optional B.ByteString) 1,
    _file_path :: ProtoField (Optional String) 2,
    _shortcut :: ProtoField (Optional String) 3,
    _buffer :: ProtoField (Optional B.ByteString) 4,
    _flag :: ProtoField (Optional B.ByteString) 5,
    _old_data :: ProtoField (Optional B.ByteString) 6,
    _file_id :: ProtoField (Optional (Variant Int32)) 7,
    _server_ip :: ProtoField (Optional (Variant Int32)) 8,
    _server_port :: ProtoField (Optional (Variant Int32)) 9,
    _file_type :: ProtoField (Optional (Variant Int32)) 10,
    _signature :: ProtoField (Optional B.ByteString) 11,
    _useful :: ProtoField (Optional (Variant Int32)) 12,
    _md5 :: ProtoField (Optional B.ByteString) 13,
    _thumb_url :: ProtoField (Optional String) 14,
    _big_url :: ProtoField (Optional String) 15,
    _orig_url :: ProtoField (Optional String) 16,
    _biz_type :: ProtoField (Optional (Variant Int32)) 17,
    _repeat_index :: ProtoField (Optional (Variant Int32)) 18,
    _repeat_image :: ProtoField (Optional (Variant Int32)) 19,
    _image_type :: ProtoField (Optional (Variant Int32)) 20,
    _index :: ProtoField (Optional (Variant Int32)) 21,
    _width :: ProtoField (Optional (Variant Int32)) 22,
    _height :: ProtoField (Optional (Variant Int32)) 23,
    _source :: ProtoField (Optional (Variant Int32)) 24,
    _size :: ProtoField (Optional (Variant Int32)) 25,
    _origin :: ProtoField (Optional (Variant Int32)) 26,
    _thumb_width :: ProtoField (Optional (Variant Int32)) 27,
    _thumb_height :: ProtoField (Optional (Variant Int32)) 28,
    _show_len :: ProtoField (Optional (Variant Int32)) 29,
    _download_len :: ProtoField (Optional (Variant Int32)) 30,
    _400_url :: ProtoField (Optional String) 31,
    _400_width :: ProtoField (Optional (Variant Int32)) 32,
    _400_height :: ProtoField (Optional (Variant Int32)) 33,
    _pb_reserve :: ProtoField (Optional B.ByteString) 34
} deriving (Eq, Show, Generic)
instance ProtoBuf CustomFace


data RichMsg = RichMsg {
    _template1 :: ProtoField (Optional B.ByteString) 1,
    _service_id :: ProtoField (Optional (Variant Int32)) 2,
    _msg_res_id :: ProtoField (Optional B.ByteString) 3,
    _rand :: ProtoField (Optional (Variant Int32)) 4,
    _seq :: ProtoField (Optional (Variant Int32)) 5
} deriving (Eq, Show, Generic)
instance ProtoBuf RichMsg

data GroupFile = GroupFile {
    _filename :: ProtoField (Optional B.ByteString) 1,
    _file_size :: ProtoField (Optional (Variant Int64)) 2,
    _file_id :: ProtoField (Optional B.ByteString) 3,
    _batch_id :: ProtoField (Optional B.ByteString) 4,
    _file_key :: ProtoField (Optional B.ByteString) 5,
    _mark :: ProtoField (Optional B.ByteString) 6,
    _sequence :: ProtoField (Optional (Variant Int64)) 7,
    _batch_item_id :: ProtoField (Optional B.ByteString) 8,
    _feed_msg_time :: ProtoField (Optional (Variant Int32)) 9,
    _pb_reserve :: ProtoField (Optional B.ByteString) 10
} deriving (Eq, Show, Generic)
instance ProtoBuf GroupFile

data ExtraInfo = ExtraInfo {
    _nick :: ProtoField (Optional B.ByteString) 1,
    _group_card :: ProtoField (Optional B.ByteString) 2,
    _level :: ProtoField (Optional (Variant Int32)) 3,
    _flags :: ProtoField (Optional (Variant Int32)) 4,
    _group_mask :: ProtoField (Optional (Variant Int32)) 5,
    _msg_tail_id :: ProtoField (Optional (Variant Int32)) 6,
    _sender_title :: ProtoField (Optional B.ByteString) 7,
    _apns_tips :: ProtoField (Optional B.ByteString) 8,
    _uin :: ProtoField (Optional (Variant Int64)) 9,
    _msg_state_flag :: ProtoField (Optional (Variant Int32)) 10,
    _apns_sound_type :: ProtoField (Optional (Variant Int32)) 11,
    _new_group_flag :: ProtoField (Optional (Variant Int32)) 12
} deriving (Eq, Show, Generic)
instance ProtoBuf ExtraInfo

data VideoFile = VideoFile {
    _file_uuid :: ProtoField (Optional B.ByteString) 1,
    _file_md5 :: ProtoField (Optional B.ByteString) 2,
    _file_name :: ProtoField (Optional B.ByteString) 3,
    _file_format :: ProtoField (Optional (Variant Int32)) 4,
    _file_time :: ProtoField (Optional (Variant Int32)) 5,
    _file_size :: ProtoField (Optional (Variant Int32)) 6,
    _thumb_width :: ProtoField (Optional (Variant Int32)) 7,
    _thumb_height :: ProtoField (Optional (Variant Int32)) 8,
    _thumb_file_md5 :: ProtoField (Optional B.ByteString) 9,
    _source :: ProtoField (Optional B.ByteString) 10,
    _thumb_file_size :: ProtoField (Optional (Variant Int32)) 11,
    _busi_type :: ProtoField (Optional (Variant Int32)) 12,
    _from_chat_type :: ProtoField (Optional (Variant Int32)) 13,
    _to_chat_type :: ProtoField (Optional (Variant Int32)) 14,
    _bool_support_progressive :: ProtoField (Optional (Variant Bool)) 15,
    _file_width :: ProtoField (Optional (Variant Int32)) 16,
    _file_height :: ProtoField (Optional (Variant Int32)) 17,
    _sub_busi_type :: ProtoField (Optional (Variant Int32)) 18,
    _video_attr :: ProtoField (Optional (Variant Int32)) 19,
    _bytes_thumb_file_urls :: ProtoField (Repeated B.ByteString) 20,
    _bytes_video_file_urls :: ProtoField (Repeated B.ByteString) 21,
    _thumb_download_flag :: ProtoField (Optional (Variant Int32)) 22,
    _video_download_flag :: ProtoField (Optional (Variant Int32)) 23,
    _pb_reserve :: ProtoField (Optional B.ByteString) 24
} deriving (Eq, Show, Generic)
instance ProtoBuf VideoFile

data AnonymousGroupMessage = AnonymousGroupMessage {
    _flags :: ProtoField (Optional (Variant Int32)) 1,
    _anon_id :: ProtoField (Optional B.ByteString) 2,
    _anon_nick :: ProtoField (Optional B.ByteString) 3,
    _head_portrait :: ProtoField (Optional (Variant Int32)) 4,
    _expire_time :: ProtoField (Optional (Variant Int32)) 5,
    _bubble_id :: ProtoField (Optional (Variant Int32)) 6,
    _rank_color :: ProtoField (Optional B.ByteString) 7
} deriving (Eq, Show, Generic)
instance ProtoBuf AnonymousGroupMessage

newtype QQWalletMsg = QQWalletMsg {
    _aio_body :: ProtoField (Optional QQWalletAioBody) 1
} deriving (Eq, Show, Generic)
instance ProtoBuf QQWalletMsg

data QQWalletAioBody = QQWalletAioBody {
    _send_uin :: ProtoField (Optional (Variant Word64)) 1,
    _sender :: ProtoField (Optional QQWalletAioElem) 2,
    _receiver :: ProtoField (Optional QQWalletAioElem) 3,
    _channel_id :: ProtoField (Optional (Variant SInt32)) 4,
    _template_id :: ProtoField (Optional (Variant SInt32)) 5,
    _resend :: ProtoField (Optional (Variant Word32)) 6,
    _msg_priority :: ProtoField (Optional (Variant Word32)) 7,
    _red_type :: ProtoField (Optional (Variant SInt32)) 8,
    _bill_no :: ProtoField (Optional B.ByteString) 9,
    _auth_key :: ProtoField (Optional B.ByteString) 10,
    _session_type :: ProtoField (Optional (Variant SInt32)) 11,
    _msg_type :: ProtoField (Optional (Variant SInt32)) 12,
    _envel_ope_id :: ProtoField (Optional (Variant SInt32)) 13,
    _name :: ProtoField (Optional B.ByteString) 14,
    _conf_type :: ProtoField (Optional (Variant SInt32)) 15,
    _msg_from :: ProtoField (Optional (Variant SInt32)) 16,
    _pc_body :: ProtoField (Optional B.ByteString) 17,
    _index :: ProtoField (Optional B.ByteString) 18,
    _red_channel :: ProtoField (Optional (Variant Word32)) 19,
    _grap_uin :: ProtoField (Repeated (Variant Word64)) 20,
    _pb_reserve :: ProtoField (Optional B.ByteString) 21
} deriving (Eq, Show, Generic)
instance ProtoBuf QQWalletAioBody

data QQWalletAioElem = QQWalletAioElem {
    _background :: ProtoField (Optional (Variant Word32)) 1,
    _icon :: ProtoField (Optional (Variant Word32)) 2,
    _title :: ProtoField (Optional B.ByteString) 3,
    _subtitle :: ProtoField (Optional B.ByteString) 4,
    _content :: ProtoField (Optional B.ByteString) 5,
    _link_url :: ProtoField (Optional B.ByteString) 6,
    _black_stripe :: ProtoField (Optional B.ByteString) 7,
    _notice :: ProtoField (Optional B.ByteString) 8,
    _title_color :: ProtoField (Optional (Variant Word32)) 9,
    _subtitle_color :: ProtoField (Optional (Variant Word32)) 10,
    _actions_priority :: ProtoField (Optional B.ByteString) 11,
    _jump_url :: ProtoField (Optional B.ByteString) 12,
    _native_ios :: ProtoField (Optional B.ByteString) 13,
    _native_android :: ProtoField (Optional B.ByteString) 14,
    _icon_url :: ProtoField (Optional B.ByteString) 15,
    _content_color :: ProtoField (Optional (Variant Word32)) 16,
    _content_bg_color :: ProtoField (Optional (Variant Word32)) 17,
    _aio_image_left :: ProtoField (Optional B.ByteString) 18,
    _aio_image_right :: ProtoField (Optional B.ByteString) 19,
    _cft_image :: ProtoField (Optional B.ByteString) 20,
    _pb_reserve :: ProtoField (Optional B.ByteString) 21
} deriving (Eq, Show, Generic)
instance ProtoBuf QQWalletAioElem


data CustomElem = CustomElem {
    _desc :: ProtoField (Optional B.ByteString) 1,
    _data :: ProtoField (Optional B.ByteString) 2,
    _enum_type :: ProtoField (Optional (Variant Int32)) 3,
    _ext :: ProtoField (Optional B.ByteString) 4,
    _sound :: ProtoField (Optional B.ByteString) 5
} deriving (Eq, Show, Generic)
instance ProtoBuf CustomElem

data GeneralFlags = GeneralFlags {
    _bubble_diy_text_id :: ProtoField (Optional (Variant Int32)) 1,
    _group_flag_new :: ProtoField (Optional (Variant Int32)) 2,
    _uin :: ProtoField (Optional (Variant Word64)) 3,
    _rp_id :: ProtoField (Optional B.ByteString) 4,
    _prp_fold :: ProtoField (Optional (Variant Int32)) 5,
    _long_text_flag :: ProtoField (Optional (Variant Int32)) 6,
    _long_text_resid :: ProtoField (Optional B.ByteString) 7,
    _group_type :: ProtoField (Optional (Variant Int32)) 8,
    _to_uin_flag :: ProtoField (Optional (Variant Int32)) 9,
    _glamour_level :: ProtoField (Optional (Variant Int32)) 10,
    _member_level :: ProtoField (Optional (Variant Int32)) 11,
    _group_rank_seq :: ProtoField (Optional (Variant Word64)) 12,
    _olympic_torch :: ProtoField (Optional (Variant Int32)) 13,
    _babyq_guide_msg_cookie :: ProtoField (Optional B.ByteString) 14,
    _uin32_expert_flag :: ProtoField (Optional (Variant Int32)) 15,
    _bubble_sub_id :: ProtoField (Optional (Variant Int32)) 16,
    _pendant_id :: ProtoField (Optional (Variant Word64)) 17,
    _rp_index :: ProtoField (Optional B.ByteString) 18,
    _pb_reserve :: ProtoField (Optional B.ByteString) 19
} deriving (Eq, Show, Generic)
instance ProtoBuf GeneralFlags

data SourceMsg = SourceMsg {
    _orig_seqs :: ProtoField (Repeated (Variant Int32)) 1,
    _sender_uin :: ProtoField (Optional (Variant Word64)) 2,
    _time :: ProtoField (Optional (Variant Int32)) 3,
    _flag :: ProtoField (Optional (Variant Int32)) 4,
    _elems :: ProtoField (Repeated Elem) 5,
    _type :: ProtoField (Optional (Variant Int32)) 6,
    _rich_msg :: ProtoField (Optional B.ByteString) 7,
    _pb_reserve :: ProtoField (Optional B.ByteString) 8,
    _src_msg :: ProtoField (Optional B.ByteString) 9,
    _to_uin :: ProtoField (Optional (Variant Word64)) 10,
    _troop_name :: ProtoField (Optional B.ByteString) 11
} deriving (Eq, Show, Generic)
instance ProtoBuf SourceMsg

data LightAppElem = LightAppElem {
    _data :: ProtoField (Optional B.ByteString) 1,
    _msg_resid :: ProtoField (Optional B.ByteString) 2
} deriving (Eq, Show, Generic)
instance ProtoBuf LightAppElem


data CommonElem = CommonElem {
    _service_type :: ProtoField (Optional (Variant Int32)) 1,
    _pb_elem :: ProtoField (Optional B.ByteString) 2,
    _business_type :: ProtoField (Optional (Variant Int32)) 3
} deriving (Eq, Show, Generic)
instance ProtoBuf CommonElem

data Elem = Elem {
    _text :: ProtoField (Optional Text) 1,
    _face :: ProtoField (Optional Face) 2,
    _online_image :: ProtoField (Optional OnlineImage) 3,
    _not_online_image :: ProtoField (Optional NotOnlineImage) 4,
    _trans_elem_info :: ProtoField (Optional TransElem) 5,
    _market_face :: ProtoField (Optional MarketFace) 6,
    _custom_face :: ProtoField (Optional CustomFace) 8,
    _rich_msg :: ProtoField (Optional RichMsg) 12,
    _group_file :: ProtoField (Optional GroupFile) 13,
    _extra_info :: ProtoField (Optional ExtraInfo) 16,
    _video_file :: ProtoField (Optional VideoFile) 19,
    _anon_group_msg :: ProtoField (Optional AnonymousGroupMessage) 21,
    _qq_wallet_msg :: ProtoField (Optional QQWalletMsg) 24,
    _custom_elem :: ProtoField (Optional CustomElem) 31,
    _general_flags :: ProtoField (Optional GeneralFlags) 37,
    _src_msg :: ProtoField (Optional SourceMsg) 45,
    _light_app :: ProtoField (Optional LightAppElem) 51,
    _common_elem :: ProtoField (Optional CommonElem) 53
} deriving (Eq, Show, Generic)
instance ProtoBuf Elem

data RichText = RichText {
    _attr :: ProtoField (Optional Attr) 1,
    _elems :: ProtoField (Repeated Elem) 2,
    _not_online_file :: ProtoField (Optional NotOnlineFile) 3,
    _ptt :: ProtoField (Optional Ptt) 4
} deriving (Eq, Show, Generic)
instance ProtoBuf RichText

data Ptt = Ptt {
    _file_type :: ProtoField (Optional (Variant Int32)) 1,
    _src_uin :: ProtoField (Optional (Variant Word64)) 2,
    _file_uuid :: ProtoField (Optional B.ByteString) 3,
    _file_md5 :: ProtoField (Optional B.ByteString) 4,
    _file_name :: ProtoField (Optional String) 5,
    _file_size :: ProtoField (Optional (Variant Int32)) 6,
    _reserve :: ProtoField (Optional B.ByteString) 7,
    _file_id :: ProtoField (Optional (Variant Int32)) 8,
    _server_ip :: ProtoField (Optional (Variant Int32)) 9,
    _server_port :: ProtoField (Optional (Variant Int32)) 10,
    _bool_valid :: ProtoField (Optional (Variant Bool)) 11,
    _signature :: ProtoField (Optional B.ByteString) 12,
    _shortcut :: ProtoField (Optional B.ByteString) 13,
    _file_key :: ProtoField (Optional B.ByteString) 14,
    _magic_ptt_index :: ProtoField (Optional (Variant Int32)) 15,
    _voice_switch :: ProtoField (Optional (Variant Int32)) 16,
    _ptt_url :: ProtoField (Optional B.ByteString) 17,
    _group_file_key :: ProtoField (Optional B.ByteString) 18,
    _time :: ProtoField (Optional (Variant Int32)) 19,
    _down_para :: ProtoField (Optional B.ByteString) 20,
    _format :: ProtoField (Optional (Variant Int32)) 29,
    _pb_reserve :: ProtoField (Optional B.ByteString) 30,
    _bytes_ptt_urls :: ProtoField (Repeated B.ByteString) 31,
    _download_flag :: ProtoField (Optional (Variant Int32)) 32
} deriving (Eq, Show, Generic)
instance ProtoBuf Ptt

data NotOnlineFile = NotOnlineFile {
    _file_type :: ProtoField (Optional (Variant Int32)) 1,
    _sig :: ProtoField (Optional B.ByteString) 2,
    _file_uuid :: ProtoField (Optional B.ByteString) 3,
    _file_md5 :: ProtoField (Optional B.ByteString) 4,
    _file_name :: ProtoField (Optional B.ByteString) 5,
    _file_size :: ProtoField (Optional (Variant Int64)) 6,
    _note :: ProtoField (Optional B.ByteString) 7,
    _reserved :: ProtoField (Optional (Variant Int32)) 8,
    _subcmd :: ProtoField (Optional (Variant Int32)) 9,
    _micro_cloud :: ProtoField (Optional (Variant Int32)) 10,
    _bytes_file_urls :: ProtoField (Repeated B.ByteString) 11,
    _download_flag :: ProtoField (Optional (Variant Int32)) 12,
    _danger_evel :: ProtoField (Optional (Variant Int32)) 50,
    _life_time :: ProtoField (Optional (Variant Int32)) 51,
    _upload_time :: ProtoField (Optional (Variant Int32)) 52,
    _abs_file_type :: ProtoField (Optional (Variant Int32)) 53,
    _client_type :: ProtoField (Optional (Variant Int32)) 54,
    _expire_time :: ProtoField (Optional (Variant Int32)) 55,
    _pb_reserve :: ProtoField (Optional B.ByteString) 56
} deriving (Eq, Show, Generic)
instance ProtoBuf NotOnlineFile

data MessageBody = MessageBody {
    _rich_text :: ProtoField (Optional RichText) 1,
    _msg_content :: ProtoField (Optional B.ByteString) 2,
    _msg_encrypt_content :: ProtoField (Optional B.ByteString) 3
} deriving (Eq, Show, Generic)
instance ProtoBuf MessageBody

data Message = Message {
    _head :: ProtoField (Optional MessageHeaad) 1,
    _content :: ProtoField (Optional ContentHead) 2,
    _body :: ProtoField (Optional MessageBody) 3
} deriving (Eq, Show, Generic)
instance ProtoBuf Message

data PushMessagePacket = PushMessagePacket {
    _message :: ProtoField (Optional Message) 1,
    _svrip :: ProtoField (Optional (Variant Int32)) 2,
    _push_token :: ProtoField (Optional B.ByteString) 3,
    _ping_flag :: ProtoField (Optional (Variant Int32)) 4,
    _general_flag :: ProtoField (Optional (Variant Int32)) 9
} deriving (Eq, Show, Generic)
instance ProtoBuf PushMessagePacket