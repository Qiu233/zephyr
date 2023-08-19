{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.PB.Msg where
import Zephyr.ProtoLite
import Zephyr.ProtoLite.Aliases
import GHC.Generics

-- Don't touch this file unless necessary.
-- As it takes too long to compile.

data C2CTempMessageHead = C2CTempMessageHead {
    _c2c_type                                  :: ProtoFieldOptional VInt32 1,
    _service_type                              :: ProtoFieldOptional VInt32 2,
    _group_uin                                 :: ProtoFieldOptional VInt64 3,
    _group_code                                :: ProtoFieldOptional VInt64 4,
    _sig                                       :: ProtoFieldOptional Bytes 5,
    _sig_type                                  :: ProtoFieldOptional VInt32 6,
    _from_phone                                :: ProtoFieldOptional String 7,
    _to_phone                                  :: ProtoFieldOptional String 8,
    _lock_display                              :: ProtoFieldOptional VInt32 9,
    _direction_flag                            :: ProtoFieldOptional VInt32 10,
    _reserved                                  :: ProtoFieldOptional Bytes 11
} deriving (Eq, Show, Generic)
instance ProtoBuf C2CTempMessageHead

data GroupInfo = GroupInfo {
    _group_code                                :: ProtoFieldOptional VInt64 1,
    _group_type                                :: ProtoFieldOptional VInt32 2,
    _group_info_seq                            :: ProtoFieldOptional VInt64 3,
    _group_card                                :: ProtoFieldOptional String 4,
    _group_rank                                :: ProtoFieldOptional Bytes 5,
    _group_level                               :: ProtoFieldOptional VInt32 6,
    _group_card_type                           :: ProtoFieldOptional VInt32 7,
    _group_name                                :: ProtoFieldOptional Bytes 8
} deriving (Eq, Show, Generic)
instance ProtoBuf GroupInfo

data MutilTransHead = MutilTransHead {
    _status                                    :: ProtoFieldOptional VInt32 1,
    _msg_id                                    :: ProtoFieldOptional VInt32 2
} deriving (Eq, Show, Generic)
instance ProtoBuf MutilTransHead

data MessageHeaad = MessageHeaad {
    _from_uin                                  :: ProtoFieldOptional VInt64 1,
    _to_uin                                    :: ProtoFieldOptional VInt64 2,
    _msg_type                                  :: ProtoFieldOptional VInt32 3,
    _c2c_cmd                                   :: ProtoFieldOptional VInt32 4,
    _msg_seq                                   :: ProtoFieldOptional VInt32 5,
    _msg_time                                  :: ProtoFieldOptional VInt32 6,
    _msg_uid                                   :: ProtoFieldOptional VInt64 7,
    _c2c_tmp_msg_head                          :: ProtoFieldOptional C2CTempMessageHead 8,
    _group_info                                :: ProtoFieldOptional GroupInfo 9,
    _from_appid                                :: ProtoFieldOptional VInt32 10,
    _from_instid                               :: ProtoFieldOptional VInt32 11,
    _user_active                               :: ProtoFieldOptional VInt32 12,
    _from_nick                                 :: ProtoFieldOptional String 14,
    _auth_uin                                  :: ProtoFieldOptional VInt64 15,
    _auth_nick                                 :: ProtoFieldOptional String 16,
    _msg_flag                                  :: ProtoFieldOptional VInt32 17,
    _auth_remark                               :: ProtoFieldOptional String 18,
    _group_name                                :: ProtoFieldOptional String 19,
    _multi_trans_head                          :: ProtoFieldOptional MutilTransHead 20,
    _public_account_group_send_flag            :: ProtoFieldOptional VInt32 22,
    _wseq_in_c2c_msg_head                      :: ProtoFieldOptional VInt32 23,
    _cpid                                      :: ProtoFieldOptional VInt64 24,
    _multi_compatible_text                     :: ProtoFieldOptional String 26,
    _auth_sex                                  :: ProtoFieldOptional VInt32 27,
    _is_src_msg                                :: ProtoFieldOptional VBool 28
} deriving (Eq, Show, Generic)
instance ProtoBuf MessageHeaad

data ContentHead = ContentHead {
    _pkg_num                                   :: ProtoFieldOptional VInt32 1,
    _pkg_index                                 :: ProtoFieldOptional VInt32 2,
    _div_seq                                   :: ProtoFieldOptional VInt32 3,
    _auto_reply                                :: ProtoFieldOptional VInt32 4
} deriving (Eq, Show, Generic)
instance ProtoBuf ContentHead

data Attr = Attr {
    _code_page                                 :: ProtoFieldOptional VInt32 1,
    _time                                      :: ProtoFieldOptional VInt32 2,
    _random                                    :: ProtoFieldOptional VInt32 3,
    _color                                     :: ProtoFieldOptional VInt32 4,
    _size                                      :: ProtoFieldOptional VInt32 5,
    _effect                                    :: ProtoFieldOptional VInt32 6,
    _char_set                                  :: ProtoFieldOptional VInt32 7,
    _pitch_and_family                          :: ProtoFieldOptional VInt32 8,
    _font_name                                 :: ProtoFieldOptional String 9,
    _reserve_data                              :: ProtoFieldOptional Bytes 10
} deriving (Eq, Show, Generic)
instance ProtoBuf Attr

data Text = Text {
    _str                                       :: ProtoFieldOptional String 1,
    _link                                      :: ProtoFieldOptional String 2,
    _attr6_buf                                 :: ProtoFieldOptional Bytes 3,
    _attr7_buf                                 :: ProtoFieldOptional Bytes 4,
    _buf                                       :: ProtoFieldOptional Bytes 11,
    _pb_reserve                                :: ProtoFieldOptional Bytes 12
} deriving (Eq, Show, Generic)
instance ProtoBuf Text

data Face = Face {
    _index                                     :: ProtoFieldOptional VInt32 1,
    _old                                       :: ProtoFieldOptional Bytes 2,
    _buf                                       :: ProtoFieldOptional Bytes 11
} deriving (Eq, Show, Generic)
instance ProtoBuf Face

data OnlineImage = OnlineImage {
    _guid                                      :: ProtoFieldOptional Bytes 1,
    _file_path                                 :: ProtoFieldOptional Bytes 2,
    _old_ver_send_file                         :: ProtoFieldOptional Bytes 3
} deriving (Eq, Show, Generic)
instance ProtoBuf OnlineImage

newtype PbReserve = PbReserve {
    _url                                       :: ProtoFieldOptional String 30
} deriving (Eq, Show, Generic)
instance ProtoBuf PbReserve

data NotOnlineImage = NotOnlineImage {
    _file_path                                 :: ProtoFieldOptional String 1,
    _file_len                                  :: ProtoFieldOptional VInt32 2,
    _download_path                             :: ProtoFieldOptional String 3,
    _old_ver_send_file                         :: ProtoFieldOptional Bytes 4,
    _img_type                                  :: ProtoFieldOptional VInt32 5,
    _previews_image                            :: ProtoFieldOptional Bytes 6,
    _pic_md5                                   :: ProtoFieldOptional Bytes 7,
    _pic_height                                :: ProtoFieldOptional VInt32 8,
    _pic_width                                 :: ProtoFieldOptional VInt32 9,
    _res_id                                    :: ProtoFieldOptional String 10,
    _flag                                      :: ProtoFieldOptional Bytes 11,
    _thumb_url                                 :: ProtoFieldOptional String 12,
    _original                                  :: ProtoFieldOptional VInt32 13,
    _big_url                                   :: ProtoFieldOptional String 14,
    _orig_url                                  :: ProtoFieldOptional String 15,
    _biz_type                                  :: ProtoFieldOptional VInt32 16,
    _result                                    :: ProtoFieldOptional VInt32 17,
    _index                                     :: ProtoFieldOptional VInt32 18,
    _op_face_buf                               :: ProtoFieldOptional Bytes 19,
    _old_pic_md5                               :: ProtoFieldOptional VBool 20,
    _thumb_width                               :: ProtoFieldOptional VInt32 21,
    _thumb_height                              :: ProtoFieldOptional VInt32 22,
    _file_id                                   :: ProtoFieldOptional VInt32 23,
    _show_len                                  :: ProtoFieldOptional VInt32 24,
    _download_len                              :: ProtoFieldOptional VInt32 25,
    _pb_reserve                                :: ProtoFieldOptional PbReserve 29
} deriving (Eq, Show, Generic)
instance ProtoBuf NotOnlineImage

data TransElem = TransElem {
    _elem_type                                 :: ProtoFieldOptional VInt32 1,
    _elem_value                                :: ProtoFieldOptional Bytes 2
} deriving (Eq, Show, Generic)
instance ProtoBuf TransElem

data MarketFace = MarketFace {
    _face_name                                 :: ProtoFieldOptional Bytes 1,
    _item_type                                 :: ProtoFieldOptional VInt32 2,
    _face_info                                 :: ProtoFieldOptional VInt32 3,
    _face_id                                   :: ProtoFieldOptional Bytes 4,
    _tab_id                                    :: ProtoFieldOptional VInt32 5,
    _sub_type                                  :: ProtoFieldOptional VInt32 6,
    _key                                       :: ProtoFieldOptional Bytes 7,
    _param                                     :: ProtoFieldOptional Bytes 8,
    _media_type                                :: ProtoFieldOptional VInt32 9,
    _image_width                               :: ProtoFieldOptional VInt32 10,
    _image_height                              :: ProtoFieldOptional VInt32 11,
    _mobile_param                              :: ProtoFieldOptional Bytes 12,
    _pb_reserve                                :: ProtoFieldOptional Bytes 13
} deriving (Eq, Show, Generic)
instance ProtoBuf MarketFace

data CustomFace = CustomFace {
    _guid                                      :: ProtoFieldOptional Bytes 1,
    _file_path                                 :: ProtoFieldOptional String 2,
    _shortcut                                  :: ProtoFieldOptional String 3,
    _buffer                                    :: ProtoFieldOptional Bytes 4,
    _flag                                      :: ProtoFieldOptional Bytes 5,
    _old_data                                  :: ProtoFieldOptional Bytes 6,
    _file_id                                   :: ProtoFieldOptional VInt32 7,
    _server_ip                                 :: ProtoFieldOptional VInt32 8,
    _server_port                               :: ProtoFieldOptional VInt32 9,
    _file_type                                 :: ProtoFieldOptional VInt32 10,
    _signature                                 :: ProtoFieldOptional Bytes 11,
    _useful                                    :: ProtoFieldOptional VInt32 12,
    _md5                                       :: ProtoFieldOptional Bytes 13,
    _thumb_url                                 :: ProtoFieldOptional String 14,
    _big_url                                   :: ProtoFieldOptional String 15,
    _orig_url                                  :: ProtoFieldOptional String 16,
    _biz_type                                  :: ProtoFieldOptional VInt32 17,
    _repeat_index                              :: ProtoFieldOptional VInt32 18,
    _repeat_image                              :: ProtoFieldOptional VInt32 19,
    _image_type                                :: ProtoFieldOptional VInt32 20,
    _index                                     :: ProtoFieldOptional VInt32 21,
    _width                                     :: ProtoFieldOptional VInt32 22,
    _height                                    :: ProtoFieldOptional VInt32 23,
    _source                                    :: ProtoFieldOptional VInt32 24,
    _size                                      :: ProtoFieldOptional VInt32 25,
    _origin                                    :: ProtoFieldOptional VInt32 26,
    _thumb_width                               :: ProtoFieldOptional VInt32 27,
    _thumb_height                              :: ProtoFieldOptional VInt32 28,
    _show_len                                  :: ProtoFieldOptional VInt32 29,
    _download_len                              :: ProtoFieldOptional VInt32 30,
    _400_url                                   :: ProtoFieldOptional String 31,
    _400_width                                 :: ProtoFieldOptional VInt32 32,
    _400_height                                :: ProtoFieldOptional VInt32 33,
    _pb_reserve                                :: ProtoFieldOptional Bytes 34
} deriving (Eq, Show, Generic)
instance ProtoBuf CustomFace


data RichMsg = RichMsg {
    _template1                                 :: ProtoFieldOptional Bytes 1,
    _service_id                                :: ProtoFieldOptional VInt32 2,
    _msg_res_id                                :: ProtoFieldOptional Bytes 3,
    _rand                                      :: ProtoFieldOptional VInt32 4,
    _seq                                       :: ProtoFieldOptional VInt32 5
} deriving (Eq, Show, Generic)
instance ProtoBuf RichMsg

data GroupFile = GroupFile {
    _filename                                  :: ProtoFieldOptional Bytes 1,
    _file_size                                 :: ProtoFieldOptional VInt64 2,
    _file_id                                   :: ProtoFieldOptional Bytes 3,
    _batch_id                                  :: ProtoFieldOptional Bytes 4,
    _file_key                                  :: ProtoFieldOptional Bytes 5,
    _mark                                      :: ProtoFieldOptional Bytes 6,
    _sequence                                  :: ProtoFieldOptional VInt64 7,
    _batch_item_id                             :: ProtoFieldOptional Bytes 8,
    _feed_msg_time                             :: ProtoFieldOptional VInt32 9,
    _pb_reserve                                :: ProtoFieldOptional Bytes 10
} deriving (Eq, Show, Generic)
instance ProtoBuf GroupFile

data ExtraInfo = ExtraInfo {
    _nick                                      :: ProtoFieldOptional Bytes 1,
    _group_card                                :: ProtoFieldOptional Bytes 2,
    _level                                     :: ProtoFieldOptional VInt32 3,
    _flags                                     :: ProtoFieldOptional VInt32 4,
    _group_mask                                :: ProtoFieldOptional VInt32 5,
    _msg_tail_id                               :: ProtoFieldOptional VInt32 6,
    _sender_title                              :: ProtoFieldOptional Bytes 7,
    _apns_tips                                 :: ProtoFieldOptional Bytes 8,
    _uin                                       :: ProtoFieldOptional VInt64 9,
    _msg_state_flag                            :: ProtoFieldOptional VInt32 10,
    _apns_sound_type                           :: ProtoFieldOptional VInt32 11,
    _new_group_flag                            :: ProtoFieldOptional VInt32 12
} deriving (Eq, Show, Generic)
instance ProtoBuf ExtraInfo

data VideoFile = VideoFile {
    _file_uuid                                 :: ProtoFieldOptional Bytes 1,
    _file_md5                                  :: ProtoFieldOptional Bytes 2,
    _file_name                                 :: ProtoFieldOptional String 3,
    _file_format                               :: ProtoFieldOptional VInt32 4,
    _file_time                                 :: ProtoFieldOptional VInt32 5,
    _file_size                                 :: ProtoFieldOptional VInt32 6,
    _thumb_width                               :: ProtoFieldOptional VInt32 7,
    _thumb_height                              :: ProtoFieldOptional VInt32 8,
    _thumb_file_md5                            :: ProtoFieldOptional Bytes 9,
    _source                                    :: ProtoFieldOptional Bytes 10,
    _thumb_file_size                           :: ProtoFieldOptional VInt32 11,
    _busi_type                                 :: ProtoFieldOptional VInt32 12,
    _from_chat_type                            :: ProtoFieldOptional VInt32 13,
    _to_chat_type                              :: ProtoFieldOptional VInt32 14,
    _bool_support_progressive                  :: ProtoFieldOptional VBool 15,
    _file_width                                :: ProtoFieldOptional VInt32 16,
    _file_height                               :: ProtoFieldOptional VInt32 17,
    _sub_busi_type                             :: ProtoFieldOptional VInt32 18,
    _video_attr                                :: ProtoFieldOptional VInt32 19,
    _bytes_thumb_file_urls                     :: ProtoFieldRepeated Bytes 20,
    _bytes_video_file_urls                     :: ProtoFieldRepeated Bytes 21,
    _thumb_download_flag                       :: ProtoFieldOptional VInt32 22,
    _video_download_flag                       :: ProtoFieldOptional VInt32 23,
    _pb_reserve                                :: ProtoFieldOptional Bytes 24
} deriving (Eq, Show, Generic)
instance ProtoBuf VideoFile

data AnonymousGroupMessage = AnonymousGroupMessage {
    _flags                                     :: ProtoFieldOptional VInt32 1,
    _anon_id                                   :: ProtoFieldOptional Bytes 2,
    _anon_nick                                 :: ProtoFieldOptional Bytes 3,
    _head_portrait                             :: ProtoFieldOptional VInt32 4,
    _expire_time                               :: ProtoFieldOptional VInt32 5,
    _bubble_id                                 :: ProtoFieldOptional VInt32 6,
    _rank_color                                :: ProtoFieldOptional Bytes 7
} deriving (Eq, Show, Generic)
instance ProtoBuf AnonymousGroupMessage

newtype QQWalletMsg = QQWalletMsg {
    _aio_body                                  :: ProtoFieldOptional QQWalletAioBody 1
} deriving (Eq, Show, Generic)
instance ProtoBuf QQWalletMsg

data QQWalletAioBody = QQWalletAioBody {
    _send_uin                                  :: ProtoFieldOptional VUInt64 1,
    _sender                                    :: ProtoFieldOptional QQWalletAioElem 2,
    _receiver                                  :: ProtoFieldOptional QQWalletAioElem 3,
    _channel_id                                :: ProtoFieldOptional VSInt32 4,
    _template_id                               :: ProtoFieldOptional VSInt32 5,
    _resend                                    :: ProtoFieldOptional VUInt32 6,
    _msg_priority                              :: ProtoFieldOptional VUInt32 7,
    _red_type                                  :: ProtoFieldOptional VSInt32 8,
    _bill_no                                   :: ProtoFieldOptional Bytes 9,
    _auth_key                                  :: ProtoFieldOptional Bytes 10,
    _session_type                              :: ProtoFieldOptional VSInt32 11,
    _msg_type                                  :: ProtoFieldOptional VSInt32 12,
    _envel_ope_id                              :: ProtoFieldOptional VSInt32 13,
    _name                                      :: ProtoFieldOptional Bytes 14,
    _conf_type                                 :: ProtoFieldOptional VSInt32 15,
    _msg_from                                  :: ProtoFieldOptional VSInt32 16,
    _pc_body                                   :: ProtoFieldOptional Bytes 17,
    _index                                     :: ProtoFieldOptional Bytes 18,
    _red_channel                               :: ProtoFieldOptional VUInt32 19,
    _grap_uin                                  :: ProtoFieldRepeated VUInt64 20,
    _pb_reserve                                :: ProtoFieldOptional Bytes 21
} deriving (Eq, Show, Generic)
instance ProtoBuf QQWalletAioBody

data QQWalletAioElem = QQWalletAioElem {
    _background                                :: ProtoFieldOptional VUInt32 1,
    _icon                                      :: ProtoFieldOptional VUInt32 2,
    _title                                     :: ProtoFieldOptional Bytes 3,
    _subtitle                                  :: ProtoFieldOptional Bytes 4,
    _content                                   :: ProtoFieldOptional Bytes 5,
    _link_url                                  :: ProtoFieldOptional Bytes 6,
    _black_stripe                              :: ProtoFieldOptional Bytes 7,
    _notice                                    :: ProtoFieldOptional Bytes 8,
    _title_color                               :: ProtoFieldOptional VUInt32 9,
    _subtitle_color                            :: ProtoFieldOptional VUInt32 10,
    _actions_priority                          :: ProtoFieldOptional Bytes 11,
    _jump_url                                  :: ProtoFieldOptional Bytes 12,
    _native_ios                                :: ProtoFieldOptional Bytes 13,
    _native_android                            :: ProtoFieldOptional Bytes 14,
    _icon_url                                  :: ProtoFieldOptional Bytes 15,
    _content_color                             :: ProtoFieldOptional VUInt32 16,
    _content_bg_color                          :: ProtoFieldOptional VUInt32 17,
    _aio_image_left                            :: ProtoFieldOptional Bytes 18,
    _aio_image_right                           :: ProtoFieldOptional Bytes 19,
    _cft_image                                 :: ProtoFieldOptional Bytes 20,
    _pb_reserve                                :: ProtoFieldOptional Bytes 21
} deriving (Eq, Show, Generic)
instance ProtoBuf QQWalletAioElem


data CustomElem = CustomElem {
    _desc                                      :: ProtoFieldOptional Bytes 1,
    _data                                      :: ProtoFieldOptional Bytes 2,
    _enum_type                                 :: ProtoFieldOptional VInt32 3,
    _ext                                       :: ProtoFieldOptional Bytes 4,
    _sound                                     :: ProtoFieldOptional Bytes 5
} deriving (Eq, Show, Generic)
instance ProtoBuf CustomElem

data GeneralFlags = GeneralFlags {
    _bubble_diy_text_id                        :: ProtoFieldOptional VInt32 1,
    _group_flag_new                            :: ProtoFieldOptional VInt32 2,
    _uin                                       :: ProtoFieldOptional VUInt64 3,
    _rp_id                                     :: ProtoFieldOptional Bytes 4,
    _prp_fold                                  :: ProtoFieldOptional VInt32 5,
    _long_text_flag                            :: ProtoFieldOptional VInt32 6,
    _long_text_resid                           :: ProtoFieldOptional Bytes 7,
    _group_type                                :: ProtoFieldOptional VInt32 8,
    _to_uin_flag                               :: ProtoFieldOptional VInt32 9,
    _glamour_level                             :: ProtoFieldOptional VInt32 10,
    _member_level                              :: ProtoFieldOptional VInt32 11,
    _group_rank_seq                            :: ProtoFieldOptional VUInt64 12,
    _olympic_torch                             :: ProtoFieldOptional VInt32 13,
    _babyq_guide_msg_cookie                    :: ProtoFieldOptional Bytes 14,
    _uin32_expert_flag                         :: ProtoFieldOptional VInt32 15,
    _bubble_sub_id                             :: ProtoFieldOptional VInt32 16,
    _pendant_id                                :: ProtoFieldOptional VUInt64 17,
    _rp_index                                  :: ProtoFieldOptional Bytes 18,
    _pb_reserve                                :: ProtoFieldOptional Bytes 19
} deriving (Eq, Show, Generic)
instance ProtoBuf GeneralFlags

data SourceMsg = SourceMsg {
    _orig_seqs                                 :: ProtoFieldRepeated VInt32 1,
    _sender_uin                                :: ProtoFieldOptional VUInt64 2,
    _time                                      :: ProtoFieldOptional VInt32 3,
    _flag                                      :: ProtoFieldOptional VInt32 4,
    _elems                                     :: ProtoFieldRepeated Elem 5,
    _type                                      :: ProtoFieldOptional VInt32 6,
    _rich_msg                                  :: ProtoFieldOptional Bytes 7,
    _pb_reserve                                :: ProtoFieldOptional Bytes 8,
    _src_msg                                   :: ProtoFieldOptional Bytes 9,
    _to_uin                                    :: ProtoFieldOptional VUInt64 10,
    _troop_name                                :: ProtoFieldOptional Bytes 11
} deriving (Eq, Show, Generic)
instance ProtoBuf SourceMsg

data LightAppElem = LightAppElem {
    _data                                      :: ProtoFieldOptional Bytes 1,
    _msg_resid                                 :: ProtoFieldOptional Bytes 2
} deriving (Eq, Show, Generic)
instance ProtoBuf LightAppElem


data CommonElem = CommonElem {
    _service_type                              :: ProtoFieldOptional VInt32 1,
    _pb_elem                                   :: ProtoFieldOptional Bytes 2,
    _business_type                             :: ProtoFieldOptional VInt32 3
} deriving (Eq, Show, Generic)
instance ProtoBuf CommonElem

data Elem = Elem {
    _text                                      :: ProtoFieldOptional Text 1,
    _face                                      :: ProtoFieldOptional Face 2,
    _online_image                              :: ProtoFieldOptional OnlineImage 3,
    _not_online_image                          :: ProtoFieldOptional NotOnlineImage 4,
    _trans_elem_info                           :: ProtoFieldOptional TransElem 5,
    _market_face                               :: ProtoFieldOptional MarketFace 6,
    _custom_face                               :: ProtoFieldOptional CustomFace 8,
    _rich_msg                                  :: ProtoFieldOptional RichMsg 12,
    _group_file                                :: ProtoFieldOptional GroupFile 13,
    _extra_info                                :: ProtoFieldOptional ExtraInfo 16,
    _video_file                                :: ProtoFieldOptional VideoFile 19,
    _anon_group_msg                            :: ProtoFieldOptional AnonymousGroupMessage 21,
    _qq_wallet_msg                             :: ProtoFieldOptional QQWalletMsg 24,
    _custom_elem                               :: ProtoFieldOptional CustomElem 31,
    _general_flags                             :: ProtoFieldOptional GeneralFlags 37,
    _src_msg                                   :: ProtoFieldOptional SourceMsg 45,
    _light_app                                 :: ProtoFieldOptional LightAppElem 51,
    _common_elem                               :: ProtoFieldOptional CommonElem 53
} deriving (Eq, Show, Generic)
instance ProtoBuf Elem

data RichText = RichText {
    _attr                                      :: ProtoFieldOptional Attr 1,
    _elems                                     :: ProtoFieldRepeated Elem 2,
    _not_online_file                           :: ProtoFieldOptional NotOnlineFile 3,
    _ptt                                       :: ProtoFieldOptional Ptt 4
} deriving (Eq, Show, Generic)
instance ProtoBuf RichText

data Ptt = Ptt {
    _file_type                                 :: ProtoFieldOptional VInt32 1,
    _src_uin                                   :: ProtoFieldOptional VUInt64 2,
    _file_uuid                                 :: ProtoFieldOptional Bytes 3,
    _file_md5                                  :: ProtoFieldOptional Bytes 4,
    _file_name                                 :: ProtoFieldOptional String 5,
    _file_size                                 :: ProtoFieldOptional VInt32 6,
    _reserve                                   :: ProtoFieldOptional Bytes 7,
    _file_id                                   :: ProtoFieldOptional VInt32 8,
    _server_ip                                 :: ProtoFieldOptional VInt32 9,
    _server_port                               :: ProtoFieldOptional VInt32 10,
    _bool_valid                                :: ProtoFieldOptional VBool 11,
    _signature                                 :: ProtoFieldOptional Bytes 12,
    _shortcut                                  :: ProtoFieldOptional Bytes 13,
    _file_key                                  :: ProtoFieldOptional Bytes 14,
    _magic_ptt_index                           :: ProtoFieldOptional VInt32 15,
    _voice_switch                              :: ProtoFieldOptional VInt32 16,
    _ptt_url                                   :: ProtoFieldOptional Bytes 17,
    _group_file_key                            :: ProtoFieldOptional Bytes 18,
    _time                                      :: ProtoFieldOptional VInt32 19,
    _down_para                                 :: ProtoFieldOptional Bytes 20,
    _format                                    :: ProtoFieldOptional VInt32 29,
    _pb_reserve                                :: ProtoFieldOptional Bytes 30,
    _bytes_ptt_urls                            :: ProtoFieldRepeated Bytes 31,
    _download_flag                             :: ProtoFieldOptional VInt32 32
} deriving (Eq, Show, Generic)
instance ProtoBuf Ptt

data NotOnlineFile = NotOnlineFile {
    _file_type                                 :: ProtoFieldOptional VInt32 1,
    _sig                                       :: ProtoFieldOptional Bytes 2,
    _file_uuid                                 :: ProtoFieldOptional Bytes 3,
    _file_md5                                  :: ProtoFieldOptional Bytes 4,
    _file_name                                 :: ProtoFieldOptional Bytes 5,
    _file_size                                 :: ProtoFieldOptional VInt64 6,
    _note                                      :: ProtoFieldOptional Bytes 7,
    _reserved                                  :: ProtoFieldOptional VInt32 8,
    _subcmd                                    :: ProtoFieldOptional VInt32 9,
    _micro_cloud                               :: ProtoFieldOptional VInt32 10,
    _bytes_file_urls                           :: ProtoFieldRepeated Bytes 11,
    _download_flag                             :: ProtoFieldOptional VInt32 12,
    _danger_evel                               :: ProtoFieldOptional VInt32 50,
    _life_time                                 :: ProtoFieldOptional VInt32 51,
    _upload_time                               :: ProtoFieldOptional VInt32 52,
    _abs_file_type                             :: ProtoFieldOptional VInt32 53,
    _client_type                               :: ProtoFieldOptional VInt32 54,
    _expire_time                               :: ProtoFieldOptional VInt32 55,
    _pb_reserve                                :: ProtoFieldOptional Bytes 56
} deriving (Eq, Show, Generic)
instance ProtoBuf NotOnlineFile

data MessageBody = MessageBody {
    _rich_text                                 :: ProtoFieldOptional RichText 1,
    _msg_content                               :: ProtoFieldOptional Bytes 2,
    _msg_encrypt_content                       :: ProtoFieldOptional Bytes 3
} deriving (Eq, Show, Generic)
instance ProtoBuf MessageBody

data Message = Message {
    _head                                      :: ProtoFieldOptional MessageHeaad 1,
    _content                                   :: ProtoFieldOptional ContentHead 2,
    _body                                      :: ProtoFieldOptional MessageBody 3
} deriving (Eq, Show, Generic)
instance ProtoBuf Message

data PushMessagePacket = PushMessagePacket {
    _message                                   :: ProtoFieldOptional Message 1,
    _svrip                                     :: ProtoFieldOptional VInt32 2,
    _push_token                                :: ProtoFieldOptional Bytes 3,
    _ping_flag                                 :: ProtoFieldOptional VInt32 4,
    _general_flag                              :: ProtoFieldOptional VInt32 9
} deriving (Eq, Show, Generic)
instance ProtoBuf PushMessagePacket

data ResvAttr = ResvAttr {
    _image_biz_type :: ProtoFieldOptional VUInt32 1,
    _image_show :: ProtoFieldOptional AnimationImageShow 7
} deriving (Eq, Show, Generic)
instance ProtoBuf ResvAttr

data AnimationImageShow = AnimationImageShow {
    _effect_id              :: ProtoFieldOptional VInt32 1,
    _animation_param        :: ProtoFieldOptional Bytes 2
} deriving (Eq, Show, Generic)
instance ProtoBuf AnimationImageShow

data MsgElemInfoServtype3 = MsgElemInfoServtype3 {
    _flash_troop_pic :: ProtoFieldOptional CustomFace 1,
    _flash_c2c_pic :: ProtoFieldOptional NotOnlineImage 2
} deriving (Eq, Show, Generic)
instance ProtoBuf MsgElemInfoServtype3

data MsgElemInfoServtype33 = MsgElemInfoServtype33 {
    _index :: ProtoFieldOptional VUInt32 1,
    _text :: ProtoFieldOptional String 2,
    _compat :: ProtoFieldOptional Bytes 3,
    _buf :: ProtoFieldOptional Bytes 4
} deriving (Eq, Show, Generic)
instance ProtoBuf MsgElemInfoServtype33

data MsgElemInfoServtype37 = MsgElemInfoServtype37 {
    _pack_id :: ProtoFieldOptional Bytes 1,
    _sticker_id :: ProtoFieldOptional Bytes 2,
    _qsid :: ProtoFieldOptional VUInt32 3,
    _source_type :: ProtoFieldOptional VUInt32 4,
    _sticker_type :: ProtoFieldOptional VUInt32 5,
    _result_id :: ProtoFieldOptional Bytes 6,
    _text :: ProtoFieldOptional String 7,
    _surprise_id :: ProtoFieldOptional Bytes 8,
    _random_type :: ProtoFieldOptional VUInt32 9
} deriving (Eq, Show, Generic)
instance ProtoBuf MsgElemInfoServtype37