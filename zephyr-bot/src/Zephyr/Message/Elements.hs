{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Zephyr.Message.Elements where
import qualified Data.ByteString.Lazy as B
import Data.Int
import Zephyr.PB.Msg (Ptt)
import Text.Printf (printf)
import Zephyr.Message.Image

data VoiceElementArgs = VoiceElementArgs {
    _name :: String,
    _md5 :: B.ByteString,
    _size :: Int32,
    _url :: String,
    _data :: B.ByteString
} deriving (Eq, Show)

data GroupFileElementArgs = GroupFileElementArgs {
    _name :: String,
    _size :: Int64,
    _path :: String,
    _bus_id :: Int32
} deriving (Eq, Show)

data ReplyElementArgs = ReplyElementArgs {
    _reply_seq :: Int32,
    _sender :: Int64,
    _group_id :: Int64,
    _time :: Int32,
    _elements :: [MessageElement]
} deriving (Eq, Show)

data ShortVideoElementArgs = ShortVideoElementArgs {
    _name :: String,
    _uuid :: B.ByteString,
    _size :: Int32,
    _thumb_size :: Int32,
    _md5 :: B.ByteString,
    _thumb_md5 :: B.ByteString
} deriving (Eq, Show)

data MusicShareElementArgs = MusicShareElementArgs {
    _music_type :: Int,
    _title :: String,
    _brief :: String,
    _summary :: String,
    _url :: String,
    _picture_url :: String,
    _music_url :: String
} deriving (Eq, Show)

data ServiceElementArgs = ServiceElementArgs {
    _id :: Int32,
    _content :: String,
    _sub_type :: String
} deriving (Eq, Show)

data ForwardElementArgs = ForwardElementArgs {
    _file_name :: String,
    _res_id :: String
} deriving (Eq, Show)

data GroupImageElementArgs = GroupImageElementArgs {
    _image_id :: String,
    _file_id :: Int64,
    _image_biz_type :: ImageBizType,
    _size :: Int32,
    _width :: Int32,
    _height :: Int32,
    _md5 :: B.ByteString,
    _url :: String
} deriving (Eq, Show)

data AtType = AT_GroupMember | AT_GuildMember | AT_GuildChannel
    deriving (Eq, Show, Enum)

data RedBagType =
    RB_Unknown_0
    | RB_Unknown_1
    | RedBagSimple -- 2
    | RedBagLucky -- 3
    -- more types to be added
    deriving (Eq, Show, Enum)

data MarketFaceElementArgs = MarketFaceElementArgs {
    _name :: String,
    _face_id :: B.ByteString,
    _tab_id :: Int32,
    _item_type :: Int32,
    _sub_type :: Int32,
    _media_type :: Int32,
    _encrypt_key :: B.ByteString,
    _content :: String
} deriving (Eq, Show)

data FriendImageElementArgs = FriendImageElementArgs {
    _image_id :: String,
    _md5 :: B.ByteString,
    _size :: Int32,
    _url :: String
} deriving (Eq, Show)

data MessageElement =
    TextElement String
    | VoiceElement VoiceElementArgs
    | GroupVoiceElement VoiceElementArgs Ptt
    | PrivateVoiceElement VoiceElementArgs Ptt
    | FaceElement Int32 String
    | AtElement AtType Int64 String
    | GroupFileElement GroupFileElementArgs
    | ReplyElement ReplyElementArgs
    | ShortVideoElement ShortVideoElementArgs
    | ServiceElement ServiceElementArgs
    | LightAppElement String
    | RedBagElement RedBagType String
    | MusicShareElement MusicShareElementArgs
    | AnimatedSticker Int32 String
    | ForwardElement ForwardElementArgs
    | GroupImageElement GroupImageElementArgs
    | FriendImageElement FriendImageElementArgs
    | MarketFaceElement MarketFaceElementArgs
    deriving (Eq)

instance Show MessageElement where
    show (TextElement s) = printf "[文字]: %s" $ show s
    show (VoiceElement v) = printf "[语音]: %s" $ show v
    show (GroupVoiceElement v _) = printf "[语音]: %s" $ show v
    show (PrivateVoiceElement v _) = printf "[语音]: %s" $ show v
    show (FaceElement i s) = printf "[表情]: %s" s
    show (AtElement t i s) = printf "[At|%s]: %s" (show t) s
    show (GroupFileElement g) = printf "[文件]: %s" $ show g
    show (ReplyElement r) = printf "[回复]: %s" $ show r
    show (ShortVideoElement s) = printf "[短视频]: %s" $ show s
    show (ServiceElement s) = printf "[服务消息]: %s" $ show s
    show (LightAppElement s) = printf "[小程序]: %s" s
    show (RedBagElement t s) = printf "[%s]: %s" (show t) s
    show (MusicShareElement m) = printf "[分享音乐]: %s" $ show m
    show (AnimatedSticker i s) = printf "[动态表情]: %s" s
    show (ForwardElement f) = printf "[合并转发]: %s" $ show f
    show (GroupImageElement g) = printf "[群图片]: %s" $ show g
    show (FriendImageElement f) = printf "[好友图片]: %s" $ show f
    show (MarketFaceElement m) = printf "[特殊贴片]: %s" $ show m