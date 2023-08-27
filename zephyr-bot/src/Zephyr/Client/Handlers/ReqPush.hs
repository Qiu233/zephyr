{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Zephyr.Client.Handlers.ReqPush where
import Zephyr.Client.Types
import Zephyr.Jce
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Zephyr.Jce.JceMap
import Zephyr.Binary (runGet, Get)
import Zephyr.Jce.Generic
import Data.Int
import Zephyr.Packet.Data.ReqPush
import Zephyr.Client.Internal
import Zephyr.Packet.JceStructs
import Data.Foldable
import Control.Monad.Cont
import Text.Printf (printf)
import Zephyr.Client.TimeoutCache as TimeoutCache
import GHC.Word
import GHC.Generics (Generic)
import Zephyr.Binary.Get (BinGet (getbe), getRemaining)
import Zephyr.Binary.FixedBytes
import Zephyr.Client.Events
import Zephyr.PB.Notify.Group0x857
import Zephyr.ProtoLite
import Control.Monad.ST.Lazy
import Data.STRef.Lazy
import Text.RE.TDFA.ByteString.Lazy as RegEx
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import Zephyr.Binary.OP (utf8FromBytes)
import qualified Data.List
import Zephyr.PB.Data
import Zephyr.PB.MsgType0x210.SubMsgType0x27

data VMsgHead = VMsgHead {
    _group_code :: Int32,
    _i_type :: Word8,
    _discarded :: Word8
} deriving (Show, Eq, Generic)
instance BinGet VMsgHead

data VBody0x0C = VBody0x0C {
    _operator :: Int32,
    _discarded :: FixedBytes 6,
    _target :: Int32,
    _time :: Int32
} deriving (Show, Eq, Generic)
instance BinGet VBody0x0C

handleGeneralGrayTips :: Client -> Int64 -> GeneralGrayTipInfo -> IO ()
handleGeneralGrayTips client group_code_ tip_info_ = do
    when (tip_info_._busi_type.unwrap == 12 && tip_info_._busi_id.unwrap == 1061) $ do
        uin_ <- getUIN client
        let (sender_, receiver_) = runST $ do
                s <- newSTRef 0
                r <- newSTRef $ fromIntegral uin_
                for_ tip_info_._msg_templ_param.unwrap $ \x -> do
                    when (x._name.unwrap == "uin_str1") $ do
                        writeSTRef s $ read @Int64 x._value.unwrap
                    when (x._name.unwrap == "uin_str2") $ do
                        writeSTRef r $ read @Int64 x._value.unwrap
                (,) <$> readSTRef s <*> readSTRef r
        when (sender_ /= 0) $ do
            dispatch client._events._group_notified $ GroupPokeNotify group_code_ sender_ receiver_
    let templ = tip_info_._templ_id.unwrap
    when (templ `elem` [1052, 1053, 1054, 1067]) $ do --群荣誉
        let (nick_, uin_) = runST $ do
                n <- newSTRef ""
                u <- newSTRef 0
                for_ tip_info_._msg_templ_param.unwrap $ \x -> do
                    when (x._name.unwrap == "nick") $ do
                        writeSTRef n x._value.unwrap
                    when (x._name.unwrap == "uin") $ do
                        writeSTRef u $ read @Int64 x._value.unwrap
                (,) <$> readSTRef n <*> readSTRef u
        dispatch client._events._group_notified $ GroupHonerChangedNotify group_code_
            (case templ of
                1052 -> HT_Performer
                1053 -> HT_Talkative
                1054 -> HT_Talkative
                1067 -> HT_Emotion
                _ -> HT_Unknown)
            uin_
            nick_

data TipCommandJson = TipCommandJson {
    _command :: Integer,
    _data :: String,
    _text :: String
} deriving (Show, Eq, Generic)
instance Aeson.FromJSON TipCommandJson where
    parseJSON = Aeson.withObject "TipCommandJson" $ \v -> TipCommandJson
        <$> v .: "cmd"
        <*> v .: "data"
        <*> v .: "text"

handleAIOGrayTips :: Client -> Int64 -> AIOGrayTipsInfo -> IO ()
handleAIOGrayTips client group_code_ tip_info_ = do
    unless (B.null tip_info_._content.unwrap) $ do
        let contents = matches $ tip_info_._content.unwrap *=~ [RegEx.re|<\{.*\}>|]
        let s = mapMaybe (Aeson.decode . B.drop 1 . B.dropEnd 1) contents :: [TipCommandJson]
        when ("头衔" `Data.List.isInfixOf` utf8FromBytes tip_info_._content.unwrap) $ do
            let uin_ = fromMaybe 0 (read @Int64 <$> listToMaybe [v._data | v <- s, v._command == 5])
            let title_ = fromMaybe "" $ listToMaybe [v._text | v <- s, v._command == 1]
            dispatch client._events._group_member_special_title_updated $
                GroupMemberSpecialTitleUpdatedEventArgs {
                    _group_code = group_code_,
                    _uin = uin_,
                    _new_title = title_
            }

handle0x2DC :: Client -> (() -> ContT () IO ()) -> PushMessageInfo -> ContT () IO ()
handle0x2DC client _continue m = do
    let bs = m._v_msg.jval
    let (head_, r) = runGet ((,) <$> getbe <*> getRemaining) bs :: (VMsgHead, B.ByteString)
    if head_._i_type == 0x0C then do
        let body_ = runGet getbe r :: VBody0x0C
        uin_ <- lift $ getUIN client
        when (fromIntegral body_._operator == uin_) $ do
            _continue ()
        lift $ dispatch client._events._group_member_muted $ GroupMemberMutedEventArgs {
            _group_code = fromIntegral head_._group_code,
            _operator_uin = fromIntegral body_._operator,
            _target_uin = fromIntegral body_._target,
            _time = body_._time
        }
    else when (head_._i_type `elem` [0x10, 0x11, 0x14, 0x15]) $ do
        let b = decode (B.drop 1 r) :: NotifyMsgBody
        when (isJust b._opt_msg_recall.pv) $ do
            let recalled = b._opt_msg_recall.unwrap._recalled_msg_list.unwrap
            for_ (filter (\x -> x._msg_type.unwrap == 2) recalled) $ \x -> do
                lift $ dispatch client._events._group_message_recalled $ GroupMessageRecalledEventArgs {
                    _group_code = fromIntegral head_._group_code,
                    _operator_uin = b._opt_msg_recall.unwrap._uin.unwrap,
                    _author_uin = x._author_uin.unwrap,
                    _message_id = x._seq.unwrap,
                    _time = x._time.unwrap
                }
        when (isJust b._opt_general_gray_tip.pv) $ do
            lift $ handleGeneralGrayTips client (fromIntegral head_._group_code) b._opt_general_gray_tip.unwrap
        when (isJust b._opt_msg_red_tips.pv) $ do
            let red_tips_ = b._opt_msg_red_tips.unwrap
            when (red_tips_._lucky_flag.unwrap == 1) $ do
                lift $ dispatch client._events._group_notified $ GroupLuckKingNotify
                    (fromIntegral head_._group_code)
                    (fromIntegral red_tips_._sender_uin.unwrap)
                    (fromIntegral red_tips_._lucky_uin.unwrap)
        when (isJust b._qq_group_digest_msg.pv) $ do
            let digest_ = b._qq_group_digest_msg.unwrap
            lift $ dispatch client._events._group_digest $ GroupDigestEventArgs {
                _group_code = fromIntegral digest_._group_code.unwrap,
                _message_id = fromIntegral digest_._seq.unwrap,
                _internal_message_id = fromIntegral digest_._random.unwrap,
                _operation_type = digest_._op_type.unwrap,
                _operate_time = digest_._op_time.unwrap,
                _sender_uin = fromIntegral digest_._sender.unwrap,
                _operator_uin = fromIntegral digest_._digest_oper.unwrap,
                _sender_nick = digest_._sender_nick.unwrap,
                _operator_nick = digest_._oper_nick.unwrap
            }
        when (isJust b._opt_msg_gray_tips.pv) $ do
            lift $ handleAIOGrayTips client (fromIntegral head_._group_code) b._opt_msg_gray_tips.unwrap

msg0x210SubHandlers :: [(Int64, Client -> B.ByteString -> IO ())]
msg0x210SubHandlers =  [
    (0x8A, msg0x210Sub0x8A),
    (0x8B, msg0x210Sub0x8A),
    (0xB3, msg0x210Sub0xB3),
    (0xD4, msg0x210Sub0xD4),
    (0x27, msg0x210Sub0x27),
    (0x122, msg0x210Sub0x122),
    (0x123, msg0x210Sub0x122),
    (0x44, msg0x210Sub0x44)
    ]
msg0x210Sub0x8A :: Client -> B.ByteString -> IO ()
msg0x210Sub0x8A client bs = do
    let s8a = decode bs :: Sub8A
    for_ s8a._msg_info.unwrap $ \x -> do
        dispatch client._events._friend_message_recalled $ FriendMessageRecalledEventArgs {
            _friend_uin = x._from_uin.unwrap,
            _message_id = x._msg_seq.unwrap,
            _time = x._msg_time.unwrap
        }
msg0x210Sub0xB3 :: Client -> B.ByteString -> IO ()
msg0x210Sub0xB3 client bs = do
    let sb3 = decode bs :: SubB3
    dispatch client._events._friend_new $ FriendNewEventArgs {
        _uin = sb3._msg_add_frd_notify.unwrap._uin.unwrap,
        _nick = sb3._msg_add_frd_notify.unwrap._nick.unwrap
    }
msg0x210Sub0xD4 :: Client -> B.ByteString -> IO ()
msg0x210Sub0xD4 client bs = do
    let sd4 = decode bs :: SubD4
    dispatch client._events._group_left sd4._uin.unwrap
msg0x210Sub0x27 :: Client -> B.ByteString -> IO ()
msg0x210Sub0x27 client bs = do
    let s27 = decode bs :: SubMsg0X27Body
    for_ s27._mod_infos.unwrap $ \x -> do
        when (isJust x._mod_group_profile.pv) $ do
            let profile_ = x._mod_group_profile.unwrap
            for_ profile_._group_profile_infos.unwrap $ \info_ -> do
                when (info_._field.unwrap == 1) $ do
                    dispatch client._events._group_renamed $ GroupRenamedEventArgs {
                        _group_code = fromIntegral profile_._group_code.unwrap,
                        _new_name = info_._value.unwrap,
                        _operator_uin = fromIntegral profile_._cmd_uin.unwrap
                    }
        when (isJust x._del_friend.pv) $ do
            let frd_uinM = listToMaybe x._del_friend.unwrap._uins.unwrap
            when (isJust frd_uinM) $ do
                dispatch client._events._friend_deleted $ fromIntegral $ fromJust frd_uinM
msg0x210Sub0x122 :: Client -> B.ByteString -> IO ()
msg0x210Sub0x122 client bs = do
    let t = decode bs :: GeneralGrayTipInfo
    let (sender_, receiver_) = runST $ do
            s <- newSTRef 0
            r <- newSTRef 0
            for_ t._msg_templ_param.unwrap $ \x -> do
                when (x._name.unwrap == "uin_str1") $ do
                    writeSTRef s $ read @Int64 x._value.unwrap
                when (x._name.unwrap == "uin_str2") $ do
                    writeSTRef r $ read @Int64 x._value.unwrap
            (,) <$> readSTRef s <*> readSTRef r
    uin_ <- getUIN client
    when (sender_ /= 0) $ do
        let r = if receiver_ == 0 then fromIntegral uin_ else receiver_
        dispatch client._events._friend_notified $ FriendPokeNotify sender_ r
msg0x210Sub0x44 :: Client -> B.ByteString -> IO ()
msg0x210Sub0x44 client bs = do
    let s44 = decode bs :: Sub44
    guard (isJust s44._group_sync_msg.pv)
    let group_sync_msg_ = s44._group_sync_msg.unwrap
    dispatch client._events._group_members_sync group_sync_msg_._grp_code.unwrap

handleReqPush :: Client -> QQPacket -> IO ()
handleReqPush client pkt = do
    let request_ = jceUnmarshal pkt._pkt_body :: RequestPacket
    let data_ = jceUnmarshal request_._s_buffer.jval :: RequestDataVersion2
    let m_ = data_._map.jval
    let jr = B.drop 1 $ fromMaybe B.empty (jlookup "req" m_ >>= jlookup "OnlinePushPack.SvcReqPushMsg")
    let (uin_, msgInfos_) = flip runGet jr $ do
            _uin <- jget :: Get (JceField Int64 0)
            _msgInfos <- jget :: Get (JceField [PushMessageInfo] 2)
            pure (_uin.jval, _msgInfos.jval)
    req <- withContext (buildDeleteOnlinePushPacket uin_ 0 B.empty pkt._pkt_seq msgInfos_) client
    sendPacket req client
    --client._logger.logInfo $ show msgInfos_
    let cache_ = client._online_push_cache
    for_ msgInfos_ $ \x -> do
        flip runContT pure $ do
            callCC $ \_continue -> do
                let k = printf "%d-%d-%d" x._msg_seq.jval x._msg_time.jval x._msg_uid.jval :: String
                f <- lift $ TimeoutCache.lookup k cache_
                when (isJust f) $ do
                    _continue ()
                _ <- lift $ insert 30000000 k () cache_
                let msg_type_ = x._msg_type.jval
                case msg_type_ of
                    0x2dc -> handle0x2DC client _continue x
                    0x210 -> do
                        let (sub_type_, r_) = flip runGet x._v_msg.jval $ do
                                _sub_type <- jget :: Get (JceField Int64 0)
                                _r <- getRemaining
                                pure (_sub_type.jval, _r)
                        let vM = Data.List.lookup sub_type_ msg0x210SubHandlers
                        when (isJust vM) $ do
                            let p = runGet (jget :: Get (JceField B.ByteString 10)) r_
                            lift $ fromJust vM client p.jval
                    _ -> pure ()