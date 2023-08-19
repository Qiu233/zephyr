{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE QuasiQuotes #-}
module Zephyr.Packet.Data.Message (
    parseMessageElems
) where
import Zephyr.PB.Msg
import Zephyr.Message.Elements
import Control.Monad.State
import Zephyr.ProtoLite
import Control.Lens
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import Zephyr.Binary
import Zephyr.Binary.Get
import Zephyr.Binary.OP
import Zephyr.PB.Msg.ObjMsg
import qualified Codec.Compression.Zlib as ZLib
import Zephyr.Utils.Common (encodeHex)
import Data.Int
import Zephyr.PB.Msg.TextMsgExt
import Control.Monad.Cont
import Data.List (isInfixOf, isPrefixOf)
import Text.RE.TDFA.String as RegEx
import Text.Printf
import Zephyr.Message.Image
import qualified Data.HashMap
import Zephyr.Message.Face
import Zephyr.Utils.Codec.JSON


parseAt :: Int64 -> Maybe String -> AtType -> MessageElement
parseAt target nameM at_ = AtElement at_ target s
    where s | isJust nameM = fromJust nameM
            | target == 0 = "@全体成员"
            | otherwise = "@" ++ show target

forwardInfoFromXML :: String -> Maybe MessageElement
forwardInfoFromXML xml = do
    let r1 = matchedText $ xml ?=~ [RegEx.re|m_resid="(.*)"|]
    let r2 = matchedText $ xml ?=~ [RegEx.re|m_fileName="(.*)"|]
    let r1_ = maybe "" (drop 9 .  dropEnd 1) r1
    let r2_ = maybe "" (drop 12 .  dropEnd 1) r2
    if null r1_ && null r2_ then Nothing
    else Just $ ForwardElement $ ForwardElementArgs {
            _res_id = r1_,
            _file_name = r2_
        }
    where
        dropEnd n = reverse . drop n . reverse

newRichXml :: String -> Int64 -> MessageElement
newRichXml template_ s_id_ = do
    ServiceElement $ ServiceElementArgs {
            _id = fromIntegral $ if s_id_ == 0 then 60 else s_id_,
            _content = template_,
            _sub_type = "xml"
        }

newRichJson :: String -> MessageElement
newRichJson json_ = do
    ServiceElement $ ServiceElementArgs {
            _id = 1,
            _content = json_,
            _sub_type = "json"
        }


data ParseContext = ParseContext {
    _break :: [MessageElement] -> ContT [MessageElement] Identity (),
    _yield :: MessageElement -> ContT [MessageElement] Identity (),
    _continue :: ContT [MessageElement] Identity (),
    _elem :: Elem
}

type MsgParser = ParseContext -> ContT [MessageElement] Identity ()


tryParseReply :: MsgParser
tryParseReply (ParseContext _break _yield _continue x) = do
    when (isJust x._src_msg.pv && not (null x._src_msg.unwrap._orig_seqs.pv.repeatedF)) $ do
        _yield $ ReplyElement $ ReplyElementArgs {
            _reply_seq = head x._src_msg.unwrap._orig_seqs.unwrap,
            _time = x._src_msg.unwrap._time.unwrap,
            _sender = fromIntegral x._src_msg.unwrap._sender_uin.unwrap,
            _group_id = fromIntegral x._src_msg.unwrap._to_uin.unwrap,
            _elements = parseMessageElems x._src_msg.unwrap._elems.unwrap
        }

tryParseQFile :: MsgParser
tryParseQFile (ParseContext _break _yield _continue x) = do
    when (isJust x._trans_elem_info.pv && x._trans_elem_info.unwrap._elem_type.unwrap == 24) $ do
        let ev = x._trans_elem_info.unwrap._elem_value.unwrap
        when (B.length ev > 3) $ do
            let (h_, pb_) = flip runGet ev $ do
                    h <- get8
                    pbLen <- get16be
                    pb <- getbs $ fromIntegral pbLen
                    pure (h, pb)
            when (h_ == 1) $ do
                let omsg = decode pb_ :: ObjMsg
                let infos_ = omsg._msg_content_info.pv.repeatedF
                unless (null infos_) $ do
                    let info = head infos_
                    let mf = info._msg_file.unwrap
                    _yield $ GroupFileElement $ GroupFileElementArgs {
                        _name = mf._file_name.unwrap,
                        _size = mf._file_size.unwrap,
                        _path = mf._file_path.unwrap,
                        _bus_id = mf._bus_id.unwrap
                    }

tryParseLightApp :: MsgParser
tryParseLightApp (ParseContext _break _yield _continue x) = do
    when (isJust x._light_app.pv && B.length x._light_app.unwrap._data.unwrap > 1) $ do
        let data_ = x._light_app.unwrap._data.unwrap
        let content_ = if B.head data_ == 0
            then B.tail data_
            else ZLib.decompress $ B.tail data_
        when (B.length content_ > 0 && B.length content_ < 1024*1024*1024) $ do
            _yield $ LightAppElement $ utf8FromBytes content_

tryParseShortVideo :: MsgParser
tryParseShortVideo (ParseContext _break _yield _continue x) = do
    when (isJust x._video_file.pv) $ do
        _yield $ ShortVideoElement $ ShortVideoElementArgs {
            _name = x._video_file.unwrap._file_name.unwrap,
            _uuid = x._video_file.unwrap._file_uuid.unwrap,
            _size = x._video_file.unwrap._file_size.unwrap,
            _thumb_size = x._video_file.unwrap._thumb_file_size.unwrap,
            _md5 = x._video_file.unwrap._file_md5.unwrap,
            _thumb_md5 = x._video_file.unwrap._thumb_file_md5.unwrap
        }

tryParseText :: MsgParser
tryParseText (ParseContext _break _yield _continue x) = do
    when (isJust x._text.pv) $ do
        let text_ = x._text.unwrap
        let str_ = text_._str.pv
        when (B.length text_._attr6_buf.unwrap > 0) $ do
            let target_ = fromIntegral $ flip runGet text_._attr6_buf.unwrap $ skip 7 >> get32be
            _yield $ parseAt target_ str_ AT_GroupMember
        when (B.length text_._pb_reserve.unwrap > 0) $ do
            let resv = decode text_._pb_reserve.unwrap :: TextResvAttr
            when (resv._at_type.unwrap == 2) $ do
                let v_ = fromIntegral resv._at_member_tiny_id.unwrap
                _yield $ parseAt v_ str_ AT_GuildMember
            when (resv._at_type.unwrap == 4) $ do
                let v_ = fromIntegral resv._at_channel_info.unwrap._channel_id.unwrap
                _yield $ parseAt v_ str_ AT_GuildChannel
        let str__ = optOrDef str_
        let v = if '\r' `elem` str__ && not ("\r\n" `Data.List.isInfixOf` str__) then do
                    flip map str__ $ \c ->
                        if c == '\r' then '\n' else c
                else str__
        _yield $ TextElement v

tryParseRichMessage :: MsgParser
tryParseRichMessage (ParseContext _break _yield _continue x) = do
    when (isJust x._rich_msg.pv) $ do
        let temp = x._rich_msg.unwrap._template1.unwrap
        let content_ = utf8FromBytes $ case B.head temp of
                0 -> B.tail temp
                1 -> ZLib.decompress $ B.tail temp
                _ -> B.empty
        unless (null content_) $ do
            let sid = x._rich_msg.unwrap._service_id.unwrap
            when (sid == 35) $ do
                let s = forwardInfoFromXML content_
                maybe _continue _yield s
            when (sid == 33) $ do
                _continue
            when ("<?xml" `isInfixOf` content_) $ do
                _yield $ newRichXml content_ $ fromIntegral sid
            when (validateJSON $ utf8ToBytes content_) $ do
                _yield $ newRichJson content_
            _yield $ TextElement content_

tryParseCustomFace :: MsgParser
tryParseCustomFace (ParseContext _break _yield _continue x) = do
    when (isJust x._custom_face.pv) $ do
        let face_ = x._custom_face.unwrap
        when (B.null face_._md5.unwrap) $ do
            _continue
        let url = if null face_._orig_url.unwrap
                then printf "https://gchat.qpic.cn/gchatpic_new/0/0-0-%s/0?term=2" $ encodeHex face_._md5.unwrap
                else "https://gchat.qpic.cn" ++ face_._orig_url.unwrap
        when ("qmeet" `isInfixOf` face_._orig_url.unwrap) $ do
            -- guild
            undefined
        let biz_type_ = if B.null face_._pb_reserve.unwrap
            then UnknownBizType
            else
                let attr = decode face_._pb_reserve.unwrap :: ResvAttr
                in toEnum $ fromIntegral attr._image_biz_type.unwrap
        _yield $ GroupImageElement GroupImageElementArgs {
            _file_id = fromIntegral face_._file_id.unwrap,
            _image_id = face_._file_path.unwrap,
            _size = face_._size.unwrap,
            _width = face_._width.unwrap,
            _height = face_._height.unwrap,
            _url = url,
            _image_biz_type = biz_type_,
            _md5 = face_._md5.unwrap
        }

tryParseMarketFace :: MsgParser
tryParseMarketFace (ParseContext _break _yield _continue x) = do
    when (isJust x._market_face.pv) $ do
        let face_ = x._market_face.unwrap
        _yield $ MarketFaceElement $ MarketFaceElementArgs {
            _name = utf8FromBytes face_._face_name.unwrap,
            _face_id = face_._face_id.unwrap,
            _tab_id = face_._tab_id.unwrap,
            _item_type = face_._item_type.unwrap,
            _sub_type = face_._sub_type.unwrap,
            _media_type = face_._media_type.unwrap,
            _encrypt_key = face_._key.unwrap,
            _content = utf8FromBytes face_._mobile_param.unwrap
        }

tryParseOfflineImage :: MsgParser
tryParseOfflineImage (ParseContext _break _yield _continue x) = do
    when (isJust x._not_online_image.pv) $ do
        let img = x._not_online_image.unwrap
        let url | isJust img._pb_reserve.pv && not (null img._pb_reserve.unwrap._url.unwrap) = printf "https://c2cpicdw.qpic.cn%s&spec=0&rf=naio" img._pb_reserve.unwrap._url.unwrap
                | not (null img._orig_url.unwrap) = "https://c2cpicdw.qpic.cn" ++ img._orig_url.unwrap
                | otherwise =
                    let dp = img._download_path.unwrap
                        dp_ = if null dp then img._res_id.unwrap else dp
                        dp__ = if "/" `isPrefixOf` dp_ then tail dp_ else dp_
                    in "https://c2cpicdw.qpic.cn/offpic_new/0/" ++ dp__ ++ "/0?term=3"
        _yield $ FriendImageElement $ FriendImageElementArgs {
            _image_id = img._file_path.unwrap,
            _size = img._file_len.unwrap,
            _url = url,
            _md5 = img._pic_md5.unwrap
        }

tryParseQQWalletMessage :: MsgParser
tryParseQQWalletMessage (ParseContext _break _yield _continue x) = do
    when (isJust x._qq_wallet_msg.pv) $ do
        let msg = x._qq_wallet_msg.unwrap
        when (isJust msg._aio_body.pv) $ do
            let body_ = msg._aio_body.unwrap
            let msg_type_ = body_._msg_type.unwrap
            when (msg_type_ <= 1000 && isJust body_._red_type.pv) $ do
                _break [RedBagElement (toEnum $ fromIntegral msg_type_) $ utf8FromBytes body_._receiver.unwrap._title.unwrap]

newFace :: Int32 -> MessageElement
newFace idx = do
    let nameMaybe = Data.HashMap.lookup idx builtInFaces
    let name = case nameMaybe of
                Nothing -> printf "未知表情"
                Just n -> n
    FaceElement idx name

tryParseFace :: MsgParser
tryParseFace (ParseContext _break _yield _continue x) = do
    when (isJust x._face.pv) $ do
        _yield $ newFace x._face.unwrap._index.unwrap

tryParseCommonElem :: MsgParser
tryParseCommonElem (ParseContext _break _yield _continue x) = do
    when (isJust x._common_elem.pv) $ do
        let ce = x._common_elem.unwrap
        let stype = ce._service_type.unwrap
        case stype of
            3 -> do
                let flash = decode ce._pb_elem.unwrap :: MsgElemInfoServtype3
                when (isJust flash._flash_troop_pic.pv) $ do
                    let tr = flash._flash_troop_pic.unwrap
                    _break [GroupImageElement $ GroupImageElementArgs {
                        _file_id = fromIntegral tr._file_id.unwrap,
                        _image_id = tr._file_path.unwrap,
                        _image_biz_type = UnknownBizType,
                        _size = tr._size.unwrap,
                        _width = tr._width.unwrap,
                        _height = tr._height.unwrap,
                        _md5 = tr._md5.unwrap,
                        _url = tr._orig_url.unwrap
                    }]
                when (isJust flash._flash_c2c_pic.pv) $ do
                    let c2c = flash._flash_c2c_pic.unwrap
                    _break [FriendImageElement $ FriendImageElementArgs {
                        _image_id = c2c._file_path.unwrap,
                        _size = c2c._file_len.unwrap,
                        _md5 = c2c._pic_md5.unwrap,
                        _url = c2c._orig_url.unwrap
                    }]
            33 -> do
                let sub = decode ce._pb_elem.unwrap :: MsgElemInfoServtype33
                _yield $ newFace $ fromIntegral sub._index.unwrap
            37 -> do
                let sub = decode ce._pb_elem.unwrap :: MsgElemInfoServtype37
                let s = sub._text.unwrap
                let s_ = if "/" `isPrefixOf` s then tail s else s
                _break [AnimatedSticker (fromIntegral sub._qsid.unwrap) s_]
            _ -> pure ()


parseMessageElems :: [Elem] -> [MessageElement]
parseMessageElems elems = do
    flip runCont id $ do
        callCC $ \break' -> do
            catMaybes <$> sequence (flip fmap elems $ \x -> do
                callCC $ \ex' -> do
                    let yield' = ex' . Just
                    let continue' = ex' Nothing
                    let ctx = ParseContext break' yield' continue' x
                    tryParseReply ctx
                    tryParseQFile ctx
                    tryParseLightApp ctx
                    tryParseShortVideo ctx
                    tryParseText ctx
                    tryParseRichMessage ctx
                    tryParseCustomFace ctx
                    tryParseMarketFace ctx
                    tryParseOfflineImage ctx
                    tryParseQQWalletMessage ctx
                    tryParseFace ctx
                    tryParseCommonElem ctx
                    pure Nothing)