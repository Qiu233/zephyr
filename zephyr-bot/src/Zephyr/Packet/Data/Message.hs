{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE QuasiQuotes #-}
module Zephyr.Packet.Data.Message where
import Zephyr.PB.Msg
import Zephyr.Message.Elements
import Control.Monad.State
import ProtoLite
import Control.Lens
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Binary
import Zephyr.PB.Msg.ObjMsg
import qualified Codec.Compression.Zlib as ZLib
import Zephyr.Utils.Common (utf8FromBytes, encodeHex)
import Data.Int
import Zephyr.PB.Msg.TextMsgExt
import Control.Monad.Cont
import Data.List (isInfixOf, isPrefixOf)
import Text.RE.TDFA.String as RegEx
import Text.Printf
import Zephyr.Message.Image
import qualified Data.HashMap
import Zephyr.Message.Face


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

data ParseContext = ParseContext {
    _break :: [MessageElement] -> ContT [MessageElement] Identity (),
    _yield :: MessageElement -> ContT [MessageElement] Identity (),
    _continue :: ContT [MessageElement] Identity (),
    _elem :: Elem
}

type MsgParser = ParseContext -> ContT [MessageElement] Identity ()


tryParseReply :: MsgParser
tryParseReply (ParseContext _break _yield _continue x) = do
    when (isJust x._src_msg.pv && not (null x._src_msg.optOrDef._orig_seqs.pv.repeatedF)) $ do
        _yield $ ReplyElement $ ReplyElementArgs {
            _reply_seq = head $ repeatedV' x._src_msg.optOrDef._orig_seqs,
            _time = x._src_msg.optOrDef._time.optOrDefV,
            _sender = fromIntegral x._src_msg.optOrDef._sender_uin.optOrDefV,
            _group_id = fromIntegral x._src_msg.optOrDef._to_uin.optOrDefV,
            _elements = parseMessageElems $ repeated' x._src_msg.optOrDef._elems
        }

tryParseQFile :: MsgParser
tryParseQFile (ParseContext _break _yield _continue x) = do
    when (isJust x._trans_elem_info.pv && x._trans_elem_info.optOrDef._elem_type.optOrDef == 24) $ do
        let ev = x._trans_elem_info.optOrDef._elem_value.optOrDef
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
                    let mf = info._msg_file.optOrDef
                    _yield $ GroupFileElement $ GroupFileElementArgs {
                        _name = mf._file_name.optOrDef,
                        _size = mf._file_size.optOrDefV,
                        _path = mf._file_path.optOrDef,
                        _bus_id = mf._bus_id.optOrDefV
                    }

tryParseLightApp :: MsgParser
tryParseLightApp (ParseContext _break _yield _continue x) = do
    when (isJust x._light_app.pv && B.length x._light_app.optOrDef._data.optOrDef > 1) $ do
        let data_ = x._light_app.optOrDef._data.optOrDef
        let content_ = if B.head data_ == 0
            then B.tail data_
            else ZLib.decompress $ B.tail data_
        when (B.length content_ > 0 && B.length content_ < 1024*1024*1024) $ do
            _yield $ LightAppElement $ utf8FromBytes content_

tryParseShortVideo :: MsgParser
tryParseShortVideo (ParseContext _break _yield _continue x) = do
    when (isJust x._video_file.pv) $ do
        _yield $ ShortVideoElement $ ShortVideoElementArgs {
            _name = x._video_file.optOrDef._file_name.optOrDef,
            _uuid = x._video_file.optOrDef._file_uuid.optOrDef,
            _size = x._video_file.optOrDef._file_size.optOrDefV,
            _thumb_size = x._video_file.optOrDef._thumb_file_size.optOrDefV,
            _md5 = x._video_file.optOrDef._file_md5.optOrDef,
            _thumb_md5 = x._video_file.optOrDef._thumb_file_md5.optOrDef
        }

tryParseText :: MsgParser
tryParseText (ParseContext _break _yield _continue x) = do
    when (isJust x._text.pv) $ do
        let text_ = x._text.optOrDef
        let str_ = text_._str.pv
        -- TODO: review early exits
        when (B.length text_._attr6_buf.optOrDef > 0) $ do
            let target_ = fromIntegral $ flip runGet text_._attr6_buf.optOrDef $ skip 7 >> get32be
            _yield $ parseAt target_ str_ AT_GroupMember
        when (B.length text_._pb_reserve.optOrDef > 0) $ do
            let resv = decode text_._pb_reserve.optOrDef :: TextResvAttr
            when (resv._at_type.optOrDef == 2) $ do
                let v_ = fromIntegral resv._at_member_tiny_id.optOrDefV
                _yield $ parseAt v_ str_ AT_GuildMember
            when (resv._at_type.optOrDef == 4) $ do
                let v_ = fromIntegral resv._at_channel_info.optOrDef._channel_id.optOrDefV
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
        let temp = x._rich_msg.optOrDef._template1.optOrDef
        let content_ = utf8FromBytes $ case B.head temp of
                0 -> B.tail temp
                1 -> ZLib.decompress $ B.tail temp
                _ -> B.empty
        unless (null content_) $ do
            let sid = x._rich_msg.optOrDef._service_id.optOrDefV
            when (sid == 35) $ do
                let s = forwardInfoFromXML content_
                maybe _continue _yield s
            when (sid == 33) $ do
                _continue
            when ("<?xml" `isInfixOf` content_) $ do
                _yield $ newRichXml content_ $ fromIntegral sid
            -- TODO: handle json
            _yield $ TextElement content_

tryParseCustomFace :: MsgParser
tryParseCustomFace (ParseContext _break _yield _continue x) = do
    when (isJust x._custom_face.pv) $ do
        let face_ = x._custom_face.optOrDef
        when (B.null face_._md5.optOrDef) $ do
            _continue
        let url = if null face_._orig_url.optOrDef
                then printf "https://gchat.qpic.cn/gchatpic_new/0/0-0-%s/0?term=2" $ encodeHex face_._md5.optOrDef
                else "https://gchat.qpic.cn" ++ face_._orig_url.optOrDef
        when ("qmeet" `isInfixOf` face_._orig_url.optOrDef) $ do
            -- guild
            undefined
        let biz_type_ = if B.null face_._pb_reserve.optOrDef
            then UnknownBizType
            else
                let attr = decode face_._pb_reserve.optOrDef :: ResvAttr
                in toEnum $ fromIntegral attr._image_biz_type.optOrDef
        _yield $ GroupImageElement GroupImageElementArgs {
            _file_id = fromIntegral face_._file_id.optOrDefV,
            _image_id = face_._file_path.optOrDef,
            _size = face_._size.optOrDefV,
            _width = face_._width.optOrDefV,
            _height = face_._height.optOrDefV,
            _url = url,
            _image_biz_type = biz_type_,
            _md5 = face_._md5.optOrDef
        }

tryParseMarketFace :: MsgParser
tryParseMarketFace (ParseContext _break _yield _continue x) = do
    when (isJust x._market_face.pv) $ do
        let face_ = x._market_face.optOrDef
        _yield $ MarketFaceElement $ MarketFaceElementArgs {
            _name = utf8FromBytes face_._face_name.optOrDef,
            _face_id = face_._face_id.optOrDef,
            _tab_id = face_._tab_id.optOrDefV,
            _item_type = face_._item_type.optOrDefV,
            _sub_type = face_._sub_type.optOrDefV,
            _media_type = face_._media_type.optOrDefV,
            _encrypt_key = face_._key.optOrDef,
            _content = utf8FromBytes face_._mobile_param.optOrDef
        }

tryParseOfflineImage :: MsgParser
tryParseOfflineImage (ParseContext _break _yield _continue x) = do
    when (isJust x._not_online_image.pv) $ do
        let img = x._not_online_image.optOrDef
        let url | isJust img._pb_reserve.pv && not (null img._pb_reserve.optOrDef._url.optOrDef) = printf "https://c2cpicdw.qpic.cn%s&spec=0&rf=naio" img._pb_reserve.optOrDef._url.optOrDef
                | not (null img._orig_url.optOrDef) = "https://c2cpicdw.qpic.cn" ++ img._orig_url.optOrDef
                | otherwise =
                    let dp = img._download_path.optOrDef
                        dp_ = if null dp then img._res_id.optOrDef else dp
                        dp__ = if "/" `isPrefixOf` dp_ then tail dp_ else dp_
                    in "https://c2cpicdw.qpic.cn/offpic_new/0/" ++ dp__ ++ "/0?term=3"
        _yield $ FriendImageElement $ FriendImageElementArgs {
            _image_id = img._file_path.optOrDef,
            _size = img._file_len.optOrDefV,
            _url = url,
            _md5 = img._pic_md5.optOrDef
        }

tryParseQQWalletMessage :: MsgParser
tryParseQQWalletMessage (ParseContext _break _yield _continue x) = do
    when (isJust x._qq_wallet_msg.pv) $ do
        let msg = x._qq_wallet_msg.optOrDef
        when (isJust msg._aio_body.pv) $ do
            let body_ = msg._aio_body.optOrDef
            let msg_type_ = body_._msg_type.optOrDefV
            when (msg_type_ <= 1000 && isJust body_._red_type.pv) $ do
                _break [RedBagElement (toEnum $ fromIntegral msg_type_) $ utf8FromBytes body_._receiver.optOrDef._title.optOrDef]

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
        _yield $ newFace x._face.optOrDef._index.optOrDefV

tryParseCommonElem :: MsgParser
tryParseCommonElem (ParseContext _break _yield _continue x) = do
    when (isJust x._common_elem.pv) $ do
        let ce = x._common_elem.optOrDef
        let stype = ce._service_type.optOrDefV
        case stype of
            3 -> do
                let flash = decode ce._pb_elem.optOrDef :: MsgElemInfoServtype3
                when (isJust flash._flash_troop_pic.pv) $ do
                    let tr = flash._flash_troop_pic.optOrDef
                    _break [GroupImageElement $ GroupImageElementArgs {
                        _file_id = fromIntegral tr._file_id.optOrDefV,
                        _image_id = tr._file_path.optOrDef,
                        _image_biz_type = UnknownBizType,
                        _size = tr._size.optOrDefV,
                        _width = tr._width.optOrDefV,
                        _height = tr._height.optOrDefV,
                        _md5 = tr._md5.optOrDef,
                        _url = tr._orig_url.optOrDef
                    }]
                when (isJust flash._flash_c2c_pic.pv) $ do
                    let c2c = flash._flash_c2c_pic.optOrDef
                    _break [FriendImageElement $ FriendImageElementArgs {
                        _image_id = c2c._file_path.optOrDef,
                        _size = c2c._file_len.optOrDefV,
                        _md5 = c2c._pic_md5.optOrDef,
                        _url = c2c._orig_url.optOrDef
                    }]
            33 -> do
                let sub = decode ce._pb_elem.optOrDef :: MsgElemInfoServtype33
                _yield $ newFace $ fromIntegral sub._index.optOrDefV
            37 -> do
                let sub = decode ce._pb_elem.optOrDef :: MsgElemInfoServtype37
                let s = sub._text.optOrDef
                let s_ = if "/" `isPrefixOf` s then tail s else s
                _break [AnimatedSticker (fromIntegral sub._qsid.optOrDefV) s_]
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