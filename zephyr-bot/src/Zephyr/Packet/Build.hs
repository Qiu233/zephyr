{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zephyr.Packet.Build (
    buildOicqRequestPacket,
    packRequest,
    TLV(..),
    packBody,
    buildUniRequestData,
    uniPackRequest,
    uniPackRequestWithSeq,
    packOIDBPackage
) where
import qualified Data.ByteString.Lazy as B
import Data.Word
import Zephyr.Binary
import Control.Monad
import Zephyr.Core.QQContext
import Zephyr.Core.Codec
import Control.Lens ((^.), view)
import Control.Monad.IO.Class
import Zephyr.Core.Request
import Zephyr.Core.Transport
import Zephyr.Core.Signature
import Zephyr.Packet.Internal
import qualified Zephyr.Encrypt.QQTea as QQTea
import Zephyr.Core.AppVersion
import Zephyr.Core.Device.Types
import Zephyr.Packet.Wrapper (wsign)
import Zephyr.ProtoLite as PL
import qualified Zephyr.Packet.Oicq as Oicq
import Zephyr.PB.Data
import Data.Int
import Zephyr.PB.OIDB
import Control.Monad.Reader (asks)
import Zephyr.Binary.Put


data TLV = TLV {
    _tlv_tag :: Word16,
    _tlv_data :: [B.ByteString]
}
marshalTLVs :: TLV -> B.ByteString
marshalTLVs (TLV tag_ data_) = do
    runPut $ do
        put16be tag_
        put16be $ fromIntegral $ length data_
        forM_ data_ putbs


whiteListCommands :: [String]
whiteListCommands =
    ["ConnAuthSvr.fast_qq_login",
    "ConnAuthSvr.sdk_auth_api",
    "ConnAuthSvr.sdk_auth_api_emp",
    "FeedCloudSvr.trpc.feedcloud.commwriter.ComWriter.DoBarrage",
    "FeedCloudSvr.trpc.feedcloud.commwriter.ComWriter.DoComment",
    "FeedCloudSvr.trpc.feedcloud.commwriter.ComWriter.DoFollow",
    "FeedCloudSvr.trpc.feedcloud.commwriter.ComWriter.DoLike",
    "FeedCloudSvr.trpc.feedcloud.commwriter.ComWriter.DoPush",
    "FeedCloudSvr.trpc.feedcloud.commwriter.ComWriter.DoReply",
    "FeedCloudSvr.trpc.feedcloud.commwriter.ComWriter.PublishFeed",
    "FeedCloudSvr.trpc.videocircle.circleprofile.CircleProfile.SetProfile",
    "friendlist.addFriend",
    "friendlist.AddFriendReq",
    "friendlist.ModifyGroupInfoReq",
    "MessageSvc.PbSendMsg",
    "MsgProxy.SendMsg",
    "OidbSvc.0x4ff_9",
    "OidbSvc.0x4ff_9_IMCore",
    "OidbSvc.0x56c_6",
    "OidbSvc.0x6d9_4",
    "OidbSvc.0x758",
    "OidbSvc.0x758_0",
    "OidbSvc.0x758_1",
    "OidbSvc.0x88d_0",
    "OidbSvc.0x89a_0",
    "OidbSvc.0x89b_1",
    "OidbSvc.0x8a1_0",
    "OidbSvc.0x8a1_7",
    "OidbSvc.0x8ba",
    "OidbSvc.0x9fa",
    "OidbSvc.oidb_0x758",
    "OidbSvcTrpcTcp.0x101e_1",
    "OidbSvcTrpcTcp.0x101e_2",
    "OidbSvcTrpcTcp.0x1100_1",
    "OidbSvcTrpcTcp.0x1105_1",
    "OidbSvcTrpcTcp.0x1107_1",
    "OidbSvcTrpcTcp.0x55f_0",
    "OidbSvcTrpcTcp.0x6d9_4",
    "OidbSvcTrpcTcp.0xf55_1",
    "OidbSvcTrpcTcp.0xf57_1",
    "OidbSvcTrpcTcp.0xf57_106",
    "OidbSvcTrpcTcp.0xf57_9",
    "OidbSvcTrpcTcp.0xf65_1",
    "OidbSvcTrpcTcp.0xf65_10 ",
    "OidbSvcTrpcTcp.0xf67_1",
    "OidbSvcTrpcTcp.0xf67_5",
    "OidbSvcTrpcTcp.0xf6e_1",
    "OidbSvcTrpcTcp.0xf88_1",
    "OidbSvcTrpcTcp.0xf89_1",
    "OidbSvcTrpcTcp.0xfa5_1",
    "ProfileService.getGroupInfoReq",
    "ProfileService.GroupMngReq",
    "QChannelSvr.trpc.qchannel.commwriter.ComWriter.DoComment",
    "QChannelSvr.trpc.qchannel.commwriter.ComWriter.DoReply",
    "QChannelSvr.trpc.qchannel.commwriter.ComWriter.PublishFeed",
    "qidianservice.135",
    "qidianservice.207",
    "qidianservice.269",
    "qidianservice.290",
    "SQQzoneSvc.addComment",
    "SQQzoneSvc.addReply",
    "SQQzoneSvc.forward",
    "SQQzoneSvc.like",
    "SQQzoneSvc.publishmood",
    "SQQzoneSvc.shuoshuo",
    "trpc.group_pro.msgproxy.sendmsg",
    "trpc.login.ecdh.EcdhService.SsoNTLoginPasswordLoginUnusualDevice",
    "trpc.o3.ecdh_access.EcdhAccess.SsoEstablishShareKey",
    "trpc.o3.ecdh_access.EcdhAccess.SsoSecureA2Access",
    "trpc.o3.ecdh_access.EcdhAccess.SsoSecureA2Establish",
    "trpc.o3.ecdh_access.EcdhAccess.SsoSecureAccess",
    "trpc.o3.report.Report.SsoReport",
    "trpc.passwd.manager.PasswdManager.SetPasswd",
    "trpc.passwd.manager.PasswdManager.VerifyPasswd",
    "trpc.qlive.relationchain_svr.RelationchainSvr.Follow",
    "trpc.qlive.word_svr.WordSvr.NewPublicChat",
    "trpc.qqhb.qqhb_proxy.Handler.sso_handle",
    "trpc.springfestival.redpacket.LuckyBag.SsoSubmitGrade",
    "wtlogin.device_lock",
    "wtlogin.exchange_emp",
    "wtlogin.login",
    "wtlogin.name2uin",
    "wtlogin.qrlogin",
    "wtlogin.register",
    "wtlogin.trans_emp",
    "wtlogin_device.login",
    "wtlogin_device.tran_sim_emp"]

buildOicqRequestPacket :: MonadIO m => Codec -> Word64 -> Word16 -> TLV -> m B.ByteString
buildOicqRequestPacket codec_ uin_ command_ tlvs_ = do
    Oicq.marshal codec_ msg
    where msg = Message (fromIntegral uin_) command_ EM_ECDH (marshalTLVs  tlvs_)

packSecSign :: Request -> ContextRM B.ByteString
packSecSign req = do
    tr <- view transport
    let d = tr ^. device
    signer <- wsign
    let qua_ = tr ^. app_version . qua
    rst <- liftIO $ signer
        (fromIntegral $ req ^. sequence_id)
        (show $ req ^. req_uin)
        (req ^. req_command)
        qua_ (req ^. req_body)
    case rst of
        Left _ -> pure B.empty
        Right (sign_, extra_, token_) -> do
            pure $ PL.encode $ SSOReserveField {
                _flag = optJust 0,
                _qimei = optJust $ d ^. qimei16,
                _newconn_flag = optJust 0,
                _uid = optJust $ show (req ^. req_uin),
                _imsi = optJust 0,
                _network_type = optJust 1,
                _ip_stack_type = optJust 1,
                _message_type = optJust 0,
                _sec_info = optJust $ SSOSecureInfo {
                    _sec_sig = optJust sign_,
                    _sec_device_token = optJust token_,
                    _sec_extra = optJust extra_
                },
                _sso_ip_origin = optJust 0
            }


packBody :: Request -> ContextRM B.ByteString
packBody req = do
    tr <- view transport
    secSign <- if (req ^. req_command) `elem` whiteListCommands
        then packSecSign req
        else pure B.empty
    pure $ runPut $ do
            withLength32Desc $ do
                when (req_type_ == RT_Login) $ do
                    put32be $ req ^. sequence_id
                    put32be $ tr ^. app_version . sub_id
                    put32be $ tr ^. app_version . sub_id
                    putbs $ B.pack [0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00]
                    let tgt_ = tr ^. signature . tgt
                    if B.length tgt_ == 0 || B.length tgt_ == 4 then
                        put32be 0x04
                    else
                        withLength32Desc_ tgt_
                withLength32Desc $ pututf8 $ req ^. req_command
                withLength32Desc_ $ tr ^. signature . out_session
                when (req_type_ == RT_Login) $ do
                    withLength32Desc $ pututf8 $ tr ^. device . imei
                    put32be 0x04
                    let ksid_ = tr ^. signature . ksid
                    put16be $ fromIntegral $ B.length ksid_ + 2
                    putbs ksid_
                withLength32Desc_ secSign
                withLength32Desc $ pututf8 $ tr ^. device . qimei16
            withLength32Desc_ $ req ^. req_body
    where
        req_type_ = req ^. req_type

packRequest :: Request -> ContextRM B.ByteString
packRequest req = do
    tr <- view transport
    let req_type_ = req ^. req_type
        enc_type_ = if B.length (tr ^. signature . d2) == 0
            then ET_EmptyKey
            else req ^. req_enc_type
    body <- packBody req
    body_ <- case enc_type_ of
        ET_EmptyKey -> QQTea.qqteaEncrypt QQTea.tea16EmptyKey body
        ET_D2Key -> QQTea.qqteaEncrypt (tr ^. signature . d2key) body
        ET_NoEncrypt -> pure body
    pure $ runPut $ withLength32Desc $ do
            put32be $ case req_type_ of
                RT_Login -> 0x0A
                RT_Simple -> 0x0B
            put8 $ case enc_type_ of
                ET_NoEncrypt -> 0x00
                ET_D2Key -> 0x01
                ET_EmptyKey -> 0x02
            case req_type_ of
                RT_Simple -> put32be $ req ^. sequence_id
                RT_Login -> do
                    case enc_type_ of
                        ET_D2Key -> withLength32Desc_ $ tr ^. signature . d2
                        _ -> put32be 4
            put8 0
            withLength32Desc $ pututf8 $ show $ req ^. req_uin
            putbs body_

buildUniRequestData :: B.ByteString -> B.ByteString
buildUniRequestData bs = do
    runPut $ do
        put8 0x0a
        putbs bs
        put8 0x0b

uniPackRequest :: String -> B.ByteString -> ContextRM Request
uniPackRequest cmd_ body_ = do
    seq_ <- fromIntegral <$> nextSeq
    uin_ <- view uin
    pure $ Request RT_Simple ET_D2Key seq_ uin_ cmd_ body_

uniPackRequestWithSeq :: Word16 -> String -> B.ByteString -> ContextRM Request
uniPackRequestWithSeq seq_ cmd_ body_ = do
    uin_ <- view uin
    pure $ Request RT_Simple ET_D2Key (fromIntegral seq_) uin_ cmd_ body_

packOIDBPackage :: ProtoBuf a => Int32 -> Int32 -> a -> ContextRM B.ByteString
packOIDBPackage cmd srv_type (buf :: a) = do
    v <- asks (._transport._app_version._sort_version)
    let def = pdef :: (OIDBSSOPkg a)
    pure $ PL.encode $ def {
            _command = optJust cmd,
            _service_type = optJust srv_type,
            _client_version = optJust $ "Android " ++ v,
            _body = optJust buf
            }