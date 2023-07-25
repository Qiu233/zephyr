{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Zephyr.Packet.Build (
    marshal,
    buildOicqRequestPacket,
    packRequest,
    TLV(..),
    packBody
) where
import qualified Data.ByteString.Lazy as B
import Data.Word
import Zephyr.Utils.Binary
import Control.Monad
import Zephyr.Core.Context
import Zephyr.Core.Codec
import Control.Lens ((^.), use)
import Control.Monad.IO.Class
import Zephyr.Core.Request
import Zephyr.Core.Transport
import Zephyr.Core.Signature
import Zephyr.Packet.Internal
import qualified Zephyr.Encrypt.ECDH as ECDH
import qualified Zephyr.Encrypt.QQTea as QQTea
import Zephyr.Core.AppVersion
import Zephyr.Core.Device.Types
import Text.Printf
import Zephyr.Packet.TLV.Prim
import Zephyr.Packet.Wrapper (wsign)
import qualified Zephyr.Utils.ProtoLite as PL


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



marshal :: MonadIO m => Codec -> Message -> m B.ByteString
marshal Codec{..} msg = do
    key_ <- case msg ^. encrypt_method of
        EM_ECDH -> pure $ _ecdh ^. ECDH.shared_key
        EM_ST -> pure _random_key
    enc_ <- QQTea.qqteaEncrypt key_ $ msg ^. msg_body
    let w = runPut $ do
            put16be 8001
            put16be $ msg ^. msg_cmd
            put16be 1
            put32be $ msg ^. msg_uin
            put8 0x3
            put8 $ case msg ^. encrypt_method of
                EM_ECDH -> 0x87
                EM_ST -> 0x45
            put8 0
            put32be 2
            put32be 0
            put32be 0
            case msg ^. encrypt_method of
                EM_ECDH -> do
                    put8 0x02
                    put8 0x01
                    putbs _random_key
                    put16be 0x0131
                    put16be $ _ecdh ^. ECDH.svr_public_key_ver
                    lvbs $ _ecdh ^. ECDH.public_key
                    putbs enc_
                EM_ST -> do
                    put8 0x1
                    put8 0x3
                    putbs _random_key
                    put16be 0x0102
                    put16be 0
                    putbs enc_
            put8 0x3
    let w2 = runPut $ do
            put8 0x02
            put16be $ fromIntegral $ B.length w + 3
            putbs w
    pure w2

buildOicqRequestPacket :: MonadIO m => Codec -> Word64 -> Word16 -> TLV -> m B.ByteString
buildOicqRequestPacket codec_ uin_ command_ tlvs_ = do
    marshal codec_ msg
    where msg = Message (fromIntegral uin_) command_ EM_ECDH (marshalTLVs  tlvs_)

packSecSign :: ContextIOT m => Request -> m B.ByteString
packSecSign req = do
    tr <- use transport
    let d = tr ^. device
    signer <- wsign
    let qua_ = tr ^. client_version . qua
    rst <- liftIO $ signer
        (fromIntegral $ req ^. sequence_id)
        (show $ req ^. req_uin)
        (req ^. req_command)
        qua_ (req ^. req_body)
    case rst of
        Left err -> error err --pure B.empty
        Right (sign_, extra_, token_) -> do
            pure $ PL.encodeMessage_ [
                    9 `PL.putPVInt32` 0,
                    12 `PL.putLenUTF8` (d ^. qimei16),
                    14 `PL.putPVInt32` 0,
                    16 `PL.putLenUTF8` show (req ^. req_uin),
                    18 `PL.putPVInt32` 0,
                    19 `PL.putPVInt32` 1,
                    20 `PL.putPVInt32` 1,
                    21 `PL.putPVInt32` 0,
                    24 `PL.putLenBytes` PL.encodeMessage_ [
                        1 `PL.putLenBytes` sign_,
                        2 `PL.putLenBytes` token_,
                        3 `PL.putLenBytes` extra_
                        ],
                    28 `PL.putPVInt32` 0
                    ]


packBody :: ContextIOT m => Request -> m B.ByteString
packBody req = do
    tr <- use transport
    secSign <- if (req ^. req_command) `elem` whiteListCommands
        then packSecSign req
        else pure B.empty
    pure $ runPut $ do
            withLength32Desc $ do
                when (req_type_ == RT_Login) $ do
                    put32be $ req ^. sequence_id
                    put32be $ tr ^. client_version . sub_id
                    put32be $ tr ^. client_version . sub_id
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

packRequest :: ContextIOT m => Request -> m B.ByteString
packRequest req = do
    tr <- use transport
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