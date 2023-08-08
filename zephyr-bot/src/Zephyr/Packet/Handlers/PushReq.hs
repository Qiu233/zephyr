{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Zephyr.Packet.Handlers.PushReq where
import Zephyr.Client.Types
import qualified Data.ByteString.Lazy as B
import Zephyr.Packet.Jce.RequestPacket as RequestPacket
import Zephyr.Packet.Jce.RequestDataVersion2
import Zephyr.Utils.Jce
import Data.Maybe
import Zephyr.Utils.Jce.JceMap
import Zephyr.Utils.Binary
import Zephyr.Utils.Jce.Internal
import Control.Monad
import Data.Int
import Data.Word
import Zephyr.Packet.Jce.SsoServerInfo
import Zephyr.Packet.Jce.FileStoragePushFSSvcList
import Zephyr.Packet.Jce.BigData
import ProtoLite
import GHC.Generics
import Control.Lens
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Zephyr.Client.Highway
import qualified Data.List
import Zephyr.Client.Events
import Zephyr.Client.Internal
import Control.Monad.Reader
import Zephyr.Packet.Jce.RequestDataVersion3 (RequestDataVersion3(RequestDataVersion3))
import Zephyr.Packet.Build (buildUniRequestData, uniPackRequest)
import Zephyr.Utils.Jce.Generic
import Zephyr.Core.Request (Request)
import Debug.Trace (traceM)

newtype C501RspBody = C501RspBody {
    _c501_rsp_body :: ProtoField (Maybe SubCmd0X501RspBody) 1281
} deriving (Show, Generic)
instance ProtoBuf C501RspBody

data SubCmd0X501RspBody = SubCmd0X501RspBody {
    _c501_sig_session :: ProtoField (Maybe B.ByteString) 1,
    _c501_session_key :: ProtoField (Maybe B.ByteString) 2,
    _c501_addrs :: ProtoField (Repeated SrvAddrs) 3
} deriving (Show, Generic)
instance ProtoBuf SubCmd0X501RspBody

data SrvAddrs = SrvAddrs {
    _service_type :: ProtoField (Maybe (Variant Word32)) 1,
    _service_addrs :: ProtoField (Repeated SrvIPAddr) 2
} deriving (Show, Generic)
instance ProtoBuf SrvAddrs

data SrvIPAddr = SrvIPAddr {
    _addr_type :: ProtoField (Maybe (Variant Word32)) 1,
    _addr_ip :: ProtoField (Maybe Word32) 2,
    _addr_port :: ProtoField (Maybe (Variant Word32)) 3,
    _addr_area :: ProtoField (Maybe (Variant Word32)) 4
} deriving (Show, Generic)
instance ProtoBuf SrvIPAddr

handlePushReqPacket :: QQPacket -> ClientOPM ()
handlePushReqPacket (QQPacket _ _ bs) = do
    let request_ = jceUnmarshal bs :: RequestPacket
    let data_ = jceUnmarshal $ request_._s_buffer.jval :: RequestDataVersion2
    let m_ = data_._map.jval
    let r1 = B.drop 1 $ fromMaybe B.empty (jlookup "PushReq" m_ >>= jlookup "ConfigPush.PushReq")
    let (t, jceBuf, seq_) = runGet ((,,) <$> getJInt 1 <*> getJBytes 2 <*> getJInt 3) r1
    rM <- if B.length jceBuf > 0 && t == 1
        then do
            let JceField servers_ = jceUnmarshal jceBuf :: JceField [SsoServerInfo] 1
            if null servers_ then
                pure Nothing
            else do
                let servers2__ = [s |
                        s <- servers_,
                        ".com" `Data.List.isInfixOf` s._server.jval
                        ]
                traceM $ "servers2__:" ++ show servers2__
                hsV <- views events (._server_updated)
                hs <- liftIO $ readTVarIO hsV
                let ea = ServerUpdatedEventArgs servers2__
                cli <- ask
                rs <- sequence [liftIO $ h cli ea | h <- hs]
                when (and rs) $ do
                    srvV <- view servers
                    liftIO $ atomically $
                        modifyTVar srvV (++ map (\x ->(x._server.jval, fromIntegral $ x._port.jval)) servers2__)
                pure $ Just ()
        else if B.length jceBuf > 0 && t == 2 then do
            let l = jceUnmarshal jceBuf :: FileStoragePushFSSvcList
            let rsp = decode $ l._big_data_channel.jval._pb_buf.jval :: C501RspBody
            hw <- view highway_session
            liftIO $ do
                let rspbody_ = rsp._c501_rsp_body.protoVal
                atomically $ writeTVar (hw ^. hw_sig_session) $
                    optionalOrDefault (rspbody_ >>= (._c501_sig_session.protoVal))
                atomically $ writeTVar (hw ^. hw_session_key) $
                    optionalOrDefault (rspbody_ >>= (._c501_session_key.protoVal))

                let addrs_ = (optionalOrDefault rspbody_)._c501_addrs.protoVal.repeated
                let addrs2_  = concat [y |
                        x <- addrs_,
                        x._service_type.protoOptDef == 10,
                        let y = x._service_addrs.protoVal.repeated
                        ]
                appendAddrs hw [(ip_,port_) |
                    z <- addrs2_,
                    let ip_ = z._addr_ip.protoOptDef,
                    let port_ = fromIntegral $ z._addr_port.protoOptDef.variant
                    ]
            pure Nothing
        else do
            pure Nothing
    case rM of
        Just _ -> pure ()
        Nothing -> do
            resp <- buildConfPushRespPacket (fromIntegral t) seq_ jceBuf
            sendPacket resp

buildConfPushRespPacket :: Int32 -> Int64 -> B.ByteString -> ClientOPM Request
buildConfPushRespPacket t_ seq_ jceBuf_ = do
    let req = runPut $ do
            putJ32 1 $ fromIntegral t_
            putJ64 2 seq_
            putJBytes 3 jceBuf_
    let buf = RequestDataVersion3 $ JceField [("PushResp", buildUniRequestData req)]
    let pkt = jdef {
            _i_version = 3,
            _s_servant_name = "QQService.ConfigPushSvc.MainServant",
            _s_func_name = "PushResp",
            _s_buffer = JceField $ jceMarshal buf,
            RequestPacket._context = JceField [],
            RequestPacket._status = JceField  []
            }
    withContext $ uniPackRequest "ConfigPushSvc.PushResp" $ jceMarshal pkt