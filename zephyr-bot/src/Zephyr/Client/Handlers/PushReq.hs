{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Zephyr.Client.Handlers.PushReq where
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
import Debug.Trace (traceM)
import Zephyr.Packet.Data.PushReq
import Text.Printf

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

handlePushReqPacket :: Client -> QQPacket ->  IO ()
handlePushReqPacket client (QQPacket _ _ bs) = do
    let request_ = jceUnmarshal bs :: RequestPacket
    let data_ = jceUnmarshal $ request_._s_buffer.jval :: RequestDataVersion2
    let m_ = data_._map.jval
    let r1 = B.drop 1 $ fromMaybe B.empty (jlookup "PushReq" m_ >>= jlookup "ConfigPush.PushReq")
    let (t, jceBuf, seq_) = runGet ((,,) <$> getJInt 1 <*> getJBytes 2 <*> getJInt 3) r1
    
    traceM $ "ConfigPushSvc.PushReq: " ++ printf "%d" t
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
                let hsV = client._events._server_updated
                let srvV = client._servers
                hs <- liftIO $ readTVarIO hsV
                let ea = ServerUpdatedEventArgs servers2__
                rs <- sequence [liftIO $ h client ea | h <- hs]
                when (and rs) $ do
                    liftIO $ atomically $
                        modifyTVar srvV (++ map (\x ->(x._server.jval, fromIntegral $ x._port.jval)) servers2__)
                pure $ Just ()
        else if B.length jceBuf > 0 && t == 2 then do
            let l = jceUnmarshal jceBuf :: FileStoragePushFSSvcList
            let rsp = decode $ l._big_data_channel.jval._pb_buf.jval :: C501RspBody
            let hw = client._highway_session
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
                let addrs3 = [(ip_,port_) |
                        z <- addrs2_,
                        let ip_ = z._addr_ip.protoOptDef,
                        let port_ = fromIntegral $ z._addr_port.protoOptDef.variant
                        ]
                traceM $ show addrs3
                appendAddrs hw addrs3
            pure Nothing
        else do
            pure Nothing
    case rM of
        Just _ -> pure ()
        Nothing -> do
            resp <- withContext (buildConfPushRespPacket (fromIntegral t) seq_ jceBuf) client
            liftIO $ sendPacket resp client

