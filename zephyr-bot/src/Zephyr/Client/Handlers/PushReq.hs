{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
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
import Zephyr.Packet.Jce.SsoServerInfo
import Zephyr.Packet.Jce.FileStoragePushFSSvcList
import Zephyr.Packet.Jce.BigData
import ProtoLite
import Control.Lens
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Zephyr.Client.Highway
import qualified Data.List
import Zephyr.Client.Events
import Zephyr.Client.Internal
import Zephyr.Packet.Data.PushReq
import Text.Printf
import Zephyr.Client.Log
import Zephyr.PB.CMD0x6FF.SubCMD0x501

handlePushReqPacket :: Client -> QQPacket -> IO ()
handlePushReqPacket client (QQPacket _ _ bs) = do
    let request_ = jceUnmarshal bs :: RequestPacket
    let data_ = jceUnmarshal $ request_._s_buffer.jval :: RequestDataVersion2
    let m_ = data_._map.jval
    let r1 = B.drop 1 $ fromMaybe B.empty (jlookup "PushReq" m_ >>= jlookup "ConfigPush.PushReq")
    let (t, jceBuf, seq_) = runGet ((,,) <$> getJInt 1 <*> getJBytes 2 <*> getJInt 3) r1
    
    client._logger.logInfo $ "Handling command = `ConfigPushSvc.PushReq`: " ++ printf "t = %d" t
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
                let hsV = client._events._server_updated
                let srvV = client._servers
                hs <- liftIO $ readTVarIO hsV
                let ea = ServerUpdatedEventArgs servers2__
                rs <- sequence [liftIO $ h ea | h <- hs]
                when (and rs) $ do
                    liftIO $ atomically $
                        modifyTVar srvV (++ map (\x ->(x._server.jval, fromIntegral $ x._port.jval)) servers2__)
                pure $ Just ()
        else if B.length jceBuf > 0 && t == 2 then do
            let l = jceUnmarshal jceBuf :: FileStoragePushFSSvcList
            let rsp = decode $ l._big_data_channel.jval._pb_buf.jval :: C501RspBody
            let hw = client._highway_session
            liftIO $ do
                let rspbody_ = rsp._rsp_body.pv
                atomically $ writeTVar (hw ^. hw_sig_session) $
                    optOrDef (rspbody_ >>= (._c501_sig_session.pv))
                atomically $ writeTVar (hw ^. hw_session_key) $
                    optOrDef (rspbody_ >>= (._c501_session_key.pv))

                let addrs_ = (optOrDef rspbody_)._c501_addrs.pv.repeatedF
                let addrs2_  = concat [y |
                        x <- addrs_,
                        x._service_type.optOrDef == 10,
                        let y = x._addrs.pv.repeatedF
                        ]
                let addrs3 = [(ip_,port_) |
                        z <- addrs2_,
                        let ip_ = z._ip.optOrDef,
                        let port_ = fromIntegral $ z._port.optOrDef.variantF
                        ]
                appendAddrs hw addrs3
            pure Nothing
        else do
            pure Nothing
    case rM of
        Just _ -> pure ()
        Nothing -> do
            resp <- withContext (buildConfPushRespPacket (fromIntegral t) seq_ jceBuf) client
            liftIO $ sendPacket resp client

