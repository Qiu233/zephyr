{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
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
import Zephyr.Client.Log
import Zephyr.Packet.JceStructs

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
    client._logger.logInfo $ show msgInfos_
    --undefined