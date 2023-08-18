{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Zephyr.Packet.Data.PushReq where
import Data.Int
import qualified Data.ByteString.Lazy as B
import Zephyr.Core.QQContext
import Zephyr.Core.Request
import Zephyr.Utils.Binary
import Zephyr.Utils.Jce.Internal
import Zephyr.Utils.Jce
import Zephyr.Packet.Build
import Zephyr.Packet.JceStructs

buildConfPushRespPacket :: Int32 -> Int64 -> B.ByteString -> ContextRM Request
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
            _context = JceField [],
            _status = JceField  []
            }
    uniPackRequest "ConfigPushSvc.PushResp" $ jceMarshal pkt