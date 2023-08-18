{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE OverloadedStrings #-}
module Zephyr.Packet.Data.ReqPush where
import Data.Int
import qualified Data.ByteString.Lazy as B
import Data.Word
import Zephyr.Core.QQContext
import Zephyr.Core.Request
import Zephyr.Jce.Generic
import Zephyr.Packet.Build (buildUniRequestData, uniPackRequestWithSeq)
import Zephyr.Jce
import Zephyr.Jce.JceMap
import Zephyr.Packet.JceStructs
import qualified Zephyr.Packet.JceStructs as DelMsgInfo (DelMsgInfo(..))

buildDeleteOnlinePushPacket :: Int64 -> Int32-> B.ByteString -> Word16 -> [PushMessageInfo] -> ContextRM Request
buildDeleteOnlinePushPacket uin_ svrip pushToken seq_ delMsg = do
    let req = (jdef @(SvcRespPushMsg DelMsgInfo)) {
        _uin = JceField uin_,
        _svr_ip = JceField svrip,
        _push_token = JceField pushToken,
        _del_infos = JceField $ flip fmap delMsg $ \m -> 
            jdef {
                DelMsgInfo._from_uin = JceField m._from_uin.jval,
                DelMsgInfo._msg_seq = JceField m._msg_seq.jval,
                DelMsgInfo._msg_cookies = JceField m._msg_cookies.jval,
                DelMsgInfo._msg_time = JceField m._msg_time.jval
            }
    }
    let b = buildUniRequestData $ jceMarshal req
    let buf = RequestDataVersion3 $ JceField $ JceMap [("resp", b)]
    let pkt = jdef {
        _i_version = 3,
        _i_request_id = JceField $ fromIntegral seq_,
        _s_servant_name = "OnlinePush",
        _s_func_name = "SvcRespPushMsg",
        _s_buffer = JceField $ jceMarshal buf
    }
    uniPackRequestWithSeq seq_ "OnlinePush.RespPush" $ jceMarshal pkt