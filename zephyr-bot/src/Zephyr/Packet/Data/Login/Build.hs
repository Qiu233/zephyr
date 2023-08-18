{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Data.Login.Build where

import Zephyr.Core.QQContext
import qualified Data.ByteString.Lazy as B
import Control.Lens
import Zephyr.Core.Transport
import Zephyr.Core.AppVersion
import qualified Zephyr.Packet.TLV.Builders as T
import Zephyr.Core.Signature
import Zephyr.Packet.Build
import Zephyr.Core.Request
import Zephyr.Binary
import Data.Bits
import Zephyr.Jce.Generic
import Zephyr.Core.Device
import Zephyr.Utils.GUID (guidBytes)
import Zephyr.Jce (jceMarshal)
import Zephyr.Packet.JceStructs
import Zephyr.Binary.Put

buildLoginPacket :: ContextRM Request
buildLoginPacket = do
    seq_ <- nextSeq
    uin_ <- view uin
    tr <- view transport
    codec_ <- view codec
    md5pass <- view password_md5
    let sub_id_ = tr ^. app_version . sub_id
    tlvs <- sequence [
        T.t18,
        T.t1,
        T.t106 md5pass,
        T.t116,
        T.t100 sub_id_,
        T.t107,
        T.t142,
        T.t144,
        T.t145,
        T.t147,
        T.t154 seq_,
        T.t141,
        T.t8,
        T.t511,
        T.t187,
        T.t188,
        T.t194,
        T.t191 0x82,
        T.t202,
        T.t177,
        T.t516,
        T.t521 0,
        T.t525,
        either error id <$> T.t544v2 "810_9" 9,
        T.t545
        ]
    let body = TLV 9 tlvs
    b2 <- buildOicqRequestPacket codec_ uin_ 0x810 body
    pure $ Request RT_Login ET_EmptyKey (fromIntegral seq_) uin_ "wtlogin.login" b2

buildTicketSubmitPacket :: String -> ContextRM Request
buildTicketSubmitPacket ticket = do
    seq_ <- nextSeq
    uin_ <- view uin
    codec_ <- view codec
    t547_ <- view (transport . signature . t547)
    tlvs <- sequence [
            T.t193 ticket,
            T.t8,
            T.t104,
            T.t116,
            if B.null t547_ then pure "" else pure $ runPut $ do
                put16be 0x547
                put16be $ fromIntegral $ B.length t547_
                putbs t547_,
            either error id <$> T.t544 "810_2" 2
        ]
    let body = TLV 2 tlvs
    b2 <- buildOicqRequestPacket codec_ uin_ 0x810 body
    pure $ Request RT_Login ET_EmptyKey (fromIntegral seq_) uin_ "wtlogin.login" b2

buildClientRegisterPacket :: ContextRM Request
buildClientRegisterPacket = do
    seq_ <- nextSeq
    uin_ <- view uin
    tr <- view transport
    let svc = jdef {
            _conn_type = 0,
            _uin = fromIntegral uin_,
            _bid = JceField (1 .|. 2 .|. 4),
            _status = 11,
            _kick_pc = 0,
            _kick_weak = 0,
            _ios_version = JceField (fromIntegral $ tr ^. device . os_version . sdk),
            _net_type = 1,
            _reg_type = 0,
            _guid = JceField (guidBytes $ tr ^. device . guid),
            _is_set_status = 0,
            _locale_id = 2052,
            _dev_name = JceField (tr ^. device . model),
            _dev_type = JceField (tr ^. device . model),
            _os_ver = JceField (tr ^. device . os_version . release),
            _open_push = 1,
            _large_seq = 1551,
            _old_sso_ip = 0,
            _new_sso_ip = 31806887127679168,
            _channel_no = "",
            _cp_id = 0,
            _vendor_name = JceField (tr ^. device . vendor_name),
            _vendor_os_name = JceField (tr ^. device . vendor_os_name),
            _b769 = JceField $ B.pack [0x0A, 0x04, 0x08, 0x2E, 0x10, 0x00, 0x0A, 0x05, 0x08, 0x9B, 0x02, 0x10, 0x00],
            _set_mute = 0
    }
    let b = runPut $ do
            put8 0x0A
            putbs $ jceMarshal svc
            put8 0x0B
    let buf = RequestDataVersion3 $ JceField [("SvcReqRegister", b)]
    let pkt = jdef {
            _i_version = 3,
            _s_servant_name = "PushService",
            _s_func_name = "SvcReqRegister",
            _s_buffer = JceField $ jceMarshal buf,
            _context = [],
            _status = []
    }
    pure $ Request RT_Login ET_D2Key (fromIntegral seq_) uin_ "StatSvc.register" (jceMarshal pkt)