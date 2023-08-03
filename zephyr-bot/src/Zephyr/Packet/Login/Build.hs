{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Login.Build where

import Zephyr.Core.QQContext
import qualified Data.ByteString.Lazy as B
import Control.Lens
import Zephyr.Core.Transport
import Zephyr.Core.AppVersion
import qualified Zephyr.Packet.TLV.Builders as T
import Zephyr.Core.Signature
import Zephyr.Packet.Build
import Zephyr.Core.Request
import Zephyr.Utils.Binary

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
