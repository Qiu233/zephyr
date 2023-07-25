{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Login where
import Zephyr.Core.Context
import qualified Data.ByteString.Lazy as B
import Control.Lens
import Prelude hiding (seq)
import qualified Zephyr.Packet.TLV.Builders as T
import Zephyr.Packet.Build
import Zephyr.Core.Request
import Zephyr.Core.Transport
import Zephyr.Core.AppVersion

data LoginCmd =
    WTLogin_Login |
    WTLogin_ExchangeEMP |
    WTLogin_TransEMP |
    StatSvc_Register |
    Client_CorrectTime
    deriving (Eq)
loginCmdCode :: LoginCmd -> String
loginCmdCode WTLogin_Login = "wtlogin.login"
loginCmdCode WTLogin_ExchangeEMP = "wtlogin.exchange_emp"
loginCmdCode WTLogin_TransEMP = "wtlogin.trans_emp"
loginCmdCode StatSvc_Register = "StatSvc.register"
loginCmdCode Client_CorrectTime = "Client.CorrectTime"

buildLoginPacket :: B.ByteString -> ContextOPM B.ByteString
buildLoginPacket md5pass = do
    seq_ <- nextSeq
    uin_ <- use uin
    tr <- use transport
    let sub_id_ = tr ^. client_version . sub_id
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

    b2 <- do
            codec_ <- use codec
            buildOicqRequestPacket codec_ uin_ 0x810 body
    let req = Request RT_Login ET_EmptyKey (fromIntegral seq_) uin_ "wtlogin.login" b2
    packRequest req


-- syncTimeDiffPacket :: ContextIOT m => m B.ByteString
-- syncTimeDiffPacket = do
--     buildLoginPacket Client_CorrectTime 0 $ runPut $ put32be 0
