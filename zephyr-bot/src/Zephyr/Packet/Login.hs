{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Login where
import Zephyr.Core.Context
import qualified Data.ByteString.Lazy as B
import Control.Lens
import qualified Zephyr.Core.Signature as Sig
import Prelude hiding (seq)
import Zephyr.Utils.Random
import qualified Zephyr.Packet.TLV.Builders as T
import Zephyr.Core.Transport
import Zephyr.Packet.Build
import Zephyr.Core.Request

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

buildLoginPacket :: ContextIOT m => LoginCmd -> TLV -> m B.ByteString
buildLoginPacket cmd body = do
    seq_ <- nextSeq
    uin_ <- use uin
    tr <- use transport
    b2 <- do
            codec_ <- use codec
            buildOicqRequestPacket codec_ uin_ 0x810 body
    let req = Request RT_Login ET_EmptyKey seq_ uin_ (loginCmdCode cmd) b2
    packRequest tr req

passwordLoginPacket :: ContextIOT m => B.ByteString -> m B.ByteString
passwordLoginPacket md5pass = do
    tlvs <- sequence [
        T.t18,
        T.t1,
        T.t106 md5pass,
        T.t116,
        T.t100,
        T.t107,
        T.t142,
        T.t144,
        T.t145,
        T.t147,
        T.t154,
        T.t141,
        T.t8,
        T.t511,
        T.t187,
        T.t188,
        T.t194,
        T.t191,
        T.t202,
        T.t177,
        T.t516,
        T.t521,
        T.t525,
        T.t544 2 9,
        T.t545
        ]
    let tlvs_ = TLV 9 tlvs
    buildLoginPacket WTLogin_Login tlvs_

-- syncTimeDiffPacket :: ContextIOT m => m B.ByteString
-- syncTimeDiffPacket = do
--     buildLoginPacket Client_CorrectTime 0 $ runPut $ put32be 0
