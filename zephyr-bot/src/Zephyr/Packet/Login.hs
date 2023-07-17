{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Login where
import Zephyr.Engine.Context
import Data.Word
import qualified Data.ByteString.Lazy as B
import Control.Lens
import Zephyr.Core.ClientApp
import qualified Zephyr.Core.Signature as Sig
import Zephyr.Utils.Binary.Put
import qualified Zephyr.Encrypt.ECDH as ECDH
import Zephyr.Engine.Packet
import Zephyr.Encrypt.QQTea (qqteaEncrypt, tea16KeyFromBytes, tea16EmptyKey)
import Zephyr.Core.Device
import Text.Printf
import Prelude hiding (seq)
import Zephyr.Utils.Random
import qualified Zephyr.Packet.TLVBuilder as T
import qualified Debug.Trace as Debug
import Zephyr.Utils.Common (encodeHex)

data LoginCmd =
    WTLogin_Login |
    WTLogin_ExchangeEMP |
    WTLogin_TransEMP |
    StatSvc_Register |
    Client_CorrectTime
    deriving (Eq)
loginCmdCode :: LoginCmd -> B.ByteString
loginCmdCode WTLogin_Login = "wtlogin.login"
loginCmdCode WTLogin_ExchangeEMP = "wtlogin.exchange_emp"
loginCmdCode WTLogin_TransEMP = "wtlogin.trans_emp"
loginCmdCode StatSvc_Register = "StatSvc.register"
loginCmdCode Client_CorrectTime = "Client.CorrectTime"

buildLoginPacket :: ContextIOT m => LoginCmd -> Word8 -> B.ByteString -> m B.ByteString
buildLoginPacket cmd type_ body = do
    seq_ <- nextSeq
    uin_ <- use uin
    sub_id_ <- use $ client_app . sub_id
    let (uin__, cmdid__, subappid__) = if cmd == WTLogin_TransEMP
            then (0, 0x812, 537065138)
            else (uin_, 0x810, sub_id_)
    b2 <- if type_ == 2
        then do
            rand_key_ <- use $ signature . Sig.rand_key
            public_key_ <- use $ ecdh . ECDH.public_key
            shared_key_ <- use $ ecdh . ECDH.shared_key
            enc <- qqteaEncrypt (tea16KeyFromBytes shared_key_) body
            let t1 = runPut $ do
                    put8 0x02
                    put8 0x01
                    putbs rand_key_
                    put16be 0x131
                    put16be 0x01
                    withTLV_ public_key_
                    putbs enc
            let t2 = runPut $ do
                    put8 0x02
                    put16be $ fromIntegral $ 29 + B.length t1
                    put16be 8001
                    put16be cmdid__
                    put16be 1
                    put32be $ fromIntegral uin__
                    put8 3
                    put8 0x87
                    put8 0
                    put32be 2
                    put32be 0
                    put32be 0
                    putbs t1
                    put8 0x03
            pure t2
        else pure body
    imei_ <- use $ device . imei
    apk_name_ <- use $ client_app . name
    let ksid = printf "|%s|%s" imei_ apk_name_ :: String
    tgt_ <- use $ signature . Sig.tgt
    session_ <- use $ signature . Sig.session
    qimei16_ <- use $ device . qimei16
    let sso' = runPut $ do
            withLength32Desc $ do
                put32be seq_
                put32be subappid__
                put32be subappid__
                putbs $ B.pack [0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00]
                withLength32Desc_ tgt_
                withLength32Desc_ $ loginCmdCode cmd
                withLength32Desc_ session_
                withLength32Desc $ pututf8 imei_
                put32be 4
                put16be $ fromIntegral $ length ksid + 2
                pututf8 ksid
                withLength32Desc $ pututf8 qimei16_
            withLength32Desc_ b2
    sso <- case type_ of
        1 -> do
            d2key_ <- use $ signature . Sig.d2key
            qqteaEncrypt (tea16KeyFromBytes d2key_) sso'
        2 -> do
            qqteaEncrypt tea16EmptyKey sso'
        _ -> pure sso'
    d2_ <- use $ signature . Sig.d2
    pure $ runPut $ withLength32Desc $ do
        put32be 0x0A
        put8 type_
        withLength32Desc_ d2_
        put8 0
        withLength32Desc $ pututf8 $ show uin_
        putbs sso

passwordLoginPacket :: ContextIOT m => B.ByteString -> m B.ByteString
passwordLoginPacket md5pass = do
    signature . Sig.session <~ randBytes 4
    signature . Sig.rand_key <~ randBytes 16
    signature . Sig.tgtgt <~ randBytes 16
    tlvs <- B.concat <$> sequence [
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
    let body_ = runPut $ do
            put16be 9
            put16be 25
            putbs tlvs
    Debug.traceM (show (B.length body_) ++ "\n" ++ encodeHex body_)
    buildLoginPacket WTLogin_Login 2 body_

syncTimeDiffPacket :: ContextIOT m => m B.ByteString
syncTimeDiffPacket = do
    buildLoginPacket Client_CorrectTime 0 $ runPut $ put32be 0
