{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Zephyr.Packet.Parse where
import qualified Data.ByteString.Lazy as B
import Control.Lens hiding (Context)
import qualified Zephyr.Core.Signature as Sig
import Data.Word
import Zephyr.Encrypt.QQTea
import Zephyr.Utils.Binary
import qualified Codec.Compression.Zlib as ZLib
import Zephyr.Utils.Common (utf8FromBytes)
import Control.Monad
import Zephyr.Core.Transport
import Zephyr.Core.Request
import Control.Monad.Trans.Except

data QQResponse = QQResponse {
    _resp_body :: Request,
    _resp_msg :: String
} deriving (Eq, Show)
$(makeLenses ''QQResponse)

getbs32be :: Get B.ByteString
getbs32be = do
    len <- get32be
    getbs (fromIntegral len - 4)

getstr32be :: Get String
getstr32be = utf8FromBytes <$> getbs32be

parseSSO :: B.ByteString -> Except String (Word32, String, String, B.ByteString)
parseSSO bs = do
    let (head_, body_) = runGet_ ((,) <$> getbs32be <*> getbs32be) bs
    let (seq_, ret_code_, msg_, cmd_, _, compressed_flag) = runGet_ (
            (,,,,,) <$> get32be <*> get32be <*> getstr32be <*> getstr32be <*> getbs32be <*> get32be
            ) head_
    when (ret_code_ /= 0) $ do throwE $ "ret_code_ /= 0: " ++ show ret_code_
    payload_ <- case compressed_flag of
            0 -> pure body_
            8 -> pure body_
            1 -> pure $ ZLib.decompress body_
            _ -> throwE $ "unknown compressed_flag = " ++ show compressed_flag
    pure (seq_, msg_, cmd_, payload_)

parsePacket :: Transport -> B.ByteString -> Except String QQResponse
parsePacket tr pkt = do
    let (type_, enc_type_, _, uin_, body_) = runGet_ (
            (,,,,) <$> get32be <*> get8 <*> get8 <*> (read @Word64 <$> getstr32be) <*> getRemaining
            ) pkt
    when (type_ /= 0x0A && type_ /= 0x0B) $ do
        throwE $ "unknown packet type = " ++ show type_
    body__ <- case enc_type_ of
            1 -> pure $ qqteaDecrypt (tr ^. signature . Sig.d2key) body_
            2 -> pure $ qqteaDecrypt tea16EmptyKey body_
            0 -> pure body_ -- TODO?
            _ -> throwE $ "unknown encryption type = " ++ show enc_type_
    (seq_, msg_, cmd_, payload_) <- except . runExcept $ parseSSO body__
    pure $ QQResponse {
        _resp_body = Request {
            _req_type = if type_ == 0x0A then RT_Login else RT_Simple,
            _req_enc_type = case enc_type_ of
                    0 -> ET_NoEncrypt
                    1 -> ET_D2Key
                    2 -> ET_EmptyKey
                    _ -> error "impossible",
            _sequence_id = seq_,
            _req_uin = uin_,
            _req_command = cmd_,
            _req_body = payload_
        },
        _resp_msg = msg_
    }
