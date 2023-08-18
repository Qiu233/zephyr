{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Oicq where
import Zephyr.Core.Codec
import qualified Data.ByteString.Lazy as B
import Control.Lens
import qualified Zephyr.Encrypt.ECDH as ECDH
import qualified Zephyr.Encrypt.QQTea as QQTea
import Zephyr.Binary
import Zephyr.Packet.Internal
import Control.Monad.Except
import Control.Monad.Trans.Except
import Zephyr.Binary.Put
import Zephyr.Binary.Get

marshal :: MonadIO m => Codec -> Message -> m B.ByteString
marshal Codec{..} msg = do
    key_ <- case msg ^. encrypt_method of
        EM_ECDH -> pure $ _ecdh ^. ECDH.shared_key
        EM_ST -> pure _random_key
    enc_ <- QQTea.qqteaEncrypt key_ $ msg ^. msg_body
    let w = runPut $ do
            put16be 8001
            put16be $ msg ^. msg_cmd
            put16be 1
            put32be $ msg ^. msg_uin
            put8 0x3
            put8 $ case msg ^. encrypt_method of
                EM_ECDH -> 0x87
                EM_ST -> 0x45
            put8 0
            put32be 2
            put32be 0
            put32be 0
            case msg ^. encrypt_method of
                EM_ECDH -> do
                    put8 0x02
                    put8 0x01
                    putbs _random_key
                    put16be 0x0131
                    put16be $ _ecdh ^. ECDH.svr_public_key_ver
                    lvbs $ _ecdh ^. ECDH.public_key
                    putbs enc_
                EM_ST -> do
                    put8 0x1
                    put8 0x3
                    putbs _random_key
                    put16be 0x0102
                    put16be 0
                    putbs enc_
            put8 0x3
    let w2 = runPut $ do
            put8 0x02
            put16be $ fromIntegral $ B.length w + 3
            putbs w
    pure w2

unmarshal_ :: Codec -> ExceptT String Get Message
unmarshal_ codec_ = do
    flag_ <- lift get8
    when (flag_ /= 0x02) $ do
        throwE $ "unknown flag: " ++ show flag_
    _ <- lift get16be -- len
    _ <- lift get16be -- version?
    cmd_ <- lift get16be
    _ <- lift get16be -- 1?
    uin_ <- lift get32be
    _ <- lift get8
    enc_type_ <- lift get8
    _ <- lift get8
    r <- B.dropEnd 1 <$> lift getRemaining
    body_ <- case enc_type_ of
        0 -> pure $ QQTea.qqteaDecrypt (codec_ ^. ecdh . ECDH.shared_key) r
        3 -> pure $ QQTea.qqteaDecrypt (codec_ ^. wt_session_ticket_key) r
        _ -> throwE $ "unknown enc_type:" ++ show enc_type_
    pure $ Message {
        _msg_cmd = cmd_,
        _msg_uin = uin_,
        _msg_body = body_,
        _encrypt_method = EM_ECDH -- meaningless
    }

unmarshal :: Codec -> B.ByteString -> Either String Message
unmarshal codec_ = runGet (runExceptT $ unmarshal_ codec_)