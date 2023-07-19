{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}

module Zephyr.Core.Codec where
import Zephyr.Encrypt.ECDH
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Random
import Control.Lens
import Data.Word
import Zephyr.Utils.Binary
import Zephyr.Encrypt.QQTea
import Control.Monad.IO.Class

data Codec = Codec {
    _ecdh :: EncryptECDH,
    _random_key :: B.ByteString,
    _wt_session_ticket_key :: B.ByteString
}
$(makeLenses ''Codec)

newCodec :: IO Codec
newCodec = do
    _ecdh <- generateDefaultKey
    _random_key <- randBytes 16
    _wt_session_ticket_key <- randBytes 16
    pure $ Codec {..}

data EncryptMethod = EM_ECDH | EM_ST

data Message =  Message {
    _msg_uin :: Word32,
    _msg_cmd :: Word16,
    _encrypt_method :: EncryptMethod,
    _msg_body :: B.ByteString
}

$(makeLenses ''Message)

marshal :: MonadIO m => Codec -> Message -> m B.ByteString
marshal Codec{..} msg = do
    key_ <- case msg ^. encrypt_method of
        EM_ECDH -> pure $ _ecdh ^. shared_key
        EM_ST -> pure _random_key
    enc_ <- qqteaEncrypt (tea16KeyFromBytes key_) $ msg ^. msg_body
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
                    put16be $ _ecdh ^. svr_public_key_ver
                    put16be $ fromIntegral . B.length $ _ecdh ^. public_key
                    putbs $ _ecdh ^. public_key
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