{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Zephyr.Core.Codec where
import Zephyr.Encrypt.ECDH
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Random

data Codec = Codec {
    _ecdh :: EncryptECDH,
    _random_key :: B.ByteString,
    _wt_session_ticket_key :: B.ByteString
}

newCodec :: IO Codec
newCodec = do
    _ecdh <- generateDefaultKey
    _random_key <- randBytes 16
    _wt_session_ticket_key <- randBytes 16
    pure $ Codec {..}

