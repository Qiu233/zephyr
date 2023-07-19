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
