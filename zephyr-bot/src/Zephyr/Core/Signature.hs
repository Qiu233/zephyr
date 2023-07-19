{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Core.Signature where
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Random (randBytes)
import Control.Lens
import Data.Word

data BigDataChannel = BigDataChannel {
    _ip :: String,
    _port :: Int,
    _sig_session :: B.ByteString,
    _session_key :: B.ByteString
}

data Signature = Signature {
    _session :: B.ByteString,
    _tgtgt :: B.ByteString,
    _tgt :: B.ByteString,
    _skey :: B.ByteString,
    _ksid :: B.ByteString,
    _d2 :: B.ByteString,
    _d2key :: B.ByteString,
    _t104 :: B.ByteString,
    _t174 :: B.ByteString,
    _t547 :: B.ByteString,
    _qrsig :: B.ByteString,
    _big_data :: BigDataChannel,
    _emp_time :: Int,
    _time_diff :: Word32
}

$(makeLenses ''Signature)

defaultSignature :: IO Signature
defaultSignature = do
    _session <- randBytes 4
    _tgtgt <- randBytes 16
    let buf0 = B.empty
    let _tgt = buf0
        _skey = buf0
        _ksid = buf0
        _d2 = buf0
        _d2key = buf0
        _t104 = buf0
        _t174 = buf0
        _t547 = buf0
        _qrsig = buf0
        _big_data = BigDataChannel "" 0 buf0 buf0
        _emp_time = 0
        _time_diff = 0
    pure $ Signature {..}

