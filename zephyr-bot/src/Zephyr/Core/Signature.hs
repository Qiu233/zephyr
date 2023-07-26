{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Core.Signature where
import qualified Data.ByteString.Lazy as B
import Control.Lens
import Data.Word
import Zephyr.Core.Device.Types
import Zephyr.Utils.Common (utf8ToBytes)
import Text.Printf
import Data.HashMap

data Signature = Signature {
    _login_bitmap :: Word64,
    _tgt :: B.ByteString,
    _tgtkey :: B.ByteString,

    _srm_token :: B.ByteString,
    _t133 :: B.ByteString,
    _encryptedA1 :: B.ByteString,
    _user_st_key :: B.ByteString,
    _user_st_web_sig :: B.ByteString,
    _skey :: B.ByteString,
    _skey_expired_time :: Word64,
    _d2 :: B.ByteString,
    _d2key :: B.ByteString,
    _device_token :: B.ByteString,

    _ps_key_map :: Map String B.ByteString,
    _pt4_token_map :: Map String B.ByteString,

    _out_session :: B.ByteString,
    _dpwd :: B.ByteString,
    
    _t104 :: B.ByteString,
    _t174 :: B.ByteString,
    _g :: B.ByteString,
    _t402 :: B.ByteString,
    _rand_seed :: B.ByteString,
    _t547 :: B.ByteString,

    _ksid :: B.ByteString
} deriving (Eq, Show)

$(makeLenses ''Signature)

defaultSignature :: Device -> IO Signature
defaultSignature dev = do
    let buf0 = B.empty
    let _out_session = B.pack [0x02, 0xB0, 0x5B, 0x8B]
    let _login_bitmap = 0
        _tgt = buf0
        _tgtkey = buf0

        _srm_token = buf0
        _t133 = buf0
        _encryptedA1 = buf0
        _user_st_key = buf0
        _user_st_web_sig = buf0
        _skey = buf0
        _skey_expired_time = 0
        _d2 = buf0
        _d2key = buf0
        _device_token = buf0

        _ps_key_map = empty
        _pt4_token_map = empty

        _ksid = utf8ToBytes $ printf "|%s|A8.2.7.27f6ea96" (dev ^. imei)
        _qrsig = buf0
        _g = buf0
        _dpwd = buf0
        _rand_seed = buf0
        _t104 = buf0
        _t174 = buf0
        _t547 = buf0
        _t402 = buf0
    pure $ Signature {..}

