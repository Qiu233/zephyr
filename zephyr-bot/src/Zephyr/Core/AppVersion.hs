{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Core.AppVersion where
import qualified Data.ByteString.Lazy as B
import Control.Lens
import Data.Word (Word32)

data ProtocolType =
    Unset |
    AndroidPhone |
    AndroidPad
    deriving (Show, Eq)

data AppVersion = AppVersion {
    _apk_id :: String,
    _sub_id :: Word32,
    _app_key :: String,
    _sort_version :: String,
    _build_time :: Word32,
    _sign :: B.ByteString,
    _sdk_ver :: String,
    _ssover :: Word32,
    _misc_bitmap :: Word32,
    _sub_sig_map :: Word32,
    _main_sig_map :: Word32,
    _qua :: String,
    _protocol :: ProtocolType
} deriving (Show, Eq)

$(makeLenses ''AppVersion)

androidPhone :: AppVersion
androidPhone = AppVersion {
    _apk_id = "com.tencent.mobileqq",
    _sub_id = 537164840,
    _app_key = "0S200MNJT807V3GE",
    _sort_version = "8.9.63.11390",
    _build_time = 1685069178,
    _sign = B.pack [0xA6, 0xB7, 0x45, 0xBF, 0x24, 0xA2, 0xC2, 0x77, 0x52, 0x77, 0x16, 0xF6, 0xF3, 0x6E, 0xB6, 0x8D],
    _sdk_ver = "6.0.0.2546",
    _ssover = 20,
    _misc_bitmap = 150470524,
    _sub_sig_map = 0x10400,
    _main_sig_map = 16724722,
    _qua = "V1_AND_SQ_8.9.63_4194_YYB_D",
    _protocol = AndroidPhone
}