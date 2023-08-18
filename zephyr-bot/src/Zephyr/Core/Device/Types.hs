{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Core.Device.Types where


import Data.Word
import qualified Data.ByteString.Lazy as B
import Text.Printf
import Control.Lens
import Zephyr.Utils.Codec (md5OfU8, md5Lazy)
import Prelude hiding (product)
import Zephyr.Binary
import Zephyr.Utils.GUID
import Data.Int
import Control.Monad.Trans.State as State
import Control.Monad (replicateM)
import System.Random (mkStdGen, Random (randoms, randomR), RandomGen)
import Zephyr.Binary.Get


-- | OSVersion incremental release codeName sdk
data OSVersion = OSVersion {
    _incremental :: Word32,
    _release :: String,
    _codeName :: String,
    _sdk :: Word32
} deriving (Eq, Show)

$(makeLenses ''OSVersion)


data Device = Device {
    _display :: String,
    _product :: String,
    _device_name :: String,
    _board :: String,
    _brand :: String,
    _model :: String,
    _bootloader :: String,
    _fingerprint :: String,
    _boot_id :: String,
    _proc_version :: String,
    _base_band :: String,
    _sim :: String,
    _os_type :: String,
    _mac_address :: String,
    _ip_address :: (Word8, Word8, Word8, Word8),
    _wifi_bssid :: String,
    _wifi_ssid :: String,
    _imei :: String,
    _android_id :: String,
    _apn :: String,
    _vendor_name :: String,
    _vendor_os_name :: String,
    _os_version :: OSVersion,
    _imsi :: B.ByteString,
    _guid :: GUID,
    _tgtgt_key :: B.ByteString,
    _qimei16 :: String,
    _qimei36 :: String
} deriving (Eq, Show)

$(makeLenses ''Device)

generateDevice :: Word64 -> Device
generateDevice uin = do
    let uinStr = show uin
    let pg = mkStdGen $ fromIntegral uin
    let hash = B.fromStrict . md5OfU8 $ uinStr
    let _android_id = printf "OICQX.%d%d.%d%c" (runGet get16be hash) (B.index hash 2) (B.index hash 3) (head uinStr) :: String
    let _incremental = runGet get32be $ B.drop 12 hash

    let _display = _android_id
        _product = "MRS4S"
        _device_name = "HIM188MOE"
        _board = "MIRAI-YYDS"
        _brand = "OICQX"
        _model = "Konata 2020"
        _bootloader = "U-boot"
        _fingerprint =
            printf "%s/%s/%s:10/%s/%d:user/release-keys"
            _brand
            _product
            _device_name
            _android_id
            _incremental
            :: String
        _boot_id = show $ createGUID hash
        _proc_version =
            printf "Linux version 4.19.71-%d (konata@takayama.github.com)"
            (runGet get16be $ B.drop 4 hash)
            :: String
        _base_band = ""
        _sim = "T-Mobile"
        _os_type = "android"
        _mac_address =
            printf "00:50:%02X:%02X:%02X:%02X"
            (B.index hash 6)
            (B.index hash 7)
            (B.index hash 8)
            (B.index hash 9)
            :: String
        _ip_address = (10, 0, B.index hash 10, B.index hash 11)
        _wifi_bssid = _mac_address
        _wifi_ssid = printf "TP-LINK-%X" uin :: String
    let (_imei, _) = randIMEI pg
    let _apn = "wifi"
        _vendor_name = "MIUI"
        _vendor_os_name = "mirai"
        _os_version = OSVersion _incremental "10" "REL" 29
    let _imsi = B.pack $ take 16 $ randoms pg
    let _guid = createGUID $ B.fromStrict . md5OfU8 $ (_imei ++ _mac_address)
        _tgtgt_key = md5Lazy $ B.pack (take 16 $ randoms pg) <> guidBytes _guid
        _qimei16 = ""
        _qimei36 = ""
    Device{..}

randIMEI :: RandomGen b => b -> ([Char], b)
randIMEI g = do --TODO: rewrite this
    (_, sum', str, p) <- State.execStateT (replicateM 14 transfer) (0::Integer, 0, "", g)
    let rst = str ++ show (rem (sum' * 9) 10)
    (rst, p)
    where
        transfer = do
            (i, sum', str, p) <- State.get
            let (r, p') = randomR @Int32 (0, 9) p
            let to_add = (r & (if even i then (*2) else id)) & (\x -> (if x >= 10 then rem x 10 +1 else x))
            put (i+1, sum' + to_add, str ++ show to_add, p')
