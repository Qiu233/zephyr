{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Zephyr.Core.Device where
import Data.Word
import qualified Data.ByteString.Lazy as B
import Text.Printf
import Control.Lens
import Control.Monad.IO.Class
import Zephyr.Utils.Codec (md5OfU8, md5Of_)
import Zephyr.Utils.Common (encodeHex, utf8ToBytes)
import Prelude hiding (product)
import Zephyr.Utils.Binary
import Zephyr.Utils.GUID
import Data.Int
import Control.Monad.Trans.State as State
import Control.Monad (replicateM)
import Zephyr.Utils.Random
import Control.Monad.State (lift)


-- | OSVersion incremental release codeName sdk
data OSVersion = OSVersion {
    _incremental :: Word32,
    _release :: String,
    _codeName :: String,
    _sdk :: Word32
} deriving Show

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
    _ip_address :: String,
    _wifi_bssid :: String,
    _wifi_ssid :: String,
    _imei :: String,
    _android_id :: String,
    _apn :: String,
    _os_version :: OSVersion,
    _imsi_md5 :: B.ByteString,
    _guid :: GUID,
    _qimei16 :: String,
    _qimei36 :: String
} deriving Show

$(makeLenses ''Device)

generateDevice :: MonadIO m => Word64 -> m Device
generateDevice uin = do
    let uinStr = show uin
    let hash = B.fromStrict . md5OfU8 $ uinStr
    let _android_id = printf "OICQX.%d%d.%d%d" (runGet_ get16be hash) (B.index hash 2) (B.index hash 3) (head uinStr) :: String
    let _incremental = runGet_ get32be hash

    let _display = _android_id
        _product = "MRS4S"
        _device_name = "HIM188MOE"
        _board = "MIRAI-YYDS"
        _brand = "OICQX"
        _model = "Qiu 2020"
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
            printf "Linux version 4.19.71-%d (Qiu)"
            (runGet_ get16be $ B.drop 4 hash)
            :: String
        _base_band = ""
        _sim = "T-Mobile"
        _os_type = "android"
        _mac_address =
            printf "00:50:%X:%X:%X:%X"
            (B.index hash 6)
            (B.index hash 7)
            (B.index hash 8)
            (B.index hash 9)
            :: String
        _ip_address = "10.0." ++ show (B.index hash 10) ++ "." ++ show (B.index hash 11)
        _wifi_bssid = _mac_address
        _wifi_ssid = printf "TP-LINK-%X" uin :: String
    _imei <- liftIO randIMEI
    let _apn = "wifi"
        _os_version = OSVersion _incremental "10" "REL" 29
    _imsi_md5 <- randBytes 16
    let _guid = createGUID $ B.fromStrict . md5Of_ $ 
                    (utf8ToBytes _imei <> utf8ToBytes _mac_address)
        _qimei16 = ""
        _qimei36 = ""
    pure $ Device{..}

randIMEI :: IO String
randIMEI = do --TODO: rewrite this
    (_, sum', str) <- State.execStateT (replicateM 14 transfer) (0::Integer, 0, "")
    pure $ str ++ show (rem (sum' * 9) 10)
    where
        transfer = do
            (i, sum', str) <- State.get
            to_add <- lift $    (randR @Int32 (0, 9) <&>
                                (if even i then (*2) else id)) <&>
                                (\x -> (if x >= 10 then rem x 10 +1 else x))
            put (i+1, sum' + to_add, str ++ show to_add)

