{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.TLV.Prim where
import Data.Word
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Binary
import Zephyr.Packet.Internal
import Control.Monad
import Zephyr.Utils.Codec (md5Lazy)
import qualified Zephyr.Encrypt.QQTea as QQTea
import Zephyr.Utils.Time (getEpochTime)
import Zephyr.Utils.Common (utf8ToBytes)
import Control.Monad.IO.Class
import System.Random (randomIO)
import Data.Bifunctor

tlv :: Word16 -> Put -> B.ByteString
tlv tag body_ = do
    let body = runPut body_
    runPut $ do
        put16be tag
        put16be $ fromIntegral $ B.length body
        putbs body

t1_ :: MonadIO m => Word32 -> (Word8, Word8, Word8, Word8) -> m B.ByteString
t1_ uin_ (a,b,c,d) = do
    r <- randomIO
    time_sec <- fromIntegral <$> getEpochTime
    pure $ tlv 0x01 $ do
        put16be 1
        put32be r
        put32be uin_
        put32be time_sec
        put8 a
        put8 b
        put8 c
        put8 d
        put16be 0

t2_ :: String -> B.ByteString -> B.ByteString
t2_ result sign = do
    tlv 0x02 $ do
        put16be 0
        lvu8 result
        lvbs sign


t8_ :: Word32 -> B.ByteString
t8_ localId = do
    tlv 0x08 $ do
        put16be 0
        put32be localId
        put16be 0

t16_ ::
    Word32 -> Word32 -> Word32 ->
    B.ByteString -> B.ByteString ->
    B.ByteString -> B.ByteString ->
        B.ByteString
t16_ ssoVersion appId subAppId guid apkId apkVersionName apkSign = do
    tlv 0x16 $ do
        put32be ssoVersion
        put32be appId
        put32be subAppId
        putbs guid
        lvbs apkId
        lvbs apkVersionName
        lvbs apkSign

t18_ :: Word32 -> Word32 -> B.ByteString
t18_ appId uin_ = do
    tlv 0x18 $ do
        put16be 1
        put32be 1536
        put32be appId
        put32be 0
        put32be uin_
        put16be 0
        put16be 0

t1B_ :: Word32 -> Word32 -> Word32 ->
    Word32 -> Word32 -> Word32 -> Word32 ->
        B.ByteString
t1B_ micro_ version_ size_ margin_ dpi_ ecLevel_ hint_ = do
    tlv 0x1B $ do
        put32be micro_
        put32be version_
        put32be size_
        put32be margin_
        put32be dpi_
        put32be ecLevel_
        put32be hint_
        put16be 0

t1D_ :: Word32 -> B.ByteString
t1D_ miscBitmap = do
    tlv 0x1D $ do
        put8 1
        put32be miscBitmap
        put32be 0
        put8 0
        put32be 0

t1F_ :: Bool ->
    B.ByteString -> B.ByteString ->
    B.ByteString -> B.ByteString ->
    Word16 -> B.ByteString
t1F_ isRoot osName osVersion simOperatorName apn networkType = do
    tlv 0x1F $ do
        putb isRoot
        lvbs osName
        lvbs osVersion
        put16be networkType
        lvbs simOperatorName
        put16be 0
        lvbs apn

t33_ :: B.ByteString -> B.ByteString
t33_ guid_ = do
    tlv 0x33 $ do
        putbs guid_

t35_ :: Word32 -> B.ByteString
t35_ productType = do
    tlv 0x35 $ do
        put32be productType

t100_ :: Word32 -> Word32 -> Word32 -> B.ByteString
t100_ ssoVersion_ protocol_ mainSigMap_ = do
    tlv 0x100 $ do
        put16be 1
        put32be ssoVersion_
        put32be 16
        put32be protocol_
        put32be 0
        put32be mainSigMap_

t104_ :: B.ByteString -> B.ByteString
t104_ data_ = do
    tlv 0x104 $ do
        putbs data_

t106_ :: (MonadIO m) => Word32 -> Word32 -> Word32 -> Word32 ->
    B.ByteString -> Bool -> B.ByteString -> B.ByteString ->
    Word32 -> m B.ByteString
t106_ uin_ salt_ appId_ ssoVer_ passwordMd5_ guidAvailable_ guid_ tgtgtKey_ wtf_ = do
    let key = md5Lazy $ passwordMd5_ <> B.pack [0,0,0,0]
            <> runPut (put32be $ if salt_ /= 0 then salt_ else uin_)
    r1 <- randomIO
    r2 <- randomIO
    r3 <- randomIO
    time_ <- fromIntegral <$> getEpochTime
    let body_ = runPut $ do
            put16be 4
            put32be r1
            put32be ssoVer_
            put32be 16
            put32be 0
            if uin_ == 0 then
                put64be $ fromIntegral salt_
            else
                put64be $ fromIntegral uin_
            put32be time_
            put32be 0
            put8 1
            putbs passwordMd5_
            putbs tgtgtKey_
            put32be wtf_
            putb guidAvailable_
            if B.length guid_ == 0 then do
                put64be r2
                put64be r3
            else do
                putbs guid_
            put32be appId_
            put32be 1
            lvu8 $ show uin_
            put16be 0
    enc_ <- QQTea.qqteaEncrypt key body_
    pure $ tlv 0x106 $ do
        putbs enc_

t107_ :: Word16 -> B.ByteString
t107_ picType = do
    tlv 0x107 $ do
        put16be picType
        put8 0
        put16be 0
        put8 1

t108_ :: B.ByteString -> B.ByteString
t108_ ksid_ = do
    tlv 0x108 $ do
        putbs ksid_

t109_ :: B.ByteString -> B.ByteString
t109_ androidId_ = do
    tlv 0x109 $ do
        putbs $ md5Lazy androidId_

t10A_ :: B.ByteString -> B.ByteString
t10A_ arr = do
    tlv 0x10A $ do
        putbs arr

t112_ :: Word32 -> B.ByteString
t112_ uin_ = do
    tlv 0x112 $ do
        pututf8 $ show uin_

t116_ :: Word32 -> Word32 -> B.ByteString
t116_ miscBitmap_ subSigMap_ = do
    tlv 0x116 $ do
        put8 0
        put32be miscBitmap_
        put32be subSigMap_
        put8 1
        put32be 1600000226

t124_ :: B.ByteString -> B.ByteString -> B.ByteString
    -> B.ByteString -> B.ByteString
t124_ osType osVersion simInfo apn = do
    tlv 0x124 $ do
        lvbs $ B.take 16 osType
        lvbs $ B.take 16 osVersion
        put16be 2
        lvbs $ B.take 16 simInfo
        lvbs $ B.pack []
        lvbs $ B.take 16 apn

t128_ :: Bool -> Bool -> Bool -> Word32 ->
    B.ByteString -> B.ByteString -> B.ByteString ->
    B.ByteString
t128_ isGuidFromFileNull isGuidAvailable isGuidChanged
    guidFlag buildModel guid buildBrand = do
    tlv 0x128 $ do
        put16be 0
        putb isGuidFromFileNull
        putb isGuidAvailable
        putb isGuidChanged
        put32be guidFlag
        lvbs $ B.take 32 buildModel
        lvbs $ B.take 16 guid
        lvbs $ B.take 16 buildBrand

t141_ :: B.ByteString -> B.ByteString -> B.ByteString
t141_ simInfo_ apn_ = do
    tlv 0x141 $ do
        put16be 1
        lvbs simInfo_
        put16be 2
        lvbs apn_

t142_ :: String -> B.ByteString
t142_ apkId = do
    tlv 0x142 $ do
        put16be 0
        lvbs $ B.take 32 $ utf8ToBytes apkId

t143_ :: B.ByteString -> B.ByteString
t143_ arr = do
    tlv 0x143 $ do
        putbs arr

t144_ :: MonadIO m => B.ByteString -> B.ByteString -> B.ByteString ->
    B.ByteString -> B.ByteString -> B.ByteString ->
    Bool -> Bool -> Bool -> Word32 ->
    B.ByteString -> B.ByteString -> B.ByteString ->
    B.ByteString -> m B.ByteString
t144_ imei devInfo osType osVersion simInfo apn
    isGuidFromFileNull isGuidAvailable isGuidChanged
    guidFlag buildModel guid buildBrand tgtgtKey = do
    let s = runPut $ do
            put16be 5
            putbs $ t109_ imei
            putbs $ t52D_ devInfo
            putbs $ t124_ osType osVersion simInfo apn
            putbs $ t128_ isGuidFromFileNull isGuidAvailable isGuidChanged
                guidFlag buildModel guid buildBrand
            putbs $ t16E_ buildModel
    enc <- QQTea.qqteaEncrypt tgtgtKey s
    pure $ tlv 0x144 $ do
        putbs enc

t145_ :: B.ByteString -> B.ByteString
t145_ guid_ = do
    tlv 0x145 $ do
        putbs guid_

t147_ :: Word32 -> B.ByteString -> B.ByteString -> B.ByteString
t147_ appId apkVersionName apkSignatureMd5 = do
    tlv 0x147 $ do
        put32be appId
        lvbs apkVersionName
        lvbs apkSignatureMd5

t154_ :: Word16 -> B.ByteString
t154_ seq_ = do
    tlv 0x154 $ do
        put32be $ fromIntegral seq_

t166_ :: Word8 -> B.ByteString
t166_ imageType = do
    tlv 0x166 $ do
        put8 imageType

t16A_ :: B.ByteString -> B.ByteString
t16A_ arr = do
    tlv 0x16A $ do
        putbs arr

t16E_ :: B.ByteString -> B.ByteString
t16E_ buildModel = do
    tlv 0x16E $ do
        putbs buildModel

t174_ :: B.ByteString -> B.ByteString
t174_ data_ = do
    tlv 0x174 $ do
        putbs data_

t177_ :: Word32 -> String -> B.ByteString
t177_ buildTime_ sdkVersion_ = do
    tlv 0x177 $ do
        put8 0x01
        put32be buildTime_
        lvu8 sdkVersion_

t17A_ :: Word32 -> B.ByteString
t17A_ value_ = do
    tlv 0x17A $ do
        put16be 4
        put32be value_

t17C_ :: String -> B.ByteString
t17C_ code_ = do
    tlv 0x17C $ do
        lvu8 code_

t187_ :: B.ByteString -> B.ByteString
t187_ macAddress = do
    tlv 0x187 $ do
        putbs $ md5Lazy macAddress

t188_ :: B.ByteString -> B.ByteString
t188_ androidId = do
    tlv 0x188 $ do
        putbs $ md5Lazy androidId

t191_ :: Word8 -> B.ByteString
t191_ k = do
    tlv 0x191 $ do
        put8 k

t193_ :: String -> B.ByteString
t193_ ticket = do
    tlv 0x193 $ do
        pututf8 ticket

t194_ :: B.ByteString -> B.ByteString
t194_ imsiMd5 = do
    tlv 0x194 $ do
        putbs imsiMd5

t197_ :: B.ByteString
t197_ = do
    tlv 0x197 $ do
        put8 0

t198_ :: B.ByteString
t198_ = do
    tlv 0x198 $ do
        put8 0

t202_ :: B.ByteString -> B.ByteString -> B.ByteString
t202_ wifiBSSID wifiSSID = do
    tlv 0x202 $ do
        lvbs $ B.take 16 wifiBSSID
        lvbs $ B.take 32 wifiSSID

t400_ :: MonadIO m => B.ByteString -> Word64 -> B.ByteString ->
    B.ByteString -> Word32 -> Word32 -> B.ByteString
    -> m B.ByteString
t400_ g uin_ guid_ dpwd j2 j3 randSeed = do
    time_sec <- fromIntegral <$> getEpochTime
    let s = runPut $ do
            put16be 1
            put64be uin_
            putbs guid_
            putbs dpwd
            put32be j2
            put32be j3
            put32be time_sec
            putbe randSeed
    enc <- QQTea.qqteaEncrypt g s
    pure $ tlv 0x400 $ do
        putbs enc

t401_ :: B.ByteString -> B.ByteString
t401_ d = do
    tlv 0x401 $ do
        putbs d

t511_ :: [String] -> B.ByteString
t511_ domains = do
    tlv 0x511 $ do
        put16be $ fromIntegral $ length domains
        forM_ domains $ \d -> do
            put8 1
            lvu8 d

t516_ :: B.ByteString
t516_ = do
    tlv 0x516 $ do
        put32be 0

t521_ :: Word32 -> B.ByteString
t521_ i = do
    tlv 0x521 $ do
        put32be i
        put16be 0

t525_ :: B.ByteString -> B.ByteString
t525_ t536' = do
    tlv 0x525 $ do
        put16be 1
        putbs t536'

t52D_ :: B.ByteString -> B.ByteString
t52D_ devInfo = do
    tlv 0x52D $ do
        putbs devInfo

t536_ :: B.ByteString -> B.ByteString
t536_ loginExtraData = do
    tlv 0x536 $ do
        putbs loginExtraData

type EnergySigner = Word64 -> String -> String -> B.ByteString -> IO (Either String B.ByteString)
type FekitSigner = Word64 -> String -> String -> String -> B.ByteString -> IO (Either String (B.ByteString, B.ByteString, B.ByteString))

t544_ :: MonadIO m => Word64 -> String -> Word32 -> String ->
    B.ByteString -> String -> EnergySigner -> m (Either String B.ByteString)
t544_ userId moduleId subCmd sdkVersion guid_ appVersion signer = do
    let salt = runPut $ do
            put64be userId
            lvbs guid_
            lvu8 sdkVersion
            put32be subCmd
    t544Custom_ userId moduleId appVersion salt signer


t544V2_ :: MonadIO m => Word64 -> String -> Word32 -> String ->
    B.ByteString -> String -> EnergySigner -> m (Either String B.ByteString)
t544V2_ userId moduleId subCmd sdkVersion guid_ appVersion signer = do
    let salt = runPut $ do
            put32be 0
            lvbs guid_
            lvu8 sdkVersion
            put32be subCmd
            put32be 0
    t544Custom_ userId moduleId appVersion salt signer

t544Custom_ :: MonadIO m => Word64 -> String -> String -> B.ByteString -> EnergySigner -> m (Either String B.ByteString)
t544Custom_ userId moduleId appVersion salt signer = do
    signed <- liftIO $ signer userId moduleId appVersion salt
    pure $ second (tlv 0x544 . putbs) signed

t545_ :: B.ByteString -> B.ByteString
t545_ qimei_ = do
    tlv 0x545 $ do
        putbs qimei_