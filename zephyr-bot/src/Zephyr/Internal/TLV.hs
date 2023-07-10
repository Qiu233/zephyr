{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Internal.TLV where
import Control.Monad.Cont
import Data.Word
import Zephyr.Utils.Binary.Types
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Binary
import Data.Int
import Zephyr.Utils.Time
import Zephyr.Utils.Random
import Zephyr.Utils.Codec (md5Of_, md5OfU8)
import Data.Bits
import Zephyr.Encrypt.QQTea
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import Zephyr.Utils.Common
import Text.Read (readMaybe)
import System.Random (randomIO)


putlv16be :: B.ByteString -> Put
putlv16be a = do
    let s = runPut $ putbe a
    let len = B.length s
    put16be $ fromIntegral len
    putbs s

putlv16LimitedBE :: Int -> B.ByteString -> Put
putlv16LimitedBE n' a = do
    let n = fromIntegral n'
    let bs = B.take n $ runPut $ putbe a
    putlv16be bs

putlvs16be :: String -> Put
putlvs16be = putlv16be . utf8ToBytes

putlvs16LimitedBE :: Int -> String -> Put
putlvs16LimitedBE n = putlv16LimitedBE n . utf8ToBytes

tlvPack :: MonadIO m => Word16 -> Put -> m B.ByteString
tlvPack packetType bodyBuilder = pure . runPut $ do
        put16be packetType
        putlv16be $ runPut bodyBuilder

t1 :: MonadIO m => Word32 -> B.ByteString -> m B.ByteString
t1 uin ip = liftIO $ do
    time <- getEpochTime
    r <- randomIO
    tlvPack 0x01 $ do
        put16be 1
        put32be r
        put32be uin
        put32be $ fromIntegral time
        putbs ip
        put16be 0

t1b :: MonadIO m => Word32-> Word32-> Word32-> Word32-> Word32-> Word32-> Word32-> m B.ByteString
t1b micro version size margin dpi ec_level hint =
    tlvPack 0x1b $ do
        put32be micro
        put32be version
        put32be size
        put32be margin
        put32be dpi
        put32be ec_level
        put32be hint
        put16be 0

t1d :: MonadIO m => Word32 -> m B.ByteString
t1d misc_bitmap =
    tlvPack 0x1d $ do
        put8 1
        put32be misc_bitmap
        put32be 0
        put8 0
        put32be 0


t1f :: MonadIO m => Bool-> String-> String-> String-> String-> Word16-> m B.ByteString
t1f is_root os_name os_version sim_operator_name apn network_type =
    tlvPack 0x1f $ do
        put8 (if is_root then 1 else 0)
        putlvs16be os_name
        putlvs16be os_version
        put16be network_type
        putlvs16be sim_operator_name
        putlvs16be "" -- wtf?
        putlvs16be apn


t2 :: MonadIO m => String -> B.ByteString -> m B.ByteString
t2 result sign =
    tlvPack 0x02 $ do
        put16be 0
        putlvs16be result
        putlv16be sign

t8 :: MonadIO m => Word32 -> m B.ByteString
t8 local_id =
    tlvPack 0x08 $ do
        put16be 0
        put32be local_id
        put16be 0

t10a :: MonadIO m => B.ByteString -> m B.ByteString
t10a arr =
    tlvPack 0x10a $ do
        putbs arr -- the body is arr itself

t16 :: MonadIO m => Word32-> Word32-> Word32-> B.ByteString-> String-> String-> B.ByteString-> m B.ByteString
t16 sso_version app_id sub_app_id guid apk_id apk_version_name apk_sign =
    tlvPack 0x16 $ do
        put32be sso_version
        put32be app_id
        put32be sub_app_id
        putbs guid
        putlvs16be apk_id
        putlvs16be apk_version_name
        putlv16be apk_sign

t16a :: MonadIO m => B.ByteString -> m B.ByteString
t16a arr =
    tlvPack 0x16a $ do
        putbs arr

t16e :: MonadIO m => B.ByteString -> m B.ByteString
t16e build_model =
    tlvPack 0x16e $ do
        putbs build_model

t17a :: MonadIO m => Word32 -> m B.ByteString
t17a value =
    tlvPack 0x17a $ do
        put32be value

t17c :: MonadIO m => String -> m B.ByteString
t17c code =
    tlvPack 0x17c $ do
        putlvs16be code

t18 :: MonadIO m => Word32 -> Word32 -> m B.ByteString
t18 app_id uin =
    tlvPack 0x18 $ do
        put16be 1
        put32be 1536
        put32be app_id
        put32be 0
        put32be uin
        put16be 0
        put16be 0

t33 :: MonadIO m => B.ByteString -> m B.ByteString
t33 guid =
    tlvPack 0x33 $ do
        putbs guid

t35 :: MonadIO m => Word32 -> m B.ByteString
t35 product_type =
    tlvPack 0x35 $ do
        put32be product_type

t52d :: MonadIO m => B.ByteString -> m B.ByteString
t52d dev_info =
    tlvPack 0x52d $ do
        putbs dev_info

t100 :: MonadIO m => Word32 -> Word32 -> Word32 -> m B.ByteString
t100 sso_version protocol main_sig_map =
    tlvPack 0x100 $ do
        put16be 1
        put32be sso_version
        put32be 16
        put32be protocol
        put32be 0
        put32be main_sig_map

t104 :: MonadIO m => B.ByteString -> m B.ByteString
t104 data_ =
    tlvPack 0x104 $ do
        putbs data_

t106 :: MonadIO m => Word32-> Word32-> Word32-> Word32-> B.ByteString-> Bool-> B.ByteString-> B.ByteString-> Word32-> m B.ByteString
t106 uin salt app_id sso_ver password_md5
        guid_available guid tgtgt_key wtf = liftIO $ do
    r <- randomIO
    time <- getEpochTime
    _guid <- if guid == nil then randBytes 16 else pure guid
    let key = tea16KeyFromBytes . B.fromStrict . md5Of_ . runPut $ do
            putbs password_md5
            putbs $ B.pack [0,0,0,0]
            put32be (if salt == 0 then uin else salt)
        body = runPut $ do
            put16be 4
            put32be r
            put32be sso_ver
            put32be 16 -- appId
            put32be 0  -- client version
            put64be $ fromIntegral (if uin == 0 then salt else uin)
            put32be $ fromIntegral time
            putbs $ B.pack [0,0,0,0] -- fake ip
            put8 0x1
            putbs password_md5
            putbs tgtgt_key
            put32be wtf
            put8 (if guid_available then 1 else 0)
            putbs _guid
            put32be app_id
            put32be 1
            putlvs16be $ show uin
            put16be 0
    qqteaEncrypt key body >>= tlvPack 0x106 . putbs
    where nil = B.replicate 16 0

t107 :: MonadIO m => Word16 -> m B.ByteString
t107 pic_type =
    tlvPack 0x107 $ do
        put16be pic_type
        put8 0
        put16be 0
        put8 01

t108 :: MonadIO m => B.ByteString -> m B.ByteString
t108 ksid =
    tlvPack 0x108 $ do
        putbs ksid

t109 :: MonadIO m => String -> m B.ByteString
t109 android_id =
    tlvPack 0x109 $ do
        putbss . md5OfU8 $ android_id

t116 :: MonadIO m => Word32 -> Word32 -> m B.ByteString
t116 misc_bitmap sub_sig_map =
    tlvPack 0x116 $ do
        put8 0
        put32be misc_bitmap
        put32be sub_sig_map
        put8 1
        put32be 1600000226 -- why?

t124 :: MonadIO m => String -> String -> String -> String -> m B.ByteString
t124 os_type os_version sim_info apn =
    tlvPack 0x124 $ do
        putls os_type
        putls os_version
        put16be 2
        putls sim_info
        putl $ B.pack [] -- TODO:
        putls apn
    where
        putl = putlv16LimitedBE 16
        putls = putl . utf8ToBytes

t128 :: MonadIO m => Bool-> Bool-> Bool-> Word32-> String-> B.ByteString-> String-> m B.ByteString
t128 is_guid_from_file_null is_guid_available
    is_guid_changed guid_flag build_model
    guid build_brand =
    tlvPack 0x128 $ do
        put16be 0
        putsw is_guid_from_file_null
        putsw is_guid_available
        putsw is_guid_changed
        put32be guid_flag
        putlvs16LimitedBE 32 build_model
        putlv16LimitedBE 16 guid
        putlvs16LimitedBE 16 build_brand
    where
        putsw x = put8 $ if x then 1 else 0

t141 :: MonadIO m => String -> String -> m B.ByteString
t141 sim_info apn =
    tlvPack 0x141 $ do
        put16be 1
        putlvs16be sim_info
        put16be 2
        putlvs16be apn

t142 :: MonadIO m => String -> m B.ByteString
t142 apk_id =
    tlvPack 0x142 $ do
        put16be 0
        putlvs16LimitedBE 32 apk_id

t143 :: MonadIO m => B.ByteString -> m B.ByteString
t143 arr =
    tlvPack 0x143 $ do
        putbs arr

t144 :: MonadIO m => String-> B.ByteString-> String-> String-> String->
        String-> Bool-> Bool-> Bool-> Word32-> String->
        B.ByteString-> String-> B.ByteString-> m B.ByteString
t144 imei dev_info os_type os_version
    sim_info apn is_guid_from_file_null
    is_guid_available is_guid_changed
    guid_flag build_model guid
    build_brand tgtgt_key = liftIO $ do
    v109 <- t109 imei
    v52d <- t52d dev_info
    v16e <- t16e $ utf8ToBytes build_model
    v124 <- t124 os_type os_version sim_info apn
    v128 <- t128 is_guid_from_file_null is_guid_available
                 is_guid_changed guid_flag build_model
                 guid build_brand
    let body = runPut $ put16be 5 >> mapM_ putbs [v109, v52d, v124, v128, v16e]
    qqteaEncrypt (tea16KeyFromBytes tgtgt_key) body >>= tlvPack 0x144 . putbs

t145 :: MonadIO m => B.ByteString -> m B.ByteString
t145 guid =
    tlvPack 0x145 $ do
        putbs guid

t147 :: MonadIO m => Word32 -> String -> B.ByteString -> m B.ByteString
t147 app_id apk_version_name apk_signature_md5 =
    tlvPack 0x147 $ do
        put32be app_id
        putlvs16LimitedBE 32 apk_version_name
        putlv16LimitedBE 32 apk_signature_md5

t154 :: MonadIO m => Word16 -> m B.ByteString
t154 sq =
    tlvPack 0x154 $ do
        put32be $ fromIntegral sq

t166 :: MonadIO m => Word8 -> m B.ByteString
t166 image_type =
    tlvPack 0x166 $ do
        put8 image_type


t174 :: MonadIO m => B.ByteString -> m B.ByteString
t174 data_ =
    tlvPack 0x174 $ do
        putbs data_


t177 :: MonadIO m => Word32 -> String -> m B.ByteString
t177 build_time sdk_version =
    tlvPack 0x177 $ do
        put8 1
        put32be build_time
        putlvs16be sdk_version


t187 :: MonadIO m => String -> m B.ByteString
t187 mac_address =
    tlvPack 0x187 $ do
        putbss . md5OfU8 $ mac_address


t188 :: MonadIO m => String -> m B.ByteString
t188 android_id =
    tlvPack 0x188 $ do
        putbss . md5OfU8 $ android_id


t191 :: MonadIO m => Word8 -> m B.ByteString
t191 k =
    tlvPack 0x191 $ do
        put8 k


t193 :: MonadIO m => String -> m B.ByteString
t193 ticket =
    tlvPack 0x193 $ do
        pututf8 ticket


t194 :: MonadIO m => B.ByteString -> m B.ByteString
t194 imsi_md5 =
    tlvPack 0x194 $ do
        putbs imsi_md5


t197 :: MonadIO m => m B.ByteString
t197 =
    tlvPack 0x197 $ do
        putbs $ B.pack [0]


t198 :: MonadIO m => m B.ByteString
t198 =
    tlvPack 0x198 $ do
        putbs $ B.pack [0]


t202 :: MonadIO m => String -> String -> m B.ByteString
t202 wifi_bssid wifi_ssid =
    tlvPack 0x202 $ do
        putlvs16LimitedBE 16 wifi_bssid
        putlvs16LimitedBE 32 wifi_ssid


t400 :: MonadIO m => B.ByteString-> Word64-> B.ByteString-> B.ByteString-> Word64-> Word64-> B.ByteString-> m B.ByteString
t400 g uin guid dpwd j2 j3 rand_seed = liftIO $ do
    time <- getEpochTime
    let body = runPut $ do
            put16be 1
            put64be uin
            putbs guid
            putbs dpwd
            put32be $ fromIntegral j2
            put32be $ fromIntegral j3
            put32be $ fromIntegral time
            putbs rand_seed
    qqteaEncrypt (tea16KeyFromBytes g) body >>= tlvPack 0x400 . putbs


t401 :: MonadIO m => B.ByteString -> m B.ByteString
t401 d =
    tlvPack 0x401 $ do
        putbs d


t511 :: MonadIO m => [String] -> m B.ByteString
t511 domains = do
    let arr2 = filter (not . null) domains
    tlvPack 0x511 $ do
        put16be . fromIntegral $ length arr2
        forM_ arr2 $ \d -> do
            let index_of = fromMaybe (-1) $ elemIndex '(' d
                index_of2 = fromMaybe (-1) $ elemIndex ')' d
            if index_of /= 0 || index_of2 <= 0 then do
                put8 0x01
                putlvs16be d
            else do
                fromMaybe (pure ()) $ (readMaybe @Int32 $ slice (index_of + 1) index_of2 d) >>= \i -> Just $ do
                    let b = if (1048576 .&. i) > 0 then 1 else 0
                    put8 (if (i .&. 134217728) > 0 then b .|. 2 else b)
                    putlvs16be $ sliceToEnd (index_of2 + 1) d

t516 :: MonadIO m => m B.ByteString
t516 = tlvPack 0x516 $ put32be 0

t521 :: MonadIO m => Word32 -> m B.ByteString
t521 i =
    tlvPack 0x521 $ do
        put32be i
        put16be 0

t525 :: MonadIO m => B.ByteString -> m B.ByteString
t525 t =
    tlvPack 0x525 $ do
        put16be 1
        putbs t

t536 :: MonadIO m => B.ByteString -> m B.ByteString
t536 login_extra_data =
    tlvPack 0x536 $ do
        putbs login_extra_data

t544 :: MonadIO m => Word32 -> Word32 -> Word64 -> B.ByteString -> String -> m B.ByteString
t544 v subCmd uin guid sdkVer = do
    let salt = runPut $ do
            if v == 2 then do
                put32be 0
                putlv16be guid
                putlvs16be sdkVer
                put32be subCmd
                put32be 0
            else do
                put64be uin
                putlv16be guid
                putlvs16be sdkVer
                put32be subCmd
    --signed <- T544ASM.sign salt
    tlvPack 0x544 $ do
        put32be v
        --putbs signed

t545 :: MonadIO m => String -> m B.ByteString
t545 qimei = do
    tlvPack 0x545 $ do
        putbss $ md5OfU8 qimei

t547 :: MonadIO m => B.ByteString -> m B.ByteString
t547 s547 = tlvPack 0x547 $ putbs s547

guidFlag :: Word32
guidFlag = (shiftL 1 24 .&. 0xFF000000) .|. 0