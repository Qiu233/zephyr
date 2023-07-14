{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
module Zephyr.Packet.TLVBuilder where

import qualified Zephyr.Internal.TLV as T
import Zephyr.Engine.Context
import qualified Data.ByteString.Lazy as B
import Control.Lens hiding (Context)
import Zephyr.Core.Device
import Zephyr.Utils.Binary
import Control.Monad.IO.Class
import Zephyr.Core.ClientApp
import Zephyr.Core.Signature as Sig
import Prelude hiding (id, seq)
import Zephyr.Utils.GUID (guidBytes)
import Zephyr.Utils.Common
import Text.Printf (printf)
import qualified Zephyr.Utils.ProtoLite as PL
import Zephyr.Encrypt.QQTea (qqteaEncrypt, tea16KeyFromBytes)
import Control.Concurrent.STM (readTVarIO)
import Zephyr.Utils.Random
import Data.Word
import Zephyr.Utils.Time (getEpochTime)
import Zephyr.Utils.Codec (md5Lazy, md5OfU8)
import Control.Monad
import qualified Zephyr.Internal.TLV.T544.ASM as T544
import System.Random (randomIO)

lv :: Put -> Put
lv p = do
    let s = runPut p
    put16be $ fromIntegral $ B.length s
    putbs s

lvbs :: B.ByteString -> Put
lvbs = lv . putbs

lvu8 :: String -> Put
lvu8 = lv . pututf8

packTLV :: Word16 -> Put -> B.ByteString
packTLV t p = runPut $ do
    put16be t
    lv p

packTLV_ :: (Monad m) => Word16 -> Put -> m B.ByteString
packTLV_ t p = pure $ packTLV t p

t1 :: ContextIOT m => m B.ByteString
t1 = do
    (a,b,c,d) <- use $ device . ip_address
    uin_ <- fromIntegral <$> use uin
    time_ <- fromIntegral <$> getEpochTime
    r <- randomIO
    packTLV_ 0x01 $ do
        put16be 1
        put32be r
        put32be uin_
        put32be time_
        put8 a
        put8 b
        put8 c
        put8 d
        put16be 0

t8 :: ContextIOT m => m B.ByteString
t8 = do
    packTLV_ 0x08 $ do
        put16be 0
        put32be 2052
        put16be 0


t16 :: ContextIOT m => m B.ByteString
t16 = do
    app_id_ <- use $ client_app . app_id
    sub_id_ <- use $ client_app . sub_id
    guid_ <- guidBytes <$> use (device . guid)
    id_ <- use $ client_app . id
    ver_ <- use $ client_app . ver
    sign_ <- use $ client_app . sign
    packTLV_ 0x16 $ do
        put32be 7
        put32be app_id_
        put32be sub_id_
        putbs guid_
        lvu8 id_
        lvu8 ver_
        lvbs sign_

t18 :: ContextIOT m => m B.ByteString
t18 = do
    app_id_ <- use $ client_app . app_id
    uin_ <- fromIntegral <$> use uin
    packTLV_ 0x18 $ do
        put16be 1
        put32be 1536
        put32be app_id_
        put32be 0
        put32be uin_
        put16be 0
        put16be 0

t1B :: ContextIOT m => m B.ByteString
t1B = do
    packTLV_ 0x1B $ do
        put32be 0
        put32be 0
        put32be 3
        put32be 4
        put32be 72
        put32be 2
        put32be 2
        put16be 0

t1D :: ContextIOT m => m B.ByteString
t1D = do
    packTLV_ 0x1D $ do
        put8 1
        put32be 184024956
        put32be 0
        put8 0
        put32be 0

t1F :: ContextIOT m => m B.ByteString
t1F = do
    packTLV_ 0x1F $ do
        put8 0
        lvu8 "android"
        lvu8 "7.1.2"
        put16be 2
        lvu8 "China Mobile GSM"
        lvu8 ""
        lvu8 "wifi"

t33 :: ContextIOT m => m B.ByteString
t33 = do
    guid_ <- guidBytes <$> use (device . guid)
    packTLV_ 0x33 $ do
        putbs guid_

t35 :: ContextIOT m => m B.ByteString
t35 = do
    packTLV_ 0x35 $ do
        put32be 8

t100 :: ContextIOT m => m B.ByteString
t100 = do
    ssover_ <- use $ client_app . ssover
    app_id_ <- use $ client_app . app_id
    sub_id_ <- use $ client_app . sub_id
    main_sig_map_ <- use $ client_app . main_sig_map
    packTLV_ 0x100 $ do
        put16be 1
        put32be ssover_
        put32be app_id_
        put32be sub_id_
        put32be 0
        put32be main_sig_map_

t104 :: ContextIOT m => m B.ByteString
t104 = do
    t104_ <- use $ signature . Sig.t104
    packTLV_ 0x104 $ do
        putbs t104_

t106 :: ContextIOT m => B.ByteString -> m B.ByteString
t106 md5pass = do
    uin_ <- use uin
    app_id_ <- use $ client_app . app_id
    sub_id_ <- use $ client_app . sub_id
    ssover_ <- use $ client_app . ssover
    guid_ <- guidBytes <$> use (device . guid)
    tgtgt_ <- use $ signature . tgtgt
    r <- randomIO
    time <- getEpochTime
    let body_ = runPut $ do
            put16be 4
            put32be r
            put32be ssover_
            put32be app_id_
            put32be 0
            put64be uin_
            put32be $ fromIntegral time
            put32be 0 -- dummy ip
            put8 1
            putbs md5pass
            putbs tgtgt_
            put32be 0
            put8 1
            putbs guid_
            put32be sub_id_
            put32be 1
            lvu8 $ show uin_
            put16be 0
    let key_ = md5Lazy $ B.concat [md5pass, B.pack [0,0,0,0], runPut $ put32be $ fromIntegral uin_]
    enc <- qqteaEncrypt (tea16KeyFromBytes key_) body_
    packTLV_ 0x106 $ do
        putbs enc

t107 :: ContextIOT m => m B.ByteString
t107 = do
    packTLV_ 0x107 $ do
        put16be 0
        put8 0
        put16be 0
        put8 1

t108 :: ContextIOT m => m B.ByteString
t108 = do
    ksid_ <- use $ signature . ksid
    ksid__ <- if B.null ksid_
            then do
                imei_ <- use $ device . imei
                name_ <- use $ client_app . name
                pure $ utf8ToBytes $ printf "|%s|%s" imei_ name_
            else pure ksid_
    packTLV_ 0x108 $ do
        putbs ksid__

t109 :: ContextIOT m => m B.ByteString
t109 = do
    imei_ <- use $ device . imei
    packTLV_ 0x109 $ do
        putbs . B.fromStrict $ md5OfU8 imei_

t10A :: ContextIOT m => m B.ByteString
t10A = do
    tgt_ <- use $ signature . tgt
    packTLV_ 0x10A $ do
        putbs tgt_

t116 :: ContextIOT m => m B.ByteString
t116 = do
    bitmap_ <- use $ client_app . bitmap
    sub_sig_map_ <- use $ client_app . sub_sig_map
    packTLV_ 0x116 $ do
        put8 0
        put32be bitmap_
        put32be sub_sig_map_
        put8 1
        put32be 1600000226

t124 :: ContextIOT m => m B.ByteString
t124 = do
    os_type_ <- use $ device . os_type
    release_ <- use $ device . os_version . release
    sim_ <- use $ device . sim
    apn_ <- use $ device . apn
    packTLV_ 0x124 $ do
        lvu8 $ take 16 os_type_
        lvu8 $ take 16 release_
        put16be 2
        lvu8 $ take 16 sim_
        put16be 0
        lvu8 $ take 16 apn_

t128 :: ContextIOT m => m B.ByteString
t128 = do
    model_ <- use $ device . model
    guid_ <- guidBytes <$> use (device . guid)
    brand_ <- use $ device . brand
    packTLV_ 0x128 $ do
        put16be 0
        put8 0
        put8 1
        put8 0
        put32be 16777216
        lvu8 $ take 32 model_
        lvbs $ B.take 16 guid_
        lvu8 $ take 16 brand_

t141 :: ContextIOT m => m B.ByteString
t141 = do
    sim_ <- use $ device . sim
    apn_ <- use $ device . apn
    packTLV_ 0x141 $ do
        put16be 1
        lvu8 sim_
        put16be 2
        lvu8 apn_

t142 :: ContextIOT m => m B.ByteString
t142 = do
    id_ <- use $ client_app . id
    packTLV_ 0x142 $ do
        put16be 0
        lvu8 $ take 32 id_

t143 :: ContextIOT m => m B.ByteString
t143 = do
    d2_ <- use $ signature . d2
    packTLV_ 0x143 $ do
        putbs d2_

t144 :: ContextIOT m => m B.ByteString
t144 = do
    tgtgt_ <- use $ signature . tgtgt
    bs <- B.concat <$> sequence [t109, t52D, t124, t128, t16E]
    let s = runPut $ do
            put16be 5
            putbs bs
    vs <- qqteaEncrypt (tea16KeyFromBytes tgtgt_) s
    packTLV_ 0x144 $ do
        putbs vs

t145 :: ContextIOT m => m B.ByteString
t145 = do
    guid_ <- guidBytes <$> use (device . guid)
    packTLV_ 0x145 $ do
        putbs guid_

t147 :: ContextIOT m => m B.ByteString
t147 = do
    app_id_ <- use $ client_app . app_id
    version_ <- use $ client_app . version
    sign_ <- use $ client_app . sign
    packTLV_ 0x147 $ do
        put32be app_id_
        lvu8 version_
        lvbs sign_

t154 :: ContextIOT m => m B.ByteString
t154 = do
    seqV <- use seq
    seq_ <- liftIO (readTVarIO seqV)
    packTLV_ 0x154 $ do
        put32be $ seq_ + 1

t16E :: ContextIOT m => m B.ByteString
t16E = do
    model_ <- use $ device . model
    packTLV_ 0x16E $ do
        pututf8 model_

t174 :: ContextIOT m => m B.ByteString
t174 = do
    t174_ <- use $ signature . Sig.t174
    packTLV_ 0x174 $ do
        putbs t174_

t177 :: ContextIOT m => m B.ByteString
t177 = do
    build_time_ <- use $ client_app . build_time
    sdk_ver_ <- use $ client_app . sdk_ver
    packTLV_ 0x177 $ do
        put8 1
        put32be build_time_
        lvu8 sdk_ver_

t17A :: ContextIOT m => m B.ByteString
t17A = do
    packTLV_ 0x17A $ do
        put32be 9

t17C :: ContextIOT m => String -> m B.ByteString
t17C code = do
    packTLV_ 0x17C $ do
        lvu8 code

t187 :: ContextIOT m => m B.ByteString
t187 = do
    mac_address_ <- use $ device . mac_address
    packTLV_ 0x187 $ do
        putbs $ B.fromStrict $ md5OfU8 mac_address_

t188 :: ContextIOT m => m B.ByteString
t188 = do
    android_id_ <- use $ device . android_id
    packTLV_ 0x188 $ do
        putbs $ B.fromStrict $ md5OfU8 android_id_

t191 :: ContextIOT m => m B.ByteString
t191 = do
    packTLV_ 0x191 $ do
        put8 0x82

t193 :: ContextIOT m => String -> m B.ByteString
t193 ticket = do
    packTLV_ 0x193 $ do
        pututf8 ticket

t194 :: ContextIOT m => m B.ByteString
t194 = do
    imsi_ <- use $ device . imsi
    packTLV_ 0x194 $ do
        putbs imsi_

t197 :: ContextIOT m => m B.ByteString
t197 = do
    packTLV_ 0x197 $ do
        lvbs $ B.pack [0]

t198 :: ContextIOT m => m B.ByteString
t198 = do
    packTLV_ 0x197 $ do
        lvbs $ B.pack [0]

t202 :: ContextIOT m => m B.ByteString
t202 = do
    wifi_bssid_ <- use $ device . wifi_bssid
    wifi_ssid_ <- use $ device . wifi_ssid
    packTLV_ 0x202 $ do
        lvu8 $ take 16 wifi_bssid_
        lvu8 $ take 32 wifi_ssid_

t400 :: ContextIOT m => m B.ByteString
t400 = do
    uin_ <- use uin
    guid_ <- guidBytes <$> use (device . guid)
    rs <- randBytes 16
    time <- fromIntegral <$> getEpochTime
    packTLV_ 0x400 $ do
        put16be 1
        put64be uin_
        putbs guid_
        putbs rs
        put32be 1
        put32be 16
        put32be time

t401 :: ContextIOT m => m B.ByteString
t401 = do
    rs <- randBytes 16
    packTLV_ 0x401 $ do
        putbs rs

t511 :: ContextIOT m => m B.ByteString
t511 = do
    let ds = [
            "aq.qq.com",
            "buluo.qq.com",
            "connect.qq.com",
            "docs.qq.com",
            "game.qq.com",
            "gamecenter.qq.com",
            -- "graph.qq.com",
            "haoma.qq.com",
            "id.qq.com",
            -- "imgcache.qq.com",
            "kg.qq.com",
            "mail.qq.com",
            "mma.qq.com",
            "office.qq.com",
            -- "om.qq.com",
            "openmobile.qq.com",
            "qqweb.qq.com",
            "qun.qq.com",
            "qzone.qq.com",
            "ti.qq.com",
            "v.qq.com",
            "vip.qq.com",
            "y.qq.com"
            ]
    packTLV_ 0x511 $ do
        put16be $ fromIntegral $ length ds
        forM_ ds $ \d -> do
            put8 1
            lvu8 d

t516 :: ContextIOT m => m B.ByteString
t516 = do
    packTLV_ 0x516 $ do
        put32be 0

t521 :: ContextIOT m => m B.ByteString
t521 = do
    packTLV_ 0x521 $ do
        put32be 0
        put16be 0

t525 :: ContextIOT m => m B.ByteString
t525 = do
    packTLV_ 0x525 $ do
        put16be 1
        put16be 0x536
        lvbs $ B.pack [1, 0]

t52D :: ContextIOT m => m B.ByteString
t52D = do
    d <- use device

    let ds = PL.encodeMessage_ [
            1 `PL.putLenUTF8` (d ^. bootloader),
            2 `PL.putLenUTF8` (d ^. proc_version),
            3 `PL.putLenUTF8` (d ^. os_version . codeName),
            4 `PL.putPVUInt32` (d ^. os_version . incremental),
            -- TODO: why oicq put word32 while proto file definition says it's string?
            -- By now I decided to follow oicq's implementation
            5 `PL.putLenUTF8` (d ^. fingerprint),
            6 `PL.putLenUTF8` (d ^. boot_id),
            7 `PL.putLenUTF8` (d ^. android_id),
            8 `PL.putLenUTF8` (d ^. base_band),
            9 `PL.putPVUInt32` (d ^. os_version . incremental)
            ]
    packTLV_ 0x52D $ do
        putbs ds

t544 :: ContextIOT m => Word32 -> Word32 -> m B.ByteString
t544 v subCmd = do
    uin_ <- use uin
    guid_ <- guidBytes <$> use (device . guid)
    sdk_ver_ <- use $ client_app . sdk_ver
    let salt = runPut $ do
            if v == 2 then do
                put32be 0
                lvbs guid_
                lvu8 sdk_ver_
                put32be subCmd
                put32be 0
            else do
                put64be uin_
                lvbs guid_
                lvu8 sdk_ver_
                put32be subCmd
    vs <- T544.sign salt
    packTLV_ 0x544 $ do
        putbs vs

t545 :: ContextIOT m => m B.ByteString
t545 = do
    qimei16_ <- use $ device . qimei16
    imei_ <- use $ device . imei
    let vs = (if null qimei16_ then imei_ else qimei16_)
    packTLV_ 0x545 $ do
        pututf8 vs

t547 :: ContextIOT m => m B.ByteString
t547 = do
    t547_ <- use $ signature . Sig.t547
    packTLV_ 0x547 $ do
        putbs t547_