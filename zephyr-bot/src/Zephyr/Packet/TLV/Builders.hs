{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant id" #-}
module Zephyr.Packet.TLV.Builders where

import Zephyr.Core.Context
import qualified Data.ByteString.Lazy as B
import Control.Lens hiding (Context)
import Zephyr.Core.Device
import Zephyr.Utils.Binary
import Control.Monad.IO.Class
import Zephyr.Core.AppVersion
import Zephyr.Core.Signature as Sig
import Prelude hiding (id, seq)
import Zephyr.Utils.GUID (guidBytes)
import Zephyr.Utils.Common
import Text.Printf (printf)
import qualified Zephyr.Utils.ProtoLite as PL
import Zephyr.Encrypt.QQTea (qqteaEncrypt)
import Control.Concurrent.STM (readTVarIO)
import Zephyr.Utils.Random
import Data.Word
import Zephyr.Utils.Time (getEpochTime)
import Zephyr.Utils.Codec (md5Lazy, md5OfU8)
import Control.Monad
import qualified Zephyr.Packet.TLV.T544 as T544
import System.Random (randomIO)
import Zephyr.Packet.Internal
import Zephyr.Core.Transport
import qualified Zephyr.Packet.TLV.Prim as Prim

packTLV :: Word16 -> Put -> B.ByteString
packTLV t p = runPut $ do
    put16be t
    lv p

packTLV_ :: (Monad m) => Word16 -> Put -> m B.ByteString
packTLV_ t p = pure $ packTLV t p

t1 :: ContextIOT m => m B.ByteString
t1 = do
    ip <- use $ transport . device . ip_address
    uin_ <- fromIntegral <$> use uin
    Prim.t1_ uin_ ip

t2 :: ContextIOT m => String -> B.ByteString -> m B.ByteString
t2 rst d = do
    pure $ Prim.t2_ rst d

t8 :: ContextIOT m => m B.ByteString
t8 = do
    pure $ Prim.t8_ 2052


t16 :: ContextIOT m => m B.ByteString
t16 = do
    ssover_ <- use $ transport . client_version . ssover
    sub_id_ <- use $ transport . client_version . sub_id
    guid_ <- guidBytes <$> use (transport . device . guid)
    apk_id_ <- utf8ToBytes <$> use (transport . client_version . apk_id)
    ver_ <- utf8ToBytes <$> use (transport . client_version . sort_version)
    sign_ <- use $ transport . client_version . sign
    pure $ Prim.t16_ ssover_ 16 sub_id_ guid_ apk_id_ ver_ sign_

t18 :: ContextIOT m => m B.ByteString
t18 = do
    uin_ <- fromIntegral <$> use uin
    pure $ Prim.t18_ 16 uin_

t1B :: ContextIOT m => Word32 -> Word32 -> Word32 -> m B.ByteString
t1B size_ margin_ ecLevel = do
    pure $ Prim.t1B_ 0 0 size_ margin_ 72 ecLevel 2

t1D :: ContextIOT m => m B.ByteString
t1D = do
    misc_bitmap_ <- use $ transport . client_version . misc_bitmap
    pure $ Prim.t1D_ misc_bitmap_

t1F :: ContextIOT m => m B.ByteString
t1F = do
    os_type_ <- utf8ToBytes <$> use (transport . device . os_type)
    apn_ <- utf8ToBytes <$> use (transport . device . apn)
    pure $ Prim.t1F_ False os_type_
        "7.1.2" "China Mobile GSM" apn_ 2

t33 :: ContextIOT m => m B.ByteString
t33 = do
    guid_ <- guidBytes <$> use (transport . device . guid)
    pure $ Prim.t33_ guid_

t35 :: ContextIOT m => m B.ByteString
t35 = do
    pure $ Prim.t35_ 8

t100 :: ContextIOT m => Word32 -> m B.ByteString
t100 protocol_ = do
    ssover_ <- use $ transport . client_version . ssover
    main_sig_map_ <- use $ transport . client_version . main_sig_map
    pure $ Prim.t100_ ssover_ protocol_ main_sig_map_

t104 :: ContextIOT m => m B.ByteString
t104 = do
    t104_ <- use $ transport . signature . Sig.t104
    pure $ Prim.t104_ t104_

t106 :: ContextIOT m => B.ByteString -> m B.ByteString
t106 md5pass = do
    uin_ <- fromIntegral <$> use uin
    sub_id_ <- use $ transport . client_version . sub_id
    ssover_ <- use $ transport . client_version . ssover
    guid_ <- guidBytes <$> use (transport . device . guid)
    tgtgt_ <- use $ transport . device . tgtgt_key
    Prim.t106_ uin_ 0 sub_id_ ssover_ md5pass True guid_ tgtgt_ 0

t107 :: ContextIOT m => m B.ByteString
t107 = do
    pure $ Prim.t107_ 0

t108 :: ContextIOT m => m B.ByteString
t108 = do
    ksid_ <- use $ transport . signature . ksid
    pure $ Prim.t108_ ksid_

-- t109 is absent because it's not directly used

t10A :: ContextIOT m => m B.ByteString
t10A = do
    tgt_ <- use $ transport . signature . tgt
    pure $ Prim.t10A_ tgt_

t116 :: ContextIOT m => m B.ByteString
t116 = do
    bitmap_ <- use $ transport . client_version . misc_bitmap
    sub_sig_map_ <- use $ transport . client_version . sub_sig_map
    pure $ Prim.t116_ bitmap_ sub_sig_map_

t124 :: ContextIOT m => m B.ByteString
t124 = do
    os_type_ <- use $ transport . device . os_type
    release_ <- use $ transport . device . os_version . release
    sim_ <- use $ transport . device . sim
    apn_ <- use $ transport . device . apn
    packTLV_ 0x124 $ do
        lvu8 $ take 16 os_type_
        lvu8 $ take 16 release_
        put16be 2
        lvu8 $ take 16 sim_
        put16be 0
        lvu8 $ take 16 apn_

t128 :: ContextIOT m => m B.ByteString
t128 = do
    model_ <- use $ transport . device . model
    guid_ <- guidBytes <$> use (transport . device . guid)
    brand_ <- use $ transport . device . brand
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
    sim_ <- use $ transport . device . sim
    apn_ <- use $ transport . device . apn
    packTLV_ 0x141 $ do
        put16be 1
        lvu8 sim_
        put16be 2
        lvu8 apn_

t142 :: ContextIOT m => m B.ByteString
t142 = do
    id_ <- use $ transport . client_version . apk_id
    packTLV_ 0x142 $ do
        put16be 0
        lvu8 $ take 32 id_

t143 :: ContextIOT m => m B.ByteString
t143 = do
    d2_ <- use $ transport . signature . d2
    packTLV_ 0x143 $ do
        putbs d2_

guidFlag :: Word32
guidFlag = 0x1000000

t144 :: ContextIOT m => m B.ByteString
t144 = do
    imei_ <- uses (transport . device . imei) utf8ToBytes
    os_type_ <- uses (transport . device . os_type) utf8ToBytes
    release_ <- uses (transport . device . os_version . release) utf8ToBytes
    sim_ <- uses (transport . device . sim) utf8ToBytes
    apn_ <- uses (transport . device . apn) utf8ToBytes
    model_ <- uses (transport . device . model) utf8ToBytes
    guid_ <- guidBytes <$> use (transport . device . guid)
    brand_ <- uses (transport . device . brand) utf8ToBytes
    tgtgt_key_ <- use $ transport . device . tgtgt_key

    d <- use $ transport . device
    let pb = PL.encodeMessage_ [
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
    Prim.t144_ imei_ pb os_type_ release_ sim_ apn_ False True False guidFlag model_ guid_ brand_ tgtgt_key_

t145 :: ContextIOT m => m B.ByteString
t145 = do
    guid_ <- guidBytes <$> use (transport . device . guid)
    pure $ Prim.t145_ guid_

t147 :: ContextIOT m => m B.ByteString
t147 = do
    version_ <- utf8ToBytes <$> use (transport . client_version . sort_version)
    sign_ <- use $ transport . client_version . sign
    pure $ Prim.t147_ 16 version_ sign_

t154 :: ContextIOT m => Word16 -> m B.ByteString
t154 seq_ = do
    pure $ Prim.t154_ seq_

t174 :: ContextIOT m => m B.ByteString
t174 = do
    t174_ <- use $ transport . signature . Sig.t174
    pure $ Prim.t174_ t174_

t177 :: ContextIOT m => m B.ByteString
t177 = do
    build_time_ <- use $ transport . client_version . build_time
    sdk_ver_ <- use $ transport . client_version . sdk_ver
    pure $ Prim.t177_ build_time_ sdk_ver_

t17A :: ContextIOT m => m B.ByteString
t17A = do
    pure $ Prim.t17A_ 9

t17C :: ContextIOT m => String -> m B.ByteString
t17C code = do
    pure $ Prim.t17C_ code

t187 :: ContextIOT m => m B.ByteString
t187 = do
    mac_address_ <- uses (transport . device . mac_address) utf8ToBytes
    pure $ Prim.t187_ mac_address_

t188 :: ContextIOT m => m B.ByteString
t188 = do
    android_id_ <- uses (transport . device . android_id) utf8ToBytes
    pure $ Prim.t188_ android_id_

t191 :: ContextIOT m => Word8 -> m B.ByteString
t191 k = do
    pure $ Prim.t191_ k

t193 :: ContextIOT m => String -> m B.ByteString
t193 ticket = do
    pure $ Prim.t193_ ticket

t194 :: ContextIOT m => m B.ByteString
t194 = do
    imsi_ <- use $ transport . device . imsi
    pure $ Prim.t194_ imsi_

t197 :: ContextIOT m => m B.ByteString
t197 = do
    pure Prim.t197_

t198 :: ContextIOT m => m B.ByteString
t198 = do
    pure Prim.t198_

t202 :: ContextIOT m => m B.ByteString
t202 = do
    wifi_bssid_ <- uses (transport . device . wifi_bssid) utf8ToBytes
    wifi_ssid_ <- uses (transport . device . wifi_ssid) utf8ToBytes
    pure $ Prim.t202_ wifi_bssid_ wifi_ssid_

t400 :: ContextIOT m => m B.ByteString
t400 = do
    uin_ <- use uin
    guid_ <- guidBytes <$> use (transport . device . guid)
    g_ <- use $ transport . signature . g
    dpwd_ <- use $ transport . signature . dpwd
    rand_seed_ <- use $ transport . signature . rand_seed
    Prim.t400_ g_ uin_ guid_ dpwd_ 1 16 rand_seed_

t401 :: ContextIOT m => m B.ByteString
t401 = do
    g_ <- use $ transport . signature . g
    pure $ Prim.t401_ g_

t511 :: ContextIOT m => m B.ByteString
t511 = do
    let ds = [
            "tenpay.com", "openmobile.qq.com", "docs.qq.com", "connect.qq.com",
            "qzone.qq.com", "vip.qq.com", "gamecenter.qq.com", "qun.qq.com", "game.qq.com",
            "qqweb.qq.com", "office.qq.com", "ti.qq.com", "mail.qq.com", "mma.qq.com"
            ]
    pure $ Prim.t511_ ds

t516 :: ContextIOT m => m B.ByteString
t516 = do
    pure Prim.t516_

t521 :: ContextIOT m => Word32 -> m B.ByteString
t521 i = do
    pure $ Prim.t521_ i

t525 :: ContextIOT m => m B.ByteString
t525 = do
    let t536_ = Prim.t536_ $ B.pack [1, 0]
    pure $ Prim.t525_ t536_

-- t544 :: ContextIOT m => String -> Word32 -> Prim.Signer -> m B.ByteString
-- t544 moduleId subCmd signer = do
--     uin_ <- use uin
--     guid_ <- guidBytes <$> use (transport . device . guid)
--     sdk_ver_ <- use $ transport . client_version . sdk_ver
--     version_ <- use $ transport . client_version . sort_version
--     pure $ Prim.t544_ uin_ moduleId subCmd sdk_ver_ guid_ version_ signer

-- t544v2 :: ContextIOT m => String -> Word32 -> Prim.Signer -> m B.ByteString
-- t544v2 moduleId subCmd signer = do
--     uin_ <- use uin
--     guid_ <- guidBytes <$> use (transport . device . guid)
--     sdk_ver_ <- use $ transport . client_version . sdk_ver
--     version_ <- use $ transport . client_version . sort_version
--     pure $ Prim.t544V2_ uin_ moduleId subCmd sdk_ver_ guid_ version_ signer

t544 :: ContextIOT m => Word32 -> Word32 -> m B.ByteString
t544 v subCmd = do
    uin_ <- use uin
    guid_ <- guidBytes <$> use (transport . device . guid)
    sdk_ver_ <- use $ transport . client_version . sdk_ver
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
    qimei16_ <- use $ transport . device . qimei16
    imei_ <- use $ transport . device . imei
    let vs = (if null qimei16_ then imei_ else qimei16_)
    pure $ Prim.t545_ $ utf8ToBytes vs
