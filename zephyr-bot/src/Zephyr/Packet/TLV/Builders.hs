{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant id" #-}
module Zephyr.Packet.TLV.Builders where

import Zephyr.Core.QQContext
import qualified Data.ByteString.Lazy as B
import Control.Lens
import Zephyr.Core.Device
import Zephyr.Binary
import Zephyr.Core.AppVersion
import Zephyr.Core.Signature as Sig
import Prelude hiding (id, seq)
import Zephyr.Utils.GUID (guidBytes)
import Data.Word
import Zephyr.Packet.Internal
import Zephyr.Core.Transport
import qualified Zephyr.Packet.TLV.Prim as Prim
import Zephyr.Packet.Wrapper (wenergy)
import Zephyr.ProtoLite as PL
import Zephyr.PB.Data as PBData
import Zephyr.Binary.OP
import Zephyr.Binary.Put

packTLV :: Word16 -> Put -> B.ByteString
packTLV t p = runPut $ do
    put16be t
    lv p

packTLV_ :: (Monad m) => Word16 -> Put -> m B.ByteString
packTLV_ t p = pure $ packTLV t p

t1 :: ContextRM B.ByteString
t1 = do
    ip <- view $ transport . device . ip_address
    uin_ <- fromIntegral <$> view uin
    Prim.t1_ uin_ ip

t2 :: String -> B.ByteString -> ContextRM B.ByteString
t2 rst d = do
    pure $ Prim.t2_ rst d

t8 :: ContextRM B.ByteString
t8 = do
    pure $ Prim.t8_ 2052


t16 :: ContextRM B.ByteString
t16 = do
    ssover_ <- view $ transport . app_version . ssover
    sub_id_ <- view $ transport . app_version . sub_id
    guid_ <- guidBytes <$> view (transport . device . guid)
    apk_id_ <- utf8ToBytes <$> view (transport . app_version . apk_id)
    ver_ <- utf8ToBytes <$> view (transport . app_version . sort_version)
    sign_ <- view $ transport . app_version . sign
    pure $ Prim.t16_ ssover_ 16 sub_id_ guid_ apk_id_ ver_ sign_

t18 :: ContextRM B.ByteString
t18 = do
    uin_ <- fromIntegral <$> view uin
    pure $ Prim.t18_ 16 uin_

t1B :: Word32 -> Word32 -> Word32 -> ContextRM B.ByteString
t1B size_ margin_ ecLevel = do
    pure $ Prim.t1B_ 0 0 size_ margin_ 72 ecLevel 2

t1D :: ContextRM B.ByteString
t1D = do
    misc_bitmap_ <- view $ transport . app_version . misc_bitmap
    pure $ Prim.t1D_ misc_bitmap_

t1F :: ContextRM B.ByteString
t1F = do
    os_type_ <- utf8ToBytes <$> view (transport . device . os_type)
    apn_ <- utf8ToBytes <$> view (transport . device . apn)
    pure $ Prim.t1F_ False os_type_
        "7.1.2" "China Mobile GSM" apn_ 2

t33 :: ContextRM B.ByteString
t33 = do
    guid_ <- guidBytes <$> view (transport . device . guid)
    pure $ Prim.t33_ guid_

t35 :: ContextRM B.ByteString
t35 = do
    pure $ Prim.t35_ 8

t100 :: Word32 -> ContextRM B.ByteString
t100 protocol_ = do
    ssover_ <- view $ transport . app_version . ssover
    main_sig_map_ <- view $ transport . app_version . main_sig_map
    pure $ Prim.t100_ ssover_ protocol_ main_sig_map_

t104 :: ContextRM B.ByteString
t104 = do
    t104_ <- view $ transport . signature . Sig.t104
    pure $ Prim.t104_ t104_

t106 :: B.ByteString -> ContextRM B.ByteString
t106 md5pass = do
    uin_ <- fromIntegral <$> view uin
    sub_id_ <- view $ transport . app_version . sub_id
    ssover_ <- view $ transport . app_version . ssover
    guid_ <- guidBytes <$> view (transport . device . guid)
    tgtgt_ <- view $ transport . device . tgtgt_key
    Prim.t106_ uin_ 0 sub_id_ ssover_ md5pass True guid_ tgtgt_ 0

t107 :: ContextRM B.ByteString
t107 = do
    pure $ Prim.t107_ 0

t108 :: ContextRM B.ByteString
t108 = do
    ksid_ <- view $ transport . signature . ksid
    pure $ Prim.t108_ ksid_

-- t109 is absent because it's not directly used

t10A :: ContextRM B.ByteString
t10A = do
    tgt_ <- view $ transport . signature . tgt
    pure $ Prim.t10A_ tgt_

t116 :: ContextRM B.ByteString
t116 = do
    bitmap_ <- view $ transport . app_version . misc_bitmap
    sub_sig_map_ <- view $ transport . app_version . sub_sig_map
    pure $ Prim.t116_ bitmap_ sub_sig_map_

t124 :: ContextRM B.ByteString
t124 = do
    os_type_ <- view $ transport . device . os_type
    release_ <- view $ transport . device . os_version . release
    sim_ <- view $ transport . device . sim
    apn_ <- view $ transport . device . apn
    packTLV_ 0x124 $ do
        lvu8 $ take 16 os_type_
        lvu8 $ take 16 release_
        put16be 2
        lvu8 $ take 16 sim_
        put16be 0
        lvu8 $ take 16 apn_

t128 :: ContextRM B.ByteString
t128 = do
    model_ <- view $ transport . device . model
    guid_ <- guidBytes <$> view (transport . device . guid)
    brand_ <- view $ transport . device . brand
    packTLV_ 0x128 $ do
        put16be 0
        put8 0
        put8 1
        put8 0
        put32be 16777216
        lvu8 $ take 32 model_
        lvbs $ B.take 16 guid_
        lvu8 $ take 16 brand_

t141 :: ContextRM B.ByteString
t141 = do
    sim_ <- view $ transport . device . sim
    apn_ <- view $ transport . device . apn
    packTLV_ 0x141 $ do
        put16be 1
        lvu8 sim_
        put16be 2
        lvu8 apn_

t142 :: ContextRM B.ByteString
t142 = do
    id_ <- view $ transport . app_version . apk_id
    packTLV_ 0x142 $ do
        put16be 0
        lvu8 $ take 32 id_

t143 :: ContextRM B.ByteString
t143 = do
    d2_ <- view $ transport . signature . d2
    packTLV_ 0x143 $ do
        putbs d2_

guidFlag :: Word32
guidFlag = 0x1000000

t144 :: ContextRM B.ByteString
t144 = do
    imei_ <- views (transport . device . imei) utf8ToBytes
    os_type_ <- views (transport . device . os_type) utf8ToBytes
    release_ <- views (transport . device . os_version . release) utf8ToBytes
    sim_ <- views (transport . device . sim) utf8ToBytes
    apn_ <- views (transport . device . apn) utf8ToBytes
    model_ <- views (transport . device . model) utf8ToBytes
    guid_ <- guidBytes <$> view (transport . device . guid)
    brand_ <- views (transport . device . brand) utf8ToBytes
    tgtgt_key_ <- view $ transport . device . tgtgt_key

    d <- view $ transport . device
    let pb = PL.encode $ PBData.DeviceInfo
            (optJust  $ d ^. bootloader)
            (optJust  $ d ^. proc_version)
            (optJust  $ d ^. os_version . codeName)
            (optJustV $ d ^. os_version . incremental)
            (optJust  $ d ^. fingerprint)
            (optJust  $ d ^. boot_id)
            (optJust  $ d ^. android_id)
            (optJust  $ d ^. base_band)
            (optJustV $ d ^. os_version . incremental)
    Prim.t144_ imei_ pb os_type_ release_ sim_ apn_ False True False guidFlag model_ guid_ brand_ tgtgt_key_

t145 :: ContextRM B.ByteString
t145 = do
    guid_ <- guidBytes <$> view (transport . device . guid)
    pure $ Prim.t145_ guid_

t147 :: ContextRM B.ByteString
t147 = do
    version_ <- utf8ToBytes <$> view (transport . app_version . sort_version)
    sign_ <- view $ transport . app_version . sign
    pure $ Prim.t147_ 16 version_ sign_

t154 :: Word16 -> ContextRM B.ByteString
t154 seq_ = do
    pure $ Prim.t154_ seq_

t174 :: ContextRM B.ByteString
t174 = do
    t174_ <- view $ transport . signature . Sig.t174
    pure $ Prim.t174_ t174_

t177 :: ContextRM B.ByteString
t177 = do
    build_time_ <- view $ transport . app_version . build_time
    sdk_ver_ <- view $ transport . app_version . sdk_ver
    pure $ Prim.t177_ build_time_ sdk_ver_

t17A :: ContextRM B.ByteString
t17A = do
    pure $ Prim.t17A_ 9

t17C :: String -> ContextRM B.ByteString
t17C code = do
    pure $ Prim.t17C_ code

t187 :: ContextRM B.ByteString
t187 = do
    mac_address_ <- views (transport . device . mac_address) utf8ToBytes
    pure $ Prim.t187_ mac_address_

t188 :: ContextRM B.ByteString
t188 = do
    android_id_ <- views (transport . device . android_id) utf8ToBytes
    pure $ Prim.t188_ android_id_

t191 :: Word8 -> ContextRM B.ByteString
t191 k = do
    pure $ Prim.t191_ k

t193 :: String -> ContextRM B.ByteString
t193 ticket = do
    pure $ Prim.t193_ ticket

t194 :: ContextRM B.ByteString
t194 = do
    imsi_ <- view $ transport . device . imsi
    pure $ Prim.t194_ imsi_

t197 :: ContextRM B.ByteString
t197 = do
    pure Prim.t197_

t198 :: ContextRM B.ByteString
t198 = do
    pure Prim.t198_

t202 :: ContextRM B.ByteString
t202 = do
    wifi_bssid_ <- views (transport . device . wifi_bssid) utf8ToBytes
    wifi_ssid_ <- views (transport . device . wifi_ssid) utf8ToBytes
    pure $ Prim.t202_ wifi_bssid_ wifi_ssid_

t400 :: ContextRM B.ByteString
t400 = do
    uin_ <- view uin
    guid_ <- guidBytes <$> view (transport . device . guid)
    g_ <- view $ transport . signature . g
    dpwd_ <- view $ transport . signature . dpwd
    rand_seed_ <- view $ transport . signature . rand_seed
    Prim.t400_ g_ uin_ guid_ dpwd_ 1 16 rand_seed_

t401 :: ContextRM B.ByteString
t401 = do
    g_ <- view $ transport . signature . g
    pure $ Prim.t401_ g_

t511 :: ContextRM B.ByteString
t511 = do
    let ds = [
            "tenpay.com", "openmobile.qq.com", "docs.qq.com", "connect.qq.com",
            "qzone.qq.com", "vip.qq.com", "gamecenter.qq.com", "qun.qq.com", "game.qq.com",
            "qqweb.qq.com", "office.qq.com", "ti.qq.com", "mail.qq.com", "mma.qq.com"
            ]
    pure $ Prim.t511_ ds

t516 :: ContextRM B.ByteString
t516 = do
    pure Prim.t516_

t521 :: Word32 -> ContextRM B.ByteString
t521 i = do
    pure $ Prim.t521_ i

t525 :: ContextRM B.ByteString
t525 = do
    let t536_ = Prim.t536_ $ B.pack [1, 0]
    pure $ Prim.t525_ t536_

t544 :: String -> Word32 -> ContextRM (Either String B.ByteString)
t544 moduleId subCmd = do
    uin_ <- view uin
    guid_ <- guidBytes <$> view (transport . device . guid)
    sdk_ver_ <- view $ transport . app_version . sdk_ver
    version_ <- view $ transport . app_version . sort_version
    signer <- wenergy
    Prim.t544_ uin_ moduleId subCmd sdk_ver_ guid_ version_ signer

t544v2 :: String -> Word32 -> ContextRM (Either String B.ByteString)
t544v2 moduleId subCmd = do
    uin_ <- view uin
    guid_ <- guidBytes <$> view (transport . device . guid)
    sdk_ver_ <- view $ transport . app_version . sdk_ver
    version_ <- view $ transport . app_version . sort_version
    signer <- wenergy
    Prim.t544V2_ uin_ moduleId subCmd sdk_ver_ guid_ version_ signer

t545 :: ContextRM B.ByteString
t545 = do
    qimei16_ <- view $ transport . device . qimei16
    imei_ <- view $ transport . device . imei
    let vs = (if null qimei16_ then imei_ else qimei16_)
    pure $ Prim.t545_ $ utf8ToBytes vs
