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

t1 :: ContextIOT m => m B.ByteString
t1 = do
    ip <- use $ device . ip_address
    uin_ <- fromIntegral <$> use uin
    T.t1 uin_ ip

t8 :: ContextIOT m => m B.ByteString
t8 = T.t8 2052


t16 :: ContextIOT m => m B.ByteString
t16 = do
    app_id_ <- use $ client_app . app_id
    sub_id_ <- use $ client_app . sub_id
    guid_ <- guidBytes <$> use (device . guid)
    id_ <- use $ client_app . id
    ver_ <- use $ client_app . ver
    sign_ <- use $ client_app . sign
    T.t16 7 app_id_ sub_id_ guid_ id_ ver_ sign_

t18 :: ContextIOT m => m B.ByteString
t18 = do
    app_id_ <- use $ client_app . app_id
    uin_ <- fromIntegral <$> use uin
    T.t18 app_id_ uin_

t1B :: ContextIOT m => m B.ByteString
t1B = T.t1b 0 0 3 4 72 2 2

t1D :: ContextIOT m => m B.ByteString
t1D = T.t1d 184024956

t1F :: ContextIOT m => m B.ByteString
t1F = T.t1f "android" "7.1.2" "China Mobile GSM" "wifi" 2

t33 :: ContextIOT m => m B.ByteString
t33 = do
    guid_ <- guidBytes <$> use (device . guid)
    T.t33 guid_

t35 :: ContextIOT m => m B.ByteString
t35 = T.t35 8

t100 :: ContextIOT m => m B.ByteString
t100 = do
    ssover_ <- use $ client_app . ssover
    app_id_ <- use $ client_app . app_id
    sub_id_ <- use $ client_app . sub_id
    main_sig_map_ <- use $ client_app . main_sig_map
    T.t100 ssover_ app_id_ sub_id_ main_sig_map_

t104 :: ContextIOT m => m B.ByteString
t104 = do
    t104_ <- use $ signature . Sig.t104
    T.t104 t104_

t106 :: ContextIOT m => B.ByteString -> m B.ByteString
t106 md5pass = do
    uin_ <- fromIntegral <$> use uin
    app_id_ <- use $ client_app . app_id
    sub_id_ <- use $ client_app . sub_id
    ssover_ <- use $ client_app . ssover
    guid_ <- guidBytes <$> use (device . guid)
    tgtgt_ <- use $ signature . tgtgt
    T.t106 uin_ app_id_ sub_id_ ssover_ md5pass guid_ tgtgt_

t107 :: ContextIOT m => m B.ByteString
t107 = do
    T.t107 0

t108 :: ContextIOT m => m B.ByteString
t108 = do
    ksid_ <- use $ signature . ksid
    ksid__ <- if B.null ksid_
            then do
                imei_ <- use $ device . imei
                name_ <- use $ client_app . name
                pure $ utf8ToBytes $ printf "|%s|%s" imei_ name_
            else pure ksid_
    T.t108 ksid__

t109 :: ContextIOT m => m B.ByteString
t109 = do
    imei_ <- use $ device . imei
    T.t109 imei_

t10A :: ContextIOT m => m B.ByteString
t10A = do
    tgt_ <- use $ signature . tgt
    T.t10a tgt_

t116 :: ContextIOT m => m B.ByteString
t116 = do
    bitmap_ <- use $ client_app . bitmap
    sub_sig_map_ <- use $ client_app . sub_sig_map
    T.t116 bitmap_ sub_sig_map_

t124 :: ContextIOT m => m B.ByteString
t124 = do
    os_type_ <- use $ device . os_type
    version_ <- use $ client_app . version
    sim_ <- use $ device . sim
    apn_ <- use $ device . apn
    T.t124 os_type_ version_ sim_ apn_

t128 :: ContextIOT m => m B.ByteString
t128 = do
    model_ <- use $ device . model
    guid_ <- guidBytes <$> use (device . guid)
    brand_ <- use $ device . brand
    T.t128 False True False 16777216 model_ guid_ brand_

t141 :: ContextIOT m => m B.ByteString
t141 = do
    sim_ <- use $ device . sim
    apn_ <- use $ device . apn
    T.t141 sim_ apn_

t142 :: ContextIOT m => m B.ByteString
t142 = do
    id_ <- use $ client_app . id
    T.t142 id_

t143 :: ContextIOT m => m B.ByteString
t143 = do
    d2_ <- use $ signature . d2
    T.t143 d2_

t144 :: ContextIOT m => m B.ByteString
t144 = do
    tgtgt_ <- use $ signature . tgtgt
    bs <- B.concat <$> sequence [t109, t52D, t124, t128, t16E]
    let s = runPut $ do
            put16be 5
            putbs bs
    vs <- qqteaEncrypt (tea16KeyFromBytes tgtgt_) s
    pure $ runPut $ do -- TODO: rewrite
        put16be 0x144
        put16be $ fromIntegral $ B.length vs
        putbs vs

t145 :: ContextIOT m => m B.ByteString
t145 = do
    guid_ <- guidBytes <$> use (device . guid)
    T.t145 guid_

t147 :: ContextIOT m => m B.ByteString
t147 = do
    app_id_ <- use $ client_app . app_id
    version_ <- use $ client_app . version
    sign_ <- use $ client_app . sign
    T.t147 app_id_ version_ sign_

t154 :: ContextIOT m => m B.ByteString
t154 = do
    seqV <- use seq
    seq_ <- liftIO (readTVarIO seqV)
    T.t154 (seq_ + 1)
            
t16E :: ContextIOT m => m B.ByteString
t16E = do
    model_ <- use $ device . model
    T.t16e (utf8ToBytes model_)

t174 :: ContextIOT m => m B.ByteString
t174 = do
    t174_ <- use $ signature . Sig.t174
    T.t174 t174_

t177 :: ContextIOT m => m B.ByteString
t177 = do
    build_time_ <- use $ client_app . build_time
    sdk_ver_ <- use $ client_app . sdk_ver
    T.t177 build_time_ sdk_ver_

t17A :: ContextIOT m => m B.ByteString
t17A = do
    T.t17a 9

t17C :: ContextIOT m => String -> m B.ByteString
t17C code = do
    T.t17c code

t187 :: ContextIOT m => m B.ByteString
t187 = do
    mac_address_ <- use $ device . mac_address
    T.t187 mac_address_

t188 :: ContextIOT m => m B.ByteString
t188 = do
    android_id_ <- use $ device . android_id
    T.t188 android_id_

t191 :: ContextIOT m => m B.ByteString
t191 = do
    T.t191 0x82

t193 :: ContextIOT m => String -> m B.ByteString
t193 ticket = do
    T.t193 ticket

t194 :: ContextIOT m => m B.ByteString
t194 = do
    imsi_ <- use $ device . imsi
    T.t194 imsi_

t197 :: ContextIOT m => m B.ByteString
t197 = do
    T.t197

t198 :: ContextIOT m => m B.ByteString
t198 = do
    T.t198

t202 :: ContextIOT m => m B.ByteString
t202 = do
    wifi_bssid_ <- use $ device . wifi_bssid
    wifi_ssid_ <- use $ device . wifi_ssid
    T.t202 wifi_bssid_ wifi_ssid_

t400 :: ContextIOT m => m B.ByteString
t400 = do
    uin_ <- use uin
    guid_ <- guidBytes <$> use (device . guid)
    T.t400 uin_ guid_

t401 :: ContextIOT m => m B.ByteString
t401 = do
    rs <- randBytes 16
    T.t401 rs

t511 :: ContextIOT m => m B.ByteString
t511 = do
    T.t511 [
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

t516 :: ContextIOT m => m B.ByteString
t516 = do
    T.t516

t521 :: ContextIOT m => m B.ByteString
t521 = do
    T.t521

t525 :: ContextIOT m => m B.ByteString
t525 = do
    T.t525 $ runPut $ do
        put16be 0x536
        put16be 2
        put8 0x1
        put8 0x0

t52D :: ContextIOT m => m B.ByteString
t52D = do
    d <- use device

    T.t52d $ PL.encodeMessage_ [
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

t544 :: ContextIOT m => Word32 -> Word32 -> m B.ByteString
t544 v subCmd = do
    uin_ <- use uin
    guid_ <- guidBytes <$> use (device . guid)
    sdk_ver_ <- use $ client_app . sdk_ver
    T.t544 v subCmd uin_ guid_ sdk_ver_

t545 :: ContextIOT m => m B.ByteString
t545 = do
    qimei16_ <- use $ device . qimei16
    imei_ <- use $ device . imei
    T.t545 (if null qimei16_ then imei_ else qimei16_)

t547 :: ContextIOT m => m B.ByteString
t547 = do
    t547_ <- use $ signature . Sig.t547
    T.t547 t547_