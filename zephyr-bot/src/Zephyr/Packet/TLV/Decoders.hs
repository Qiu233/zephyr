{-# LANGUAGE TypeApplications #-}
module Zephyr.Packet.TLV.Decoders (
    decodeT130,
    decodeT119,

) where
import qualified Data.ByteString.Lazy as B
import Zephyr.Core.Context
import qualified Zephyr.Encrypt.QQTea as QQTea
import Zephyr.Packet.Internal
import Data.Proxy (Proxy(Proxy))
import Data.Word
import Zephyr.Utils.Binary.Get
import Data.HashMap
import Zephyr.Core.Transport
import Zephyr.Core.Signature
import Control.Lens
import Zephyr.Utils.Common
import Control.Monad
import Zephyr.Core.Codec (wt_session_ticket_key)
import Zephyr.Utils.Time
import Zephyr.Utils.Binary
import Zephyr.Utils.Codec
import Zephyr.Core.Device
import Data.Maybe
import GHC.Stack (HasCallStack)
import Control.Monad.IO.Class

decodeT130 :: B.ByteString -> ContextOPM ()
decodeT130 bs = do
    liftIO $ putStrLn $ "decodeT130: " ++ encodeHex bs
    pure () -- ?

decodeT113 :: B.ByteString -> ContextOPM ()
decodeT113 bs = do
    liftIO $ putStrLn $ "decodeT113: " ++ encodeHex bs
    pure () -- ?

readT11A :: B.ByteString -> (String, Word16, Word16)
readT11A bs = flip runGet bs $ do
    _ <- get16be
    age_ <- fromIntegral <$> get8
    gender_ <- fromIntegral <$> get8
    nickLen <- get8
    nick_ <- utf8FromBytes <$> getbs (fromIntegral nickLen)
    pure (nick_, age_, gender_)

readT512 :: B.ByteString -> (Map String B.ByteString, Map String B.ByteString)
readT512 bs = do
    let ls = flip runGet bs $ do
            len <- fromIntegral <$> get16be
            replicateM len $ do
                domain_ <- utf8FromBytes <$> getlv
                psKey <- getlv
                pt4Token <- getlv
                pure (domain_, psKey, pt4Token)
    let a = fromList $ fmap (\(d, k, _) -> (d, k)) ls
    let b = fromList $ fmap (\(d, _, t) -> (d, t)) ls
    (a, b)

decodeT119 :: HasCallStack => B.ByteString -> B.ByteString -> ContextOPM ()
decodeT119 data_ ek_ = do
    let d = B.drop 2 $ QQTea.qqteaDecrypt ek_ data_
    let es = flip runGet d $ getTLVEntries (Proxy @Word16) :: Map Word16 B.ByteString
    mp (es !> 0x130) decodeT130
    mp (es !> 0x113) decodeT113
    mp (es !> 0x108) (transport . signature . ksid .=)
    let (nick_, age_, gender_) = maybe ("", 0, 0) readT11A (es !> 0x11A)
    let (psKeyMap, pt4TokenMap) = maybe (empty, empty) readT512 (es !> 0x512)
    mp (es !> 0x134) (codec . wt_session_ticket_key .=)
    zoom (transport . signature) $ do
        let st dst src = dst .= (es ?> src)
        login_bitmap .= 0
        mp (es !> 0x16A) (srm_token .=)
        mp (es !> 0x133) (t133 .=)
        mp (es !> 0x106) (encryptedA1 .=)
        st tgt 0x10A
        st tgtkey 0x10D
        st user_st_key 0x10E
        st user_st_web_sig 0x103
        st skey 0x120
        now <- getEpochTime
        skey_expired_time .= fromIntegral (now + 21600)
        st d2 0x143
        st d2key 0x305
        st device_token 0x322

        ps_key_map .= psKeyMap
        pt4_token_map .= pt4TokenMap
    md5pass <- use password_md5
    when (B.length md5pass == 16) $ do
        uin_ <- use uin
        let key = md5Lazy $ runPut $ do
                putbs md5pass
                put32be 0
                put32be $ fromIntegral uin_
        enc_ <- use $ transport . signature . encryptedA1
        let dec_ = QQTea.qqteaDecrypt key enc_
        when (B.length dec_ > (51+16)) $ do
            transport . device . tgtgt_key .= B.take 16 (B.drop 51 dec_)
    qqprofile . nickname .= nick_
    qqprofile . age .= age_
    qqprofile . gender .= gender_
    where
        (?>) d k = fromMaybe mempty (Data.HashMap.lookup k d)
        (!>) = flip Data.HashMap.lookup
        mp :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
        mp = flip (maybe (pure ()))