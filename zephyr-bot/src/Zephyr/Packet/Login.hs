{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}
module Zephyr.Packet.Login where
import Zephyr.Core.Context
import qualified Data.ByteString.Lazy as B
import Control.Lens
import Prelude hiding (seq)
import qualified Zephyr.Packet.TLV.Builders as T
import Zephyr.Packet.Build
import Zephyr.Core.Request
import Zephyr.Core.Transport
import Zephyr.Core.AppVersion
import Zephyr.Utils.Binary.Get
import Zephyr.Packet.Internal
import Data.Proxy (Proxy(Proxy))
import Data.Word
import Data.HashMap
import Control.Monad.IO.Class
import Zephyr.Core.Signature
import Zephyr.Utils.Random
import Zephyr.Utils.Common (utf8ToBytes, utf8FromBytes)
import Zephyr.Core.Device (guid, tgtgt_key)
import Zephyr.Utils.Codec
import Zephyr.Utils.GUID (guidBytes)
import Zephyr.Packet.Login.CalcPow
import Data.Maybe (isJust, fromMaybe)
import Zephyr.Utils.Binary
import Zephyr.Packet.TLV.Decoders
import GHC.Stack (HasCallStack)

buildLoginPacket :: ContextOPM B.ByteString
buildLoginPacket = do
    seq_ <- nextSeq
    uin_ <- use uin
    tr <- use transport
    codec_ <- use codec
    md5pass <- use password_md5
    let sub_id_ = tr ^. client_version . sub_id
    tlvs <- sequence [
        T.t18,
        T.t1,
        T.t106 md5pass,
        T.t116,
        T.t100 sub_id_,
        T.t107,
        T.t142,
        T.t144,
        T.t145,
        T.t147,
        T.t154 seq_,
        T.t141,
        T.t8,
        T.t511,
        T.t187,
        T.t188,
        T.t194,
        T.t191 0x82,
        T.t202,
        T.t177,
        T.t516,
        T.t521 0,
        T.t525,
        either error id <$> T.t544v2 "810_9" 9,
        T.t545
        ]
    let body = TLV 9 tlvs
    b2 <- buildOicqRequestPacket codec_ uin_ 0x810 body
    let req = Request RT_Login ET_EmptyKey (fromIntegral seq_) uin_ "wtlogin.login" b2
    packRequest req

buildTicketSubmitPacket :: String -> ContextOPM B.ByteString
buildTicketSubmitPacket ticket = do
    seq_ <- nextSeq
    uin_ <- use uin
    codec_ <- use codec
    t547_ <- use (transport . signature . t547)
    tlvs <- sequence [
            T.t193 ticket,
            T.t8,
            T.t104,
            T.t116,
            if B.null t547_ then pure "" else pure $ runPut $ do
                put16be 0x547
                put16be $ fromIntegral $ B.length t547_
                putbs t547_,
            either error id <$> T.t544 "810_2" 2
        ]
    let body = TLV 2 tlvs
    b2 <- buildOicqRequestPacket codec_ uin_ 0x810 body
    let req = Request RT_Login ET_EmptyKey (fromIntegral seq_) uin_ "wtlogin.login" b2
    packRequest req


-- syncTimeDiffPacket :: ContextIOT m => m B.ByteString
-- syncTimeDiffPacket = do
--     buildLoginPacket Client_CorrectTime 0 $ runPut $ put32be 0

data LoginError =
    UnknownLoginError |
    NeedCaptcha |
    OtherLoginError |
    UnsafeDeviceError |
    SMSNeededError |
    TooManySMSRequestError |
    SMSOrVerifyNeededError |
    SliderNeededError |
    NoResponse
    deriving (Eq, Show)

data LoginResponse = LoginResponse {
    _success :: Bool,
    _code :: Word8,
    _login_error :: LoginError,

    _captcha_image :: B.ByteString,
    _captcha_sign :: B.ByteString,

    _verify_url :: String,
    _sms_phone :: String,

    _error_message :: String
} deriving (Eq, Show)
$(makeLenses ''LoginResponse)

defaultResponse :: LoginResponse
defaultResponse = LoginResponse {
    _success = False,
    _code = 0,
    _login_error = UnknownLoginError,

    _captcha_image = "",
    _captcha_sign = "",

    _verify_url = "",
    _sms_phone = "",

    _error_message = ""
}

decodeLoginResponse :: HasCallStack => B.ByteString -> ContextOPM LoginResponse
decodeLoginResponse bs = do
    mp (es !> 0x402) $ \x -> do
        guid_ <- uses (transport . device . guid) guidBytes
        zoom (transport . signature) $ do
            rs <- utf8ToBytes <$> randString 16
            dpwd .= rs
            t402 .= x
            g .= md5Lazy (guid_ <> rs <> x)
    mp (es !> 0x546) $ \x -> do
        y <- calcPow x
        transport . signature . t547 .= y
    case t of
        0 -> do -- success
            mp (es !> 0x403) (transport . signature . rand_seed .=)
            tgtgt_key_ <- use (transport . device . tgtgt_key)
            decodeT119 (es ?> 0x119) tgtgt_key_
            pure $ defaultResponse { _success = True }
        2 -> do -- captcha
            transport . signature . t104 .= es ?> 0x104
            if has_ es 0x192 then do
                let tmp = es ! 0x192
                pure $ defaultResponse {
                    _success = False,
                    _code = t,
                    _verify_url = utf8FromBytes tmp,
                    _login_error = SliderNeededError
                }
            else if has_ es 0x165 then do
                let tmp = es ! 0x165
                let (sign_, data_) = flip runGet_ tmp $ do
                        slen <- get16be
                        _ <- get16be
                        sign__ <- getbs $ fromIntegral slen
                        data__ <- getRemaining
                        pure (sign__, data__)
                pure $ defaultResponse {
                    _success = False,
                    _code = t,
                    _login_error = NeedCaptcha,
                    _captcha_image = data_,
                    _captcha_sign = sign_
                }
            else
                pure $ defaultResponse {
                    _success = False,
                    _code = t,
                    _login_error = UnknownLoginError
                }
        40 -> do -- frozen
            pure $ defaultResponse {
                _success = False,
                _code = t,
                _error_message = "账号被冻结",
                _login_error = UnknownLoginError
            }
        162 -> do -- too many sms
            pure $ defaultResponse {
                _code = t,
                _login_error = TooManySMSRequestError
            }
        204 -> do -- device lock
            transport . signature . t104 .= es ?> 0x104
            transport . signature . rand_seed .= es ?> 0x403
            undefined
        t_ -> do
            ps <-
                if t_ == 160 || t_ == 239 then do
                    if has_ es 0x174 then do
                        let t174_ = es ! 0x174
                        transport . signature . t104 .= es ?> 0x104
                        transport . signature . t174 .= t174_
                        transport . signature . rand_seed .= es ?> 0x403
                        let phone_ = flip runGet_ (es ! 0x178) $ do
                                _ <- getlv
                                utf8FromBytes <$> getlv
                        if has_ es 0x204 then do
                            pure $ Just $ defaultResponse {
                                _success = False,
                                _code = t,
                                _login_error = SMSOrVerifyNeededError,
                                _verify_url = utf8FromBytes $ es ! 0x204,
                                _sms_phone = phone_,
                                _error_message = utf8FromBytes $ es ?> 0x17E
                            }
                        else do
                            pure $ Just $ defaultResponse {
                                _success = False,
                                _code = t,
                                _login_error = SMSNeededError,
                                _sms_phone = phone_,
                                _error_message = utf8FromBytes $ es ?> 0x17E
                            }
                    else if has_ es 0x17B then do -- second time
                        transport . signature . t104 .= es ?> 0x104
                        pure $ Just $ defaultResponse {
                            _success = False,
                            _code = t,
                            _login_error = SMSNeededError
                        }
                    else if has_ es 0x204 then do
                        pure $ Just $ defaultResponse {
                            _success = False,
                            _code = t,
                            _login_error = UnsafeDeviceError,
                            _verify_url = utf8FromBytes $ es ! 0x204,
                            _error_message = ""
                        }
                    else pure Nothing
            else pure Nothing
            case ps of
                Just x -> pure x
                Nothing -> do
                    if has_ es 0x149 then do
                        let r_ = flip runGet_ (es ! 0x149) $ do
                                _ <- get16be
                                _ <- getlv -- title
                                utf8FromBytes <$> getlv
                        pure $ defaultResponse {
                            _success = False,
                            _code = t,
                            _login_error = OtherLoginError,
                            _error_message = r_
                        }
                    else if has_ es 0x146 then do
                        let r_ = flip runGet_ (es ! 0x149) $ do
                                _ <- get32be
                                _ <- getlv -- title
                                utf8FromBytes <$> getlv
                        pure $ defaultResponse {
                            _success = False,
                            _code = t,
                            _login_error = OtherLoginError,
                            _error_message = r_
                        }
                    else pure $ defaultResponse {
                        _success = False,
                        _code = t,
                        _login_error = NoResponse
                    }
    where
        (?>) d k = fromMaybe mempty (Data.HashMap.lookup k d)
        (_, t, _, r) = flip runGet_ bs $ do
            (,,,) <$> get16be <*> get8 <*> get16be <*> getRemaining
        es = runGet_ (getTLVEntries (Proxy @Word16)) r :: Map Word16 B.ByteString
        (!>) = flip Data.HashMap.lookup
        has_ = flip Data.HashMap.member
        mp :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
        mp = flip (maybe (pure ()))