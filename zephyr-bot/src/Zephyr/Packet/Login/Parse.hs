{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Login.Parse where
import GHC.Stack
import qualified Data.ByteString.Lazy as B
import Zephyr.Core.Context
import Zephyr.Packet.Login.Types
import Data.Word
import Data.HashMap
import Zephyr.Core.Transport
import Zephyr.Core.Device
import Zephyr.Utils.GUID
import Control.Lens
import Zephyr.Utils.Common
import Zephyr.Utils.Random
import Zephyr.Core.Signature
import Zephyr.Utils.Codec
import Zephyr.Packet.Login.Pow
import Zephyr.Packet.TLV.Decoders
import Zephyr.Packet.Internal
import Zephyr.Utils.Binary
import Data.Maybe
import Data.Proxy




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
                let (sign_, data_) = flip runGet tmp $ do
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
                        let phone_ = flip runGet (es ! 0x178) $ do
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
                        let r_ = flip runGet (es ! 0x149) $ do
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
                        let r_ = flip runGet (es ! 0x149) $ do
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
        (_, t, _, r) = flip runGet bs $ do
            (,,,) <$> get16be <*> get8 <*> get16be <*> getRemaining
        es = runGet (getTLVEntries (Proxy @Word16)) r :: Map Word16 B.ByteString
        (!>) = flip Data.HashMap.lookup
        has_ = flip Data.HashMap.member
        mp :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
        mp = flip (maybe (pure ()))