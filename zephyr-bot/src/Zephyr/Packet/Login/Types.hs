{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Login.Types where

import Control.Lens
import Data.Word
import qualified Data.ByteString.Lazy as B

-- data LoginError =
--     UnknownLoginError |
--     NeedCaptcha |
--     OtherLoginError |
--     UnsafeDeviceError |
--     SMSNeededError |
--     TooManySMSRequestError |
--     SMSOrVerifyNeededError |
--     SliderNeededError |
--     NoResponse
--     deriving (Eq, Show)

-- data LoginResponse = LoginResponse {
--     _success :: Bool,
--     _code :: Word8,
--     _login_error :: LoginError,

--     _captcha_image :: B.ByteString,
--     _captcha_sign :: B.ByteString,

--     _verify_url :: String,
--     _sms_phone :: String,

--     _error_message :: String
-- } deriving (Eq, Show)
-- $(makeLenses ''LoginResponse)

data LoginResponse =
    LoginSuccess |
    AccountFrozen |
    DeviceLockLogin |
    NeedCaptcha !B.ByteString !B.ByteString |
    SMSNeeded !String !String | -- msg phone
    TooManySMSRequest |
    SliderNeeded !String | -- url
    VerificationNeeded !String !String !String | -- msg url phone
    UnknownLoginResponse !Word8 !String
    deriving (Eq, Show)


-- defaultResponse :: LoginResponse
-- defaultResponse = LoginResponse {
--     _success = False,
--     _code = 0,
--     _login_error = UnknownLoginError,

--     _captcha_image = "",
--     _captcha_sign = "",

--     _verify_url = "",
--     _sms_phone = "",

--     _error_message = ""
-- }