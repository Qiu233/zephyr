{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Login.Types where

import Data.Word
import qualified Data.ByteString.Lazy as B

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

