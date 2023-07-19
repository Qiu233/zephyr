{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FunctionalDependencies #-}
module Zephyr.Core.Request where
import Data.Word
import qualified Data.ByteString.Lazy as B
import Control.Lens

data RequestType = RT_Login | RT_Simple
    deriving (Eq, Show)

data EncryptType = ET_NoEncrypt | ET_D2Key | ET_EmptyKey
    deriving (Eq, Show)

data Request = Request {
    _req_type :: RequestType,
    _enc_type :: EncryptType,
    _sequence_id :: Word32,
    _req_uin :: Word64,
    _req_command :: String,
    _req_body :: B.ByteString
} deriving (Eq, Show)
$(makeLenses ''Request)