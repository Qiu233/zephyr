{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Utils.Codec (
    module Zephyr.Utils.Codec.JSON,
    md5Of,
    md5OfU8,
    md5Lazy,
    sha256,
    formatIPv4
) where
import qualified Crypto.Hash as Hash
import Data.ByteArray
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Common (utf8ToBytes)
import Data.Word
import Zephyr.Utils.Binary.Types
import Data.Bits
import Zephyr.Utils.Codec.JSON

md5Of :: (ByteArrayAccess arr, ByteArray bout) => arr -> bout
md5Of bs = convert (Hash.hash bs :: Hash.Digest Hash.MD5)


sha256 :: (ByteArrayAccess arr, ByteArray bout) => arr -> bout
sha256 bs = convert (Hash.hash bs :: Hash.Digest Hash.SHA256)


md5Lazy :: B.ByteString -> B.ByteString
md5Lazy = B.fromStrict . md5Of . B.toStrict

md5OfU8 :: ByteArray arr => String -> arr
md5OfU8 = md5Of . B.toStrict . utf8ToBytes

formatIPv4 :: Word32 -> String
formatIPv4 ip = show (ip .>. 24) ++ "." ++ show ((ip .>. 16) .&. 0xff) ++ "." ++ show ((ip .>. 8) .&. 0xff) ++ "." ++ show (ip .&. 0xff)