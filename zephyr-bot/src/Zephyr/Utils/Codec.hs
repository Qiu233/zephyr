{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Utils.Codec where
import qualified Crypto.Hash as Hash
import Data.ByteArray
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Common (utf8ToBytes)

md5Of :: (ByteArrayAccess arr, ByteArray bout) => arr -> bout
md5Of bs = convert (Hash.hash bs :: Hash.Digest Hash.MD5)


md5Of_ :: ByteArray arr => B.ByteString -> arr
md5Of_ = md5Of . B.toStrict

md5Lazy :: B.ByteString -> B.ByteString
md5Lazy = B.fromStrict . md5Of_


md5OfU8 :: ByteArray arr => String -> arr
md5OfU8 = md5Of_ . utf8ToBytes