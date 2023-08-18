{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Binary.OP where
import Data.Bits
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8


infixl 7 .<.
infixl 7 .>.
(.<.) :: Bits a => a -> Int -> a
(.<.) = shiftL
(.>.) :: Bits a => a -> Int -> a
(.>.) = shiftR


utf8ToBytes :: String -> B.ByteString
utf8ToBytes = UTF8.fromString

utf8FromBytes :: B.ByteString -> String
utf8FromBytes = UTF8.toString
