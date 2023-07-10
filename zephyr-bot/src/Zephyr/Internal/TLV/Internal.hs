{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Internal.TLV.Internal where

import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Binary
import Control.Monad.IO.Class
import Data.Word
import Zephyr.Utils.Common
import Data.Bits

putlv16be :: B.ByteString -> Put
putlv16be a = do
    let s = runPut $ putbe a
    let len = B.length s
    put16be $ fromIntegral len
    putbs s

putlv16LimitedBE :: Int -> B.ByteString -> Put
putlv16LimitedBE n' a = do
    let n = fromIntegral n'
    let bs = B.take n $ runPut $ putbe a
    putlv16be bs

putlvs16be :: String -> Put
putlvs16be = putlv16be . utf8ToBytes

putlvs16LimitedBE :: Int -> String -> Put
putlvs16LimitedBE n = putlv16LimitedBE n . utf8ToBytes

tlvPack :: MonadIO m => Word16 -> Put -> m B.ByteString
tlvPack packetType bodyBuilder = pure . runPut $ do
        put16be packetType
        putlv16be $ runPut bodyBuilder

guidFlag :: Word32
guidFlag = (shiftL 1 24 .&. 0xFF000000) .|. 0