{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Engine.Packet where
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Binary

withTLV :: Put -> Put
withTLV p = do
    let s = runPut p
    put16be (fromIntegral $ B.length s)
    putbs s

withTLV_ :: B.ByteString -> Put
withTLV_ bs = withTLV $ putbs bs

withLength32Desc :: Put -> Put
withLength32Desc p = do
    let s = runPut p
    put32be (fromIntegral (B.length s) + 4)
    putbs s

withLength32Desc_ :: B.ByteString -> Put
withLength32Desc_ bs = withLength32Desc $ putbs bs