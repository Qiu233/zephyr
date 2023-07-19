{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Builder where
import Zephyr.Utils.Binary
import qualified Data.ByteString.Lazy as B

lv :: Put -> Put
lv p = do
    let s = runPut p
    put16be $ fromIntegral $ B.length s
    putbs s

lvbs :: B.ByteString -> Put
lvbs = lv . putbs

lvu8 :: String -> Put
lvu8 = lv . pututf8


withLength32Desc :: Put -> Put
withLength32Desc p = do
    let s = runPut p
    put32be (fromIntegral (B.length s) + 4)
    putbs s

withLength32Desc_ :: B.ByteString -> Put
withLength32Desc_ bs = withLength32Desc $ putbs bs