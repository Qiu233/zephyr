{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Zephyr.Packet.Internal where
import Zephyr.Utils.Binary
import qualified Data.ByteString.Lazy as B
import Data.HashMap
import Data.Proxy
import Data.Hashable

lv :: Put -> Put
lv p = do
    let s = runPut p
    put16be $ fromIntegral $ B.length s
    putbs s

lvbs :: B.ByteString -> Put
lvbs = lv . putbs

lvu8 :: String -> Put
lvu8 = lv . pututf8

getlv :: Get B.ByteString
getlv = getbs . fromIntegral =<< get16be


withLength32Desc :: Put -> Put
withLength32Desc p = do
    let s = runPut p
    put32be (fromIntegral (B.length s) + 4)
    putbs s

withLength32Desc_ :: B.ByteString -> Put
withLength32Desc_ bs = withLength32Desc $ putbs bs

getTLVEntry :: (BinGet a, BinGet b, Integral b) => Proxy b -> Get (a, B.ByteString)
getTLVEntry (_ :: Proxy b) = do
    t <- getbe
    l <- getbe @b
    v <- getbs $ fromIntegral l
    pure (t, v)

getTLVEntries' :: (BinGet a, BinGet b, Integral b) => Proxy b -> Get [(a, B.ByteString)]
getTLVEntries' (p :: Proxy b) = do
    e <- tryGet $ getTLVEntry p
    case e of
        Nothing -> pure []
        Just e' -> do
            es <- getTLVEntries' p
            pure $ e' : es

getTLVEntries :: (BinGet a, BinGet b, Integral b, Hashable a, Ord a) => Proxy b -> Get (Map a B.ByteString)
getTLVEntries p = fromList <$> getTLVEntries' p