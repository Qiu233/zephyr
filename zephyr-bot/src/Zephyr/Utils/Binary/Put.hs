{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Utils.Binary.Put where
import GHC.Generics

import qualified Data.ByteString.Lazy as B
import Control.Monad.Trans.Writer

import Zephyr.Utils.Binary.Types
import Data.Word
import Data.Int
import GHC.Float
import qualified Data.ByteString.Lazy.UTF8 as UTF8

class GBinPut f where
    gputle :: f a -> Put
    gputge :: f a -> Put

class BinPut a where
    putle :: a -> Put
    default putle :: (Generic a, GBinPut (Rep a)) => a -> Put
    putle a = gputle $ from a

    putge :: a -> Put
    default putge :: (Generic a, GBinPut (Rep a)) => a -> Put
    putge a = gputge $ from a


-- instance GBinPut a => GBinPut (M1 i c a) where
--     gputle = gputle . unM1
--     gputge = gputge . unM1

-- instance BinPut a => GBinPut (K1 i a) where
--     gputle = putle . unK1
--     gputge = putge . unK1

instance (GBinPut a, GBinPut b) => GBinPut (a :*: b) where
    gputle (a :*: b) = gputle a >> gputle b
    gputge (a :*: b) = gputge a >> gputge b

-- instance GBinPut (a :+: b) where
--     gputle _ = undefined
--     gputge _ = undefined

runPut :: Put -> B.ByteString
runPut = execWriter

put8 :: Word8 -> Put
put8 = tell . B.singleton

put16le :: Word16 -> Put
put16le x = do
    put8 $ fromIntegral x
    put8 $ fromIntegral $ x >.> 8

put16ge :: Word16 -> Put
put16ge x = do
    put8 $ fromIntegral $ x >.> 8
    put8 $ fromIntegral x

put32le :: Word32 -> Put
put32le x = do
    put16le $ fromIntegral x
    put16le $ fromIntegral $ x >.> 16

put32ge :: Word32 -> Put
put32ge x = do
    put16ge $ fromIntegral $ x >.> 16
    put16ge $ fromIntegral x

put64le :: Word64 -> Put
put64le x = do
    put32le $ fromIntegral x
    put32le $ fromIntegral $ x >.> 32

put64ge :: Word64 -> Put
put64ge x = do
    put32ge $ fromIntegral $ x >.> 32
    put32ge $ fromIntegral x

instance BinPut Word8 where
    putle = put8
    putge = put8
instance BinPut Word16 where
    putle = put16le
    putge = put16ge
instance BinPut Word32 where
    putle = put32le
    putge = put32ge
instance BinPut Word64 where
    putle = put64le
    putge = put64ge

instance BinPut Float where
    putle = put32le . castFloatToWord32
    putge = put32ge . castFloatToWord32
instance BinPut Double where
    putle = put64le . castDoubleToWord64
    putge = put64ge . castDoubleToWord64

instance BinPut Int16 where
    putle = put16le . fromIntegral
    putge = put16ge . fromIntegral
instance BinPut Int32 where
    putle = put32le . fromIntegral
    putge = put32ge . fromIntegral
instance BinPut Int64 where
    putle = put64le . fromIntegral
    putge = put64ge . fromIntegral

instance BinPut a => BinPut [a] where
    putle = mapM_ putle
    putge = mapM_ putge

putbs :: B.ByteString -> Put
putbs = tell

pututf8 :: String -> Put
pututf8 s = putbs (UTF8.fromString s)

putPrefLE :: (BinPut a, BinPut b) => (a -> b) -> a -> Put
putPrefLE f a = do
    putle $ f a
    putle a