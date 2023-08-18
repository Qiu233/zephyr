{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Binary.Put where
import GHC.Generics

import qualified Data.ByteString.Lazy as B
import Control.Monad.Trans.Writer

import Zephyr.Binary.Types
import Data.Word
import Data.Int
import GHC.Float
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString as SB
import Zephyr.Binary.OP

class GBinPut f where
    gputle :: f a -> Put
    gputbe :: f a -> Put

class BinPut a where
    putle :: a -> Put
    default putle :: (Generic a, GBinPut (Rep a)) => a -> Put
    putle a = gputle $ from a

    putbe :: a -> Put
    default putbe :: (Generic a, GBinPut (Rep a)) => a -> Put
    putbe a = gputbe $ from a


instance GBinPut a => GBinPut (M1 i c a) where
    gputle = gputle . unM1
    gputbe = gputbe . unM1

instance BinPut a => GBinPut (K1 i a) where
    gputle = putle . unK1
    gputbe = putbe . unK1

instance (GBinPut a, GBinPut b) => GBinPut (a :*: b) where
    gputle (a :*: b) = gputle a >> gputle b
    gputbe (a :*: b) = gputbe a >> gputbe b


runPut :: Put -> B.ByteString
runPut = execWriter

{-# INLINE put8 #-}
{-# INLINE put16le #-}
{-# INLINE put16be #-}
{-# INLINE put32le #-}
{-# INLINE put32be #-}
{-# INLINE put64le #-}
{-# INLINE put64be #-}

put8 :: Word8 -> Put
put8 = tell . B.singleton

putb :: Bool -> Put
putb v = put8 $ if v then 1 else 0

put16le :: Word16 -> Put
put16le x = do
    put8 $ fromIntegral x
    put8 $ fromIntegral $ x .>. 8

put16be :: Word16 -> Put
put16be x = do
    put8 $ fromIntegral $ x .>. 8
    put8 $ fromIntegral x

put32le :: Word32 -> Put
put32le x = do
    put16le $ fromIntegral x
    put16le $ fromIntegral $ x .>. 16

put32be :: Word32 -> Put
put32be x = do
    put16be $ fromIntegral $ x .>. 16
    put16be $ fromIntegral x

put64le :: Word64 -> Put
put64le x = do
    put32le $ fromIntegral x
    put32le $ fromIntegral $ x .>. 32

put64be :: Word64 -> Put
put64be x = do
    put32be $ fromIntegral $ x .>. 32
    put32be $ fromIntegral x

instance BinPut Word8 where
    putle = put8
    putbe = put8
instance BinPut Word16 where
    putle = put16le
    putbe = put16be
instance BinPut Word32 where
    putle = put32le
    putbe = put32be
instance BinPut Word64 where
    putle = put64le
    putbe = put64be

instance BinPut Float where
    putle = put32le . castFloatToWord32
    putbe = put32be . castFloatToWord32
instance BinPut Double where
    putle = put64le . castDoubleToWord64
    putbe = put64be . castDoubleToWord64

instance BinPut Int16 where
    putle = put16le . fromIntegral
    putbe = put16be . fromIntegral
instance BinPut Int32 where
    putle = put32le . fromIntegral
    putbe = put32be . fromIntegral
instance BinPut Int64 where
    putle = put64le . fromIntegral
    putbe = put64be . fromIntegral

instance BinPut B.ByteString where
    putle = putbs
    putbe = putbs

putbs :: B.ByteString -> Put
putbs = tell

putbss :: SB.ByteString -> Put
putbss = putbs . B.fromStrict

pututf8 :: String -> Put
pututf8 s = putbs (UTF8.fromString s)

putListBE :: BinPut a => [a] -> Put
putListBE = mapM_ putbe
