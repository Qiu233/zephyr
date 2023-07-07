{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Utils.Binary.Get where

import Zephyr.Utils.Binary.Types
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Word
import Data.Bits
import Data.Int
import GHC.Float
import qualified Data.ByteString.Lazy.UTF8 as UTF8

class GBinGet f where
    ggetle :: Get (f a)
    ggetge :: Get (f a)
class BinGet a where
    getle :: Get a
    default getle :: (Generic a, GBinGet (Rep a)) => Get a
    getle = fmap to ggetle

    getge :: Get a
    default getge :: (Generic a, GBinGet (Rep a)) => Get a
    getge = fmap to ggetge

instance (GBinGet a, GBinGet b) => GBinGet (a :*: b) where
    ggetle = (:*:) <$> ggetle <*> ggetle
    ggetge = (:*:) <$> ggetge <*> ggetge

get8 :: Get Word8
get8 = Get $ \bs -> case B.uncons bs of
    Just (w, bs') -> Success w bs'
    Nothing -> TooFewBytes

get16le :: Get Word16
get16le = do
    a <- fromIntegral <$> get8
    b <- fromIntegral <$> get8
    pure $ a .|. (b <.< 8)

get16ge :: Get Word16
get16ge = do
    a <- fromIntegral <$> get8
    b <- fromIntegral <$> get8
    pure $ b .|. (a <.< 8)

get32le :: Get Word32
get32le = do
    a <- fromIntegral <$> get16le
    b <- fromIntegral <$> get16le
    pure $ a .|. (b <.< 16)
get32ge :: Get Word32
get32ge = do
    a <- fromIntegral <$> get16ge
    b <- fromIntegral <$> get16ge
    pure $ b .|. (a <.< 16)

get64le :: Get Word64
get64le = do
    a <- fromIntegral <$> get32le
    b <- fromIntegral <$> get32le
    pure $ a .|. (b <.< 32)
get64ge :: Get Word64
get64ge = do
    a <- fromIntegral <$> get32ge
    b <- fromIntegral <$> get32ge
    pure $ b .|. (a <.< 32)

instance BinGet Word8 where
    getle = get8
    getge = get8

instance BinGet Word16 where
    getle = get16le
    getge = get16ge

instance BinGet Word32 where
    getle = get32le
    getge = get32ge

instance BinGet Word64 where
    getle = get64le
    getge = get64ge

instance BinGet Float where
    getle = castWord32ToFloat <$> get32le
    getge = castWord32ToFloat <$> get32ge

instance BinGet Double where
    getle = castWord64ToDouble <$> get64le
    getge = castWord64ToDouble <$> get64ge

instance BinGet Int16 where
    getle = fromIntegral <$> get16le
    getge = fromIntegral <$> get16ge

instance BinGet Int32 where
    getle = fromIntegral <$> get32le
    getge = fromIntegral <$> get32ge

instance BinGet Int64 where
    getle = fromIntegral <$> get64le
    getge = fromIntegral <$> get64ge

getbs :: Int -> Get B.ByteString
getbs len' = Get $ \bs ->
    if B.length bs < len then
        TooFewBytes
    else
        Success (B.take len bs) (B.drop len bs)
    where len = fromIntegral len'

getbsToUTF8 :: Int -> Get String
getbsToUTF8 len = UTF8.toString <$> getbs len
