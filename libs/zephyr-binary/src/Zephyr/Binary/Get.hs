{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Binary.Get where

import Zephyr.Binary.Types
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Word
import Data.Bits
import Data.Int
import GHC.Float
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Control.Monad (replicateM)
import Data.Functor (void)
import Zephyr.Binary.OP

class GBinGet f where
    ggetle :: Get (f a)
    ggetbe :: Get (f a)
class BinGet a where
    getle :: Get a
    default getle :: (Generic a, GBinGet (Rep a)) => Get a
    getle = fmap to ggetle

    getbe :: Get a
    default getbe :: (Generic a, GBinGet (Rep a)) => Get a
    getbe = fmap to ggetbe

instance (GBinGet a, GBinGet b) => GBinGet (a :*: b) where
    ggetle = (:*:) <$> ggetle <*> ggetle
    ggetbe = (:*:) <$> ggetbe <*> ggetbe

instance GBinGet a => GBinGet (M1 i c a) where
    ggetle = M1 <$> ggetle
    ggetbe = M1 <$> ggetbe

instance BinGet a => GBinGet (K1 i a) where
    ggetle = K1 <$> getle
    ggetbe = K1 <$> getbe


runGet_ :: Get a -> B.ByteString -> Either String a
runGet_ (Get f) bs = case f bs of
    Success a _ -> Right a
    DError e -> Left e
    TooFewBytes -> Left "Too few bytes"

runGet :: Get a -> B.ByteString -> a
runGet f bs = either error id (runGet_ f bs)

{-# INLINE get8 #-}
{-# INLINE get16le #-}
{-# INLINE get16be #-}
{-# INLINE get32le #-}
{-# INLINE get32be #-}
{-# INLINE get64le #-}
{-# INLINE get64be #-}

get8 :: Get Word8
get8 = Get $ \bs -> case B.uncons bs of
    Just (w, bs') -> Success w bs'
    Nothing -> TooFewBytes

get16le :: Get Word16
get16le = do
    a <- fromIntegral <$> get8
    b <- fromIntegral <$> get8
    pure $ a .|. (b .<. 8)

get16be :: Get Word16
get16be = do
    a <- fromIntegral <$> get8
    b <- fromIntegral <$> get8
    pure $ b .|. (a .<. 8)

get32le :: Get Word32
get32le = do
    a <- fromIntegral <$> get16le
    b <- fromIntegral <$> get16le
    pure $ a .|. (b .<. 16)
get32be :: Get Word32
get32be = do
    a <- fromIntegral <$> get16be
    b <- fromIntegral <$> get16be
    pure $ b .|. (a .<. 16)

get64le :: Get Word64
get64le = do
    a <- fromIntegral <$> get32le
    b <- fromIntegral <$> get32le
    pure $ a .|. (b .<. 32)
get64be :: Get Word64
get64be = do
    a <- fromIntegral <$> get32be
    b <- fromIntegral <$> get32be
    pure $ b .|. (a .<. 32)

instance BinGet Word8 where
    getle = get8
    getbe = get8

instance BinGet Word16 where
    getle = get16le
    getbe = get16be

instance BinGet Word32 where
    getle = get32le
    getbe = get32be

instance BinGet Word64 where
    getle = get64le
    getbe = get64be

instance BinGet Float where
    getle = castWord32ToFloat <$> get32le
    getbe = castWord32ToFloat <$> get32be

instance BinGet Double where
    getle = castWord64ToDouble <$> get64le
    getbe = castWord64ToDouble <$> get64be

instance BinGet Int16 where
    getle = fromIntegral <$> get16le
    getbe = fromIntegral <$> get16be

instance BinGet Int32 where
    getle = fromIntegral <$> get32le
    getbe = fromIntegral <$> get32be

instance BinGet Int64 where
    getle = fromIntegral <$> get64le
    getbe = fromIntegral <$> get64be

instance (BinGet a, BinGet b) => BinGet (a, b) where
    getle = (,) <$> getle <*> getle
    getbe = (,) <$> getbe <*> getbe

instance (BinGet a, BinGet b, BinGet c) => BinGet (a, b, c) where
    getle = (,,) <$> getle <*> getle <*> getle
    getbe = (,,) <$> getbe <*> getbe <*> getbe

instance (BinGet a, BinGet b, BinGet c, BinGet d) => BinGet (a, b, c, d) where
    getle = (,,,) <$> getle <*> getle <*> getle <*> getle
    getbe = (,,,) <$> getbe <*> getbe <*> getbe <*> getbe


getbs :: Int -> Get B.ByteString
getbs len' = Get $ \bs ->
    if B.length bs < len then
        TooFewBytes
    else
        Success (B.take len bs) (B.drop len bs)
    where len = fromIntegral len'

getutf8 :: Int -> Get String
getutf8 len = UTF8.toString <$> getbs len

getListOfBE :: BinGet a => Int -> Get [a]
getListOfBE len = replicateM len getbe

isEmpty :: Get Bool
isEmpty = Get $ \bs -> Success (B.null bs) bs

getRemaining :: Get B.ByteString
getRemaining = Get $ \bs -> Success bs B.empty

tryGet :: Get a -> Get (Maybe a)
tryGet (Get f) = Get $ \bs -> case f bs of
    Success a bs' -> Success (Just a) bs'
    DError _ -> Success Nothing bs
    TooFewBytes -> Success Nothing bs

getb :: Get Bool
getb = (/=0) <$> get8

skip :: Int -> Get ()
skip len = void $ getbs len