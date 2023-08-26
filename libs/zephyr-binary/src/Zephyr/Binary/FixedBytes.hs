{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Binary.FixedBytes where
import qualified Data.ByteString.Lazy as B
import GHC.TypeLits
import Zephyr.Binary.Get
import Zephyr.Binary.Types
import Zephyr.Binary.Put
import Text.Printf

newtype FixedBytes (n :: Nat) = FixedBytes B.ByteString
    deriving (Eq)

instance KnownNat n => Show (FixedBytes n) where
    show (FixedBytes bs) = show $ B.unpack bs

instance KnownNat n => BinGet (FixedBytes n) where
    getle = getFixedBytes
    getbe = getFixedBytes

instance KnownNat n => BinPut (FixedBytes n) where
    putle = putFixedBytes
    putbe = putFixedBytes

getFixedBytes :: forall n. KnownNat n => Get (FixedBytes n)
getFixedBytes = FixedBytes <$> getbs n
    where
        n = fromIntegral $ natVal (undefined :: FixedBytes n)

putFixedBytes :: forall n. KnownNat n => FixedBytes n -> Put
putFixedBytes (FixedBytes bs) = putbs $ B.take n bs
    where
        n = fromIntegral $ natVal (undefined :: FixedBytes n)

packFixed :: forall n. KnownNat n => B.ByteString -> FixedBytes n
packFixed bs = if B.length bs == n
    then FixedBytes bs
    else error $ printf "Expected %d bytes, got %d bytes" n (B.length bs)
    where n = fromIntegral $ natVal (undefined :: FixedBytes n)

unpackFixed :: forall n. KnownNat n => FixedBytes n -> B.ByteString
unpackFixed (FixedBytes bs) = if B.length bs == n
    then bs
    else error $ printf "Expected %d bytes, got %d bytes" n (B.length bs)
    where n = fromIntegral $ natVal (undefined :: FixedBytes n)