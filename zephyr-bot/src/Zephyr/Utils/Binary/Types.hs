{-# LANGUAGE FlexibleContexts #-}
module Zephyr.Utils.Binary.Types (
    PutM, Put , Get(..),
    DecodeResult(..),
    (.<.), (.>.),
) where
import qualified Data.ByteString.Lazy as B
import Control.Monad.Trans.Writer
import Data.Bits

infixl 7 .<.
infixl 7 .>.
(.<.) :: Bits a => a -> Int -> a
(.<.) = shiftL
(.>.) :: Bits a => a -> Int -> a
(.>.) = shiftR

data DecodeResult a =
    Success !a !B.ByteString |
    TooFewBytes

type PutM = Writer B.ByteString
type Put = PutM ()

newtype Get a = Get { runGetInner :: B.ByteString -> DecodeResult a }
instance Functor Get where
    fmap f (Get g) = Get $ \bs -> case g bs of
        Success a bs' -> Success (f a) bs'
        TooFewBytes -> TooFewBytes
instance Applicative Get where
    pure a = Get $ \bs -> Success a bs
    Get f <*> Get a = Get $ \bs -> case f bs of
        Success f' bs' -> case a bs' of
            Success a' bs'' -> Success (f' a') bs''
            TooFewBytes -> TooFewBytes
        TooFewBytes -> TooFewBytes
instance Monad Get where
    return = pure
    Get a >>= f = Get $ \bs -> case a bs of
        Success a' bs' -> runGetInner (f a') bs'
        TooFewBytes -> TooFewBytes
