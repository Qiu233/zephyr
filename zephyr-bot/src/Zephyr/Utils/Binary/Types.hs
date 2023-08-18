{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Zephyr.Utils.Binary.Types (
    PutM, Put , Get(..),
    DecodeResult(..),
    (.<.), (.>.),
    runGetInner
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
    DError !String |
    TooFewBytes

type PutM = Writer B.ByteString
type Put = PutM ()

newtype Get a = Get (B.ByteString -> DecodeResult a)
runGetInner :: Get a -> B.ByteString -> DecodeResult a
runGetInner (Get f) = f
instance Functor Get where
    fmap f (Get g) = Get $ \bs -> case g bs of
        Success a bs' -> Success (f a) bs'
        DError e -> DError e
        TooFewBytes -> TooFewBytes
instance Applicative Get where
    pure a = Get $ \bs -> Success a bs
    Get f <*> Get a = Get $ \bs -> case f bs of
        Success f' bs' -> case a bs' of
            Success a' bs'' -> Success (f' a') bs''
            DError e -> DError e
            TooFewBytes -> TooFewBytes
        DError e -> DError e
        TooFewBytes -> TooFewBytes
instance Monad Get where
    return = pure
    Get a >>= f = Get $ \bs -> case a bs of
        Success a' bs' -> runGetInner (f a') bs'
        DError e -> DError e
        TooFewBytes -> TooFewBytes
instance MonadFail Get where
    fail e = Get $ \_ -> DError e