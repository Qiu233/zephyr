{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Data.Login.Pow (calcPow) where
import qualified Data.ByteString.Lazy as B
import Control.Monad.IO.Class
import Zephyr.Utils.Binary.Get
import Zephyr.Utils.Binary
import Control.Monad
import Data.Bits
import Zephyr.Utils.Codec
import Zephyr.Packet.Internal
import System.Random (randomRIO)

integer2bs :: Integer -> B.ByteString
integer2bs i = B.pack $ reverse $ inner i
    where
        inner 0 = []
        inner j = fromIntegral (j .&. 0xff) : inner (j `shiftR` 8)

bs2integer :: B.ByteString -> Integer
bs2integer bs = inner $ reverse $ B.unpack bs
    where inner [] = 0
          inner (x:xs) = fromIntegral x .|. inner xs `shiftL` 8


calcPow :: MonadIO m => B.ByteString -> m B.ByteString
calcPow bs = do
    let (a, typ, c, ok, e, f, src, tgt, cpy) = flip runGet bs $ do
            (,,,,,,,,) <$> get8 <*> get8 <*> get8 <*> getb <*> get16be <*> get16be <*> getlv <*> getlv <*> getlv
    (ok_, dst, elp, cnt) <- if typ == 2 && B.length tgt == 32
        then do
            -- start <- getEpochTimeMS
            let s = bs2integer src
            let v = until ((== tgt) . B.fromStrict . sha256 . B.toStrict . integer2bs) (+1) s
            let cnt = v - s
            -- elp <- subtract start <$> getEpochTimeMS
            elp <- liftIO $ randomRIO (100, 2000)
            pure (True, integer2bs v, elp, cnt)
        else
            pure (ok, B.empty, 0, 0)
    pure $ runPut $ do
        put8 a
        put8 typ
        put8 c
        putb ok
        put16be e
        put16be f
        lvbs src
        lvbs tgt
        lvbs cpy
        when ok_ $ do
            lvbs dst
            put32be elp
            put32be $ fromIntegral cnt