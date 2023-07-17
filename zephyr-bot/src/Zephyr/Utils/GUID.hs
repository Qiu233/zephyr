module Zephyr.Utils.GUID (
    GUID,
    guidBytes,
    createGUID
) where
import Data.Word
import Text.Printf
import Zephyr.Utils.Binary
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Common
import Data.Char (toLower)

newtype GUID = GUID (Word32, Word32, Word32, Word32)
    deriving (Eq)

instance Show GUID where
    show g = do
        let s = guidBytes g
            a = encodeHex $ B.take 4 s
            b = encodeHex $ B.take 2 $ B.drop 4 s
            c = encodeHex $ B.take 2 $ B.drop 6 s
            d = encodeHex $ B.take 2 $ B.drop 8 s
            e = encodeHex $ B.drop 10 s
        toLower <$> printf "%s-%s-%s-%s-%s" a b c d e

guidBytes :: GUID -> B.ByteString
guidBytes (GUID (a,b,c,d)) = do
    runPut $ do
            put32le a
            put32le b
            put32le c
            put32le d

createGUID :: B.ByteString -> GUID
createGUID = do
    runGet_ $ do
        a <- get32le
        b <- get32le
        c <- get32le
        d <- get32le
        return $ GUID (a,b,c,d)