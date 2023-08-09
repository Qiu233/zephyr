{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Utils.Jce.Internal where
import Data.Word
import qualified Data.ByteString.Lazy as B
import Data.Bits
import Data.Int
import Control.Monad (replicateM, forM_, replicateM_)
import Control.Monad.Fix
import Zephyr.Utils.Binary
import Zephyr.Utils.Jce.JceMap
import GHC.Stack

type JceEntry a = (Word8, a)


putHead :: Word8 -> Word8 -> Put
putHead type_ tag = do
    if tag < 0xf then
        put8 $ (tag `shiftL` 4) .|. type_
    else do
        put8 (0xf0 .|. type_)
        put8 tag

isByte :: Int64 -> Bool
isByte v = v >= -128 && v <= 127

isShort :: Int64 -> Bool
isShort v = v >= -32768 && v <= 32767

isInt :: Int64 -> Bool
isInt v = v >= -2147483648 && v <= 2147483647

putJ8 :: Word8 -> Int64 -> Put
putJ8 t v = do
    if v == 0 then do
        putHead 12 t
    else do
        putHead 0 t
        put8 $ fromIntegral v

putJ16 :: Word8 -> Int64 -> Put
putJ16 t v = do
    if isByte v then putJ8 t v
    else do
        putHead 1 t
        put16be $ fromIntegral v

putJ32 :: Word8 -> Int64 -> Put
putJ32 t v = do
    if isShort v then putJ16 t v
    else do
        putHead 2 t
        put32be $ fromIntegral v

putJ64 :: Word8 -> Int64 -> Put
putJ64 t v = do
    if isInt v then putJ32 t v
    else do
        putHead 3 t
        put64be $ fromIntegral v

putJInt :: Integral n => Word8 -> n -> Put
putJInt i = putJ64 i . fromIntegral

putJString :: Word8 -> String -> Put
putJString t v = do
    let l = length v
    if l > 255 then do
        putHead 7 t
        put32be $ fromIntegral l
    else do
        putHead 6 t
        put8 $ fromIntegral l
    pututf8 v

putJBytes :: Word8 -> B.ByteString -> Put
putJBytes t v = do
    let l = B.length v
    putHead 13 t
    put8 0
    putJInt 0 l
    putbs v


getHead :: Get (Word8, Word8)
getHead = do
    b1 <- get8
    tag_ <- if (b1 .&. 0xf0) == 0xf0 then
                get8
            else pure $ b1 `shiftR` 4
    let type_ = b1 .&. 0x0f
    pure (tag_, type_)

ueError :: Show a => Word8 -> a -> Get b
ueError t e = fail $ "unexpected type: " ++ show e ++ " at " ++ show t

skipFieldValue :: Word8 -> Get ()
skipFieldValue type_ = do
    case type_ of
        0 -> skip 1
        1 -> skip 2

        2 -> skip 4
        4 -> skip 4

        3 -> skip 8
        5 -> skip 8

        6 -> do
            l <- fromIntegral <$> get8
            skip l
        7 -> do
            l <- fromIntegral <$> get32be
            skip l
        8 -> do
            s <- fromIntegral <$> getJInt 0
            replicateM_ (s * 2) $ do
                (_, type__) <- getHead
                skipFieldValue type__
        9 -> do
            s <- fromIntegral <$> getJInt 0
            replicateM_ s $ do
                (_, type__) <- getHead
                skipFieldValue type__
        13 -> do
            _ <- getHead
            s <- fromIntegral <$> getJInt 0
            skip s
        10 -> do
            skipToStructEnd
        _ -> pure ()

skipTo :: Word8 -> Get Word8
skipTo tag = do
    (tag_, type_) <- getHead
    if tag_ == tag then pure type_
    else do
        skipFieldValue type_
        skipTo tag

skipToStructEnd :: Get ()
skipToStructEnd = do
    fix $ \loop -> do
        (_, type_) <- getHead
        if type_ == 11 then pure ()
        else do
            skipFieldValue type_
            loop

getJInt :: Word8 -> Get Int64
getJInt tag = do
    type_ <- skipTo tag
    case type_ of
        12 -> pure 0
        0 -> fromIntegral <$> get8
        1 -> fromIntegral <$> get16be
        2 -> fromIntegral <$> get32be
        3 -> fromIntegral <$> get64be
        _ -> ueError tag type_

getJString :: Word8 -> Get String
getJString tag = do
    type_ <- skipTo tag
    case type_ of
        6 -> do
            l <- fromIntegral <$> get8
            getutf8 l
        7 -> do
            l <- fromIntegral <$> get32be
            getutf8 l
        _ -> ueError tag type_


getJBytes :: Word8 -> Get B.ByteString
getJBytes tag = do
    type_ <- skipTo tag
    case type_ of
        13 -> do
            _ <- getHead
            l <- fromIntegral <$> getJInt 0
            getbs l
        _ -> ueError tag type_


class JceData a where
    gjput :: Word8 -> a -> Put
    gjget :: HasCallStack => Word8 -> Get a
    gjdef :: a


instance {-# OVERLAPPING #-} JceData Int64 where
    gjput = putJInt
    gjget = getJInt
    gjdef = 0

instance {-# OVERLAPPING #-} JceData Int32 where
    gjput = putJInt
    gjget n = fromIntegral <$> getJInt n
    gjdef = 0

instance {-# OVERLAPPING #-} JceData Int16 where
    gjput = putJInt
    gjget n = fromIntegral <$> getJInt n
    gjdef = 0

instance {-# OVERLAPPING #-} JceData Word8 where
    gjput = putJInt
    gjget n = fromIntegral <$> getJInt n
    gjdef = 0

instance {-# OVERLAPPING #-} JceData String where
    gjput = putJString
    gjget = getJString
    gjdef = ""

instance {-# OVERLAPPING #-} JceData B.ByteString where
    gjput = putJBytes
    gjget = getJBytes
    gjdef = B.empty

instance {-# OVERLAPPING #-} (JceData a, JceData b) => JceData (JceMap a b) where
    gjput n (JceMap m) = do
        putHead 8 n
        let l = length m
        putJInt 0 l
        forM_ m $ \(k, v) -> do
            gjput 0 k
            gjput 1 v
    gjget n = do
        type_ <- skipTo n
        case type_ of
            8 -> do
                s <- fromIntegral <$> getJInt 0
                ls <- replicateM s $ do
                    k <- gjget 0
                    v <- gjget 1
                    pure (k, v)
                pure $ JceMap ls
            _ -> ueError n type_
    gjdef = JceMap []

instance {-# OVERLAPPING #-} JceData a => JceData [a] where
    gjput n m = do
        putHead 9 n
        let l = length m
        putJInt 0 l
        forM_ m $ \v -> do
            gjput 0 v
    gjget n = do
        type_ <- skipTo n
        case type_ of
            9 -> do
                s <- fromIntegral <$> getJInt 0
                replicateM s $ gjget 0
            _ -> ueError n type_
    gjdef = []

