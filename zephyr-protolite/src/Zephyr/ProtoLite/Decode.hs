{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.ProtoLite.Decode where
import Data.Int
import Data.Bits

import Zephyr.ProtoLite.Types
import Data.Word
import Control.Monad
import Zephyr.Binary
import Zephyr.Binary.Get

getTag :: Get PTag
getTag = do
    v <- pvIntDirect <$> getPVInt
    let tag = v `shiftR` 3
    let wt = v .&. 0x07
    pure $ PTag tag (fromIntegral wt)

getValue :: PTag -> Get PValue
getValue (PTag _ _wireType) = case _wireType of
    0 -> PVVariant <$> getPVInt
    1 -> PVI64 <$> get64le
    2 -> do
        len <- pvIntDirect <$> getPVInt :: Get Int32
        PVLenPrefixed <$> getbs (fromIntegral len)
    5 -> PVI32 <$> get32le
    _ -> fail "Unknown wire type"

getEntry :: Get PMessageEntry
getEntry = do
    tag <- getTag
    value <- getValue tag
    pure $ PMessageEntry tag value

getEntries :: Get [PMessageEntry]
getEntries = do
    empty <- isEmpty
    if empty then pure []
    else do entry <- getEntry
            entries <- getEntries
            pure (entry:entries)

getMessage :: Get [PMessageEntry]
getMessage = getEntries

plookupAll :: Word32 -> [PMessageEntry] -> [PValue]
plookupAll fn entries = do
    (PMessageEntry (PTag f _) v) <- entries
    guard $ f == fn
    pure v
