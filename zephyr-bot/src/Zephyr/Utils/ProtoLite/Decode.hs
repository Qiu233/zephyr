{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Utils.ProtoLite.Decode where
import Zephyr.Utils.Binary
import Data.Int
import Data.Bits

import Zephyr.Utils.ProtoLite.Types




getTag :: Get PTag
getTag = do
    v <- get32le
    let tag = v `shiftR` 3
    let wt = v .&. 0x07
    pure $ PTag tag (fromIntegral wt)

getValue :: PTag -> Get PValue
getValue tag = case _wireType tag of
    0 -> PVVariant <$> getPVInt
    1 -> PVI64 <$> get64le
    2 -> do
        len <- pvIntDirect <$> getPVInt :: Get Int32
        PVLenPrefixed <$> getbs (fromIntegral len)
    5 -> PVI32 <$> get32le
    _ -> error "Unknown wire type"

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

getMessage :: Get PMessage
getMessage = PMessage <$> getEntries

