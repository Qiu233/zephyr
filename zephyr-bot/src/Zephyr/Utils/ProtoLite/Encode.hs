{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Utils.ProtoLite.Encode where

import Zephyr.Utils.ProtoLite.Types
import Zephyr.Utils.Binary
import Data.Int
import Data.Bits
import qualified Data.ByteString.Lazy as B
import GHC.Float (castFloatToWord32, castDoubleToWord64)
import Data.Word
import Zephyr.Utils.Common (utf8ToBytes)

putTag :: PTag -> Put
putTag (PTag _fieldNumber _wireType) = do
    let v = PVInt $ (fromIntegral _fieldNumber `shiftL` 3) .|. fromIntegral _wireType
    putPVInt v


putPVIntDirect :: Integral p => Word32 -> p -> PMessageEntry
putPVIntDirect field value = do
    let tag = PTag field 0
    let v = PVVariant $ pvIntDirect' value
    PMessageEntry tag v
putPVSInt :: (Integral p,Bits p) => Word32 -> p -> PMessageEntry
putPVSInt field value = do
    let tag = PTag field 0
    let v = PVVariant $ pvSInt' value
    PMessageEntry tag v


putPVInt32 :: Word32 -> Int32 -> PMessageEntry
putPVInt32 = putPVIntDirect
putPVInt64 :: Word32 -> Int64 -> PMessageEntry
putPVInt64 = putPVIntDirect
putPVUInt32 :: Word32 -> Word32 -> PMessageEntry
putPVUInt32 = putPVIntDirect
putPVUInt64 :: Word32 -> Word64 -> PMessageEntry
putPVUInt64 = putPVIntDirect
putPVSInt32 :: Word32 -> Int32 -> PMessageEntry
putPVSInt32 = putPVSInt
putPVSInt64 :: Word32 -> Int64 -> PMessageEntry
putPVSInt64 = putPVSInt

putPVBool :: Word32 -> Bool -> PMessageEntry
putPVBool field value = putPVInt32 field (if value then 1 else 0)

putFixedU32 :: Word32 -> Word32 -> PMessageEntry
putFixedU32 field value = do
    let tag = PTag field 5
    let v = PVI32 value
    PMessageEntry tag v
putFixedI32 :: Word32 -> Int32 -> PMessageEntry
putFixedI32 field value = putFixedU32 field (fromIntegral value)
putFixedF32 :: Word32 -> Float -> PMessageEntry
putFixedF32 field value = putFixedU32 field (castFloatToWord32 value)

putFixedU64 :: Word32 -> Word64 -> PMessageEntry
putFixedU64 field value = do
    let tag = PTag field 1
    let v = PVI64 value
    PMessageEntry tag v
putFixedI64 :: Word32 -> Int64 -> PMessageEntry
putFixedI64 field value = putFixedU64 field (fromIntegral value)
putFixedF64 :: Word32 -> Double -> PMessageEntry
putFixedF64 field value = putFixedU64 field (castDoubleToWord64 value)




putLenBytes :: Word32 -> B.ByteString -> PMessageEntry
putLenBytes field value = do
    let tag = PTag field 2
    let v = PVLenPrefixed value
    PMessageEntry tag v

putLenUTF8 :: Word32 -> String -> PMessageEntry
putLenUTF8 field value = putLenBytes field (utf8ToBytes value)

putValue :: PValue -> Put
putValue (PVVariant v) = putPVInt v
putValue (PVI32 v) = put32le v
putValue (PVI64 v) = put64le v
putValue (PVLenPrefixed v) = do
    putPVInt $ pvIntDirect' $ B.length v
    putbs v

encodeMessage_ :: [PMessageEntry] -> B.ByteString
encodeMessage_ entries = runPut $ do
    mapM_ putEntry entries
    where
        putEntry (PMessageEntry tag value) = do
            putTag tag
            putValue value

encodeMessage :: PMessage -> B.ByteString
encodeMessage (PMessage entries) = encodeMessage_ entries

