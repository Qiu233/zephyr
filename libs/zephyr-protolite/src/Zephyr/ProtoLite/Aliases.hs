module Zephyr.ProtoLite.Aliases (
    UInt32,
    UInt64,
    UFixed32,
    UFixed64,
    SFixed32,
    SFixed64,
    FFixed32,
    FFixed64,
    ProtoFieldOptional,
    ProtoFieldRepeated,
    ProtoFieldPacked,
    ProtoFieldRequired,
    Bytes,
    module Data.Int,
    module Data.Word
) where
import Zephyr.ProtoLite.Generic
import Data.Int
import Data.Word
import Data.ByteString.Lazy (LazyByteString)


type UInt32 = Word32
type UInt64 = Word64

type UFixed32 = Fixed32 Word32
type UFixed64 = Fixed64 Word64
type SFixed32 = Fixed32 Int32
type SFixed64 = Fixed64 Int64
type FFixed32 = Fixed32 Float
type FFixed64 = Fixed64 Double

type ProtoFieldOptional t = ProtoField (Optional t)
type ProtoFieldRepeated t = ProtoField (Repeated t)
type ProtoFieldPacked   t = ProtoField (Packed t)
type ProtoFieldRequired t = ProtoField t

type Bytes = LazyByteString