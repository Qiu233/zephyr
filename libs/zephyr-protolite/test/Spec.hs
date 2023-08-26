{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
import Test.Hspec
import Zephyr.ProtoLite
import Zephyr.ProtoLite.Aliases
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Text.Printf
import Data.Int


data ProtoFixed = ProtoFixed {
    fixedu32 :: ProtoField UFixed32 1,
    fixedu64 :: ProtoField UFixed64 2,
    fixedi32 :: ProtoField SFixed32 3,
    fixedi64 :: ProtoField SFixed64 4,
    fixedf32 :: ProtoField FFixed32 5,
    fixedf64 :: ProtoField FFixed64 6
} deriving (Show, Eq, Generic)
instance ProtoBuf ProtoFixed

data ProtoVariant = ProtoVariant {
    varu32 :: ProtoField UInt32 1,
    varu64 :: ProtoField UInt64 2,
    vars32 :: ProtoField SInt32 3,
    vars64 :: ProtoField SInt64 4,
    var32 :: ProtoField Int32 5,
    var64 :: ProtoField Int64 6
} deriving (Show, Eq, Generic)
instance ProtoBuf ProtoVariant

data ProtoNested = ProtoNested {
    nested1 :: ProtoField ProtoFixed 1,
    nested2 :: ProtoField ProtoVariant 2
} deriving (Show, Eq, Generic)
instance ProtoBuf ProtoNested

data ProtoSkipped = ProtoSkipped {
    skipped1 :: ProtoField UFixed32 1,
    skipped2 :: ProtoField (Optional UFixed32) 3,
    skipped3 :: ProtoField (Optional String) 10
} deriving (Show, Eq, Generic)
instance ProtoBuf ProtoSkipped

data ProtoOptional = ProtoOptional {
    optfu32 :: ProtoField (Optional UFixed32) 1,
    optvs32 :: ProtoField (Optional SInt32) 2
} deriving (Show, Eq, Generic)
instance ProtoBuf ProtoOptional

data ProtoListTest = ProtoListTest {
    listu32 :: ProtoField (Repeated UFixed32) 1,
    listv32 :: ProtoField (Repeated UInt32) 2
} deriving (Show, Eq, Generic)
instance ProtoBuf ProtoListTest

data ProtoPacked = ProtoPacked {
    packedu32 :: ProtoField (Packed UFixed32) 1,
    packedv32 :: ProtoField (Packed SInt64) 2
} deriving (Show, Eq, Generic)
instance ProtoBuf ProtoPacked

data ProtoFloat = ProtoFloat {
    float32 :: ProtoField FFixed32 1,
    float64 :: ProtoField FFixed64 2
} deriving (Show, Eq, Generic)
instance ProtoBuf ProtoFloat

main :: IO ()
main = hspec $ do
    describe "tests" $ do
        it "fixed" $ do
            let v = ProtoFixed {
                    fixedu32 = 0x12345678,
                    fixedu64 = 0x123456789abcdef0,
                    fixedi32 = 0x12345678,
                    fixedi64 = 0x123456789abcdef0,
                    fixedf32 = 0x12345678,
                    fixedf64 = 0x123456789abcdef0
            }
            let bs = encode v
            let v' = decode bs :: ProtoFixed
            print v
            printHex bs
            print v'
            v' `shouldBe` v
        it "variant" $ do
            let v = ProtoVariant {
                    varu32 = 0x12345678,
                    varu64 = 0x123456789abcdef0,
                    vars32 = 0x12345678,
                    vars64 = 0x123456789abcdef0,
                    var32 = 0x12345678,
                    var64 = 0x123456789abcdef0
            }
            let bs = encode v
            let v' = decode bs :: ProtoVariant
            print v
            printHex bs
            print v'
            v' `shouldBe` v
        it "nested" $ do
            let v = ProtoNested {
                    nested1 = ProtoField $ ProtoFixed {
                        fixedu32 = 0x12345678,
                        fixedu64 = 0x123456789abcdef0,
                        fixedi32 = 0x12345678,
                        fixedi64 = 0x123456789abcdef0,
                        fixedf32 = 0x12345678,
                        fixedf64 = 0x123456789abcdef0
                    },
                    nested2 = ProtoField $ ProtoVariant {
                        varu32 = 0x12345678,
                        varu64 = 0x123456789abcdef0,
                        vars32 = 0x12345678,
                        vars64 = 0x123456789abcdef0,
                        var32 = 0x12345678,
                        var64 = 0x123456789abcdef0
                    }
            }
            let bs = encode v
            let v' = decode bs :: ProtoNested
            print v
            printHex bs
            print v'
            v' `shouldBe` v
        it "skipped" $ do
            let v = ProtoSkipped {
                    skipped1 = 0x12345678,
                    skipped2 = ProtoField $ Just 0x12345678,
                    skipped3 = ProtoField $ Just "hello"
            }
            let bs = encode v
            let v' = decode bs :: ProtoSkipped
            print v
            printHex bs
            print v'
            v' `shouldBe` v
        it "optional" $ do
            let v = ProtoOptional {
                    optfu32 = ProtoField $ Just 0x12345678,
                    optvs32 = ProtoField $ Just 0x12345678
            }
            let bs = encode v
            let v' = decode bs :: ProtoOptional
            print v
            printHex bs
            print v'
            print $ v.optfu32.unwrap
            v' `shouldBe` v
        it "list" $ do
            let v = ProtoListTest {
                    listu32 = ProtoField $ Repeated [0x12345678, 0x12345678],
                    listv32 = ProtoField $ Repeated [0x12345678, 0x12345678]
            }
            let bs = encode v
            let v' = decode bs :: ProtoListTest
            print v
            printHex bs
            print v'
            v' `shouldBe` v
        it "packed" $ do
            let v = ProtoPacked {
                    packedu32 = ProtoField $ Packed [0x12345678, 0x12345678],
                    packedv32 = ProtoField $ Packed [0x12345678, 0x12345678]
            }
            let bs = encode v
            let v' = decode bs :: ProtoPacked
            print v
            printHex bs
            print v'
            v' `shouldBe` v
        it "float" $ do
            let v = ProtoFloat {
                    float32 = pfield 0.123456,
                    float64 = pfield 0.12345678910
            }
            let bs = encode v
            let v' = decode bs :: ProtoFloat
            print v
            printHex bs
            print v'
            v' `shouldBe` v


encodeHex :: B.ByteString -> String
encodeHex = concatMap (printf "%02X") . B.unpack

printHex :: B.ByteString -> IO ()
printHex = putStrLn . encodeHex