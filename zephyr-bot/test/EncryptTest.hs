{-# LANGUAGE RecordWildCards #-}
module EncryptTest where
import Zephyr.Encrypt.QQTea
import Test.Hspec
import Data.Maybe (fromJust)
import Zephyr.Utils.Common
import Zephyr.Encrypt.ECDH
import Common
import qualified Data.ByteString.Lazy as B

constKey :: B.ByteString
constKey = fromJust $ decodeHex "2D624E782A4D55BAA5B97418A0FE6240"

encryptTest :: SpecWith ()
encryptTest = do
    describe "Encrypt Tests" $ do
        it "keygen test" $ do
            EncryptECDH {..} <- generateDefaultKey
            printHex _shared_key
            printHex _public_key

        it "single enc test" $ do
            shouldBe (tea16Encrypt constKey 0x12345678) 0xE6CC2CF01893FE29
        
        it "single dec test" $ do
            shouldBe (tea16Decrypt constKey 0x12345678) 0x153E0951A0D87F96

        it "enc test" $ do
            let dbs = fromJust $ decodeHex "057546115E218CDF7867733880CE7874A2C8C041B1428BC3065EEA1FA38848C7CBE40E6BE51920DC7E1F12A7E912F417F83FE516D6BFC3AD965CED9744977F06"
            --[5,117,70,17,94,33,140,223,120,103,115,56,128,206,120,116,162,200,192,65,177,66,139,195,6,94,234,31,163,136,72,199,203,228,14,107,229,25,32,220,126,31,18,167,233,18,244,23,248,63,229,22,214,191,195,173,150,92,237,151,68,151,127,6]
            print $ encodeHex dbs
            ed <- qqteaEncrypt constKey dbs
            printHex ed

        it "public key fetching" $ do
            pk <- fetchPubKey 1234567890
            shouldNotBe pk Nothing
            print "Fetching key:\n"
            print pk