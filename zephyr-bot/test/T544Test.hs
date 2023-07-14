{-# LANGUAGE TypeApplications #-}
module T544Test where

import Zephyr.Internal.TLV.T544.ASM
import Zephyr.Utils.Common
import Test.Hspec
import qualified Data.ByteString.Lazy as B
import Common (testHex, printHex)
import Zephyr.Internal.TLV.T544.HardCoded
import Data.Word
import Control.Monad.State (execState, runState)
import qualified Crypto.Cipher.RC4 as RC4
import qualified Data.ByteArray as BArr

sampleA :: B.ByteString
sampleA = decodeHex_ "000102030405060708090A0B0C0D0E0F"
sampleM :: B.ByteString
sampleM = decodeHex_ "000102030405060708090A0B0C0D0E0F1011121314"

sampleI4 :: UnknownI4
sampleI4 = (4222, 309238, 45917, 401729)

constW :: [Word32]
constW = [1287836604,2867352241,4071814407,977795764,3991887420,1191644301,3048601994,2415002430,1721035855,563339458,2485524808,466885238,708236512,195376162,2676174186,2220308252,2041333951,1913224349,3985265143,1776117483,2829033798,3667320283,924617772,1589766855,4028131102,713996997,496121577,1129569326,1570657796,1997597889,1786960424,701564422,3824236961,2497828192,4267904840,3618769230,3488618159,1527890895,2775764103,1925236169,1129761775,407308320,3174417575,3488940398]

t544Test :: SpecWith ()
t544Test = do
    describe "t544" $ do
        it "sub-a-pure" $ do
            testHex "0001127D0401B1F10809B9560C0B2F4E" $ do
                pure $ sub_a sampleA sampleI4
        it "sub-b-pure" $ do
            testHex "0005B14204052779080F1AFD0C0DB952" $ do
                pure $ sub_b sampleA sampleI4
        it "sub-c-pure" $ do
            testHex "637C777BF26B6FC53001672BFED7AB76" $ do
                pure $ sub_c tableB sampleA
        it "sub-d-pure" $ do
            testHex "00050A0F04090E03080D02070C01060B" $ do
                pure $ sub_d tableD sampleA
        it "sub-e-pure" $ do
            testHex "02070005060304010A0F080D0E0B0C09" $ do
                pure $ sub_e tableC sampleA
        it "sub-f-pure" $ do
            let w = sub_f tableE tableF tableB
            shouldBe w constW
        it "sub-aa-pure" $ do
            let vs = [0,1,20,60,100,111,180,220,255]
            let es = [136,133,156,243,220,214,12,83,102]
            let w = sub_aa 16 tableA (B.unpack sampleA) <$> vs
            shouldBe w es
        it "transform-inner" $ do
            testHex "2EAE26A7299E1298139612960D8C09855E5E667779" $ do
                pure $ B.pack $ transformInner (B.unpack sampleM) transFormTableEncode

        it "sub-ad-pure" $ do
            let st = emptyT544State
            let st' = flip execState st $ do
                    initState [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
            let st'' = flip execState st' $ do
                    sub_ad
            let ex1 = emptyT544State {
                    _s = [1634760805,857760878,2036477234,1797285236,50462976,117835012,185207048,252579084,50462976,117835012,185207048,252579084,0,0,50462976,117835012],
                    _org = [1634760805,857760878,2036477234,1797285236,50462976,117835012,185207048,252579084,50462976,117835012,185207048,252579084,0,0,0,0],
                    _nr = 20,
                    _p = 0
                    }
                ex2 = emptyT544State {
                    _s = [2035271344,2715433526,949994243,2209826416,248278142,1115411064,1239262759,1049102170,561815565,2002612488,1831192533,2276786389,2218672374,3957061604,1317051263,1161337054],
                    _org = [1634760805,857760878,2036477234,1797285236,50462976,117835012,185207048,252579084,50462976,117835012,185207048,252579084,0,0,0,0],
                    _nr = 20,
                    _p = 0
                    }
            shouldBe st' ex1
            shouldBe st'' ex2
        it "encrypt" $ do
            let st = emptyT544State
            let (v, st_) = flip runState st $ do
                    initState [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
                    encrypt $ B.unpack sampleA
            printHex $ B.pack v
            print st_
            shouldBe (B.pack v) (decodeHex_ "AD69B91809D063781CBFD3015126F845")
            let st__ = T544State {_s = [465266861,2137380109,182040084,1257646941,2144875839,2840913923,1870811444,46661506,686450996,791482033,2860098036,3788257598,4017023337,3578597266,1614016518,447986178], _org = [1634760805,857760878,2036477234,1797285236,50462976,117835012,185207048,252579084,50462976,117835012,185207048,252579084,0,0,0,0], _nr = 20, _p = 16}
            shouldBe st_ st__
        it "tencentCrc32" $ do
            let v = tencentCrc32 tab [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18]
            shouldBe v 700209753
        it "teB" $ do
            let e = B.unpack $ decodeHex_ "3196E9797221571BEC10D1EE5EF26F9A00D8A34CF2"
            let (_, r) = tencentEncryptB [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
            shouldBe r e
        it "teA" $ do
            let e = decodeHex_ "AD69B91809D063781CBFD3015126F845"
            let bs = B.pack [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
            let k = B.pack [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
            let k2 = B.pack [0,1,2,3,4,5,6,7]
            let r = tencentEncryptionA bs k k2
            shouldBe r e

        it "rc4" $ do
            let key = BArr.pack @BArr.ScrubbedBytes $ B.unpack . decodeHex_ $ "57A361242865BD15"
            let (_, encKey) = RC4.combine (RC4.initialize key) key
            let w = B.pack $ BArr.unpack encKey
            shouldBe w (decodeHex_ "CB4940DAAB19CD7A")

        it "sign" $ do
            let a = B.pack [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
            v <- sign a
            printHex v
            shouldBe (encodeHex v) "0C05ABC940339F0ADF2C28558C2BC366A91D47C27D56C335D44F3F000000007125722E00000000"