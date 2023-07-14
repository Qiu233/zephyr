{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module TLVTest where
import Test.Hspec
import qualified Data.ByteString.Lazy as B
import Data.Word
import Templates
import Common
import Zephyr.Internal.TLV
import Zephyr.Utils.Common
import Data.Bits
import qualified Zephyr.Packet.TLVBuilder as T
import Control.Monad.State
import Zephyr.Engine.Context
import Zephyr.Core.Device
import Zephyr.Core.ClientApp

constGUID :: B.ByteString
constGUID = B.pack [142, 27, 163, 177, 172, 31, 181, 137, 118, 115, 8, 126, 24, 49, 54, 169]
constTGTGTKey :: B.ByteString
constTGTGTKey = B.pack [199, 12, 183, 107, 3, 28, 81, 148, 116, 20, 229, 112, 0, 64, 152, 255]

constUIN :: Word32
constUIN = 349195854
constOSNAME :: String
constOSNAME = "android"
constOSVERSION :: String
constOSVERSION = "7.1.2"
constSIMINFO :: String
constSIMINFO = "T-Mobile"
constIMEI :: String
constIMEI = "468356291846738"

constIMEIMD5 :: B.ByteString
constIMEIMD5 = "9792b1bba1867318bf782af418306ef8"

constWIFIBSSID :: String
constWIFIBSSID = "00:50:56:C0:00:08"
constWIFISSID :: String
constWIFISSID = "<unknown ssid>"


constAPN :: String
constAPN = "wifi"
constAPKSIGN :: B.ByteString
constAPKSIGN = B.pack [
        0xA6, 0xB7, 0x45, 0xBF, 0x24, 0xA2, 0xC2, 0x77, 0x52, 0x77, 0x16, 0xF6, 0xF3, 0x6E, 0xB6,
        0x8D]


constAPKID :: String
constAPKID = "com.tencent.mobileqq"
constAPPID :: Word32
constAPPID = 537066738
constSUBAPPID :: Word32
constSUBAPPID = 537066738
constSSOVERSION :: Word32
constSSOVERSION = 15
constSDKVERSION :: String
constSDKVERSION = "6.0.0.2454"
constMISCBITMAP :: Word32
constMISCBITMAP = 184024956
constSUBSIGMAP :: Word32
constSUBSIGMAP = 0x10400
constMAINSIGMAP :: Word32
constMAINSIGMAP = 34869472
constMACADDRESS :: String
constMACADDRESS = "00:50:56:C0:00:08"
constISROOT :: Bool
constISROOT = False
constANDROIDID :: String
constANDROIDID = "QKQ1.191117.002"
constAPKVERSIONNAME :: String
constAPKVERSIONNAME = "2.0.5"
constDEVINFO :: B.ByteString
constDEVINFO = "dev_info_dev_info_dev_info_dev_info_dev_info_"
constBUILDMODEL :: String
constBUILDMODEL = "mirai"
constBUILDBRAND :: String
constBUILDBRAND = "mamoe"
constOSTYPE :: String
constOSTYPE = "android"



test1 :: IO ()
test1 = do
    printHex =<< t1 constUIN (192, 168, 1, 1)

test1b :: IO ()
test1b = do
    testHex "001B001E000000000000000000000003000000040000004800000002000000020000" $
        t1b 0 0 3 4 72 2 2

test1d :: IO ()
test1d = do
    testHex "001D000E010AF7FF7C000000000000000000" $
        t1d constMISCBITMAP

test1f :: IO ()
test1f = do
    testHex "001F002D000007616E64726F69640005372E312E32000200104368696E61204D6F62696C652047534D0000000477696669" $
        t1f constOSNAME constOSVERSION "China Mobile GSM" constAPN 2

test2 :: IO ()
test2 = do
    testHex "0002001000000006726573756C7400047369676E" $
        t2 "result" "sign"

test8 :: IO ()
test8 = do
    testHex "0008000800000001E2400000" $
        t8 123456

test10a :: IO ()
test10a = do
    testHex "010A000F343638333536323931383436373338" $
        t10a $ utf8ToBytes constIMEI

test16 :: IO ()
test16 = do
    testHex "0016004B0000000F2002FCF22002FCF28E1BA3B1AC1FB5897673087E183136A\
        \90014636F6D2E74656E63656E742E6D6F62696C6571710005322E302E350010A6B74\
        \5BF24A2C277527716F6F36EB68D" $
        t16 constSSOVERSION constAPPID constSUBAPPID constGUID constAPKID constAPKVERSIONNAME constAPKSIGN

test16a :: IO ()
test16a = do
    testHex "016A000F343638333536323931383436373338" $
        t16a $ utf8ToBytes constIMEI

test16e :: IO ()
test16e = do
    testHex "016E000F343638333536323931383436373338" $
        t16e $ utf8ToBytes constIMEI

test17a :: IO ()
test17a = do
    testHex "017A000414D04E4E" $
        t17a constUIN

test17c :: IO ()
test17c = do
    testHex "017C0011000F343638333536323931383436373338" $
        t17c constIMEI

test18 :: IO ()
test18 = do
    testHex "001800160001000006002002FCF20000000014D04E4E00000000" $
        t18 constAPPID constUIN

test33 :: IO ()
test33 = do
    testHex "003300108E1BA3B1AC1FB5897673087E183136A9" $
        t33 constGUID

test35 :: IO ()
test35 = do
    testHex "0035000400000008" $
        t35 8

test52d :: IO ()
test52d = do
    testHex "052D002D6465765F696E666F5F6465765F696E666F5F6465765F696E666F5F64\
    \65765F696E666F5F6465765F696E666F5F" $
        t52d constDEVINFO

test100 :: IO ()
test100 = do
    testHex "0100001600010000000F000000100000000200000000021410E0" $
        t100 constSSOVERSION 16 2 constMAINSIGMAP

test104 :: IO ()
test104 = do
    testHex "010400108E1BA3B1AC1FB5897673087E183136A9" $
        t104 constGUID

test106 :: IO ()
test106 = do
    printHex =<< t106 constUIN 16 constSUBAPPID constSSOVERSION (B.pack [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])
            constGUID constTGTGTKey

test107 :: IO ()
test107 = do
    testHex "01070006000300000001" $
        t107 3

test108 :: IO ()
test108 = do
    testHex "0108000F343638333536323931383436373338" $
        t108 $ utf8ToBytes constIMEI

test109 :: IO ()
test109 = do
    testHex "010900103D7FDE24DE24F54F1D8D4E0073F846CD" $
        t109 constANDROIDID

test116 :: IO ()
test116 = do
    testHex "0116000E00021410E000010400015F5E10E2" $
        t116 constMAINSIGMAP constSUBSIGMAP

test124 :: IO ()
test124 = do
    testHex "012400240007616E64726F69640005372E312E3200020008\
    \542D4D6F62696C650000000477696669" $
        t124 constOSTYPE constOSVERSION constSIMINFO constAPN
    
test128 :: IO ()
test128 = do
    testHex "0128002900000001000000001000056D6972616900108E1BA3\
    \B1AC1FB5897673087E183136A900056D616D6F65" $
        t128 False True False 16 constBUILDMODEL constGUID constBUILDBRAND

test141 :: IO ()
test141 =do
    testHex "0141001400010008542D4D6F62696C650002000477696669" $
        t141 constSIMINFO constAPN

test142 :: IO ()
test142 = do
    testHex "0142001800000014636F6D2E74656E63656E742E6D6F62696C657171" $
        t142 constAPKID

test143 :: IO ()
test143 = do
    testHex "01430003010203" $
        t143 $ B.pack [1, 2, 3]

test144 :: IO ()
test144 = do
    printHex =<< t144 constIMEI constDEVINFO constOSTYPE constOSVERSION constSIMINFO constAPN
            False True False 16 constBUILDBRAND constGUID constBUILDBRAND constTGTGTKey

test145 :: IO ()
test145 = do
    testHex "014500108E1BA3B1AC1FB5897673087E183136A9" $
        t145 constGUID

test147 :: IO ()
test147 = do
    testHex "0147001D000000100005322E302E350010A6B745BF24A2C277527716F6F36EB68D" $
        t147 16 constAPKVERSIONNAME constAPKSIGN

test154 :: IO ()
test154 = do
    testHex "0154000400003636" $
        t154 ((0x3635 + 1) .&. 0x7FFF)

test166 :: IO ()
test166 = do
    testHex "0166000101" $
        t166 1

test174 :: IO ()
test174 = do
    testHex "017400108E1BA3B1AC1FB5897673087E183136A9" $
        t174 constGUID

test177 :: IO ()
test177 = do
    testHex "01770011010AF7FF7C000A362E302E302E32343534" $
        t177 constMISCBITMAP constSDKVERSION

test187 :: IO ()
test187 = do
    testHex "0187001010D2717E7D06A80D409FBB21516EBEC0" $
        t187 constMACADDRESS

test188 :: IO ()
test188 = do
    testHex "018800103D7FDE24DE24F54F1D8D4E0073F846CD" $
        t188 constANDROIDID

test191 :: IO ()
test191 = do
    testHex "019100017F" $
        t191 127

test193 :: IO ()
test193 = do
    testHex "0193000B736F6D65207469636B6574" $
        t193 "some ticket"

test194 :: IO ()
test194 = do
    testHex "019400203937393262316262613138363733313862663738326166343138333036656638" $
        t194 constIMEIMD5

test197 :: IO ()
test197 = do
    testHex "0197000100" t197

test198 :: IO ()
test198 = do
    testHex "0198000100" t198

test202 :: IO ()
test202 = do
    testHex "02020022001030303A35303A35363A43303A30303A30000E3C756E6B6E6F776E20737369643E" $
        t202 constWIFIBSSID constWIFISSID


test401 :: IO ()
test401 = do
    testHex "040100108E1BA3B1AC1FB5897673087E183136A9" $
        t401 constGUID

test511 :: IO ()
test511 = do
    testHex "051100D3000E01000A74656E7061792E636F6D0100116F70656E6D6F62696C\
    \652E71712E636F6D01000B646F63732E71712E636F6D01000E636F6E6E6563742E7171\
    \2E636F6D01000C717A6F6E652E71712E636F6D01000A7669702E71712E636F6D010011\
    \67616D6563656E7465722E71712E636F6D01000A71756E2E71712E636F6D01000B6761\
    \6D652E71712E636F6D01000C71717765622E71712E636F6D01000D6F66666963652E71\
    \712E636F6D01000974692E71712E636F6D01000B6D61696C2E71712E636F6D01000A6D\
    \6D612E71712E636F6D" $
        t511 [  "tenpay.com",
                "openmobile.qq.com",
                "docs.qq.com",
                "connect.qq.com",
                "qzone.qq.com",
                "vip.qq.com",
                "gamecenter.qq.com",
                "qun.qq.com",
                "game.qq.com",
                "qqweb.qq.com",
                "office.qq.com",
                "ti.qq.com",
                "mail.qq.com",
                "mma.qq.com"    ]

test516 :: IO ()
test516 = do
    testHex "0516000400000000" t516

test525 :: IO ()
test525 = do
    testHex "052500160001053600108E1BA3B1AC1FB5897673087E183136A9" $
        t525 =<< t536 constGUID

test544 :: IO ()
test544 = do
    printHex =<< t544 2 9 (fromIntegral constUIN) constGUID constSDKVERSION



-- tlvTest :: SpecWith ()
-- tlvTest = do
--     describe "TLV Tests" $ do
        
--         $(tlvTestTemplate' 
--             [0x1, 0x1b, 0x1d, 0x1f, 
--             0x2, 0x8, 0x10a, 0x16,
--             0x16a, 0x16e, 0x17a, 0x17c,
--             0x18, 0x33, 0x35, 0x52d,
--             0x100, 0x104, 0x106, 0x107,
--             0x108, 0x109, 0x116, 0x124,
--             0x128, 0x141, 0x142, 0x143,
--             0x144, 0x145, 0x147, 0x154,
--             0x166, 0x174, 0x177, 0x187,
--             0x188, 0x191, 0x193, 0x194,
--             0x197, 0x198, 0x202,
--             0x401, 0x511, 0x516,
--             0x525, 0x544])

--     describe "T52D" $ do
        
--         it "t52D" $ do
--             let uin_ = 1234567890
--             let dev = generateDevice uin_
--             ctx <- newContext uin_ dev androidPhone
--             v <- evalStateT (T.t52D :: StateT Context IO B.ByteString) ctx
--             printHex v
--             putStrLn ""