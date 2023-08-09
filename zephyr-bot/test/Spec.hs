import Test.Hspec
import EncryptTest
--import TLVTest (tlvTest)
import JceTest
import DeviceTest

main :: IO ()
main = hspec $ do
    encryptTest
    --tlvTest
    deviceTest
    jceTest