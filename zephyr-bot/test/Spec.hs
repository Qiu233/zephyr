import Test.Hspec
import EncryptTest
--import TLVTest (tlvTest)
import DeviceTest

main :: IO ()
main = hspec $ do
    encryptTest
    --tlvTest
    deviceTest