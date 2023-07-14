import Test.Hspec
import EncryptTest
--import TLVTest (tlvTest)
import DeviceTest
import T544Test

main :: IO ()
main = hspec $ do
    encryptTest
    t544Test
    --tlvTest
    deviceTest