module DeviceTest where
import Test.Hspec
import Zephyr.Core.Device (generateDevice)

deviceTest :: SpecWith ()
deviceTest = do
    
    describe "Device Tests" $ do

        it "Validation" $ do
            dev <- generateDevice 1234567890
            print dev