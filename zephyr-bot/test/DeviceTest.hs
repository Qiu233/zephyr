{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE OverloadedStrings #-}
module DeviceTest where
import Test.Hspec
import Common
import Zephyr.Core.Device
import Zephyr.Utils.Common
import Zephyr.Core.Device.QIMEI
import Zephyr.Core.ClientApp (androidPhone)
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString.Lazy as B

device :: Device
device = generateDevice 1234567890


deviceTest :: SpecWith ()
deviceTest = do
    
    describe "Device Tests" $ do

        it "Validation" $ do
            print device

        it "Payload" $ do
            payload <- genRandomPayloadByDevice androidPhone device
            print payload

        it "AES" $ do
            let k = "0123456789abcdef"
            let e = aesEncrypt "abcdefg" k
            let d = aesDecrypt e k
            printHex e
            printHex d

        it "RequestQIMEI" $ do
            let e = requestQImei androidPhone device
            v <- runExceptT e
            -- print v
            case v of
                Left err -> putStrLn err
                Right payload -> print payload