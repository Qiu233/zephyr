{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE OverloadedStrings #-}
module DeviceTest where
import Test.Hspec
import Common
import Zephyr.Core.Device
import Zephyr.Core.Device.QIMEI
import Zephyr.Core.AppVersion (androidPhone)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad

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
            e <- requestQImei_ androidPhone device
            shouldNotBe e Nothing
            forM_ e print