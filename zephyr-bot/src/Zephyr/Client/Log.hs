{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Log where
import qualified Data.ByteString.Lazy as B


data Logger = Logger {
    logInfo :: String -> IO (),
    logWarning :: String -> IO (),
    logError :: String -> IO (),
    logDebug :: String -> IO (),
    logDump :: B.ByteString -> String -> IO ()
}