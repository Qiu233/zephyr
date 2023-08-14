{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Init where
import Zephyr.Core.QQContext
import Network.Socket
import Zephyr.Client.Types
import Control.Concurrent.STM
import Data.HashMap
import qualified Data.ByteString.Lazy as B
import Zephyr.Client.Handlers
import Zephyr.Client.Highway
import Control.Lens
import Zephyr.Core.Transport
import Zephyr.Core.AppVersion
import qualified Control.Exception as Ex
import Zephyr.Client.Log
import Data.Time
import Text.Printf

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    Ex.bracket (open addr) close client
    where
        resolve = do
            let hints = defaultHints { addrSocketType = Stream }
            head <$> getAddrInfo (Just hints) (Just host) (Just port)
        open addr = Ex.bracketOnError (openSocket addr) close $ \sock -> do
            connect sock $ addrAddress addr
            pure sock


clientMain :: QQContext -> (Client -> IO ()) -> IO ()
clientMain ctx clientMainInner = do
    runTCPClient "msfwifi.3g.qq.com" "8080" $ \sock -> do
        c <- newClient ctx sock
        clientMainInner c

newClient :: QQContext -> Socket -> IO Client
newClient ctx sock = do
    let handlers_ = newTVarIO emptyHandlers
    c <- Client <$>
            newTMVarIO ctx <*>
            pure defaultLogger <*>
            pure sock <*>
            newTVarIO [] <*>
            newTVarIO False <*>
            newTVarIO False <*>
            newTVarIO B.empty <*> newEmptyTMVarIO <*> newTMVarIO Data.HashMap.empty <*>
            emptyEvents <*>
            handlers_ <*>
            defaultHighwaySession (ctx ^. uin) (fromIntegral $ ctx ^. transport . app_version . sub_id)
    setDefaultHandlers c
    pure c

defaultLogger :: Logger
defaultLogger = Logger {
    logInfo = \s -> do
        time <- getCurrentTime
        putStrLn $ printf "[%s] [INFO]: %s" (show time) s,
    logWarning = \s -> do
        time <- getCurrentTime
        putStrLn $ printf "[%s] [WARN]: %s" (show time) s,
    logError = \s -> do
        time <- getCurrentTime
        putStrLn $ printf "[%s] [ERROR]: %s" (show time) s,
    logDebug = \s -> do
        time <- getCurrentTime
        putStrLn $ printf "[%s] [DEBUG]: %s" (show time) s,
    logDump = \_ _ -> pure ()
}