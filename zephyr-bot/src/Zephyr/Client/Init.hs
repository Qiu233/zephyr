{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
module Zephyr.Client.Init where
import Zephyr.Core.QQContext
import Network.Socket
import Zephyr.Client.Types
import Zephyr.Client.Internal
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
import Control.Concurrent.Async
import Data.IORef
import Control.Monad.Fix
import Control.Monad
import Control.Concurrent
import Zephyr.Core.Request
import Control.Monad.Except
import Zephyr.Client.Works.Login
import Zephyr.Client.TimeoutCache
import Zephyr.Client.Events

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
    _context <- newTMVarIO ctx
    let _logger = defaultLogger
    let _socket = sock
    _servers <- newTVarIO []
    _online <- newTVarIO False
    _net_loop <- newTVarIO False
    _out_buffer <- newTMVarIO B.empty
    _online_push_cache <- newTimeoutCache
    _promises <- newTMVarIO Data.HashMap.empty
    _events <- emptyEvents
    _handlers <- newTVarIO emptyHandlers
    _highway_session <- defaultHighwaySession (ctx ^. uin) (fromIntegral $ ctx ^. transport . app_version . sub_id)
    let c = Client {..}
    setDefaultHandlers c
    pure c

ftime :: UTCTime -> String
ftime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

defaultLogger :: Logger
defaultLogger = Logger {
    logInfo = \s -> do
        time <- getCurrentTime
        putStrLn $ printf "[%s] [INFO]: %s" (ftime time) s,
    logWarning = \s -> do
        time <- getCurrentTime
        putStrLn $ printf "[%s] [WARN]: %s" (ftime time) s,
    logError = \s -> do
        time <- getCurrentTime
        putStrLn $ printf "[%s] [ERROR]: %s" (ftime time) s,
    logDebug = \s -> do
        time <- getCurrentTime
        putStrLn $ printf "[%s] [DEBUG]: %s" (ftime time) s,
    logDump = \_ _ -> pure ()
}

beginHeartbeat :: Client -> IO (Async ())
beginHeartbeat client = do
    times <- newIORef (0 :: Int)
    async $ fix $ \k -> do
        online_ <- isClientOnline client
        when online_ $ do
            threadDelay 30_000_000
            (seq_, uin_) <- withContext ((,) <$> nextSeq <*> view uin) client
            let req_ = Request RT_Login ET_NoEncrypt (fromIntegral seq_) uin_ "Heartbeat.Alive" B.empty
            runExceptT (sendAndWait req_ client) >>= \case
                Left e -> do
                    client._logger.logError "心跳失败: "
                    client._logger.logError e
                Right _ -> do
                    modifyIORef times (+1)
                    t <- readIORef times
                    when (t >= 7) $ do
                        registerClient client
                        writeIORef times 0
            k
