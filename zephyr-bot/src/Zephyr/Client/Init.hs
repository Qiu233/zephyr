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
import Control.Monad.IO.Class
import qualified Control.Exception as Ex
import Control.Monad
import Control.Monad.Reader

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


clientMain :: QQContext -> ClientOPM () -> IO ()
clientMain ctx clientMainInner = do
    liftIO $ runTCPClient "msfwifi.3g.qq.com" "8080" $ \sock -> do
        c <- newClient ctx sock
        void $ runReaderT clientMainInner c

newClient :: QQContext -> Socket -> IO Client
newClient ctx sock = do
    Client <$>
        newTMVarIO ctx <*>
        pure sock <*>
        newTVarIO [] <*>
        newTVarIO False <*>
        newTVarIO B.empty <*> newEmptyTMVarIO <*> newTMVarIO Data.HashMap.empty <*>
        emptyEvents <*>
        newTVarIO defaultHandlers <*>
        defaultHighwaySession (ctx ^. uin) (fromIntegral $ ctx ^. transport . app_version . sub_id)
