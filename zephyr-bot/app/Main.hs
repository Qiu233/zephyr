{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where
import Network.Socket
import qualified Control.Exception as Ex
import Network.Socket.ByteString.Lazy
import Zephyr.Core.Context
import Data.Word
import System.Environment
import Control.Lens hiding (Context)
import Zephyr.Utils.Common
import Zephyr.Core.Device.Types
import Zephyr.Core.Transport
import Zephyr.Core.AppVersion (androidPhone)
import Zephyr.Packet.Login
import Control.Monad.State
import Zephyr.Utils.Codec (md5OfU8)
import qualified Data.ByteString.Lazy as B
import Zephyr.Core.Device.QIMEI
import Control.Concurrent.STM
import Zephyr.Utils.Binary
import Control.Exception
import Zephyr.Packet.Parse
import Zephyr.Encrypt.ECDH (fetchPubKey)
import Control.Monad.Except (runExcept)
import Zephyr.Core.Request


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

data Client = Client {
    _context :: Context,
    _socket :: Socket,
    _buffer :: TVar B.ByteString
}
$(makeLenses ''Client)

type ClientOPM a = StateT Client IO a

fetchQIMEI :: ClientOPM ()
fetchQIMEI = do
    dev <- use $ context . transport . device
    ver_ <- use $ context . transport . client_version
    liftIO $ putStrLn "trying to fetch public key"
    imeis <- liftIO $ requestQImei_ ver_ dev
    liftIO $ print imeis
    case imeis of
        Nothing -> pure ()
        Just (q16, q36) -> do
            context . transport . device . qimei16 .= q16
            context . transport . device . qimei36 .= q36

parsePacket_ :: B.ByteString -> ClientOPM (Either String QQResponse)
parsePacket_ bs = do
    tr <- use $ context . transport
    pure $ runExcept $ parsePacket tr bs

clientMainInner :: B.ByteString -> ClientOPM ()
clientMainInner md5pass = do
    fetchQIMEI
    sock <- use Main.socket
    v <- zoom context $ do
        buildLoginPacket md5pass
    liftIO $ sendAll sock v
    liftIO $ putStrLn "Waiting:"
    -- bs <- liftIO $ recv sock 1024
    -- liftIO $ print $ B.length bs
    -- liftIO $ putStrLn $ encodeHex bs
    p <- getPacket
    liftIO $ print $ B.length p
    liftIO $ putStrLn $ encodeHex p
    sso <- parsePacket_ p
    case sso of
        Left err -> liftIO $ putStrLn err
        Right sso_ -> do
            liftIO $ print sso_
            let pl = sso_ ^. resp_body . req_body
            liftIO $ print $ B.length pl
            liftIO $ putStrLn $ encodeHex pl

clientMain :: Context -> B.ByteString -> IO ()
clientMain ctx md5pass = do
    _buffer <- liftIO $ newTVarIO B.empty
    liftIO $ runTCPClient "120.233.17.147" "8080" $ \sock -> do
        let c = Client ctx sock _buffer
        void $ execStateT (clientMainInner md5pass) c


getPacket :: ClientOPM B.ByteString
getPacket = do
    sock <- use Main.socket
    v <- use buffer
    vs <- liftIO $ recv sock 1024
    when (B.null vs) $ error  "Connection closed"
    liftIO $ atomically $ modifyTVar v (<> vs)
    r <- liftIO $ atomically $ stateTVar v $ \x -> do
        if B.length x >= 4 then do
            let contentM = runGetInner (do
                    len <- get32be
                    getbs $ fromIntegral (len - 4)) x
            case contentM of
                TooFewBytes -> (B.empty, x)
                Success y r -> (y, r)
        else
            (B.empty, x)
    if B.null r then
        getPacket
    else
        pure r

main :: IO ()
main = do
    uin <- getEnv "UIN" <&> read @Word64
    password <- getEnv "PASSWORD" <&> B.fromStrict . md5OfU8
    let dev = generateDevice uin
    ctx <- newContext uin dev androidPhone "http://127.0.0.1:6543"
    clientMain ctx password