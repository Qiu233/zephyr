{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Zephyr.Core.ClientApp (androidPhone)
import Zephyr.Packet.Login
import Control.Monad.State
import Zephyr.Utils.Codec (md5OfU8)
import qualified Data.ByteString.Lazy as B
import Zephyr.Core.Device.QIMEI
import Control.Concurrent.STM
import Zephyr.Utils.Binary
import Control.Exception
import Zephyr.Engine.Packet.Parse
import Zephyr.Encrypt.ECDH (fetchPubKey)


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

clientMain :: (ContextIOT m) => B.ByteString -> m ()
clientMain md5pass = do
    buffer <- liftIO $ newTVarIO B.empty
    let getP = getPacket buffer
    v <- passwordLoginPacket md5pass
    s <- syncTimeDiffPacket
    liftIO $ print $ B.length v
    liftIO $ putStrLn $ encodeHex v
    ctx <- get
    liftIO $ runTCPClient "120.233.17.147" "8080" $ \sock -> do
        sendAll sock s
        vs <- getP sock
        sso <- parsePacket ctx vs
        let p = sso ^. payload
        print $ B.length p
        putStrLn $ encodeHex p

        sendAll sock v
        putStrLn "Waiting:"
        bs <- recv sock 1024
        print $ B.length bs
        putStrLn $ encodeHex bs


getPacket :: TVar B.ByteString -> Socket -> IO B.ByteString
getPacket v sock = do
    vs <- recv sock 1024
    when (B.null vs) $ error  "Connection closed"
    atomically $ modifyTVar v (<> vs)
    r <- atomically $ stateTVar v $ \x -> do
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
        getPacket v sock
    else
        pure r

main :: IO ()
main = do
    uin <- getEnv "UIN" <&> read @Word64
    password <- getEnv "PASSWORD" <&> B.fromStrict . md5OfU8
    let dev = generateDevice uin
    ctx <- newContext uin dev androidPhone
    putStrLn "trying to fetch public key"
    imeis <- requestQImei_ androidPhone dev
    print imeis
    let ctx_ = maybe ctx (
            \(q16, q36) ->
                ctx & transport . device . qimei16 .~ q16
                    & transport . device . qimei36 .~ q36
            ) imeis
    evalStateT (clientMain password) ctx_