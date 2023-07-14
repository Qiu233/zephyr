{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where
import Network.Socket
import qualified Control.Exception as Ex
import Network.Socket.ByteString.Lazy
import Zephyr.Engine.Context (newContext, Context, ContextIOT, device)
import Data.Word
import System.Environment
import Control.Lens hiding (Context)
import Zephyr.Utils.Common
import Zephyr.Core.Device.Types
import Zephyr.Core.ClientApp (androidPhone)
import Zephyr.Packet.Login
import Control.Monad.State
import Zephyr.Utils.Codec (md5OfU8)
import qualified Data.ByteString.Lazy as B
import Zephyr.Core.Device.QIMEI


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
    v <- passwordLoginPacket md5pass
    s <- syncTimeDiffPacket
    liftIO $ print $ B.length v
    liftIO $ putStrLn $ encodeHex v
    liftIO $ runTCPClient "120.233.17.147" "8080" $ \sock -> do
        sendAll sock s
        putStrLn "Waiting:"
        vs <- recv sock 1024
        print $ B.length vs
        putStrLn $ encodeHex vs

main :: IO ()
main = do
    uin <- getEnv "UIN" <&> read @Word64
    password <- getEnv "PASSWORD" <&> B.fromStrict . md5OfU8
    let dev = generateDevice uin
    ctx <- newContext uin dev androidPhone
    imeis <- requestQImei_ androidPhone dev
    print imeis
    let ctx_ = either (const ctx) (
            \(q16, q36) ->
                ctx & device . qimei16 .~ q16
                    & device . qimei36 .~ q36
            ) imeis
    evalStateT (clientMain password) ctx_