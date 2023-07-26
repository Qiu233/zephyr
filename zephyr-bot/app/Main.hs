{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where
import Network.Socket
import qualified Control.Exception as Ex
import Network.Socket.ByteString.Lazy
import Zephyr.Core.QQContext
import Data.Word
import System.Environment
import Control.Lens
import Zephyr.Utils.Common
import Zephyr.Core.Device.Types
import Zephyr.Core.Transport
import Zephyr.Core.AppVersion (androidPhone)
import Zephyr.Packet.Login.Types
import Zephyr.Packet.Login.Build
import Zephyr.Packet.Login.Parse
import Control.Monad.State
import Zephyr.Utils.Codec (md5OfU8)
import qualified Data.ByteString.Lazy as B
import Zephyr.Core.Device.QIMEI
import Control.Concurrent.STM
import Zephyr.Utils.Binary
import Zephyr.Packet.Parse
import Zephyr.Core.Request
import GHC.Stack (HasCallStack)
import Text.Printf (printf)


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
    _context :: QQContext,
    _socket :: Socket,
    _buffer :: TVar B.ByteString
}
$(makeLenses ''Client)

type ClientOPM a = StateT Client IO a

fetchQIMEI :: ClientOPM ()
fetchQIMEI = do
    dev <- use $ context . transport . device
    ver_ <- use $ context . transport . app_version
    liftIO $ putStrLn "trying to fetch qimei"
    imeis <- liftIO $ requestQImei_ ver_ dev
    liftIO $ print imeis
    case imeis of
        Nothing -> do
            liftIO $ putStrLn "fetch qimei failed, retrying..."
            fetchQIMEI
        Just (q16, q36) -> do
            context . transport . device . qimei16 .= q16
            context . transport . device . qimei36 .= q36

parsePacket' :: HasCallStack => B.ByteString -> ClientOPM QQResponse
parsePacket' bs = do
    d_ <- zoom context $ do
        parsePacket bs
    case d_ of
        Left err -> error err
        Right v -> pure v

sendP :: B.ByteString -> ClientOPM ()
sendP bs = do
    sock <- use Main.socket
    liftIO $ sendAll sock bs

login :: ClientOPM ()
login = do
    v <- zoom context buildLoginPacket
    sendP v
    p <- getPacket
    pkt <- parsePacket' p
    rsp_ <- zoom context $ decodeLoginResponse (pkt ^. resp_body . req_body)
    go rsp_
    where 
        go rsp = do
            case rsp of
                LoginSuccess -> do
                    liftIO $ putStrLn "登录成功"
                AccountFrozen -> do
                    liftIO $ putStrLn "账号被冻结"
                DeviceLockLogin -> do
                    liftIO $ putStrLn "设备锁"
                    undefined
                UnknownLoginResponse t msg -> do
                    liftIO $ putStrLn "未知错误:"
                    liftIO $ printf "code = %d\n" t
                    liftIO $ putStrLn msg
                NeedCaptcha data_ sign_ -> do
                    liftIO $ putStrLn "需要Captcha"
                    liftIO $ putStrLn "image: "
                    liftIO $ putStrLn $ encodeHex data_
                    liftIO $ putStrLn "sign: "
                    liftIO $ putStrLn $ encodeHex sign_
                SliderNeeded url -> do
                    liftIO $ putStrLn $ "链接: " ++ url
                    liftIO $ putStrLn "清输入ticket: "
                    ticket <- liftIO getLine
                    v <- zoom context $ buildTicketSubmitPacket ticket
                    sendP v
                    p <- getPacket
                    pkt <- parsePacket' p
                    rsp2 <- zoom context $ decodeLoginResponse $ pkt ^. resp_body . req_body
                    go rsp2
                VerificationNeeded msg url phone -> do
                    liftIO $ putStrLn "需要扫码或短信验证码登录"
                    liftIO $ putStrLn "请通过链接扫码后重启程序: "
                    liftIO $ putStrLn msg
                    liftIO $ putStrLn $ "链接: " ++ url
                    liftIO $ putStrLn $ "手机号(为空说明不支持): " ++ phone
                SMSNeeded msg phone -> do
                    liftIO $ putStrLn "需要短信验证码登录"
                    liftIO $ putStrLn msg
                    liftIO $ putStrLn $ "手机: " ++ phone
                TooManySMSRequest -> do
                    liftIO $ putStrLn "短信请求过于频繁"
    

clientMainInner :: ClientOPM ()
clientMainInner = do
    fetchQIMEI
    login

outputbs :: B.ByteString -> ClientOPM ()
outputbs bs = do
    liftIO $ print $ show $ B.length bs
    liftIO $ putStrLn $ encodeHex bs

clientMain :: QQContext -> IO ()
clientMain ctx = do
    _buffer <- liftIO $ newTVarIO B.empty
    liftIO $ runTCPClient "msfwifi.3g.qq.com" "8080" $ \sock -> do
        let c = Client ctx sock _buffer
        void $ execStateT clientMainInner c


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
    ctx <- newContext uin password dev androidPhone "http://127.0.0.1:6543"
    clientMain ctx