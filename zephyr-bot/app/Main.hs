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
    liftIO $ putStrLn "trying to fetch qimei"
    imeis <- liftIO $ requestQImei_ ver_ dev
    liftIO $ print imeis
    case imeis of
        Nothing -> pure ()
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
            liftIO $ print rsp
            if rsp ^. success then
                liftIO $ putStrLn "Login Success"
            else
                case rsp ^. login_error of
                    NoResponse -> do
                        liftIO $ putStrLn "无有效响应，请检查Parse算法"
                    UnknownLoginError -> do
                        liftIO $ putStrLn "未知错误:"
                        liftIO $ printf "code = %d\n" (rsp ^. code)
                        liftIO $ putStrLn $ rsp ^. error_message
                    NeedCaptcha -> do -- impossible
                        liftIO $ putStrLn "需要Captcha"
                        liftIO $ putStrLn "image: "
                        liftIO $ putStrLn $ encodeHex $ rsp ^. captcha_image
                        liftIO $ putStrLn "sign: "
                        liftIO $ putStrLn $ encodeHex $ rsp ^. captcha_sign
                    SliderNeededError -> do
                        liftIO $ putStrLn $ "链接: " ++ rsp ^. verify_url
                        liftIO $ putStrLn "清输入ticket: "
                        ticket <- liftIO getLine
                        v <- zoom context $ buildTicketSubmitPacket ticket
                        sendP v
                        p <- getPacket
                        pkt <- parsePacket' p
                        rsp2 <- zoom context $ decodeLoginResponse $ pkt ^. resp_body . req_body
                        go rsp2
                    SMSOrVerifyNeededError -> do
                        liftIO $ putStrLn "需要扫码或验证码(已废除)"
                        liftIO $ putStrLn "请通过链接扫码后重启程序: "
                        liftIO $ putStrLn $ rsp ^. error_message
                        liftIO $ putStrLn $ "链接: " ++ rsp ^. verify_url
                    SMSNeededError -> do -- impossible
                        liftIO $ putStrLn "需要短信认证"
                        liftIO $ putStrLn $ rsp ^. error_message
                        liftIO $ putStrLn $ "手机: " ++ rsp ^. sms_phone
                    UnsafeDeviceError -> do
                        liftIO $ putStrLn "不安全设备，需要扫码认证"
                        liftIO $ putStrLn $ "链接: " ++ rsp ^. verify_url
                    TooManySMSRequestError -> do -- impossible
                        liftIO $ putStrLn "短信请求过于频繁"
                    OtherLoginError -> do
                        liftIO $ putStrLn "其他错误: "
                        liftIO $ printf "code = %d\n" (rsp ^. code)
                        liftIO $ putStrLn $ rsp ^. error_message
    

clientMainInner :: ClientOPM ()
clientMainInner = do
    fetchQIMEI
    login

outputbs :: B.ByteString -> ClientOPM ()
outputbs bs = do
    liftIO $ print $ show $ B.length bs
    liftIO $ putStrLn $ encodeHex bs

clientMain :: Context -> IO ()
clientMain ctx = do
    _buffer <- liftIO $ newTVarIO B.empty
    liftIO $ runTCPClient "120.233.17.147" "8080" $ \sock -> do
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