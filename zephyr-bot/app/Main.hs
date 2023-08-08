{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
module Main (main) where
import Zephyr.Core.QQContext
import Data.Word
import System.Environment
import Control.Lens
import Zephyr.Utils.Common
import Zephyr.Core.Device.Types
import Zephyr.Core.AppVersion (androidPhone)
import Zephyr.Packet.Login.Types
import Zephyr.Packet.Login.Build
import Zephyr.Packet.Login.Parse
import Control.Monad.State
import Zephyr.Utils.Codec (md5OfU8)
import qualified Data.ByteString.Lazy as B
import Text.Printf (printf)
import Zephyr.Client.Types
import Zephyr.Client.Login
import Zephyr.Client.Internal
import Zephyr.Client.Init
import Control.Monad.Except (runExceptT)
import Control.Concurrent
import Zephyr.Core.Request
import Data.IORef
import Control.Monad.Reader
import Control.Concurrent.Async
import Control.Monad.STM
import Control.Concurrent.STM.TVar

login :: ReaderT Client IO Bool
login = do
    v <- withContext buildLoginPacket
    pkt <- sendAndWait_ v
    rsp_ <- withContextM $ decodeLoginResponse (pkt ^. pkt_body)
    go rsp_
    where
        go rsp = do
            case rsp of
                LoginSuccess -> do
                    liftIO $ putStrLn "登录成功"
                    onlineV <- view online
                    liftIO $ atomically $ writeTVar onlineV True
                    return True
                AccountFrozen -> do
                    liftIO $ putStrLn "账号被冻结"
                    return False
                DeviceLockLogin -> do
                    liftIO $ putStrLn "设备锁"
                    undefined
                UnknownLoginResponse t msg -> do
                    liftIO $ putStrLn "未知错误:"
                    liftIO $ printf "code = %d\n" t
                    liftIO $ putStrLn msg
                    return False
                NeedCaptcha data_ sign_ -> do
                    liftIO $ putStrLn "需要Captcha"
                    liftIO $ putStrLn "image: "
                    liftIO $ putStrLn $ encodeHex data_
                    liftIO $ putStrLn "sign: "
                    liftIO $ putStrLn $ encodeHex sign_
                    undefined
                SliderNeeded url -> do
                    liftIO $ putStrLn $ "链接: " ++ url
                    liftIO $ putStrLn "清输入ticket: "
                    ticket <- liftIO getLine
                    v <- withContext $ buildTicketSubmitPacket ticket
                    pkt <- sendAndWait_ v
                    rsp2 <- withContextM $ decodeLoginResponse $ pkt ^. pkt_body
                    go rsp2
                VerificationNeeded msg url phone -> do
                    liftIO $ putStrLn "需要扫码或短信验证码登录"
                    liftIO $ putStrLn "请通过链接扫码后重启程序: "
                    liftIO $ putStrLn msg
                    liftIO $ putStrLn $ "链接: " ++ url
                    liftIO $ putStrLn $ "手机号(为空说明不支持): " ++ phone
                    pure False
                SMSNeeded msg phone -> do
                    liftIO $ putStrLn "需要短信验证码登录"
                    liftIO $ putStrLn msg
                    liftIO $ putStrLn $ "手机: " ++ phone
                    undefined
                TooManySMSRequest -> do
                    liftIO $ putStrLn "短信请求过于频繁"
                    pure False

registerClient :: ReaderT Client IO ()
registerClient = do
    p <- withContext buildClientRegisterPacket
    pkt <- sendAndWait_ p
    rst <- withContext $ runExceptT $ decodeClientRegisterResponse $ pkt ^. pkt_body
    case rst of
        Left e -> do
            liftIO $ putStrLn "客户端注册失败: "
            liftIO $ print e
        Right _ -> do
            --liftIO $ putStrLn "客户端注册成功"
            pure ()

beginHeartbeat :: ReaderT Client IO (Async ())
beginHeartbeat = do
    times <- liftIO $ newIORef (0 :: Int)
    let f = fix $ \k -> do
            online_ <- isClientOnline
            when online_ $ do
                liftIO $ threadDelay 30_000_000
                (seq_, uin_) <- withContext ((,) <$> nextSeq <*> view uin)
                let req_ = Request RT_Login ET_NoEncrypt (fromIntegral seq_) uin_ "Heartbeat.Alive" B.empty
                runExceptT (sendAndWait req_) >>= \case
                    Left e -> do
                        liftIO $ putStrLn "心跳失败: "
                        liftIO $ print e
                    Right _ -> do
                        liftIO $ modifyIORef times (+1)
                        t <- liftIO $ readIORef times
                        when (t >= 7) $ do
                            registerClient
                            liftIO $ writeIORef times 0
                k
    s <- asks (runReaderT f)
    liftIO $ async s


clientMainInner :: ReaderT Client IO ()
clientMainInner = do
    fetchQIMEI
    _ <- startNetLoop
    s <- login
    when s $ do
        registerClient
        beginHeartbeat >>= liftIO . wait

main :: IO ()
main = do
    uin_ <- getEnv "UIN" <&> read @Word64
    password <- getEnv "PASSWORD" <&> B.fromStrict . md5OfU8
    qsign <- getEnv "QSIGN"

    let dev = generateDevice uin_
    ctx <- newContext uin_ password dev androidPhone qsign
    clientMain ctx clientMainInner