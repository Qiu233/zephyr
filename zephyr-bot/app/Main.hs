{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main (main) where
import Zephyr.Core.QQContext
import Data.Word
import System.Environment
import Control.Lens
import Zephyr.Utils.Common
import Zephyr.Core.Device.Types
import Zephyr.Core.AppVersion (androidPhone)
import Zephyr.Packet.Data.Login.Types
import Zephyr.Packet.Data.Login.Build
import Zephyr.Packet.Data.Login.Parse
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
import Control.Concurrent.Async
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Zephyr.Client.Log
import Zephyr.Client.Works.Group

login :: Client -> IO Bool
login client = do
    v <- withContext buildLoginPacket client
    pkt <- sendAndWait_ v client
    rsp_ <- withContextM (decodeLoginResponse (pkt ^. pkt_body)) client
    let go = fix $ \k rsp -> do
            case rsp of
                LoginSuccess -> do
                    liftIO $ putStrLn "登录成功"
                    let onlineV = client._online
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
                    v_ <- withContext (buildTicketSubmitPacket ticket) client
                    pkt_ <- sendAndWait_ v_ client
                    rsp2 <- withContextM (decodeLoginResponse $ pkt_ ^. pkt_body) client
                    k rsp2
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
    go rsp_

registerClient :: Client -> IO ()
registerClient client = do
    p <- withContext buildClientRegisterPacket client
    pkt <- sendAndWait_ p client
    rst <- withContext (runExceptT $ decodeClientRegisterResponse $ pkt ^. pkt_body) client
    case rst of
        Left e -> do
            client._logger.logError "客户端注册失败: "
            client._logger.logError e
        Right _ -> do
            --liftIO $ putStrLn "客户端注册成功"
            pure ()

beginHeartbeat :: Client -> IO (Async ())
beginHeartbeat client = do
    times <- liftIO $ newIORef (0 :: Int)
    let f = fix $ \k -> do
            online_ <- isClientOnline client
            when online_ $ do
                liftIO $ threadDelay 30_000_000
                (seq_, uin_) <- withContext ((,) <$> nextSeq <*> view uin) client
                let req_ = Request RT_Login ET_NoEncrypt (fromIntegral seq_) uin_ "Heartbeat.Alive" B.empty
                runExceptT (sendAndWait req_ client) >>= \case
                    Left e -> do
                        client._logger.logError "心跳失败: "
                        client._logger.logError e
                    Right _ -> do
                        liftIO $ modifyIORef times (+1)
                        t <- liftIO $ readIORef times
                        when (t >= 7) $ do
                            registerClient client
                            liftIO $ writeIORef times 0
                k
    liftIO $ async f


clientMainInner :: Client -> IO ()
clientMainInner client = do
    fetchQIMEI client
    _ <- startNetLoop client
    s <- login client
    when s $ do
        registerClient client
        p <- beginHeartbeat client
        gs <- runExceptT $ getGroupList client
        print gs
        wait p

main :: IO ()
main = do
    uin_ <- getEnv "UIN" <&> read @Word64
    password <- getEnv "PASSWORD" <&> B.fromStrict . md5OfU8
    qsign <- getEnv "QSIGN"

    let dev = generateDevice uin_
    ctx <- newContext uin_ password dev androidPhone qsign
    clientMain ctx clientMainInner