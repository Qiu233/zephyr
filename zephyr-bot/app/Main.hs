{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.Except (runExceptT)

login :: ClientOPM Bool
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
                    undefined
                SMSNeeded msg phone -> do
                    liftIO $ putStrLn "需要短信验证码登录"
                    liftIO $ putStrLn msg
                    liftIO $ putStrLn $ "手机: " ++ phone
                    undefined
                TooManySMSRequest -> do
                    liftIO $ putStrLn "短信请求过于频繁"
                    pure False

registerClient :: ClientOPM ()
registerClient = do
    p <- withContext buildClientRegisterPacket
    pkt <- sendAndWait_ p
    rst <- withContext $ runExceptT $ decodeClientRegisterResponse $ pkt ^. pkt_body
    case rst of
        Left e -> do
            liftIO $ putStrLn "客户端注册失败: "
            liftIO $ print e
        Right _ -> do
            liftIO $ putStrLn "客户端注册成功"


clientMainInner :: ClientOPM ()
clientMainInner = do
    fetchQIMEI
    _ <- startNetLoop
    s <- login
    when s $ do
        registerClient

main :: IO ()
main = do
    uin_ <- getEnv "UIN" <&> read @Word64
    password <- getEnv "PASSWORD" <&> B.fromStrict . md5OfU8
    let dev = generateDevice uin_
    ctx <- newContext uin_ password dev androidPhone "http://127.0.0.1:6543"
    clientMain ctx clientMainInner