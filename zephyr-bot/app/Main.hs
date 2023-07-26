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

login :: ClientOPM ()
login = do
    v <- withContext buildLoginPacket
    pkt <- sendAndWait_ v
    rsp_ <- withContext $ decodeLoginResponse (pkt ^. pkt_body)
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
                    v <- withContext $ buildTicketSubmitPacket ticket
                    pkt <- sendAndWait_ v
                    rsp2 <- withContext $ decodeLoginResponse $ pkt ^. pkt_body
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
    _ <- startNetLoop
    login

main :: IO ()
main = do
    uin_ <- getEnv "UIN" <&> read @Word64
    password <- getEnv "PASSWORD" <&> B.fromStrict . md5OfU8
    let dev = generateDevice uin_
    ctx <- newContext uin_ password dev androidPhone "http://127.0.0.1:6543"
    clientMain ctx clientMainInner