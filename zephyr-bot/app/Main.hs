{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main (main) where
import Zephyr.Core.QQContext
import Data.Word
import System.Environment
import Control.Lens
import Zephyr.Core.Device.Types
import Zephyr.Core.AppVersion (androidPhone)
import Control.Monad.State
import Zephyr.Utils.Codec (md5OfU8)
import qualified Data.ByteString.Lazy as B
import Zephyr.Client.Types
import Zephyr.Client.Actions.Login
import Zephyr.Client.Internal
import Zephyr.Client.Init
import Control.Monad.Except (runExceptT)
import Control.Concurrent.Async
import Zephyr.Client.Log
import Zephyr.Client.Actions.Group
import Zephyr.Core.Entity.Group
import Control.Concurrent.STM
import Text.Printf
import Zephyr.Utils.Common
import Zephyr.Packet.Data.Login.Types

login_ :: Client -> IO Bool
login_ client = do
    rsp_ <- login client
    go rsp_
    where
        go = fix $ \k rsp -> do
            case rsp of
                LoginSuccess -> do
                    client._logger.logInfo "登录成功"
                    let onlineV = client._online
                    atomically $ writeTVar onlineV True
                    return True
                AccountFrozen -> do
                    client._logger.logError "账号被冻结"
                    return False
                DeviceLockLogin -> do
                    client._logger.logError "设备锁"
                    undefined
                UnknownLoginResponse t msg -> do
                    client._logger.logError $ "未知错误: " ++ printf "code = %d " t++ msg
                    return False
                NeedCaptcha data_ sign_ -> do
                    client._logger.logInfo "需要Captcha: " 
                    client._logger.logInfo $ "image: " ++ encodeHex data_
                    client._logger.logInfo $ "sign: " ++ encodeHex sign_
                    undefined
                SliderNeeded url -> do
                    client._logger.logInfo $ printf "需要滑块验证: "
                    client._logger.logInfo $ "链接: " ++ url
                    client._logger.logInfo "请输入ticket"
                    ticket <- getLine
                    rsp2 <- submitTicket client ticket
                    k rsp2
                VerificationNeeded msg url phone -> do
                    client._logger.logInfo "需要扫码或短信验证码登录"
                    client._logger.logInfo "请通过链接扫码后重启程序: "
                    client._logger.logInfo msg
                    client._logger.logInfo $ "链接: " ++ url
                    client._logger.logInfo $ "手机号(为空说明不支持): " ++ phone
                    pure False
                SMSNeeded msg phone -> do
                    client._logger.logInfo "需要短信验证码登录"
                    client._logger.logInfo msg
                    client._logger.logInfo $ "手机: " ++ phone
                    pure False
                TooManySMSRequest -> do
                    client._logger.logError "短信请求过于频繁"
                    pure False

fetchAndStoreQIMEI :: Client -> IO ()
fetchAndStoreQIMEI client = do
    let req = fetchQIMEI client
    (q16, q36) <- fix $ \loop -> do
        imeis <- req
        case imeis of
            Nothing -> do
                client._logger.logInfo "fetching qimei failed, retrying..."
                loop
            Just r -> pure r
    storeQIMEI client q16 q36

clientMainInner :: Client -> IO ()
clientMainInner client = do
    fetchAndStoreQIMEI client
    _ <- startNetLoop client
    s <- login_ client
    when s $ do
        _ <- registerClient client
        p <- beginHeartbeat client
        gs <- runExceptT $ fetchGroupList client
        case gs of
            Left e -> do
                client._logger.logError "获取群列表失败: "
                client._logger.logError e
            Right (g:_) -> do
                print gs
                gi <- runExceptT $ fetchGroupInfo client g._code
                print gi
            Right _ -> pure ()
        wait p

main :: IO ()
main = do
    uin_ <- getEnv "UIN" <&> read @Word64
    password <- getEnv "PASSWORD" <&> B.fromStrict . md5OfU8
    qsign <- getEnv "QSIGN"

    let dev = generateDevice uin_
    ctx <- newContext uin_ password dev androidPhone qsign
    clientMain ctx clientMainInner