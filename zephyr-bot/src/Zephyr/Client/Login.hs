{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Login where
import Zephyr.Client.Types
import Zephyr.Client.Internal
import Zephyr.Core.QQContext
import Zephyr.Core.Transport
import Control.Lens
import Zephyr.Core.Device
import Control.Monad.Reader
import Zephyr.Client.Log
import Zephyr.Packet.Data.Login.Build
import Control.Monad.Except
import Zephyr.Packet.Data.Login.Parse
import Zephyr.Packet.Data.Login.Types
import Control.Concurrent.STM
import Text.Printf
import Zephyr.Utils.Common

fetchQIMEI :: Client -> IO ()
fetchQIMEI client = do
    (dev_, ver_) <- flip withContext client $ do
        dev_ <- asks (._transport._device)
        ver_ <- asks (._transport._app_version)
        pure (dev_, ver_)
    let req = requestQImei_ ver_ dev_
    let rc = fix $ \loop -> do
            imeis <- req
            case imeis of
                Nothing -> do
                    client._logger.logInfo "fetching qimei failed, retrying..."
                    loop
                Just r -> pure r
    (q16, q36) <- rc
    flip withContextM client $ do
        transport . device . qimei16 .= q16
        transport . device . qimei36 .= q36

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
            --putStrLn "客户端注册成功"
            pure ()


login :: Client -> IO Bool
login client = do
    v <- withContext buildLoginPacket client
    pkt <- sendAndWait_ v client
    rsp_ <- withContextM (decodeLoginResponse (pkt ^. pkt_body)) client
    let go = fix $ \k rsp -> do
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
                    v_ <- withContext (buildTicketSubmitPacket ticket) client
                    pkt_ <- sendAndWait_ v_ client
                    rsp2 <- withContextM (decodeLoginResponse $ pkt_ ^. pkt_body) client
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
    go rsp_
