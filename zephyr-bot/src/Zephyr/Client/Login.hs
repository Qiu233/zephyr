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