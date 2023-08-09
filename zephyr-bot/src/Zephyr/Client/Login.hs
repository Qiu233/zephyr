{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Login where
import Zephyr.Client.Types
import Zephyr.Client.Internal
import Zephyr.Core.QQContext
import Zephyr.Core.Transport
import Control.Lens
import Control.Monad.IO.Class
import Zephyr.Core.Device
import Control.Monad.Reader

fetchQIMEI :: Client -> IO ()
fetchQIMEI client = do
    withContextM rc client
    where 
        rc = fix $ \fetchQIMEI_ -> do
            dev <- use (transport . device)
            ver_ <- use (transport . app_version)
            liftIO $ putStrLn "trying to fetch qimei"
            imeis <- liftIO $ requestQImei_ ver_ dev
            liftIO $ print imeis
            case imeis of
                Nothing -> do
                    liftIO $ putStrLn "fetching qimei failed, retrying..."
                    fetchQIMEI_
                Just (q16, q36) -> do
                    transport . device . qimei16 .= q16
                    transport . device . qimei36 .= q36