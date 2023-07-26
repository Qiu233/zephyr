{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Login where
import Zephyr.Client.Types
import Zephyr.Client.Internal
import Zephyr.Core.QQContext
import Zephyr.Core.Transport
import Control.Lens
import Control.Monad.IO.Class
import Zephyr.Core.Device

fetchQIMEI :: ClientOPM ()
fetchQIMEI = do
    (dev, ver_) <- withContext $ do
        (,) <$> use (transport . device) <*> use (transport . app_version)
    liftIO $ putStrLn "trying to fetch qimei"
    imeis <- liftIO $ requestQImei_ ver_ dev
    liftIO $ print imeis
    case imeis of
        Nothing -> do
            liftIO $ putStrLn "fetching qimei failed, retrying..."
            fetchQIMEI
        Just (q16, q36) -> do
            withContext $ do
                transport . device . qimei16 .= q16
                transport . device . qimei36 .= q36