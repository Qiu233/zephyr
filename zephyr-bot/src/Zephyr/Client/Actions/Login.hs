{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Actions.Login where
import Zephyr.Client.Types
import Zephyr.Client.Internal
import Zephyr.Core.QQContext
import Zephyr.Core.Transport
import Control.Lens
import Zephyr.Core.Device
import Control.Monad.Reader
import Zephyr.Packet.Data.Login.Build
import Control.Monad.Except
import Zephyr.Packet.Data.Login.Parse
import Zephyr.Packet.Data.Login.Types

fetchQIMEI :: Client -> IO (Maybe (String, String))
fetchQIMEI client = do
    (dev_, ver_) <- flip withContext client $ do
        dev_ <- asks (._transport._device)
        ver_ <- asks (._transport._app_version)
        pure (dev_, ver_)
    requestQImei_ ver_ dev_

storeQIMEI :: Client -> String -> String -> IO ()
storeQIMEI client q16 q36 = do
    flip withContextM client $ do
        transport . device . qimei16 .= q16
        transport . device . qimei36 .= q36

registerClient :: Client -> IO (Maybe String)
registerClient client = do
    p <- withContext buildClientRegisterPacket client
    pkt <- sendAndWait_ p client
    rst <- withContext (runExceptT $ decodeClientRegisterResponse $ pkt ^. pkt_body) client
    case rst of
        Left e -> do
            pure $ Just e
        Right _ -> do
            pure Nothing

login :: Client -> IO LoginResponse
login client = do
    v <- withContext buildLoginPacket client
    pkt <- sendAndWait_ v client
    withContextM (decodeLoginResponse (pkt ^. pkt_body)) client

submitTicket :: Client -> String -> IO LoginResponse
submitTicket client ticket = do
    v <- withContext (buildTicketSubmitPacket ticket) client
    pkt <- sendAndWait_ v client
    withContextM (decodeLoginResponse (pkt ^. pkt_body)) client