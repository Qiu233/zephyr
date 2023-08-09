{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Handlers where
import Zephyr.Client.Types
import Data.HashMap
import Zephyr.Client.Handlers.PushReq
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar


emptyHandlers :: (Map String (QQPacket -> IO ()))
emptyHandlers = Data.HashMap.empty

setDefaultHandlers :: Client -> IO ()
setDefaultHandlers client = do
    let hs = fromList [
            ("ConfigPushSvc.PushReq", handlePushReqPacket client)
            ]
    atomically $ modifyTVar (client._handlers) (Data.HashMap.union hs)
    