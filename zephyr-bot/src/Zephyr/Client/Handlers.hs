{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Handlers where
import Zephyr.Client.Types
import Data.HashMap
import Zephyr.Client.Handlers.PushReq
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Zephyr.Client.Handlers.PbPushGroupMsg


emptyHandlers :: (Map String (QQPacket -> IO ()))
emptyHandlers = Data.HashMap.empty

setDefaultHandlers :: Client -> IO ()
setDefaultHandlers client = do
    let hs = fromList [
            ("ConfigPushSvc.PushReq", handlePushReqPacket client),
            ("OnlinePush.PbPushGroupMsg", handleGroupMessagePacket client)
            ]
    atomically $ modifyTVar (client._handlers) (Data.HashMap.union hs)
    