{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Handlers where
import Zephyr.Client.Types
import Data.HashMap
import Zephyr.Client.Handlers.PushReq

defaultHandlers :: (Map String (QQPacket -> Client -> IO ()))
defaultHandlers =fromList [
        ("ConfigPushSvc.PushReq", handlePushReqPacket)
        ]
