{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Handlers where
import Zephyr.Client.Types
import Data.HashMap
import Zephyr.Packet.Handlers.PushReq


defaultHandlers :: (Map String (QQPacket -> ClientOPM ()))
defaultHandlers =fromList [
        ("ConfigPushSvc.PushReq", handlePushReqPacket)
        ]