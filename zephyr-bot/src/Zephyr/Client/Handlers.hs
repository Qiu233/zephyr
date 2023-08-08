{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Handlers where
import Zephyr.Client.Types
import Data.HashMap
import Zephyr.Packet.Handlers.PushReq
import Control.Monad.Reader


defaultHandlers :: (Map String (QQPacket -> ReaderT Client IO ()))
defaultHandlers =fromList [
        ("ConfigPushSvc.PushReq", handlePushReqPacket)
        ]