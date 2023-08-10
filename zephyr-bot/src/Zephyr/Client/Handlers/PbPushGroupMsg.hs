{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
module Zephyr.Client.Handlers.PbPushGroupMsg where
import Zephyr.Client.Types
import ProtoLite
import Zephyr.Client.Log
import Zephyr.PB.Msg


handleGroupMessagePacket :: Client -> QQPacket -> IO ()
handleGroupMessagePacket client packet = do
    let pkt = decode packet._pkt_body :: PushMessagePacket
    client._logger.logInfo $ "Group message packet: " ++ show pkt
    --undefined