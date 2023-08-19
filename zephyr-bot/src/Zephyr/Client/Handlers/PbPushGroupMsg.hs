{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Handlers.PbPushGroupMsg where
import Zephyr.Client.Types
import Zephyr.ProtoLite
import Zephyr.Client.Log
import Zephyr.PB.Msg
import Zephyr.Packet.Data.Message (parseMessageElems)
import Text.Printf


handleGroupMessagePacket :: Client -> QQPacket -> IO ()
handleGroupMessagePacket client packet = do
    let m = decode packet._pkt_body :: PushMessagePacket
    -- client._logger.logInfo $ "Group message packet: " ++ show m
    let s = repeated' m._message.optOrDef._body.optOrDef._rich_text.optOrDef._elems
    let es = parseMessageElems s
    client._logger.logInfo $ printf "Parsed message elements:  %s" $ concatMap (\x -> (printf "{%s} " $ show x) :: String) es
    --undefined