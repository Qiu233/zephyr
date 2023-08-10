{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Events where
import Zephyr.Packet.Jce.SsoServerInfo


data ServerUpdatedEventArgs = ServerUpdatedEventArgs {
    _servers :: [SsoServerInfo]
}

-- data GroupMessageReceiptEventArgs = GroupMessageReceiptEventArgs {
--     _rand :: Int32,
--     _seq :: Int32,
--     _msg :: 
-- }