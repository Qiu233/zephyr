{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Jce.SsoServerInfo where
import Zephyr.Utils.Jce
import GHC.Generics
import Data.Int

data SsoServerInfo = SsoServerInfo {
    _server :: JceField String 1,
    _port :: JceField Int32 2,
    _location :: JceField String 8
} deriving (Show, Eq, Generic)
instance Jce SsoServerInfo