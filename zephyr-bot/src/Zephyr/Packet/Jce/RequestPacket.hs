{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Jce.RequestPacket where

import Zephyr.Packet.Jce.JceCommon
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Jce.JceMap



data RequestPacket = RequestPacket{
    _i_version :: JceField Int16 1,
    _c_packet_type :: JceField Word8 2,
    _i_message_type :: JceField Int32 3,
    _i_request_id :: JceField Int32 4,
    _s_servant_name :: JceField String 5,
    _s_func_name :: JceField String 6,
    _s_buffer :: JceField B.ByteString 7,
    _i_timeout :: JceField Int32 8,
    _context :: JceField (JceMap String String) 9,
    _status :: JceField (JceMap String String) 10
} deriving (Show, Eq, Generic)
instance Jce RequestPacket