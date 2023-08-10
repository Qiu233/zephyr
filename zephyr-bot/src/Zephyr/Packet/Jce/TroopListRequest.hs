{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Jce.TroopListRequest where
import Zephyr.Utils.Jce
import Data.Int
import Data.Word
import qualified Data.ByteString.Lazy as B
import GHC.Generics


data TroopListRequest = TroopListRequest {
    _uin :: JceField Int64 0,
    _get_msf_msg_flag :: JceField Word8 1,
    _cookies :: JceField B.ByteString 2,
    _group_info :: JceField [Int64] 3,
    _group_flag_ext :: JceField Word8 4,
    _version :: JceField Int32 5,
    _company_id :: JceField Int64 6,
    _version_num :: JceField Int64 7,
    _get_long_group_name :: JceField Word8 8
} deriving (Show, Generic)
instance Jce TroopListRequest