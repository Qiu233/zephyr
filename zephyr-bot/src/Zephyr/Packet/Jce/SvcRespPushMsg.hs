{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
module Zephyr.Packet.Jce.SvcRespPushMsg where
import Data.Int
import Zephyr.Utils.Jce.Generic
import GHC.Generics
import qualified Data.ByteString.Lazy as B

data SvcRespPushMsg a = SvcRespPushMsg {
    _uin          :: JceField Int64 0,
    _del_infos    :: JceField [a] 1,
    _svr_ip       :: JceField Int32 2,
    _push_token   :: JceField B.ByteString 3,
    _service_type :: JceField Int32 4
} deriving (Show, Eq, Generic)
instance Jce a => Jce (SvcRespPushMsg a)