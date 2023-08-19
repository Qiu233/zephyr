{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.PB.OIDB where

import Zephyr.ProtoLite.Aliases
import GHC.Generics (Generic)
import Zephyr.ProtoLite

data OIDBSSOPkg t = OIDBSSOPkg {
    _command        :: ProtoFieldOptional VInt32 1,
    _service_type   :: ProtoFieldOptional VInt32 2,
    _result         :: ProtoFieldOptional VInt32 3,
    _body           :: ProtoFieldOptional t 4,
    _error_msg      :: ProtoFieldOptional String 5,
    _client_version :: ProtoFieldOptional String 6
} deriving (Eq, Show, Generic)
instance ProtoBuf t => ProtoBuf (OIDBSSOPkg t)