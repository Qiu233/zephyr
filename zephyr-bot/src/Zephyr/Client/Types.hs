{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Types where
import Zephyr.Core.QQContext
import Network.Socket
import Control.Concurrent.STM
import qualified Data.ByteString.Lazy as B
import Control.Lens
import Data.HashMap
import Data.Word
import Zephyr.Client.Highway
import Zephyr.Client.Events
import Zephyr.Client.Log
import Zephyr.Client.TimeoutCache

data QQPacket = QQPacket {
    _pkt_seq :: Word16,
    _pkt_cmd :: String,
    _pkt_body :: B.ByteString
} deriving (Eq, Show)
$(makeLenses ''QQPacket)

data Client = Client {
    _context :: TMVar QQContext,
    _logger :: Logger,
    _socket :: Socket,
    _servers :: TVar [(String, Int)],

    _online :: TVar Bool,
    _net_loop :: TVar Bool,

    _out_buffer :: TMVar B.ByteString,
    _promises :: TMVar(Map Word16 (TMVar QQPacket)),
    _online_push_cache :: TimeoutCache String (),

    _events :: Events,
    _handlers :: TVar (Map String (QQPacket -> IO ())),
    _highway_session :: HighwaySession
}
$(makeLenses ''Client)

isClientOnline :: Client -> IO Bool
isClientOnline client = do
    let c = client._online
    readTVarIO c

getUIN :: Client -> IO Word64
getUIN client = do
    let c = client._context
    ctx <- atomically $ readTMVar c
    pure ctx._uin