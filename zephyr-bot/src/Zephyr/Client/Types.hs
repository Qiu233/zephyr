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
import Control.Monad.Reader (ReaderT, MonadIO (..))

data QQPacket = QQPacket {
    _pkt_seq :: Word16,
    _pkt_cmd :: String,
    _pkt_body :: B.ByteString
} deriving (Eq, Show)
$(makeLenses ''QQPacket)

data Client = Client {
    _context :: TMVar QQContext,
    _socket :: Socket,

    _online :: TVar Bool,

    _in_buffer :: TVar B.ByteString,
    _out_buffer :: TMVar B.ByteString,
    _promises :: TMVar(Map Word16 (TMVar QQPacket))
}
$(makeLenses ''Client)

type ClientOPM = ReaderT Client IO
newClient :: QQContext -> Socket -> IO Client
newClient ctx sock = do
    Client <$>
        newTMVarIO ctx <*> pure sock <*>
        newTVarIO False <*>
        newTVarIO B.empty <*> newEmptyTMVarIO <*> newTMVarIO Data.HashMap.empty

isClientOnline :: ClientOPM Bool
isClientOnline = do
    c <- view online
    liftIO $ readTVarIO c