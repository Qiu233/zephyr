{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Message.Message where
import Data.Int
import Zephyr.Message.Elements
import qualified Zephyr.PB.Msg as PBMsg

data GroupMessage = GroupMessage {
    _id :: Int32,
    _internal_id :: Int32,
    _group_code :: Int64,
    _group_name :: String,
    _sender :: Sender,
    _time :: Int32,
    _elements :: [MsgElement],
    _original_objects :: PBMsg.Message
} deriving (Eq, Show)

data Sender = Sender {
    _uin :: Int64,
    _nickname :: String,
    _cardname :: String,
    _anonymous_info :: AnonymousInfo,
    _is_friend :: Bool
} deriving (Eq, Show)

data AnonymousInfo = AnonymousInfo {
    _anonymous_id :: String,
    _anonymous_nick :: String
} deriving (Eq, Show)