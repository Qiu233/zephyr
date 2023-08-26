{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Events where
import Zephyr.Packet.JceStructs
import Data.Int
import Control.Concurrent.STM
import Data.Word

type EventHandler a = a -> IO ()

newtype ServerUpdatedEventArgs = ServerUpdatedEventArgs {
    _servers :: [SsoServerInfo]
}

data GroupMemberMutedEventArgs = GroupMemberMutedEventArgs {
    _group_code :: Int64,
    _operator_uin :: Int64,
    _target_uin :: Int64,
    _time       :: Int32
}

data GroupMessageRecalledEventArgs = GroupMessageRecalledEventArgs {
    _group_code :: Int64,
    _operator_uin :: Int64,
    _author_uin :: Int64,
    _message_id :: Int32,
    _time       :: Int32
}

data HonorType =
    HT_Unknown
    | HT_Talkative
    | HT_Performer
    | HT_Legend
    | HT_StrongNewbie
    | HT_Emotion

data GroupNotificationEventArgs =
    GroupPokeNotify Int64 Int64 Int64
    | GroupLuckKingNotify Int64 Int64 Int64
    | GroupHonerChangedNotify Int64 HonorType Int64 String

data FriendNotificationEventArgs = FriendPokeNotify Int64 Int64

data GroupDigestEventArgs = GroupDigestEventArgs {
    _group_code :: Int64,
    _message_id :: Int32,
    _internal_message_id :: Int32,
    _operation_type :: Int32,
    _operate_time :: Word32,
    _sender_uin :: Int64,
    _operator_uin :: Int64,
    _sender_nick :: String,
    _operator_nick :: String
}

data GroupMemberSpecialTitleUpdatedEventArgs = GroupMemberSpecialTitleUpdatedEventArgs {
    _group_code :: Int64,
    _uin :: Int64,
    _new_title :: String
}

data FriendMessageRecalledEventArgs = FriendMessageRecalledEventArgs {
    _friend_uin :: Int64,
    _message_id :: Int32,
    _time :: Int64
}

data FriendNewEventArgs = FriendNewEventArgs {
    _uin :: Int64,
    _nick :: String
}

data GroupRenamedEventArgs = GroupRenamedEventArgs {
    _group_code :: Int64,
    _new_name :: String,
    _operator_uin :: Int64
}

-- data GroupMessageReceiptEventArgs = GroupMessageReceiptEventArgs {
--     _rand :: Int32,
--     _seq :: Int32,
--     _msg :: 
-- }

type Event a = TVar [EventHandler a]

data Events = Events {
    _server_updated     :: Event ServerUpdatedEventArgs,
    _group_member_muted :: Event GroupMemberMutedEventArgs,
    _group_message_recalled :: Event GroupMessageRecalledEventArgs,
    _group_notified :: Event GroupNotificationEventArgs,
    _friend_notified :: Event FriendNotificationEventArgs,
    _group_digest :: Event GroupDigestEventArgs,
    _group_member_special_title_updated :: Event GroupMemberSpecialTitleUpdatedEventArgs,
    _friend_message_recalled :: Event FriendMessageRecalledEventArgs,
    _friend_new :: Event FriendNewEventArgs,
    _group_left :: Event Int64,
    _group_renamed :: Event GroupRenamedEventArgs,
    _friend_deleted :: Event Int64
}

emptyEvents :: IO Events
emptyEvents = Events
    <$> newTVarIO [] <*> newTVarIO [] <*> newTVarIO [] <*> newTVarIO []
    <*> newTVarIO [] <*> newTVarIO [] <*> newTVarIO [] <*> newTVarIO []
    <*> newTVarIO [] <*> newTVarIO [] <*> newTVarIO [] <*> newTVarIO []

dispatch :: Event a -> a -> IO ()
dispatch e a = do
    hs <- readTVarIO e
    sequence_ [h a | h <- hs]