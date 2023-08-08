{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Zephyr.Packet.Jce.FileStoragePushFSSvcList where
import Zephyr.Utils.Jce.Generic
import Data.Int
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Zephyr.Packet.Jce.BigData

data FileStorageServerInfo = FileStorageServerInfo {
    _server :: JceField String 1,
    _port :: JceField Int32 2
} deriving (Show, Eq, Generic)
instance Jce FileStorageServerInfo


data FileStoragePushFSSvcList = FileStoragePushFSSvcList {
    _upload_list :: JceField [FileStorageServerInfo] 0,
    _pic_download_list :: JceField [FileStorageServerInfo] 1,
    _gpic_download_list :: JceField [FileStorageServerInfo] 2,
    _qzone_proxy_service_list :: JceField [FileStorageServerInfo] 3,
    _url_encode_service_list :: JceField [FileStorageServerInfo] 4,
    _big_data_channel :: JceField BigDataChannel 5,
    _vip_emotion_list :: JceField [FileStorageServerInfo] 6,
    _c2c_pic_down_list :: JceField [FileStorageServerInfo] 7,
    _ptt_list :: JceField B.ByteString 10
} deriving (Show, Eq, Generic)
instance Jce FileStoragePushFSSvcList