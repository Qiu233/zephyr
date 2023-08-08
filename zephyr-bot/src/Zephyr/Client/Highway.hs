{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Highway where
import Data.Int
import qualified Data.ByteString.Lazy as B
import Data.Word
import Network.Socket
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Concurrent.STM

type HWAddr = (Word32, Int)

data HighwaySession = HighwaySession {
    _hw_uin :: String,
    _hw_app_id :: Int32,
    _hw_sig_session :: TVar B.ByteString,
    _hw_session_key :: TVar B.ByteString,

    _hw_seq :: TVar Int32,

    _hw_sso_addr :: TVar (Int32, [HWAddr]),

    _hw_idle :: TVar (Int, [(Socket, HWAddr, Int64)])
}
$(makeLenses ''HighwaySession)


defaultHighwaySession :: Word64 -> Int32 -> IO HighwaySession
defaultHighwaySession uin_ _hw_app_id = do
    hw_sso_addr_ <- newTVarIO (0, [])
    hw_idle_ <- newTVarIO (0, [])
    let _hw_uin = show uin_


        _hw_sso_addr = hw_sso_addr_

        _hw_idle = hw_idle_
    _hw_sig_session <- newTVarIO B.empty
    _hw_session_key <- newTVarIO B.empty
    _hw_seq <- newTVarIO 0
    pure HighwaySession {..}

appendAddrs :: HighwaySession -> [HWAddr] -> IO ()
appendAddrs HighwaySession {..} addrs = do
    atomically $ modifyTVar _hw_sso_addr $ \(idx, xs) -> (idx, xs ++ addrs)