{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Engine.Context where
import Zephyr.Core.Device (Device)
import Data.Word
import Zephyr.Core.ClientApp
import Zephyr.Core.Signature
import Control.Concurrent.STM
import System.Random (randomIO)
import Control.Lens hiding (Context)
import Control.Monad.State
import Zephyr.Encrypt.ECDH
import Prelude hiding (seq)

data Context = Context {
    _uin :: Word64,
    _device :: Device,
    _client_app :: ClientApp,
    _signature :: Signature,

    _ecdh :: EncryptECDH,

    _seq :: TVar Word32
}

class (MonadState Context m, MonadIO m) => ContextIOT m

$(makeLenses ''Context)

newContext :: Word64 -> Device -> ClientApp -> IO Context
newContext _uin _device _client_app = do
    _signature <- defaultSignature
    _seq <- newTVarIO =<< randomIO
    _ecdh <- generateDefaultKey
    pure $ Context {..}

readSeq :: ContextIOT m => m Word32
readSeq = do
    seq_ <- use seq
    liftIO $ readTVarIO seq_