{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Core.Context where
import Zephyr.Core.Device (Device)
import Data.Word
import Zephyr.Core.AppVersion
import Zephyr.Core.Signature
import Control.Concurrent.STM
import System.Random (randomIO)
import Control.Lens hiding (Context)
import Control.Monad.State
import Prelude hiding (seq)
import Zephyr.Core.Codec
import Zephyr.Core.Transport

data Context = Context {
    _uin :: Word64,
    _transport :: Transport,

    _codec :: Codec,

    _seq :: TVar Word16
}

type ContextIOT m = (MonadState Context m, MonadIO m)

$(makeLenses ''Context)

newContext :: Word64 -> Device -> AppVersion -> IO Context
newContext _uin _device _client_version = do
    _signature <- defaultSignature _device
    _seq <- newTVarIO =<< randomIO
    _codec <- newCodec
    let _transport = Transport { .. }
    pure $ Context {..}

nextSeq :: ContextIOT m => m Word16
nextSeq = do
    seq_ <- use seq
    liftIO $ atomically $ stateTVar seq_ (\x -> (x, x+1))