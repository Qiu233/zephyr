{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
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
import Prelude hiding (seq)
import Zephyr.Core.Codec

data Context = Context {
    _uin :: Word64,
    _device :: Device,
    _client_app :: ClientApp,
    _signature :: Signature,

    _codec :: Codec,

    _seq :: TVar Word32
}

type ContextIOT m = (MonadState Context m, MonadIO m)

$(makeLenses ''Context)

newContext :: Word64 -> Device -> ClientApp -> IO Context
newContext _uin _device _client_app = do
    _signature <- defaultSignature
    _seq <- newTVarIO =<< randomIO
    _codec <- newCodec
    pure $ Context {..}

nextSeq :: ContextIOT m => m Word32
nextSeq = do
    seq_ <- use seq
    liftIO $ atomically $ stateTVar seq_ (\x -> (x, x+1))