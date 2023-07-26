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
import qualified Data.ByteString.Lazy as B

data QQProfile = QQProfile {
    _nickname :: String,
    _age :: Word16,
    _gender :: Word16
}

$(makeLenses ''QQProfile)

data Context = Context {
    _uin :: Word64,
    _password_md5 :: B.ByteString,
    _transport :: Transport,

    _codec :: Codec,
    _sign_server :: String,
    _seq :: TVar Word16,

    _qqprofile :: QQProfile
}

type ContextOPM = StateT Context IO

$(makeLenses ''Context)

newContext :: Word64 -> B.ByteString -> Device -> AppVersion -> String -> IO Context
newContext _uin _password_md5 _device _app_version _sign_server = do
    _signature <- defaultSignature _device
    _seq <- newTVarIO =<< randomIO
    _codec <- newCodec
    let _transport = Transport { .. }
    let _qqprofile = QQProfile { _nickname = "", _age = 0, _gender = 0 }
    pure $ Context {..}

nextSeq :: ContextOPM Word16
nextSeq = do
    seq_ <- use seq
    liftIO $ atomically $ stateTVar seq_ (\x -> (x, x+1))