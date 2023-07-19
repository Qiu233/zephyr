{-# LANGUAGE TemplateHaskell #-}
module Zephyr.Core.Transport where
import Zephyr.Core.Signature (Signature)
import Zephyr.Core.Device (Device)
import Zephyr.Core.ClientApp ( ClientApp )
import Control.Lens

data Transport = Transport {
    _signature :: Signature,
    _client_version :: ClientApp,
    _device :: Device
} deriving (Eq, Show)
$(makeLenses ''Transport)
