{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Core.Transport where
import Zephyr.Core.Signature (Signature)
import Zephyr.Core.Device (Device)
import Zephyr.Core.AppVersion ( AppVersion )
import Control.Lens

data Transport = Transport {
    _signature :: Signature,
    _app_version :: AppVersion,
    _device :: Device
} deriving (Eq, Show)
$(makeLenses ''Transport)
