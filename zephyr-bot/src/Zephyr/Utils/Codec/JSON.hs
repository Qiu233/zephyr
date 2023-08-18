{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Utils.Codec.JSON where

import Data.Aeson.Decoding.ByteString.Lazy
import qualified Data.ByteString.Lazy as B
import Data.Foldable

validateJSON :: B.ByteString -> Bool
validateJSON bs = do
    null . toList $ lbsToTokens bs