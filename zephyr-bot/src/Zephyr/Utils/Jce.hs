module Zephyr.Utils.Jce (
    JceField(..),
    Jce,
    JceMap,
    jceMarshal,
    jceUnmarshal
) where

import Zephyr.Utils.Jce.Generic
import Zephyr.Utils.Jce.Internal
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Binary


jceMarshal :: Jce a => a -> B.ByteString
jceMarshal = runPut . jput

jceUnmarshal :: Jce a => B.ByteString -> a
jceUnmarshal = runGet jget