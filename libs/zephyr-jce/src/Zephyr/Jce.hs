module Zephyr.Jce (
    JceField(..),
    Jce,
    jceMarshal,
    jceUnmarshal,
    jceUnmarshal_,
    jdef,
) where

import Zephyr.Jce.Generic
import qualified Data.ByteString.Lazy as B
import Zephyr.Binary
import Zephyr.Binary.Get


jceMarshal :: Jce a => a -> B.ByteString
jceMarshal = runPut . jput

jceUnmarshal :: Jce a => B.ByteString -> a
jceUnmarshal = runGet jget

jceUnmarshal_ :: Jce a => B.ByteString -> Either String a
jceUnmarshal_ = runGet_ jget