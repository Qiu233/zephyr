module Zephyr.Utils.Jce (
    JceField(..),
    Jce,
    jceMarshal,
    jceUnmarshal,
    jceUnmarshal_,
    jdef,
) where
import GHC.Stack

import Zephyr.Utils.Jce.Generic
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Binary


jceMarshal :: Jce a => a -> B.ByteString
jceMarshal = runPut . jput

jceUnmarshal :: HasCallStack => Jce a => B.ByteString -> a
jceUnmarshal = runGet jget

jceUnmarshal_ :: Jce a => B.ByteString -> Either String a
jceUnmarshal_ = runGet_ jget