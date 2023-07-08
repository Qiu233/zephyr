{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Utils.Random where
import System.Random.Stateful
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import Control.Monad

defaultRandGen :: AtomicGenM StdGen
defaultRandGen = globalStdGen

randDefault ::  (Random a, MonadIO m) => m a
randDefault  = applyAtomicGen random defaultRandGen

randBytes :: MonadIO m => Int -> m B.ByteString
randBytes n = B.pack <$> replicateM n randDefault

randR :: (Random a, MonadIO m) => (a, a) -> m a
randR r = applyAtomicGen (randomR r) defaultRandGen