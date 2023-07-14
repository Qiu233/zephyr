{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Utils.Time where
import Data.Time.Clock.POSIX
import Control.Monad.IO.Class

-- | Get epoch time in seconds.
getEpochTime :: MonadIO m => m Int
getEpochTime = liftIO $ round <$> getPOSIXTime

getEpochTimeMS :: MonadIO m => m Int
getEpochTimeMS = liftIO $ round . (* 1000) <$> getPOSIXTime