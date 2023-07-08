{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Utils.Time where
import Data.Time.Clock.POSIX

-- | Get epoch time in seconds.
getEpochTime :: IO Int
getEpochTime = round <$> getPOSIXTime

getEpochTimeMS :: IO Int
getEpochTimeMS = round . (* 1000) <$> getPOSIXTime