module Zephyr.Client.TimeoutCache (
    newTimeoutCache,
    Zephyr.Client.TimeoutCache.insert,
    Zephyr.Client.TimeoutCache.lookup,
    TimeoutCache
) where
import Data.HashMap
import Control.Concurrent.STM
import Data.Hashable
import Control.Concurrent
import Control.Concurrent.Async

newtype TimeoutCache k v = TimeoutCache (TVar (Map k v))

newTimeoutCache :: IO (TimeoutCache k v)
newTimeoutCache = TimeoutCache <$> newTVarIO empty

insert :: (Hashable k, Ord k) => Int -> k -> v -> TimeoutCache k v -> IO (Async ())
insert timeout k v (TimeoutCache cache) = do
    atomically $ modifyTVar cache (Data.HashMap.insert k v)
    async $ do
        threadDelay timeout
        atomically $ modifyTVar cache (Data.HashMap.delete k)

lookup :: (Hashable k, Ord k) => k -> TimeoutCache k v -> IO (Maybe v)
lookup k (TimeoutCache cache) = do
    v <- readTVarIO cache
    pure $ Data.HashMap.lookup k v