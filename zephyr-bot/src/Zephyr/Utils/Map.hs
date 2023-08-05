{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Utils.Map where
import Data.Maybe
import Data.HashMap
import Data.Hashable


(?>) :: (Monoid a, Hashable k, Ord k) => Map k a -> k -> a
(?>) d k = fromMaybe mempty (Data.HashMap.lookup k d)


(!>) :: (Hashable k, Ord k) => Map k a -> k -> Maybe a
(!>) = flip Data.HashMap.lookup

mp :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
mp = flip (maybe (pure ()))