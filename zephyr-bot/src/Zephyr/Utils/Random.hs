{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Utils.Random where
import System.Random.Stateful
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import Control.Monad



randBytes :: MonadIO m => Int -> m B.ByteString
randBytes n = B.pack <$> replicateM n randomIO

randPick :: MonadIO m => [b] -> m b
randPick xs = do
    i <- randomRIO (0, length xs - 1)
    pure $ xs !! i

randPickn :: MonadIO m => Int -> [b] -> m [b]
randPickn n xs = do
    is <- replicateM n $ randomRIO (0, length xs - 1)
    pure $ map (xs !!) is

randR :: (Random a, MonadIO m) => (a, a) -> m a
randR = randomRIO