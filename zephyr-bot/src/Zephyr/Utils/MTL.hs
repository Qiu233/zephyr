{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zephyr.Utils.MTL where
import Control.Monad.Trans.Maybe
import qualified Control.Exception as Ex


hoistMaybe  :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe m = MaybeT (pure m)


-- | Swallow all exceptions
tryMaybe :: IO a -> MaybeT IO a
tryMaybe o = MaybeT $ Ex.handle (\(_ :: Ex.SomeException) -> pure Nothing) . fmap Just $ o