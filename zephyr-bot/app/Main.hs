{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main (main) where
import Zephyr.Core.QQContext
import Data.Word
import System.Environment
import Control.Lens
import Zephyr.Core.Device.Types
import Zephyr.Core.AppVersion (androidPhone)
import Control.Monad.State
import Zephyr.Utils.Codec (md5OfU8)
import qualified Data.ByteString.Lazy as B
import Zephyr.Client.Types
import Zephyr.Client.Works.Login
import Zephyr.Client.Internal
import Zephyr.Client.Init
import Control.Monad.Except (runExceptT)
import Control.Concurrent.Async
import Zephyr.Client.Log
import Zephyr.Client.Works.Group
import Zephyr.Core.Entity.Group


clientMainInner :: Client -> IO ()
clientMainInner client = do
    fetchQIMEI client
    _ <- startNetLoop client
    s <- login client
    when s $ do
        registerClient client
        p <- beginHeartbeat client
        gs <- runExceptT $ fetchGroupList client
        case gs of
            Left e -> do
                client._logger.logError "获取群列表失败: "
                client._logger.logError e
            Right (g:_) -> do
                print gs
                gi <- runExceptT $ fetchGroupInfo client g._code
                print gi
            Right _ -> pure ()
        wait p

main :: IO ()
main = do
    uin_ <- getEnv "UIN" <&> read @Word64
    password <- getEnv "PASSWORD" <&> B.fromStrict . md5OfU8
    qsign <- getEnv "QSIGN"

    let dev = generateDevice uin_
    ctx <- newContext uin_ password dev androidPhone qsign
    clientMain ctx clientMainInner