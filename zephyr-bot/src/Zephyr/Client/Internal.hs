{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}
module Zephyr.Client.Internal where
import Zephyr.Core.QQContext
import Control.Monad.IO.Class
import Control.Concurrent.STM
import qualified Data.ByteString.Lazy as B
import Zephyr.Client.Types
import Control.Monad
import Control.Monad.State
import Zephyr.Utils.Binary
import Control.Lens
import Network.Socket.ByteString.Lazy
import Zephyr.Packet.Build
import Zephyr.Core.Request
import Zephyr.Packet.Parse
import Control.Monad.Except
import Control.Monad.Trans.Except (throwE)
import Data.HashMap
import Control.Concurrent.Async
import Control.Monad.Reader
import Control.Concurrent
import Data.Word

withContextM :: ContextOPM a -> ReaderT Client IO a
withContextM o = do
    v <- view context
    a <- liftIO $ atomically $ takeTMVar v
    (r, a') <- liftIO $ runStateT o a
    liftIO $ atomically $ putTMVar v a'
    pure r

withContext :: ContextRM a -> ReaderT Client IO a
withContext o = do
    v <- view context
    a <- liftIO $ atomically $ readTMVar v
    liftIO $ runReaderT o a


getResponse :: ExceptT String (ReaderT Client IO) QQResponse
getResponse = do
    sock <- view Zephyr.Client.Types.socket
    v <- view in_buffer
    vs <- liftIO $ recv sock 1024
    when (B.null vs) $ throwE  "Connection closed"
    liftIO $ atomically $ modifyTVar v (<> vs)
    r <- liftIO $ atomically $ stateTVar v $ \x -> do
        if B.length x >= 4 then do
            let contentM = runGetInner (do
                    len <- get32be
                    getbs $ fromIntegral (len - 4)) x
            case contentM of
                DError "Too few bytes" -> (B.empty, x)
                Success y r -> (y, r)
                DError err -> error err
        else
            (B.empty, x)
    if B.null r then
        getResponse
    else do
        t <- lift $ withContextM $ parsePacket r
        liftEither t

getResponse_ :: ReaderT Client IO QQResponse
getResponse_ = do
    r <- runExceptT getResponse
    case r of
        Left err -> error err
        Right v -> pure v

sendBytes :: B.ByteString -> ReaderT Client IO ()
sendBytes bs = do
    v <- view out_buffer
    liftIO $ atomically $ putTMVar v bs

sendPacket :: Request -> ReaderT Client IO ()
sendPacket r = do
    bs <- withContext $ packRequest r
    sendBytes bs

netLoopSend :: ReaderT Client IO ()
netLoopSend = do
    v <- view out_buffer
    bs <- liftIO $ atomically $ takeTMVar v
    unless (B.null bs) $ do
        sock <- view Zephyr.Client.Types.socket
        liftIO $ sendAll sock bs
    netLoopSend

netLoopRecv :: ReaderT Client IO ()
netLoopRecv = do
    r <- runExceptT getResponse
    case r of
        Left err -> do
            liftIO $ putStrLn $ "packet dropped due to: " ++ err
        Right v -> do
            let seq_ = fromIntegral $ v ^. resp_body . sequence_id
            let cmd_ = v ^. resp_body . req_command
            let pkt = QQPacket {
                    _pkt_seq = seq_,
                    _pkt_cmd = cmd_,
                    _pkt_body = v ^. resp_body . req_body
                }
            handlersV_ <- view handlers
            handlers_ <- liftIO $ readTVarIO handlersV_
            let handlerM_ = Data.HashMap.lookup cmd_ handlers_
            case handlerM_ of
                Just handler_ -> do
                    handler_ pkt
                Nothing -> do
                    promisesV_ <- view promises
                    promises_ <- liftIO $ atomically $ takeTMVar promisesV_
                    case Data.HashMap.lookup seq_ promises_ of
                        Nothing -> liftIO $ do
                            putStrLn "packet discarded due to no promise: "
                            print pkt
                        Just promise -> liftIO $ do
                            s <- atomically $ tryPutTMVar promise pkt
                            unless s $ do
                                putStrLn "packet discarded due to promise already filled: "
                                print pkt
                    liftIO $ atomically $ putTMVar promisesV_ promises_
    netLoopRecv

startNetLoop :: ReaderT Client IO (Async ())
startNetLoop = do
    r <- ask
    let recv_ = runReaderT netLoopRecv r
    let send_ = runReaderT netLoopSend r
    liftIO $ async $ concurrently_ recv_ send_

waitTimeout :: Int -> Async a -> IO (Maybe a)
waitTimeout i t = do
    race (threadDelay i) (wait t) >>= \case
        Left _ -> return Nothing
        Right r -> return (Just r)

putPromise :: Word16 -> ReaderT Client IO (TMVar QQPacket)
putPromise seqID = do
    promisesV_ <- view promises
    promise <- liftIO newEmptyTMVarIO
    liftIO $ atomically $ do
        promises_ <- takeTMVar promisesV_
        putTMVar promisesV_ $ insert seqID promise promises_
        pure promise

removePromise :: Word16 -> ReaderT Client IO ()
removePromise seqID = do
    promisesV_ <- view promises
    liftIO $ atomically $ do
        promises_ <- takeTMVar promisesV_
        putTMVar promisesV_ $ delete seqID promises_

waitForPacket :: Int -> Word16 -> ExceptT String (ReaderT Client IO) QQPacket
waitForPacket timeout seqID = do
    promise <- lift $ putPromise seqID
    rst <- liftIO $ withAsync (atomically $ takeTMVar promise) $ \async_ -> do
        waitTimeout timeout async_
    lift $ removePromise seqID
    maybe (throwE "No response received within time limit.") pure rst

sendAndWait :: Request -> ExceptT String (ReaderT Client IO) QQPacket
sendAndWait r = do
    let seq_ = fromIntegral $ r ^. sequence_id
    lift $ sendPacket r
    waitForPacket 1000000 seq_

sendAndWait_ :: Request -> (ReaderT Client IO) QQPacket
sendAndWait_ r = do
    pkt <- runExceptT (sendAndWait r)
    case pkt of
        Left err -> error err
        Right v -> pure v
