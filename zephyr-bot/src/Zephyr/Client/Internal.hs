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
import Zephyr.Client.Log
import Text.Printf

withContextM :: ContextOPM a -> Client -> IO a
withContextM o client = do
    let v = client._context
    a <- atomically $ takeTMVar v
    (r, a') <- runStateT o a
    atomically $ putTMVar v a'
    pure r

withContext :: ContextRM a -> Client -> IO a
withContext o client = do
    let v = client._context
    a <- atomically $ readTMVar v
    runReaderT o a


getResponse :: Client -> ExceptT String IO QQResponse
getResponse client = do
    let sock = client._socket
    let v = client._in_buffer
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
        getResponse client
    else do
        t <- lift $ withContextM (parsePacket r) client
        liftEither t

getResponse_ :: Client -> IO QQResponse
getResponse_ client = do
    r <- runExceptT $ getResponse client
    case r of
        Left err -> error err
        Right v -> pure v

sendBytes :: B.ByteString -> Client ->  IO ()
sendBytes bs cli = do
    let v = cli._out_buffer
    atomically $ putTMVar v bs

sendPacket :: Request -> Client -> IO ()
sendPacket r client= do
    bs <- withContext (packRequest r) client
    sendBytes bs client

netLoopSend :: Client -> IO ()
netLoopSend client = do
    let v = client._out_buffer
    let sock = client._socket
    let rc = fix $ \k -> do
            bs <- atomically $ takeTMVar v
            unless (B.null bs) $ do
                sendAll sock bs
            k
    rc

netLoopRecv :: Client -> IO ()
netLoopRecv client = do
    let handlersV_ = client._handlers
    let promisesV_ = client._promises
    fix $ \k -> do
        r <- runExceptT $ getResponse client
        case r of
            Left err -> do
                putStrLn $ "packet dropped due to: " ++ err
            Right v -> do
                let seq_ = fromIntegral $ v ^. resp_body . sequence_id
                let cmd_ = v ^. resp_body . req_command
                let pkt = QQPacket {
                        _pkt_seq = seq_,
                        _pkt_cmd = cmd_,
                        _pkt_body = v ^. resp_body . req_body
                    }
                handlers_ <- readTVarIO handlersV_
                let handlerM_ = Data.HashMap.lookup cmd_ handlers_
                case handlerM_ of
                    Just handler_ -> do
                        handler_ pkt
                    Nothing -> do
                        promises_ <- atomically $ takeTMVar promisesV_
                        case Data.HashMap.lookup seq_ promises_ of
                            Nothing -> do
                                client._logger.logWarning $ "packet discarded due to no promise or handler: " ++ printf "command = %s" pkt._pkt_cmd
                            Just promise -> do
                                s <- atomically $ tryPutTMVar promise pkt
                                unless s $ do
                                    client._logger.logWarning $ "packet discarded due to promise already filled: " ++ printf "command = %s" pkt._pkt_cmd
                        atomically $ putTMVar promisesV_ promises_
        k

startNetLoop :: Client -> IO (Async ())
startNetLoop client = do
    let recv_ = netLoopRecv client
    let send_ = netLoopSend client
    async $ concurrently_ recv_ send_

waitTimeout :: Int -> Async a -> IO (Maybe a)
waitTimeout i t = do
    race (threadDelay i) (wait t) >>= \case
        Left _ -> return Nothing
        Right r -> return (Just r)

putPromise :: Word16 -> Client -> IO (TMVar QQPacket)
putPromise seqID client = do
    let promisesV_ = client._promises
    promise <- newEmptyTMVarIO
    atomically $ do
        promises_ <- takeTMVar promisesV_
        putTMVar promisesV_ $ insert seqID promise promises_
        pure promise

removePromise :: Word16 -> Client -> IO ()
removePromise seqID client = do
    let promisesV_ = client._promises
    atomically $ do
        promises_ <- takeTMVar promisesV_
        putTMVar promisesV_ $ delete seqID promises_

waitForPacket :: Int -> Word16 -> Client -> ExceptT String IO QQPacket
waitForPacket timeout seqID client = do
    promise <- lift $ putPromise seqID client
    rst <- liftIO $ withAsync (atomically $ takeTMVar promise) $ \async_ -> do
        waitTimeout timeout async_
    lift $ removePromise seqID client
    maybe (throwE "No response received within time limit.") pure rst

sendAndWait :: Request -> Client -> ExceptT String IO QQPacket
sendAndWait r client = do
    let seq_ = fromIntegral $ r ^. sequence_id
    lift $ sendPacket r client
    waitForPacket 1000000 seq_ client

sendAndWait_ :: Request -> Client -> IO QQPacket
sendAndWait_ r client = do
    pkt <- runExceptT (sendAndWait r client)
    case pkt of
        Left err -> error err
        Right v -> pure v
