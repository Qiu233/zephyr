{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}
module Zephyr.Client.Internal where
import Network.Socket
import qualified Control.Exception as Ex
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

withContextM :: ContextOPM a -> ClientOPM a
withContextM o = do
    v <- view context
    a <- liftIO $ atomically $ takeTMVar v
    (r, a') <- liftIO $ runStateT o a
    liftIO $ atomically $ putTMVar v a'
    pure r

withContext :: ContextRM a -> ClientOPM a
withContext o = do
    v <- view context
    a <- liftIO $ atomically $ readTMVar v
    liftIO $ runReaderT o a


runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    Ex.bracket (open addr) close client
    where
        resolve = do
            let hints = defaultHints { addrSocketType = Stream }
            head <$> getAddrInfo (Just hints) (Just host) (Just port)
        open addr = Ex.bracketOnError (openSocket addr) close $ \sock -> do
            connect sock $ addrAddress addr
            pure sock

clientMain :: QQContext -> ClientOPM () -> IO ()
clientMain ctx clientMainInner = do
    liftIO $ runTCPClient "msfwifi.3g.qq.com" "8080" $ \sock -> do
        c <- newClient ctx sock
        void $ runReaderT clientMainInner c


getResponse :: ExceptT String ClientOPM QQResponse
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

getResponse_ :: ClientOPM QQResponse
getResponse_ = do
    r <- runExceptT getResponse
    case r of
        Left err -> error err
        Right v -> pure v

sendPacket :: Request -> ClientOPM ()
sendPacket r = do
    bs <- withContext $ packRequest r
    v <- view out_buffer
    liftIO $ atomically $ putTMVar v bs

netLoopSend :: ClientOPM ()
netLoopSend = do
    v <- view out_buffer
    bs <- liftIO $ atomically $ takeTMVar v
    unless (B.null bs) $ do
        sock <- view Zephyr.Client.Types.socket
        liftIO $ sendAll sock bs
    netLoopSend

netLoopRecv :: ClientOPM ()
netLoopRecv = do
    r <- runExceptT getResponse
    case r of
        Left err -> do
            liftIO $ putStrLn $ "packet dropped due to: " ++ err
        Right v -> do
            let seq_ = fromIntegral $ v ^. resp_body . sequence_id
            let pkt = QQPacket {
                    _pkt_seq = seq_,
                    _pkt_cmd = v ^. resp_body . req_command,
                    _pkt_body = v ^. resp_body . req_body
                }
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

startNetLoop :: ClientOPM (Async ())
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

putPromise :: Word16 -> ClientOPM (TMVar QQPacket)
putPromise seqID = do
    promisesV_ <- view promises
    promise <- liftIO newEmptyTMVarIO
    liftIO $ atomically $ do
        promises_ <- takeTMVar promisesV_
        putTMVar promisesV_ $ insert seqID promise promises_
        pure promise

removePromise :: Word16 -> ClientOPM ()
removePromise seqID = do
    promisesV_ <- view promises
    liftIO $ atomically $ do
        promises_ <- takeTMVar promisesV_
        putTMVar promisesV_ $ delete seqID promises_

waitForPacket :: Int -> Word16 -> ExceptT String ClientOPM QQPacket
waitForPacket timeout seqID = do
    promise <- lift $ putPromise seqID
    rst <- liftIO $ withAsync (atomically $ takeTMVar promise) $ \async_ -> do
        waitTimeout timeout async_
    lift $ removePromise seqID
    maybe (throwE "No response received within time limit.") pure rst

sendAndWait :: Request -> ExceptT String ClientOPM QQPacket
sendAndWait r = do
    let seq_ = fromIntegral $ r ^. sequence_id
    lift $ sendPacket r
    waitForPacket 1000000 seq_

sendAndWait_ :: Request -> ClientOPM QQPacket
sendAndWait_ r = do
    pkt <- runExceptT (sendAndWait r)
    case pkt of
        Left err -> error err
        Right v -> pure v