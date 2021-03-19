{-# LANGUAGE MultiWayIf #-}
module Ergvein.Index.Server.TCPService.Server where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async.Lifted (Async,async)
import Control.Concurrent.STM
import Control.Immortal                (Thread,create,stop)
import Control.Exception               (AsyncException(..),throwIO)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Random
import Control.Monad.Trans.Except
import qualified Control.Concurrent.Async as Async
import Data.Attoparsec.ByteString
import Data.ByteString.Builder
import Data.Either.Combinators
import Data.Set (Set)
import Data.Word
import Network.Socket

import Ergvein.Text
import Ergvein.Index.Protocol.Deserialization
import Ergvein.Index.Protocol.Serialization
import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Metrics
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.TCPService.MessageHandler
import Ergvein.Index.Server.TCPService.Socket as S

import qualified Data.ByteString as BS
import qualified Data.Set        as Set
import qualified Network.Socket.ByteString as NS


----------------------------------------------------------------
-- Async stuff
----------------------------------------------------------------

-- | Unhandled exception in child thread.
data ExceptionInLinkedThread = ExceptionInLinkedThread SomeException
  deriving Show
instance Exception ExceptionInLinkedThread

-- | Create worker thread
withLinkedWorker
  :: (MonadBaseControl IO m)
  => m a -> (Async () -> m b) -> m b
withLinkedWorker action cont = restoreM =<< do
  -- FIXME: test that we correctly deal with blocking calls with
  --        throwTo. async use uninterruptibleCancel for example.
  liftBaseWith $ \runInIO -> do
    tid <- myThreadId
    mask $ \restore -> do
      -- Here we spawn worker thread which will throw unhandled exception to main thread.
      a <- Async.async $ restore (runInIO action) `catch` \e -> do
        unless (ignoreException e) $ throwTo tid (ExceptionInLinkedThread e)
        throwIO e
      restore (runInIO (cont (() <$ a))) `finally` Async.cancel a

-- | Same as 'withLinkedWorker' for use in cases when
withLinkedWorker_ :: (MonadBaseControl IO m) => m a -> m b -> m b
withLinkedWorker_ action = withLinkedWorker action . const

-- Exception to ignore for linked threads
ignoreException :: SomeException -> Bool
ignoreException e
  | Just Async.AsyncCancelled <- fromException e = True
  | Just ThreadKilled         <- fromException e = True
  | otherwise = False


-- | Set of worker threads
newtype WorkersUnion = WorkersUnion (TVar (Set ThreadId))

-- | Create handler for set of worker threads which all will be
-- limited once 'withWorkersUnion' terminates.
withWorkersUnion
  :: (MonadIO m, MonadMask m)
  => (WorkersUnion -> m a) -> m a
withWorkersUnion = bracket ini fini
  where
    ini  = WorkersUnion <$> liftIO (newTVarIO mempty)
    fini (WorkersUnion tidsVar) = liftIO $ do
      tids <- readTVarIO tidsVar
      forM_ tids $ \tid -> forkIO $ throwTo tid Async.AsyncCancelled

-- | Spawn worker thread which will be terminated when
-- 'withWorkersUnion' exits.
spawnWorker
  :: (MonadBaseControl IO m, MonadMask m)
  => WorkersUnion -> m a -> m ()
spawnWorker (WorkersUnion tidsVar) action = do
  mask_ $ do
    a <- async action
    liftBase $ atomically $ modifyTVar' tidsVar $ Set.insert (Async.asyncThreadId a)

----------------------------------------------------------------
-- Server
----------------------------------------------------------------


runTcpSrv :: ServerM Thread
runTcpSrv = create $ logOnException "runTcpSrv" . tcpSrv

tcpSrv :: Thread -> ServerM ()
tcpSrv thread = do
  cfg <- serverConfig
  let port = show . cfgServerTcpPort $ cfg
      host = cfgServerHostname cfg
  unlift <- askUnliftIO
  liftIO $ withSocketsDo $ do
    addr <- resolve port host
    putStrLn $ "Starting TCP server on " <> show addr
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock numberOfQueuedConnections
    unliftIO unlift $ mainLoop thread sock
  where
    numberOfQueuedConnections = 5
    resolve port host = do
      let hints = defaultHints {
              addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            , addrFamily = AF_INET
            , addrProtocol = 0
            }
      addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
      pure addr

mainLoop :: Thread -> Socket -> ServerM ()
mainLoop thread sock = do
  -- In main thread we just wait until shutdown flag is enabled
  withLinkedWorker_ acceptLoop $ do
    shutdownFlagRef <- getShutdownFlag
    liftIO $ atomically $ check =<< readTVar shutdownFlagRef
    liftIO $ stop thread
  where
    -- We spawn worker threads for each accepted connection. Each
    -- thread own socket and will close it whenever it finish
    -- execution normally or abnormally
    acceptLoop = withWorkersUnion $ \wrkUnion -> forever $ do
      (newSock, newSockAddr) <- liftIO $ accept sock
      spawnWorker wrkUnion $
        runConnection (newSock, newSockAddr) `finally` liftIO (close newSock)


runConnection :: (Socket, SockAddr) -> ServerM ()
runConnection (sock, addr) = incGaugeWhile activeConnsGauge $ do
  evalResult <- runExceptT $ evalMsg sock addr
  case evalResult of
    Right (msgs@(MVersionACK _ : _), _) -> do --peer version match ours
      sendChan <- liftIO newTChanIO
      writeMsg sendChan msgs
      -- We run sending of messages in the separate thread
      withLinkedWorker (sendLoop sock sendChan) $ \a -> do
        listenLoop  sendChan
        writingDone sendChan
        liftIO $ Async.wait a
    Left err -> do
      logErrorN $ "<" <> showt addr <> ">: Rejecting client on handshake phase with: " <> showt err
      rawSendMsg $ MReject err
    Right (MReject r : _, _) -> do
      logErrorN $ "<" <> showt addr <> ">: Rejecting client on handshake phase with: " <> showt r
      rawSendMsg $ MReject r
    Right msg -> do
      logErrorN $ "<" <> showt addr
        <> ">: Impossible! Tried to send something that is not MVersionACK or MReject to client at handshake: "
        <> showt msg
  where
    rawSendMsg = liftIO . sendLazy sock . toLazyByteString . messageBuilder

    writeMsg    ch = liftIO . atomically . writeTChan ch . Just
    writingDone ch = liftIO $ atomically $ writeTChan ch $ Nothing

    listenLoop :: TChan (Maybe [Message]) -> ServerM ()
    listenLoop destinationChan = fix $ \loop -> do
      evalResult <- runExceptT $ evalMsg sock addr
      case evalResult of
        Right (msgs, closeIt) -> do
          writeMsg destinationChan msgs
          case closeIt of
            True  -> logInfoN $ "<" <> showt addr <> ">: Closing connection on our side"
            False -> loop
        Left Reject {..} | rejectMsgCode == ZeroBytesReceived -> do
          logInfoN $ "<" <> showt addr <> ">: Client closed the connection"
        Left err -> do
          logErrorN $ "<" <> showt addr <> ">: Rejecting client with: " <> showt err
          writeMsg destinationChan [MReject err]


-- | Send loop will send both messages generated by listen loop and
--   broadcasted ones. Nothing sent over local channel means we should
--   terminate.
sendLoop :: Socket -> TChan (Maybe [Message]) -> ServerM ()
sendLoop sock sendChan = do
  broadChan <- liftIO . atomically . dupTChan
           =<< broadcastChannel
  liftIO $ fix $ \loop -> do
    msgs <- atomically $  Left  <$> readTChan sendChan
                      <|> Right <$> readTChan broadChan
    case msgs of
      Left (Just ms) -> sendMsg (foldMap messageBuilder ms) >> loop
      Left Nothing   -> pure ()
      Right m        -> sendMsg (messageBuilder m) >> loop
  where
    sendMsg = sendLazy sock . toLazyByteString

evalMsg :: Socket -> SockAddr -> ExceptT Reject ServerM ([Message], Bool)
evalMsg sock addr = response =<< request =<< messageHeader
  where
    recvVarInt = liftIO $ do
      hbs <- NS.recv sock 1
      if BS.null hbs then pure hbs else do
        let hb = BS.head hbs
        if | hb == 0xFF -> (hbs <>) <$> NS.recv sock 8
           | hb == 0XFE -> (hbs <>) <$> NS.recv sock 4
           | hb == 0xFD -> (hbs <>) <$> NS.recv sock 2
           | otherwise -> pure hbs

    messageHeader :: ExceptT Reject ServerM MessageHeader
    messageHeader = do
      mid <- messageId =<< recvVarInt
      if not $ messageHasPayload mid then pure (MessageHeader mid 0) else do
        l <- messageLength =<< recvVarInt
        pure $ MessageHeader mid l

    messageId :: BS.ByteString -> ExceptT Reject ServerM MessageType
    messageId bs
      | BS.null bs = except $ Left $ Reject MVersionType ZeroBytesReceived "Expected bytes for message header (id)"
      | otherwise = ExceptT . pure . mapLeft (\_ -> Reject MVersionType MessageHeaderParsing "Failed to parse header message id") . eitherResult $ parse messageTypeParser bs

    messageLength :: BS.ByteString -> ExceptT Reject ServerM Word32
    messageLength bs
      | BS.null bs = except $ Left $ Reject MVersionType ZeroBytesReceived "Expected bytes for message header (length)"
      | otherwise = ExceptT . pure . mapLeft (\_ -> Reject MVersionType MessageHeaderParsing "Failed to parse header message length") . eitherResult $ parse messageLengthParser bs

    request :: MessageHeader -> ExceptT Reject ServerM Message
    request MessageHeader {..} = do
      messageBytes <- if not $ messageHasPayload msgType
        then pure mempty
        else liftIO $ NS.recv sock $ fromIntegral msgSize
      except $ mapLeft (\_-> Reject msgType MessageParsing "Failed to parse message body") $ eitherResult $ parse (messageParser msgType) messageBytes

    response :: Message -> ExceptT Reject ServerM ([Message], Bool)
    response msg = (lift $ handleMsg addr msg) `catch` (\(e :: SomeException) -> do
      logErrorN $ "<" <> showt addr <> ">: Rejecting peer as exception occured in while handling it message: " <> showt e
      except $ Left $ Reject (messageType msg) InternalServerError $ showt e
      )
