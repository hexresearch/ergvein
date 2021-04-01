{-# LANGUAGE MultiWayIf #-}
module Ergvein.Index.Server.TCPService.Server where

import Control.Applicative
import Control.Concurrent.STM
import Control.Immortal                (Thread,create,stop)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Random
import Control.Monad.Trans.Except
import Data.ByteString.Builder
import Data.Either.Combinators
import Data.Foldable
import Data.Word
import Network.Socket

import Ergvein.Index.Protocol.Deserialization
import Ergvein.Index.Protocol.Serialization
import Ergvein.Index.Protocol.Types
import Ergvein.Index.Protocol.Utils
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Metrics
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.PeerDiscovery.Discovery
import Ergvein.Index.Server.TCPService.MessageHandler
import Ergvein.Index.Server.TCPService.Supervisor
import Ergvein.Text

import Ergvein.Index.Server.TCPService.Socket as S
import qualified Control.Concurrent.Async     as Async
import qualified Data.ByteString              as BS
import qualified Network.Socket.ByteString    as NS


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
  sendChan <- liftIO newTChanIO
  withLinkedWorker (sendLoop sock sendChan) $ \a -> do
    ownVer <- ownVersion
    writeMsg sendChan $ MVersion ownVer
    listenLoop  sendChan
    writingDone sendChan
    liftIO $ Async.wait a
  where
    writeMsg    ch = liftIO . atomically . writeTChan ch . Just
    writingDone ch = liftIO $ atomically $ writeTChan ch $ Nothing

    listenLoop :: TChan (Maybe Message) -> ServerM ()
    listenLoop destinationChan = fix $ \loop -> do
      evalResult <- runExceptT $ evalMsg sock addr
      case evalResult of
        Right (msg, closeIt) -> do
          traverse_ (writeMsg destinationChan) msg
          case closeIt of
            True  -> logInfoN $ "<" <> showt addr <> ">: Closing connection on our side"
            False -> loop
        Left Reject {..} | rejectMsgCode == ZeroBytesReceived -> do
          logInfoN $ "<" <> showt addr <> ">: Client closed the connection"
        Left err -> do
          logErrorN $ "<" <> showt addr <> ">: Rejecting client with: " <> showt err
          writeMsg destinationChan $ MReject err


-- | Send loop will send both messages generated by listen loop and
--   broadcasted ones. Nothing sent over local channel means we should
--   terminate.
sendLoop :: Socket -> TChan (Maybe Message) -> ServerM ()
sendLoop sock sendChan = do
  broadChan <- liftIO . atomically . dupTChan
           =<< broadcastChannel
  liftIO $ fix $ \loop -> do
    msgs <- atomically $  Left  <$> readTChan sendChan
                      <|> Right <$> readTChan broadChan
    case msgs of
      Left (Just ms) -> sendMsg (messageBuilder ms) >> loop
      Left Nothing   -> pure ()
      Right m        -> sendMsg (messageBuilder m) >> loop
  where
    sendMsg = sendLazy sock . toLazyByteString

evalMsg :: Socket -> SockAddr -> ExceptT Reject ServerM (Maybe Message, Bool)
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
      | otherwise = ExceptT . pure . mapLeft (\_ -> Reject MVersionType MessageHeaderParsing "Failed to parse header message id") . parseTillEndOfInput messageTypeParser $ bs

    messageLength :: BS.ByteString -> ExceptT Reject ServerM Word32
    messageLength bs
      | BS.null bs = except $ Left $ Reject MVersionType ZeroBytesReceived "Expected bytes for message header (length)"
      | otherwise = ExceptT . pure . mapLeft (\_ -> Reject MVersionType MessageHeaderParsing "Failed to parse header message length") . parseTillEndOfInput messageLengthParser $ bs

    request :: MessageHeader -> ExceptT Reject ServerM Message
    request MessageHeader {..} = do
      messageBytes <- if not $ messageHasPayload msgType
        then pure mempty
        else liftIO $ NS.recv sock $ fromIntegral msgSize
      except $ mapLeft (\_-> Reject msgType MessageParsing "Failed to parse message body") $ parseTillEndOfInput (messageParser msgType) messageBytes

    response :: Message -> ExceptT Reject ServerM (Maybe Message, Bool)
    response msg = (lift $ handleMsg addr msg) `catch` (\(e :: SomeException) -> do
      logErrorN $ "<" <> showt addr <> ">: Rejecting peer as exception occured in while handling it message: " <> showt e
      except $ Left $ Reject (messageType msg) InternalServerError $ showt e
      )
