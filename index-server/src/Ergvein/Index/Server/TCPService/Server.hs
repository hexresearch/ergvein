{-# LANGUAGE MultiWayIf #-}
module Ergvein.Index.Server.TCPService.Server where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM
import Control.Immortal
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Random
import Control.Monad.Trans.Except
import Data.Attoparsec.ByteString
import Data.ByteString.Builder
import Data.Either.Combinators
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
import Ergvein.Index.Server.TCPService.Connections

import qualified Data.ByteString as BS
import qualified Network.Socket.ByteString as NS

-- Pinger thread
runPinger :: ServerM Thread
runPinger = create $ const $ logOnException "runPinger" $ do
  broadChan <- liftIO . atomically . dupTChan =<< broadcastChannel
  forever $ liftIO $ do
    threadDelay 5000000
    msg <- MPing <$> randomIO
    atomically $ writeTChan broadChan msg

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
  mainLoopId <- fork $ forever $ do
    (newSock, newSockAddr) <- liftIO $ accept sock
    fork $ runConnection (newSock, newSockAddr)
  -- Wait until shutdown flag turns on then perform cleanup
  do shutdownFlagRef <- getShutdownFlag
     liftIO $ atomically $ check =<< readTVar shutdownFlagRef
  closeAllConnections
  liftIO $ do
    killThread mainLoopId
    stop thread

runConnection :: (Socket, SockAddr) -> ServerM ()
runConnection (sock, addr) = incGaugeWhile activeConnsGauge $ do
  do tid <- liftIO myThreadId
     openConnection tid addr sock
  evalResult <- runExceptT $ evalMsg
  case evalResult of
    Right (msgs@(MVersionACK _ : _), _) -> do --peer version match ours
      sendChan <- liftIO newTChanIO
      liftIO $ forM_ msgs $ writeMsg sendChan
      -- Spawn message sender thread
      void $ fork $ sendLoop sendChan
      -- Start message listener
      listenLoop sendChan
    Left err -> do
      logErrorN $ "<" <> showt addr <> ">: Rejecting client on handshake phase with: " <> showt err
      liftIO $ do
        rawSendMsg $ MReject err
        threadDelay 100000
      closeConnection addr
    Right (MReject r : _, _) -> do
      logErrorN $ "<" <> showt addr <> ">: Rejecting client on handshake phase with: " <> showt r
      liftIO $ do
        rawSendMsg $ MReject r
        threadDelay 100000
      closeConnection addr
    Right msg -> do
      logErrorN $ "<" <> showt addr <> ">: Impossible! Tried to send something that is not MVersionACK or MReject to client at handshake: " <> showt msg
      closeConnection addr
  where
    rawSendMsg :: Message -> IO ()
    rawSendMsg = sendLazy sock . toLazyByteString . messageBuilder

    writeMsg :: TChan Builder -> Message -> IO ()
    writeMsg destinationChan = atomically . writeTChan destinationChan . messageBuilder

    sendLoop :: TChan Builder -> ServerM ()
    sendLoop sendChan = do
      broadChan <- liftIO . atomically . dupTChan
               =<< broadcastChannel
      liftIO $ forever $ do
        msgs <- atomically $ readAllTVar $  readTChan sendChan
                                        <|> messageBuilder <$> readTChan broadChan
        sendLazy sock $ toLazyByteString $ mconcat msgs

    listenLoop :: TChan Builder -> ServerM ()
    listenLoop destinationChan = listenLoop'
      where
        listenLoop' = do
          evalResult <- runExceptT $ evalMsg
          case evalResult of
            Right (msgs, closeIt) -> do
              liftIO $ forM_ msgs $ writeMsg destinationChan
              if closeIt then do
                logInfoN $ "<" <> showt addr <> ">: Closing connection on our side"
                liftIO $ threadDelay 100000
                closeConnection addr
              else listenLoop'
            Left Reject {..} | rejectMsgCode == ZeroBytesReceived -> do
              logInfoN $ "<" <> showt addr <> ">: Client closed the connection"
              closeConnection addr
            Left err -> do
              logErrorN $ "<" <> showt addr <> ">: Rejecting client with: " <> showt err
              liftIO $ do
                writeMsg destinationChan $ MReject err
                threadDelay 100000
              closeConnection addr

    evalMsg :: ExceptT Reject ServerM ([Message], Bool)
    evalMsg = response =<< request =<< messageHeader
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
