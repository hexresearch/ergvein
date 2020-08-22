module Ergvein.Index.Server.TCPService.Server where

import Control.Concurrent
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM
import Control.Immortal
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Attoparsec.ByteString
import Data.ByteString.Builder
import Data.Either.Combinators
import Data.Foldable (traverse_)
import Data.Time.Clock.POSIX
import Data.Maybe
import Data.Word
import Network.Socket
import Network.Socket.ByteString.Lazy
import System.IO

import Ergvein.Text
import Ergvein.Index.Protocol.Deserialization
import Ergvein.Index.Protocol.Serialization
import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.TCPService.MessageHandler
import Ergvein.Index.Server.TCPService.Socket

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Network.Socket.ByteString as NS

-- Pinger thread
runPinger :: ServerM Thread
runPinger = create $ const $ logOnException $ do
  broadChan <- liftIO . atomically . dupTChan =<< asks envBroadcastChannel
  forever $ liftIO $ do
    threadDelay 5000000
    msg <- MPing <$> randomIO
    atomically $ writeTChan broadChan msg

runTcpSrv :: ServerM Thread
runTcpSrv = create $ logOnException . tcpSrv

tcpSrv :: Thread -> ServerM ()
tcpSrv thread = do
  port <- show . cfgServerTcpPort <$> serverConfig
  unlift <- askUnliftIO
  liftIO $ withSocketsDo $ do
    addr <- resolve port
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock 5
    unliftIO unlift $ mainLoop thread sock
  where
    numberOfQueuedConnections = 5
    resolve port = do
      let hints = defaultHints {
              addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            , addrFamily = AF_INET
            , addrProtocol = 0
            }
      addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
      pure addr

mainLoop :: Thread -> Socket -> ServerM ()
mainLoop thread sock = do
  openedConnectionsRef <- asks envOpenConnections
  mainLoopId <- fork $ forever $ do
    connection <- liftIO $ accept sock
    connectionThreadId <- fork $ runConnection connection
    liftIO $ atomically $ modifyTVar openedConnectionsRef $ M.insert (snd connection) (connectionThreadId , fst connection)
  shutdownFlagRef <- getShutdownFlag
  forever $ liftIO $ do
    shutdownFlag <- readTVarIO shutdownFlagRef
    when shutdownFlag $ performShutdown openedConnectionsRef mainLoopId
    threadDelay 1000000
  where
   performShutdown openedConnectionsRef mainLoopId = do
    traverse closeConnection =<< M.elems <$> readTVarIO openedConnectionsRef
    killThread mainLoopId
    atomically $ writeTVar openedConnectionsRef M.empty
    stop thread

runConnection :: (Socket, SockAddr) -> ServerM ()
runConnection (sock, addr) = do
  handshakeStatus <- handshake
  if handshakeStatus then do
    sendChan <- liftIO newTChanIO
    -- report handshake success
    writeMsg sendChan $ MVersionACK VersionACK
    ver <- ownVersion
    writeMsg sendChan $ MVersion ver
    -- Spawn message sender thread
    fork $ sendLoop sendChan
    -- Spawn broadcaster loop
    fork $ broadcastLoop sendChan
    -- Start message listener
    listenLoop sendChan
  else closePeerConnection addr
  where
    writeMsg :: TChan LBS.ByteString -> Message -> ServerM ()
    writeMsg destinationChan = liftIO . atomically . writeTChan destinationChan . toLazyByteString . messageBuilder

    broadcastLoop :: TChan LBS.ByteString -> ServerM ()
    broadcastLoop destinationChan = do
      broadChan <- liftIO . atomically . dupTChan =<< asks envBroadcastChannel
      forever $ do
        msg <- liftIO $ atomically $ readTChan broadChan
        writeMsg destinationChan msg

    handshake :: ServerM Bool
    handshake = do
      headerBytes <- liftIO $ messageHeaderBytes
      message     <- runExceptT $ fetchMessage sock headerBytes
      pure $ case message of
        Right (MVersion Version {..}) | protocolVersion == versionVersion -> True
        _ -> False

    messageHeaderBytes = NS.recv sock 8

    sendLoop :: TChan LBS.ByteString -> ServerM ()
    sendLoop sendChan = liftIO $ forever $ do
      msgs <- atomically $ readAllTVar sendChan
      sendLazy sock $ mconcat msgs

    listenLoop :: TChan LBS.ByteString -> ServerM ()
    listenLoop destinationChan = do
      -- Try to get the header. Empty string means that the client closed the connection
      headerBytes <- liftIO $ messageHeaderBytes
      if BS.null headerBytes then do
          logInfoN $ "<" <> showt addr <> ">: Client closed the connection"
          closePeerConnection addr
      else do
        evalResult <- runExceptT $ evalMsg addr sock headerBytes
        case evalResult of
          Right Nothing    -> pure ()
          Right (Just msg) -> writeMsg destinationChan msg
          Left err         -> do
            logInfoN $ "failed to handle msg: " <> showt err
            writeMsg destinationChan $ MReject err
        listenLoop destinationChan

fetchMessage :: Socket -> BS.ByteString -> ExceptT Reject ServerM Message
fetchMessage sock messageHeaderBytes = request =<< messageHeader
  where
    messageHeader :: ExceptT Reject ServerM MessageHeader
    messageHeader = ExceptT . pure . mapLeft (\_-> Reject MessageHeaderParsing) . eitherResult $ parse messageHeaderParser messageHeaderBytes

    request :: MessageHeader -> ExceptT Reject ServerM Message
    request MessageHeader {..} = do
      messageBytes <- liftIO $ NS.recv sock $ fromIntegral msgSize
      ExceptT $ pure $ mapLeft (\_-> Reject MessageParsing) $ eitherResult $ parse (messageParser msgType) messageBytes


evalMsg :: SockAddr -> Socket -> BS.ByteString -> ExceptT Reject ServerM (Maybe Message)
evalMsg addr sock messageHeaderBytes = do
  printDelim
  let hdr = eitherResult $ parse messageHeaderParser messageHeaderBytes
  liftIO $ print $ BS.unpack messageHeaderBytes
  liftIO $ print hdr
  response =<< request =<< messageHeader
  where
    printDelim = liftIO $ print "===================================================="
    messageHeader :: ExceptT Reject ServerM MessageHeader
    messageHeader = ExceptT . pure . mapLeft (\_-> Reject MessageHeaderParsing) . eitherResult $ parse messageHeaderParser messageHeaderBytes

    request :: MessageHeader -> ExceptT Reject ServerM Message
    request MessageHeader {..} = do
      messageBytes <- liftIO $ NS.recv sock $ fromIntegral msgSize
      liftIO $ print $ show msgType <> ": " <> show (msgSize, BS.length messageBytes)
      liftIO $ print $ "Parse     :" <> show (eitherResult $ parse (messageParser msgType) messageBytes)
      liftIO $ print $ "ParseOnly :" <> show (parseOnly (messageParser msgType) messageBytes)
      ExceptT $ pure $ mapLeft (\_-> Reject MessageParsing) $ eitherResult $ parse (messageParser msgType) messageBytes

    response :: Message -> ExceptT Reject ServerM (Maybe Message)
    response msg = ExceptT $ (Right <$> handleMsg addr msg) `catch` (\(SomeException ex) -> pure $ Left $ Reject $ InternalServerError)
