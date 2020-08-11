module Ergvein.Index.Server.TCPService.Server where

import Control.Concurrent
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM
import Control.Immortal
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Attoparsec.ByteString
import Data.ByteString.Builder
import Data.Either.Combinators
import Data.Foldable (traverse_)
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
    liftIO $ atomically $ modifyTVar openedConnectionsRef (M.insert connectionThreadId $ fst connection)
  shutdownFlagRef <- getShutdownFlag
  forever $ liftIO $ do
    shutdownFlag <- readTVarIO shutdownFlagRef
    when shutdownFlag $ performShutdown openedConnectionsRef mainLoopId
    threadDelay 1000000
  where
   performShutdown openedConnectionsRef mainLoopId = do
    (connectionThreadIds, connections) <- unzip . M.toList <$> (liftIO $ readTVarIO openedConnectionsRef)
    traverse_ close connections
    traverse_ killThread connectionThreadIds
    killThread mainLoopId
    atomically $ writeTVar openedConnectionsRef M.empty
    stop thread

runConnection :: (Socket, SockAddr) -> ServerM ()
runConnection (sock, addr) = do
  sendChan <- liftIO newTChanIO
  -- Spawn message sender thread
  fork $ sendLoop sendChan
  -- Start message listener
  listenLoop sendChan
  where
    handshake :: ServerM Bool
    handshake = do
      headerBytes <- liftIO $ messageHeaderBytes
      message     <- runExceptT $ fetchMessage sock headerBytes
      pure True

    messageHeaderBytes = NS.recv sock 8

    sendLoop :: TChan LBS.ByteString -> ServerM ()
    sendLoop sendChan = liftIO $ forever $ do
      msgs <- atomically $ readAllTVar sendChan
      sendLazy sock $ mconcat msgs

    listenLoop :: TChan LBS.ByteString -> ServerM ()
    listenLoop destinationChan = do
      let writeMsg = liftIO . atomically . writeTChan destinationChan . toLazyByteString . messageBuilder
      -- Try to get the header. Empty string means that the client closed the connection
      headerBytes <- liftIO $ messageHeaderBytes
      if BS.null headerBytes then do
          logInfoN $ "<" <> showt addr <> ">: Client closed the connection"
          liftIO $ close sock
      else do
        evalResult <- runExceptT $ evalMsg sock headerBytes
        case evalResult of
          Right Nothing    -> pure ()
          Right (Just msg) -> writeMsg msg
          Left err         -> do
            logInfoN $ "failed to handle msg: " <> showt err
            writeMsg $ RejectMsg err
        listenLoop destinationChan


fetchMessage :: Socket -> BS.ByteString -> ExceptT RejectMessage ServerM Message
fetchMessage sock messageHeaderBytes = request =<< messageHeader 
  where
    messageHeader :: ExceptT RejectMessage ServerM MessageHeader
    messageHeader = ExceptT . pure . mapLeft (\_-> RejectMessage MessageHeaderParsing) . eitherResult $ parse messageHeaderParser messageHeaderBytes

    request :: MessageHeader -> ExceptT RejectMessage ServerM Message
    request MessageHeader {..} = do
      messageBytes <- liftIO $ NS.recv sock $ fromIntegral msgSize
      ExceptT $ pure $ mapLeft (\_-> RejectMessage MessageParsing) $ eitherResult $ parse (messageParser msgType) messageBytes


evalMsg :: Socket -> BS.ByteString -> ExceptT RejectMessage ServerM (Maybe Message)
evalMsg sock messageHeaderBytes = response =<< request =<< messageHeader 
  where
    messageHeader :: ExceptT RejectMessage ServerM MessageHeader
    messageHeader = ExceptT . pure . mapLeft (\_-> RejectMessage MessageHeaderParsing) . eitherResult $ parse messageHeaderParser messageHeaderBytes

    request :: MessageHeader -> ExceptT RejectMessage ServerM Message
    request MessageHeader {..} = do
      messageBytes <- liftIO $ NS.recv sock $ fromIntegral msgSize
      ExceptT $ pure $ mapLeft (\_-> RejectMessage MessageParsing) $ eitherResult $ parse (messageParser msgType) messageBytes

    response :: Message -> ExceptT RejectMessage ServerM (Maybe Message)
    response msg = ExceptT $ (Right <$> handleMsg msg) `catch` (\(SomeException ex) -> pure $ Left $ RejectMessage $ InternalServerError)
