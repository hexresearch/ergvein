module Ergvein.Index.Server.TCPService.Server where

import Control.Concurrent
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM
import Control.Immortal
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Except
import Data.Attoparsec.ByteString
import Data.ByteString.Builder
import Data.Either.Combinators
import Network.Socket

import Ergvein.Index.Protocol.Deserialization
import Ergvein.Index.Protocol.Serialization
import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.TCPService.MessageHandler
import Ergvein.Index.Server.TCPService.Socket
import Ergvein.Text

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
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
    listen sock 4
    unliftIO unlift $ mainLoop thread sock
  where
    numberOfQueuedConnections = 4
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
  fork $ forever $ do
    conn <- liftIO $ accept sock
    fork $ runConn conn
  flagVar <- getShutdownFlag
  forever $ liftIO $ do
    shutdownFlag <- readTVarIO flagVar
    when shutdownFlag $ stop thread
    threadDelay 1000000

runConn :: (Socket, SockAddr) -> ServerM ()
runConn (sock, addr) = do
  sendChan <- liftIO newTChanIO
  -- Spawn message sender thread
  fork $ sendLoop sendChan
  -- Start message listener
  listenLoop sendChan
  where
    sendLoop :: TChan LBS.ByteString -> ServerM ()
    sendLoop sendChan = liftIO $ forever $ do
      msgs <- atomically $ readAllTVar sendChan
      sendLazy sock $ mconcat msgs

    listenLoop :: TChan LBS.ByteString -> ServerM ()
    listenLoop destinationChan = do
      let writeMsg = liftIO . atomically . writeTChan destinationChan . toLazyByteString . messageBuilder
      -- Try to get the header. Empty string means that the client closed the connection
      messageHeaderBytes <- liftIO $ NS.recv sock 8
      if BS.null messageHeaderBytes
        then do
          logInfoN $ "<" <> showt addr <> ">: Client closed the connection"
          liftIO $ close sock
        else do
          evalResult <- runExceptT $ evalMsg sock messageHeaderBytes
          case evalResult of
            Right Nothing    -> pure ()
            Right (Just msg) -> writeMsg msg
            Left err         -> do
              logInfoN $ "failed to handle msg: " <> showt err
              writeMsg $ RejectMsg err
          listenLoop destinationChan

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