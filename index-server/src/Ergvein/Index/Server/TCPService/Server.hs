module Ergvein.Index.Server.TCPService.Server where

import Control.Concurrent
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM
import Control.Immortal
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Attoparsec.ByteString
import Data.ByteString.Builder
import Data.Maybe
import Data.Word
import Network.Socket
import Network.Socket.ByteString.Lazy
import System.IO

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
import qualified Network.Socket.ByteString as NS

runTcpSrv :: ServerM Thread
runTcpSrv = create $ logOnException . tcpSrv

tcpSrv :: Thread -> ServerM ()
tcpSrv thread = do
  port <- fromIntegral . cfgServerTcpPort <$> serverConfig
  unlift <- askUnliftIO
  liftIO $ withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 5
    unliftIO unlift $ mainLoop thread sock

mainLoop :: Thread -> Socket -> ServerM ()
mainLoop thread sock = do
  flagVar <- getShutdownFlag
  fork $ forever $ do
    conn <- liftIO $ accept sock
    fork $ runConn conn
  forever $ liftIO $ do
    shutdownFlag <- readTVarIO flagVar
    when shutdownFlag $ stop thread
    threadDelay 1000000

runConn :: (Socket, SockAddr) -> ServerM ()
runConn (sock, _) = do
  unlift <- askUnliftIO
  liftIO $ do
    hdl <- socketToHandle sock ReadWriteMode
    sendChan <- newTChanIO
    forkIO $ forever $ do
      msgs <- atomically $ readAllTVar sendChan
      sendLazy sock $ mconcat msgs
    unliftIO unlift $ listenLoop sendChan
  pure ()
  where
    listenLoop :: TChan LBS.ByteString -> ServerM ()
    listenLoop destinationChan = forever $ do
      evalResult <- evalMsg sock
      let messageBytes = toLazyByteString $ messageBuilder $ 
            case evalResult of
              Right answer -> answer
              Left reject  -> RejectMsg reject
      liftIO $ atomically $ writeTChan destinationChan messageBytes

evalMsg :: Socket -> ServerM (Either RejectMessage Message)
evalMsg sock = do
  messageHeaderBytes <- liftIO $ NS.recv sock 8
  let messageHeaderParsingResult = parse messageHeaderParser messageHeaderBytes
  case messageHeaderParsingResult of
    Done _ MessageHeader {..} -> do
      messageBytes <- liftIO $ NS.recv sock $ fromIntegral msgSize
      let messageParsingResult = (parse $ messageParser msgType) messageBytes
      case messageParsingResult of
        Done _ msg -> do
          resp <- handleMsg msg `catch` (\(SomeException ex) -> pure $ Nothing)
          case resp of
            Just msg -> pure $ Right msg
            _ -> pure $ Left $ RejectMessage $ InternalServerError
        _ -> pure $ Left $ RejectMessage MessageParsing
    _ -> pure $ Left $ RejectMessage MessageHeaderParsing
