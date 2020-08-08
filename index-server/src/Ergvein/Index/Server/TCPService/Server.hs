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
  flagVar <- getShutdownFlag
  fork $ forever $ do
    conn <- liftIO $ accept sock
    fork $ runConn conn
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
          evalResult <- evalMsg sock messageHeaderBytes
          case evalResult of
            Right msg -> writeMsg msg
            Left err -> do
              logInfoN $ "failed to handle msg: " <> showt err
              writeMsg $ RejectMsg err
          listenLoop destinationChan

evalMsg :: Socket -> BS.ByteString -> ServerM (Either RejectMessage Message)
evalMsg sock messageHeaderBytes = do
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
