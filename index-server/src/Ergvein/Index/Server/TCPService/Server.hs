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
import Control.Monad.Trans.Except
import Data.Attoparsec.ByteString
import Data.ByteString.Builder
import Data.Either.Combinators
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

import qualified Data.ByteString.Lazy as LBS
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
    fork $ registerConnection (newSock, newSockAddr)
  shutdownFlagRef <- getShutdownFlag
  forever $ do
    shutdownFlag <- liftIO $ readTVarIO shutdownFlagRef
    when shutdownFlag $ performShutdown mainLoopId
    liftIO $ threadDelay 1000000
  where
   performShutdown :: HasConnectionsManagement m => ThreadId -> m ()
   performShutdown mainLoopId = do
    closeAllConnections
    liftIO $ do
      killThread mainLoopId
      stop thread

registerConnection :: (Socket, SockAddr) -> ServerM ()
registerConnection (sock, addr) = do
  connectionThreadId <- fork $ runConnection (sock, addr)
  openConnection connectionThreadId addr sock

runConnection :: (Socket, SockAddr) -> ServerM ()
runConnection (sock, addr) = incGaugeWhile activeConnsGauge $ do
  evalResult <- runExceptT $ evalMsg
  case evalResult of
    Right (msgs@(MVersionACK _ : _)) -> do --peer version match ours
      sendChan <- liftIO newTChanIO
      liftIO $ forM_ msgs $ writeMsg sendChan
      -- Spawn message sender thread
      void $ fork $ sendLoop sendChan
      -- Spawn broadcaster loop
      void $ fork $ broadcastLoop sendChan
      -- Start message listener
      listenLoop sendChan
    _ -> closeConnection addr
  where
    writeMsg :: TChan LBS.ByteString -> Message -> IO ()
    writeMsg destinationChan = atomically . writeTChan destinationChan . toLazyByteString . messageBuilder

    broadcastLoop :: HasBroadcastChannel m => TChan LBS.ByteString -> m ()
    broadcastLoop destinationChan = do
      channel <- broadcastChannel
      broadChan <- liftIO $ atomically $ dupTChan channel
      liftIO $ forever $ do
        msg <- atomically $ readTChan broadChan
        writeMsg destinationChan msg

    sendLoop :: TChan LBS.ByteString -> ServerM ()
    sendLoop sendChan = liftIO $ forever $ do
      msgs <- atomically $ readAllTVar sendChan
      sendLazy sock $ mconcat msgs

    listenLoop :: TChan LBS.ByteString -> ServerM ()
    listenLoop destinationChan = listenLoop'
      where
        listenLoop' = do
          evalResult <- runExceptT $ evalMsg
          case evalResult of
            Right msgs -> do
              liftIO $ forM_ msgs $ writeMsg destinationChan
              listenLoop'
            Left Reject {..} | rejectMsgCode == ZeroBytesReceived -> do
              logInfoN $ "<" <> showt addr <> ">: Client closed the connection"
              closeConnection addr
            Left err -> do
              logInfoN $ "failed to handle msg: " <> showt err
              liftIO $ writeMsg destinationChan $ MReject err

    evalMsg :: ExceptT Reject ServerM [Message]
    evalMsg = response =<< request =<< messageHeader =<< messageHeaderBytes
      where
        messageHeaderBytesFetch = NS.recv sock 8

        messageHeaderBytes :: ExceptT Reject ServerM BS.ByteString
        messageHeaderBytes = do
          fetchedBytes <- liftIO messageHeaderBytesFetch
          if not (BS.null fetchedBytes) then
            except $ Right fetchedBytes
          else
            except $ Left $ Reject ZeroBytesReceived

        messageHeader :: BS.ByteString -> ExceptT Reject ServerM MessageHeader
        messageHeader = ExceptT . pure . mapLeft (\_-> Reject MessageHeaderParsing) . eitherResult . parse messageHeaderParser

        request :: MessageHeader -> ExceptT Reject ServerM Message
        request MessageHeader {..} = do
          messageBytes <- liftIO $ NS.recv sock $ fromIntegral msgSize
          except $ mapLeft (\_-> Reject MessageParsing) $ eitherResult $ parse (messageParser msgType) messageBytes

        response :: Message -> ExceptT Reject ServerM [Message]
        response msg = (lift $ handleMsg addr msg) `catch` (\SomeException{} -> except $ Left $ Reject InternalServerError)
