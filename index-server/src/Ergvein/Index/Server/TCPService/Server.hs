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
import Ergvein.Index.Server.TCPService.Socket as S

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
    connectionThreadId <- fork $ registerConnection connection
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


newConnection :: SockAddr -> ServerM ()
newConnection addr = do
  (maybeHost, maybePort) <- liftIO $ getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True addr
  registerConnection =<< connectSock (fromJust maybeHost) (fromJust maybePort)

registerConnection :: (Socket, SockAddr) -> ServerM ()
registerConnection (sock, addr) = do
  openedConnectionsRef <- asks envOpenConnections
  connectionThreadId <- fork $ runConnection (sock, addr)
  liftIO $ atomically $ modifyTVar openedConnectionsRef $ M.insert addr (connectionThreadId , sock)

runConnection :: (Socket, SockAddr) -> ServerM ()
runConnection (sock, addr) =  do
  evalResult <- runExceptT $ evalMsg
  case evalResult of
    Right (Just (MVersionACK _)) -> do --peer version match ours
      sendChan <- liftIO newTChanIO
      writeMsg sendChan $ MVersionACK VersionACK
      -- Spawn message sender thread
      fork $ sendLoop sendChan
      -- Spawn broadcaster loop
      fork $ broadcastLoop sendChan
      -- Start message listener
      listenLoop sendChan
    _ -> closePeerConnection addr 
  where
    writeMsg :: TChan LBS.ByteString -> Message -> ServerM ()
    writeMsg destinationChan = liftIO . atomically . writeTChan destinationChan . toLazyByteString . messageBuilder

    broadcastLoop :: TChan LBS.ByteString -> ServerM ()
    broadcastLoop destinationChan = do
      broadChan <- liftIO . atomically . dupTChan =<< asks envBroadcastChannel
      forever $ do
        msg <- liftIO $ atomically $ readTChan broadChan
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
            Right (Just msg) -> do
              writeMsg destinationChan msg
              listenLoop'
            Right Nothing -> 
              listenLoop'
            Left Reject {..} | rejectMsgCode == ZeroBytesReceived -> do
              logInfoN $ "<" <> showt addr <> ">: Client closed the connection"
              closePeerConnection addr
            Left err -> do
              logInfoN $ "failed to handle msg: " <> showt err
              writeMsg destinationChan $ MReject err
          

    evalMsg :: ExceptT Reject ServerM (Maybe Message)
    evalMsg = response =<< request =<< messageHeader =<< messageHeaderBytes
      where
        messageHeaderBytesFetch = NS.recv sock 8

        messageHeaderBytes :: ExceptT Reject ServerM BS.ByteString
        messageHeaderBytes = do
          fetchedBytes <- lift $ liftIO messageHeaderBytesFetch
          if BS.null fetchedBytes then
            except $ Right fetchedBytes
          else
            except $ Left $ Reject MessageParsing

        messageHeader :: BS.ByteString -> ExceptT Reject ServerM MessageHeader
        messageHeader = ExceptT . pure . mapLeft (\_-> Reject MessageHeaderParsing) . eitherResult . parse messageHeaderParser
    
        request :: MessageHeader -> ExceptT Reject ServerM Message
        request MessageHeader {..} = do
          messageBytes <- liftIO $ NS.recv sock $ fromIntegral msgSize
          liftIO $ print $ show msgType <> ": " <> show (msgSize, BS.length messageBytes)
          liftIO $ print $ "Parse     :" <> show (eitherResult $ parse (messageParser msgType) messageBytes)
          liftIO $ print $ "ParseOnly :" <> show (parseOnly (messageParser msgType) messageBytes)
          except $ mapLeft (\_-> Reject MessageParsing) $ eitherResult $ parse (messageParser msgType) messageBytes
    
        response :: Message -> ExceptT Reject ServerM (Maybe Message)
        response msg = (lift $ handleMsg addr msg) `catch` (\(SomeException ex) -> except $ Left $ Reject InternalServerError)