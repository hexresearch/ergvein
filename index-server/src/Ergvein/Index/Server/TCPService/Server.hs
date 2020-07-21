module Ergvein.Index.Server.TCPService.Server where

import Control.Concurrent
import Data.Attoparsec.ByteString
import Data.ByteString.Builder
import Network.Socket
import System.IO
import Data.Word
import Data.Maybe
import Ergvein.Index.Protocol.Deserialization
import Ergvein.Index.Protocol.Serialization
import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.Monad
import Control.Monad.IO.Unlift
import Control.Immortal
import Control.Monad.Logger
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Dependencies
import Control.Concurrent.STM
import Ergvein.Index.Server.TCPService.MessageHandler
import Control.Monad.Catch

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
mainLoop thread sock = go sock
  where 
  go s = do
    conn <- liftIO $ accept sock
    forkIO <$> (toIO $ runConn conn)
    shutdownFlag <- liftIO . readTVarIO =<< getShutdownFlag
    if shutdownFlag then
      go s
    else
      liftIO $ stop thread

runConn :: (Socket, SockAddr) -> ServerM ()
runConn (sock, _) = do
  messageHeaderBytes <- liftIO $ NS.recv sock 8
  let messageHeaderParsingResult = parse messageHeaderParser messageHeaderBytes
  hdl <- liftIO $ socketToHandle sock ReadWriteMode
  case messageHeaderParsingResult of
    Done _ MessageHeader {..} -> do
      messageBytes <- liftIO $ NS.recv sock $ fromIntegral msgSize
      let messageParsingResult = (parse $ messageParser msgType) messageBytes
      case messageParsingResult of
        Done _ msg -> do
          resp <- handleMsg msg `catch` (\(SomeException ex) -> pure $ RejectMsg $ RejectMessage $ InternalServerError)
          liftIO $ hPutBuilder hdl $ messageBuilder resp
        _ -> liftIO $ hPutBuilder hdl $ messageBuilder $ RejectMsg $ RejectMessage MessageParsing
    _ -> liftIO $ hPutBuilder hdl $ messageBuilder $ RejectMsg $ RejectMessage MessageHeaderParsing
  liftIO $ hClose hdl