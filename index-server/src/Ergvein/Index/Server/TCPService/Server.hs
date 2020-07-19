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
  h <- liftIO $ maybeResult . (parse messageHeaderParser) <$> NS.recv sock 8
  case h of
    Just MessageHeader {..} -> do
      msg <- liftIO $ fromJust <$> maybeResult . (parse $ messageParser msgType) <$> (NS.recv sock $ fromIntegral msgSize)
      resp <- handleMsg msg
      liftIO $ do
        hdl <- socketToHandle sock ReadWriteMode
      --hPutBuilder hdl msg
        hClose hdl
    Nothing -> do
      liftIO $ close sock
      pure ()