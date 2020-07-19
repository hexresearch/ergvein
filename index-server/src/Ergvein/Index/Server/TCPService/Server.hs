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

import qualified Network.Socket.ByteString as NS

tcpSrv :: ServerM ()
tcpSrv = do
  unlift <- askUnliftIO
  liftIO $ withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    unliftIO unlift $ mainLoop sock

mainLoop :: Socket -> ServerM ()
mainLoop sock = do
  conn <- liftIO $ accept sock
  forkIO <$> (toIO $ runConn conn)
  mainLoop sock

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

handleMsg :: Message -> ServerM Message
handleMsg = undefined