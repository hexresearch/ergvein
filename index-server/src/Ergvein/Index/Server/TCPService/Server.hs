module Ergvein.Index.Server.TCPService.Server where

import Ergvein.Index.Protocol.Types
import Network.Socket
import Control.Concurrent
import System.IO
import qualified Network.Socket.ByteString as NS

tcpSrv = withSocketsDo $ do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock
    runConn conn
    mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    NS.send sock "Hello!\n"
    close sock