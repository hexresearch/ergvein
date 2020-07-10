module Ergvein.Index.Server.TCPService.Server where

import Control.Concurrent
import Data.Attoparsec.ByteString
import Data.ByteString.Builder
import Network.Socket
import System.IO

import Ergvein.Index.Protocol.Deserialization
import Ergvein.Index.Protocol.Serialization
import Ergvein.Index.Protocol.Types

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
  forkIO $ runConn conn
  mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
  h <- maybeResult . readMessageInfo <$> NS.recv sock 8
  case h of
    Just (t,s)-> do
      hdl <- socketToHandle sock ReadWriteMode
      cnt <- NS.recv sock $ fromIntegral s
      let msg = pingMsg 1 
      hPutBuilder hdl msg
      hClose hdl
      pure ()
    Nothing -> do
      close sock
      pure ()
