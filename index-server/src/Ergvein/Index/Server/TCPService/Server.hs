module Ergvein.Index.Server.TCPService.Server where

import Ergvein.Index.Protocol.Types
import Network.Socket
import Control.Concurrent
import System.IO
import qualified Network.Socket.ByteString as NS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Builder
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as ABS
import Data.Word

import Data.Binary.Get

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
  hdl <- socketToHandle sock ReadWriteMode
  let msg = pingMsg 1 
  hPutBuilder hdl msg
  hClose hdl

readMessageInfo :: BS.ByteString -> Result (MessageType, Word32)
readMessageInfo str = (`parse` str) $ do
    t <- sizeP2
    s <- sizeP1
    pure (t, s)


sizeP2 :: Parser MessageType
sizeP2 = toEnum . fromIntegral <$> sizeP1

sizeP1 :: Parser Word32
sizeP1 = runGet getWord32be . BSL.fromStrict <$> ABS.take 4