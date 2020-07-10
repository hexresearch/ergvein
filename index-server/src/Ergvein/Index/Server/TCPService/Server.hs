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
import Data.Attoparsec.Binary

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
  h <- maybeResult . readMessageInfo <$> NS.recv sock 8
  hdl <- socketToHandle sock ReadWriteMode
  let msg = pingMsg 1 
  hPutBuilder hdl msg
  hClose hdl

readMessageInfo :: BS.ByteString -> Result (MessageType, Word32)
readMessageInfo str = (`parse` str) $ do
    messageType <- messageTypeParser
    messageSize <- anyWord32be
    pure (messageType, messageSize)

messageTypeParser :: Parser MessageType
messageTypeParser = do
  w32 <- anyWord32be
  case word32toMessageType w32 of
   Just messageType -> pure messageType
   _                -> fail "out of message type bounds"

word32toMessageType :: Word32 -> Maybe MessageType 
word32toMessageType = \case
  0  -> Just Version
  1  -> Just VersionACK
  2  -> Just FiltersRequest
  3  -> Just FiltersResponse
  4  -> Just FilterEvent
  5  -> Just PeerRequest
  6  -> Just PeerResponse
  7  -> Just FeeRequest
  8  -> Just FeeResponse
  9  -> Just IntroducePeer
  10 -> Just Reject 
  11 -> Just Ping
  12 -> Just Pong
  _  -> Nothing