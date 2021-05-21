module Network.Socket.Manager.Peeker(
    MonadPeeker(..)
  , PeekerEnv(..)
  , ReceiveException(..)
  , receiveExactly
  , recv
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString (ByteString)
import GHC.Generics

import qualified Control.Exception.Safe as Ex
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NSB

-- | Interface monad for `socket` widget that allows to peek exact amount of
-- bytes from socket to parse next portion of data.
class Monad m => MonadPeeker m where
  -- | Peek exact amount of bytes. Can throw `ReceiveException` when connection
  -- is closed from other side or broken.
  peek :: Int -> m ByteString
  -- | Peek all bytes that are inside socket. Can throw `ReceiveException` when connection
  -- is closed from other side or broken.
  peekAll :: m ByteString

-- | Environment for `MonadPeeker` implementation
data PeekerEnv = PeekerEnv !(TVar Bool) !N.Socket

-- | Implementaiton based on 'receiveExactly'. Can throw 'ReceiveException'.
instance MonadIO m => MonadPeeker (ReaderT PeekerEnv m) where
  peek n = do
    PeekerEnv ivar sock <- ask
    liftIO $ receiveExactly ivar sock n
  {-# INLINE peek #-}
  peekAll = do
    PeekerEnv ivar sock <- ask
    liftIO $ receiveAll ivar sock 100
  {-# INLINe peekAll #-}

-- | Exception occured when receiving bytes from socket fails.
data ReceiveException =
    ReceiveEndOfInput -- ^ Connection was closed from other side
  | ReceiveInterrupted -- ^ Receiving was interrupted by our side
  deriving (Eq, Show, Generic)

instance Ex.Exception ReceiveException

-- | Helper to read excact amount of bytes from socket with possible interruption.
-- Can throw 'ReceiveException'.
receiveExactly :: TVar Bool -> N.Socket -> Int -> IO ByteString
receiveExactly intVar sock n = go n mempty
  where
    go i acc = do
      needExit <- liftIO . atomically $ readTVar intVar
      when needExit $ Ex.throw ReceiveInterrupted
      mbs <- recv sock i
      case mbs of
        Nothing -> Ex.throw ReceiveEndOfInput
        Just bs -> let
          l = BS.length bs
          acc' = acc <> BB.byteString bs
          in if l < i then go (i - l) acc' else pure . BSL.toStrict . BB.toLazyByteString $ acc'

-- | Helper to read all available data form socket with possible interruption.
-- Can throw 'ReceiveException'.
receiveAll :: TVar Bool -> N.Socket -> Int -> IO ByteString
receiveAll intVar sock n = go mempty
  where
    go acc = do
      needExit <- liftIO . atomically $ readTVar intVar
      when needExit $ Ex.throw ReceiveInterrupted
      mbs <- recv sock n
      case mbs of
        Nothing -> Ex.throw ReceiveEndOfInput
        Just bs -> let
          l = BS.length bs
          acc' = acc <> BB.byteString bs
          in if l == n then go acc' else pure . BSL.toStrict . BB.toLazyByteString $ acc'


-- | Read up to a limited number of bytes from a socket.
--
-- Returns `Nothing` if the remote end closed the connection or end-of-input was
-- reached. The number of returned bytes might be less than the specified limit,
-- but it will never 'BS.null'.
recv :: MonadIO m => N.Socket -> Int -> m (Maybe BS.ByteString)
recv sock nbytes = liftIO $ do
  bs <- liftIO (NSB.recv sock nbytes)
  if BS.null bs
     then pure Nothing
     else pure (Just bs)
{-# INLINABLE recv #-}
