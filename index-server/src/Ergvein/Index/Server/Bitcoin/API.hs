module Ergvein.Index.Server.Bitcoin.API
  (
    BtcConnectionScheme(..)
  , BitcoinApiMonad(..)
  , requestBlock
  , actualHeight
  ) where

import Control.Monad.IO.Class
import Network.Haskoin.Block

import Ergvein.Socket.BTC (BtcSocket)
import qualified Ergvein.Socket.BTC as BTC

import qualified Network.Bitcoin.Api.Client  as BitcoinApi
import Network.Bitcoin.Api.Blockchain

data BtcConnectionScheme = BtcConTCP | BtcConRPC

class BitcoinApiMonad m where
  nodeRpcCall :: (BitcoinApi.Client -> IO a) -> m a
  getSocketConn :: m BtcSocket
  getBtcConnectionScheme :: m BtcConnectionScheme
  restartSocketConn :: m ()

requestBlock :: (MonadIO m, BitcoinApiMonad m) => BlockHash -> m Block
requestBlock bh = do
  btcsock <- getSocketConn
  BTC.requestBlock btcsock bh

actualHeight :: (Monad m, BitcoinApiMonad m) => m BlockHeight
actualHeight = fromIntegral <$> nodeRpcCall getBlockCount
