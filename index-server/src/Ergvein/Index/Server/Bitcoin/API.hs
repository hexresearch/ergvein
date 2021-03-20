module Ergvein.Index.Server.Bitcoin.API
  (
    BtcConnectionScheme(..)
  , BitcoinApiMonad(..)
  , requestBlock
  ) where

import Control.Monad.IO.Class
import Network.Haskoin.Block

import Ergvein.Socket.BTC (BtcSocket)
import qualified Ergvein.Socket.BTC as BTC

import qualified Network.Bitcoin.Api.Client  as BitcoinApi

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
