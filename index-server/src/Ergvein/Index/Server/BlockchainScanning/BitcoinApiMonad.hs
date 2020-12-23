module Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad where

import Control.Monad.IO.Class
import Network.Haskoin.Block

import Ergvein.Index.Server.TCPService.BTC as BTC

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
