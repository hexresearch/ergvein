module Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad where

import qualified Network.Bitcoin.Api.Client  as BitcoinApi

class BitcoinApiMonad m where
  nodeRpcCall :: (BitcoinApi.Client -> IO a) -> m a

