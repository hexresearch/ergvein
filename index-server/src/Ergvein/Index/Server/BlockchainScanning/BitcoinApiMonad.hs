module Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad where

import qualified Network.Bitcoin.Api.Client  as BitcoinApi
import Control.Monad
import Control.Monad.IO.Unlift
import  Control.Monad.Reader
import Ergvein.Index.Server.Config

class BitcoinApiMonad m where
  nodeRpcCall :: (BitcoinApi.Client -> IO a) -> m a

