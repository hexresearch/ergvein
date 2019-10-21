module Ergvein.Index.Server.BlockchainScanner where

import Data.Text (Text, pack)
import Control.Monad.IO.Unlift
import Control.Immortal
import Ergvein.Index.Server.Config
import Network.Bitcoin.Api.Client
import Network.Bitcoin.Api.Blockchain
import Network.Bitcoin.Api.Misc

import qualified Data.Bitcoin.Block                     as Btc

import qualified Data.Text.IO as T


blocksStream :: Integer -> Config -> IO ()
blocksStream grantedHeight cfg = let
    btcNodeClient :: (Client -> IO a) -> IO a
    btcNodeClient = withClient (configBTCNodeHost cfg) (configBTCNodePort cfg) (configBTCNodeUser cfg) (configBTCNodePassword cfg) 
    go h = do
        blockHash <- btcNodeClient $ flip getBlockHash h
        block <- btcNodeClient $ flip getBlock blockHash
        pure ()
    in sequence_ $ go <$> [0..grantedHeight - 1]

startBlockchainScanner :: MonadUnliftIO m => Config -> m Thread
startBlockchainScanner cfg = let 
    in create $ \thread -> liftIO $ do
        x <-  blocksStream 10 cfg
        T.putStrLn $ pack $ show x
        mortalize thread