module Ergvein.Index.Server.BlockchainScanner where

import Data.Text (Text, pack)
import Control.Monad.IO.Unlift
import Control.Immortal
import Ergvein.Index.Server.Config
import Network.Bitcoin.Api.Client
import Network.Bitcoin.Api.Blockchain
import Network.Bitcoin.Api.Misc
import Ergvein.Index.Server.Environment

import qualified Data.Bitcoin.Block                     as Btc

import qualified Data.Text.IO as T


btcNodeClient :: Config -> (Client -> IO a) -> IO a
btcNodeClient c = withClient (configBTCNodeHost c) (configBTCNodePort c) (configBTCNodeUser c) (configBTCNodePassword c) 

blocksStream :: Integer -> ServerEnv -> IO ()
blocksStream grantedHeight env = let 
    client :: (Client -> IO a) -> IO a
    client = btcNodeClient $ envConfig env
    go h = do
        blockHash <- client $ flip getBlockHash h
        block <- client $ flip getBlock blockHash
        pure ()
    in sequence_ $ go <$> [0..grantedHeight - 1]

startBlockchainScanner :: MonadUnliftIO m => ServerEnv -> m Thread
startBlockchainScanner env = let 
    in create $ \thread -> liftIO $ do
        x <-  blocksStream 10 env
        T.putStrLn $ pack $ show x
        mortalize thread