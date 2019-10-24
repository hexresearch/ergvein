module Ergvein.Index.Server.BlockchainScanner where

import Data.Text (Text, pack)
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.IO.Unlift
import Control.Immortal
import Ergvein.Index.Server.Config
import Network.Bitcoin.Api.Client
import Network.Bitcoin.Api.Blockchain
import Network.Bitcoin.Api.Misc
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Database.Persist.Sql
import Ergvein.Types.Currency
import Data.Maybe
import Database.Esqueleto
import Database.Persist.Class

import qualified Data.Bitcoin.Block                     as Btc

import qualified Data.Text.IO as T


btcNodeClient :: Config -> (Client -> IO a) -> IO a
btcNodeClient c = withClient (configBTCNodeHost c) (configBTCNodePort c) (configBTCNodeUser c) (configBTCNodePassword c)

runDbQuery :: ServerEnv -> QueryT (ReaderT DBPool (LoggingT IO)) a -> IO a
runDbQuery env query = runStdoutLoggingT $ flip runReaderT (envPool env) $ runDb $ query
    
blocksStream ::  Integer -> ServerEnv -> IO ()
blocksStream grantedHeight env = do
    
    scannedHeight2 <-  runDbQuery env $ getScannedHeight BTC
    let x = 0 `fromMaybe` (scannedHeightRecHeight  <$> entityVal <$> scannedHeight2) 
    sequence_ $ go <$> [0..grantedHeight - 1]
    where
        client :: (Client -> IO a) -> IO a
        client = btcNodeClient $ envConfig env
        go h = do
            blockHash <- client $ flip getBlockHash h
            block <- client $ flip getBlock blockHash
            
            pure ()


startBlockchainScanner :: MonadUnliftIO m => ServerEnv -> m Thread
startBlockchainScanner env = let 
    in create $ \thread -> liftIO $ do
        x <-  blocksStream 10 env
        T.putStrLn $ pack $ show x
        mortalize thread