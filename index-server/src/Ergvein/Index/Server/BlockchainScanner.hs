module Ergvein.Index.Server.BlockchainScanner where

import Control.Concurrent
import Control.Immortal
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Maybe
import Data.Text (Text, pack)
import Data.Word
import Database.Esqueleto
import Database.Persist.Class
import Database.Persist.Sql
import Network.Bitcoin.Api.Blockchain
import Network.Bitcoin.Api.Client
import Network.Bitcoin.Api.Misc

import qualified Data.Bitcoin.Block as Btc
import qualified Data.Text.IO       as T

import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.Environment
import Ergvein.Types.Currency

btcNodeClient :: Config -> (Client -> IO a) -> IO a
btcNodeClient c = withClient (configBTCNodeHost c) (configBTCNodePort c) (configBTCNodeUser c) (configBTCNodePassword c)

runDbQuery :: DBPool -> QueryT (ReaderT DBPool (LoggingT IO)) a -> IO a
runDbQuery pool query = runStdoutLoggingT $ flip runReaderT pool $ runDb $ query

scannedBlockCount :: DBPool -> Currency -> IO BlockHeight
scannedBlockCount pool currency =
    fromMaybe defaultCount <$> persistedCount
    where
      persistedCount = do
        entity <- runDbQuery pool $ getScannedHeight currency
        pure $ scannedHeightRecHeight . entityVal <$> entity
      defaultCount = case currency of BTC  -> 0
                                      ERGO -> 0

bTCBlockScanner :: ServerEnv -> BlockHeight -> IO ()
bTCBlockScanner env blockHeightToScan = do
    let cfg = envConfig env
    blockHash <- btcNodeClient cfg $ flip getBlockHash $ fromIntegral blockHeightToScan
    block <- btcNodeClient cfg $ flip getBlock blockHash
    runDbQuery (envPool env) $ updateScannedHeight BTC $ blockHeightToScan
    pure ()

blocksToScan :: ServerEnv -> Currency -> IO [BlockHeight]
blocksToScan env currency = do
    actual  <- actualHeight
    scanned <- scannedBlockCount (envPool env) BTC
    pure [scanned..actual]
    where
      cfg = envConfig env
      actualHeight = fromIntegral <$>
        case currency of
          BTC  -> btcNodeClient cfg getBlockCount
          ERGO -> undefined

scannerThread :: MonadUnliftIO m => Int -> IO [BlockHeight] -> (BlockHeight -> IO ()) -> m Thread
scannerThread scanDelay heightsM scanner = 
    create iteration
    where
      delay = threadDelay scanDelay
      iteration thread = liftIO $ do
        heights <- heightsM
        sequence_ $ scanner <$> heights
        delay

startBlockchainScanner :: MonadUnliftIO m => ServerEnv -> m [Thread]
startBlockchainScanner env = sequenceA 
    [
    scannerThread 5000000 (blocksToScan env BTC) $ bTCBlockScanner env
    ]