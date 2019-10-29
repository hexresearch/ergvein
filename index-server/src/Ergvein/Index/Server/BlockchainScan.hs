module Ergvein.Index.Server.BlockchainScan where

import Control.Concurrent
import Control.Immortal
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Maybe
import Data.Either
import Database.Persist.Sql
import Network.Bitcoin.Api.Blockchain
import Network.Bitcoin.Api.Client
import Data.Serialize (decode, encode)

import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.Environment
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.HexString as HS
import qualified Network.Haskoin.Block as HK
import qualified Network.Haskoin.Transaction as HK
import qualified Data.Text.IO as T
import Data.Text (Text, pack)

btcNodeClient :: Config -> (Client -> IO a) -> IO a
btcNodeClient cfg = withClient 
  (configBTCNodeHost     cfg)
  (configBTCNodePort     cfg)
  (configBTCNodeUser     cfg)
  (configBTCNodePassword cfg)

scannedBlockHeight :: DBPool -> Currency -> IO (Maybe BlockHeight)
scannedBlockHeight pool currency = do
        entity <- runDbQuery pool $ getScannedHeight currency
        pure $ scannedHeightRecHeight . entityVal <$> entity

data Unspent = Unspent {
  txHash :: String,
  pubKey :: String,
  amount :: MoneyUnit
}

data Spent = Spent  {
  txHashTarget :: String,
  stxHash :: String,
  spubKey :: String
}

utxo :: HS.HexString -> ([Unspent], [Spent])
utxo str = let
  block = decode $ HS.toBytes str
  b = HK.blockTxns $ fromRight (error "") block
  in mconcat $ f <$> b
  where f tx = mempty

bTCBlockScanner :: ServerEnv -> BlockHeight -> IO ()
bTCBlockScanner env blockHeightToScan = do
    let cfg = envConfig env
    blockHash <- btcNodeClient cfg $ flip getBlockHash $ fromIntegral blockHeightToScan
    blockM <-  btcNodeClient cfg $ flip getBlockRaw blockHash
    let block :: Either String HK.Block
        block = decode $ HS.toBytes $ fromMaybe (error "") blockM
        b = fromRight (error "") block
    let x =  HK.txOut =<< HK.blockTxns b
    T.putStrLn $ pack $ show $ HK.outValue <$> x
    runDbQuery (envPool env) $ updateScannedHeight BTC blockHeightToScan
    pure ()

blockHeightsToScan :: ServerEnv -> Currency -> IO [BlockHeight]
blockHeightsToScan env currency = do
    actual  <- actualHeight
    scanned <- scannedBlockHeight (envPool env) currency
    let start = fromMaybe startHeight $ succ <$> scanned
    pure [start..actual]
    where
      cfg = envConfig env
      actualHeight = fromIntegral <$> case currency of BTC  -> btcNodeClient cfg getBlockCount
                                                       ERGO -> undefined
      startHeight = case currency of BTC  -> 0
                                     ERGO -> 0

scannerThread :: MonadUnliftIO m => Int -> IO [BlockHeight] -> (BlockHeight -> IO ()) -> m Thread
scannerThread scanDelay heightsM scanner = 
    create iteration
    where
      iteration thread = liftIO $ do
        heights <- heightsM
        sequence_ $ scanner <$> heights
        threadDelay scanDelay

startBlockchainScanner :: MonadUnliftIO m => ServerEnv -> m [Thread]
startBlockchainScanner env = sequenceA 
    [
    scannerThread (configBlockchainScanDelay $ envConfig  env) (blockHeightsToScan env BTC) $ bTCBlockScanner env
    ]