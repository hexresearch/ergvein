module Ergvein.Wallet.Blocks.Storage(
    BlocksStorage
  , HasBlocksStorage(..)
  , openBlocksStorage
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text (Text, unpack)
import Database.LMDB.Simple
import Ergvein.Wallet.Blocks.Types
import Ergvein.Wallet.Native
import System.Directory

import qualified Ergvein.Wallet.Blocks.BTC.Types as BTC

-- | Name of filters sub storage folder in global storage folder
blocksStorageName :: Text
blocksStorageName = "blocks"

getBlocksStoragePath :: (Monad m, HasStoreDir m) => m Text
getBlocksStoragePath = do
  st <- getStoreDir
  pure $ st <> "/" <> blocksStorageName

openBlocksStorage :: (MonadIO m, MonadMask m, HasStoreDir m) => m BlocksStorage
openBlocksStorage = do
  fn <- getBlocksStoragePath
  let path = unpack fn
  liftIO $ do
    storeEx <- doesDirectoryExist path
    unless storeEx $ createDirectory path
  e <- liftIO $ openEnvironment path $ defaultLimits {
      maxDatabases = 6 -- TODO: update when we need more dbs for new currencies
    , mapSize = 1024 * 1024 * 4 * 1024 } -- 4 GB max size
  liftIO $ readWriteTransaction e BTC.initBtcDbs
  pure e
