module Ergvein.Wallet.Headers.Btc.Queries(
    BTCHeadersStorage(..)
  , runBTCHeaderQuery
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Foldable (traverse_)
import Data.Maybe
import Database.LMDB.Simple
import Network.Haskoin.Block

import Ergvein.Wallet.Headers.Btc.Types
import Ergvein.Wallet.Headers.Types
import Ergvein.Wallet.Platform

insertHeader :: MonadIO m => BlockNode -> HeadersStorage -> m ()
insertHeader bhead e = liftIO . readWriteTransaction e $ do
  db <- getBtcHeadersDB
  put db (headerHash $ nodeHeader bhead) $ Just bhead

insertMultipleHeaders :: MonadIO m => [BlockNode] -> HeadersStorage -> m ()
insertMultipleHeaders bheads e = liftIO . readWriteTransaction e $ do
  db <- getBtcHeadersDB
  flip traverse_ bheads $ \bhead ->
    put db (headerHash $ nodeHeader bhead) $ Just bhead

getHeader :: MonadIO m => BlockHash -> HeadersStorage -> m (Maybe BlockNode)
getHeader bhash e = liftIO . readOnlyTransaction e $ do
  db <- getBtcHeadersDB
  get db bhash

setBestHeader :: MonadIO m => BlockNode -> HeadersStorage -> m ()
setBestHeader bhead e = liftIO . readWriteTransaction e $ do
  db <- getBtcBestHeaderDB
  put db () $ Just bhead

getBestHeader :: MonadIO m => HeadersStorage -> m BlockNode
getBestHeader e = liftIO . readOnlyTransaction e $ do
  db <- getBtcBestHeaderDB
  fromMaybe defaultBestBlock <$> get db ()

defaultBestBlock :: BlockNode
defaultBestBlock = genesisNode btcNetwork

newtype BTCHeadersStorage m a = BTCHeadersStorage {unBTCHeadersStorage :: ReaderT HeadersStorage m a}
  deriving (Functor,  Applicative, Monad, MonadReader HeadersStorage, MonadIO)

instance MonadIO m => BlockHeaders (BTCHeadersStorage m) where
  addBlockHeader bh = BTCHeadersStorage $ insertHeader bh =<< ask
  getBlockHeader bh = BTCHeadersStorage $ getHeader bh =<< ask
  getBestBlockHeader = BTCHeadersStorage $ getBestHeader =<< ask
  setBestBlockHeader bh = BTCHeadersStorage $ setBestHeader bh =<< ask
  addBlockHeaders bhs = BTCHeadersStorage $ insertMultipleHeaders bhs =<< ask

runBTCHeaderQuery :: (HasHeadersStorage m) => BTCHeadersStorage m a -> m a
runBTCHeaderQuery ma = do
  hs <- getHeadersStorage
  runReaderT (unBTCHeadersStorage ma) hs
