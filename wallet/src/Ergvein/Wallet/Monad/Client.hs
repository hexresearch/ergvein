module Ergvein.Wallet.Monad.Client
  (
    MonadClient(..)
  , module Ergvein.Index.API.Types
  ) where

import Data.Text
import Reflex

import Ergvein.Index.API.Types
import Ergvein.Wallet.Monad.Base

-- | Implements client logic, requesting multiple indexers and verifying results
class MonadBaseConstr t m => MonadClient t m | m -> t where
  --  Helper functions
  -- | Set the number of required confirmations
  setRequiredUrlNum :: Event t Int -> m ()
  -- | Get a list of indexer urls. At most N urls, where N is set by setRequiredUrlNum
  getUrlList :: Event t () -> m (Event t [Text])
  -- | Add a number of urls to the set of valid urls
  addUrls :: Event t [Text] -> m ()
  -- | Remove a number of urls from the set of valid urls
  invalidateUrls :: Event t [Text] -> m ()
  -- Client functions
  getBalance :: Event t BalanceRequest -> m (Event t BalanceResponse)
  getTxHashHistory :: Event t TxHashHistoryRequest -> m (Event t TxHashHistoryResponse)
  getTxMerkleProof :: Event t TxMerkleProofRequest -> m (Event t TxMerkleProofResponse)
  getTxHexView :: Event t TxHexViewRequest -> m (Event t TxHexViewResponse)
  getTxFeeHistogram :: Event t TxFeeHistogramRequest -> m (Event t TxFeeHistogramResponse)
  txBroadcast :: Event t TxBroadcastRequest -> m (Event t TxBroadcastResponse)
