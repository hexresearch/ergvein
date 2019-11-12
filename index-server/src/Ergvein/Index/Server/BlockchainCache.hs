module Ergvein.Index.Server.BlockchainCache where 

import Ergvein.Types.Transaction
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Data.Monoid
import Control.Monad.IO.Unlift
import qualified Data.Map as Map
import qualified Data.Set as Set
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.DB.Queries
import Data.Function
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Database.Persist.Types

data BCCache = BCCache
  { txInsBy'OutTxHash'TxOutIndex :: Set.Set (TxHash, TxOutIndex)
  , txOuts :: [TxOutRec]
  }

instance Semigroup BCCache where
  (<>) a b = mempty
    { txInsBy'OutTxHash'TxOutIndex = on Set.union txInsBy'OutTxHash'TxOutIndex a b
    , txOuts = on (++) txOuts a b
    }

instance Monoid BCCache where
  mempty = BCCache
    { txInsBy'OutTxHash'TxOutIndex = mempty
    , txOuts = mempty
    }

class MonadUnliftIO m => BCache m where
  getCache :: m (TVar BCCache)

fromPersisted :: DBPool -> IO BCCache
fromPersisted pool = do 
  txInEntities <- runDbQuery pool getAllTxIn
  txOutEntities <- runDbQuery pool getAllTxOut
  txEntities <- runDbQuery pool getAllTx
  let s = Set.fromList $ (\x' -> (txInRecTxHash x', txInRecTxOutIndex x')) <$> entityVal <$> txInEntities
  pure $ BCCache s $ entityVal <$> txOutEntities