module Ergvein.Wallet.Filters.Btc.Queries(
    insertFilter
  , getFilter
  ) where

import Control.Lens
import Control.Monad.Haskey
import Control.Monad.State.Strict
import Data.BTree.Alloc
import Data.Maybe
import Ergvein.Filters.Btc 
import Ergvein.Wallet.Filters.Btc.Types
import Network.Haskoin.Block

import qualified Data.BTree.Impure as B

insertFilter :: AllocM m => BlockHeight -> BtcAddrFilter -> SchemaBtc -> m SchemaBtc
insertFilter h f = schemaBtcFilters %%~ B.insert h f

getFilter :: AllocReaderM m => BlockHeight -> SchemaBtc -> m (Maybe BtcAddrFilter)
getFilter k s = B.lookup k (s ^. schemaBtcFilters)

