module Ergvein.Wallet.Headers.Btc.Queries(
    insertHeader
  , getHeader
  , setBestHeader
  , getBestHeader
  ) where

import Control.Lens
import Control.Monad.Haskey
import Data.BTree.Alloc
import Data.Maybe
import Ergvein.Wallet.Headers.Btc.Types
import Network.Haskoin.Block

import qualified Data.BTree.Impure as B

insertHeader :: AllocM m => BlockNode -> SchemaBtc -> m SchemaBtc
insertHeader n = schemaBtcHeaders %%~ B.insert (blockNodeId n) n

getHeader :: AllocReaderM m => BlockHash -> SchemaBtc -> m (Maybe BlockNode)
getHeader k s = B.lookup k (s ^. schemaBtcHeaders)

setBestHeader :: AllocM m => BlockNode -> SchemaBtc -> m SchemaBtc
setBestHeader n = schemaBtcBestBlock %%~ B.insert () n

getBestHeader :: AllocReaderM m => SchemaBtc -> m BlockNode
getBestHeader s = fromMaybe defaultBestBlock <$> B.lookup () (s ^. schemaBtcBestBlock)
