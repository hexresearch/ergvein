module Ergvein.Wallet.Headers.Btc.Types(
    SchemaBtc(..)
  , emptySchemaBtc
  , schemaBtcHeaders
  , schemaBtcBestBlock
  , blockNodeId
  , defaultBestBlock
  ) where

import Control.Lens (Lens', lens)
import Control.Monad.Haskey
import Data.Binary (Binary(..))
import Data.BTree.Impure (Tree)
import Data.BTree.Primitives (Value)
import Data.Text (Text)
import Ergvein.Wallet.Platform
import GHC.Generics (Generic)
import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Data.BTree.Primitives.Key

import qualified Data.BTree.Impure as B
import qualified Data.Serialize as S

data SchemaBtc = SchemaBtc {
  _schemaBtcHeaders   :: Tree BlockHash BlockNode
, _schemaBtcBestBlock :: Tree () BlockNode
} deriving (Generic, Show)

instance Binary SchemaBtc
instance Value SchemaBtc

emptySchemaBtc :: SchemaBtc
emptySchemaBtc = SchemaBtc B.empty B.empty

schemaBtcHeaders :: Lens' SchemaBtc (Tree BlockHash BlockNode)
schemaBtcHeaders = lens _schemaBtcHeaders $ \s x -> s { _schemaBtcHeaders = x }

schemaBtcBestBlock :: Lens' SchemaBtc (Tree () BlockNode)
schemaBtcBestBlock = lens _schemaBtcBestBlock $ \s x -> s { _schemaBtcBestBlock = x }

blockNodeId :: BlockNode -> BlockHash
blockNodeId = headerHash . nodeHeader

defaultBestBlock :: BlockNode
defaultBestBlock = genesisNode btcNetwork

deriving instance Generic BlockHash
deriving instance Generic BlockHeader
deriving instance Generic BlockNode
instance Binary BlockHash
instance Binary BlockHeader
instance Binary BlockNode

instance Binary Hash256 where
  put = put . S.runPut . S.put
  {-# INLINE put #-}
  get = do
    bs <- get
    either fail pure $ S.runGet S.get bs
  {-# INLINE get #-}

instance Value BlockNode
instance Value BlockHash
instance Key BlockHash
