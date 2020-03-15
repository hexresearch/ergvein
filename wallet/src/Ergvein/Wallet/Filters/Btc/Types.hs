module Ergvein.Wallet.Filters.Btc.Types(
    SchemaBtc(..)
  , emptySchemaBtc
  , schemaBtcFilters
  , schemaBtcHeights
  ) where

import Control.Lens (Lens', lens)
import Control.Monad.Haskey
import Control.Monad.Haskey.Haskoin
import Data.Binary (Binary(..))
import Data.BTree.Impure (Tree)
import Data.BTree.Primitives (Value)
import Data.BTree.Primitives.Key
import Data.Text (Text)
import Ergvein.Filters.Btc
import Ergvein.Wallet.Platform
import GHC.Generics (Generic)
import Network.Haskoin.Block
import Network.Haskoin.Crypto

import qualified Data.BTree.Impure as B
import qualified Data.Serialize as S

data SchemaBtc = SchemaBtc {
  _schemaBtcFilters   :: Tree BlockHash BtcAddrFilter
, _schemaBtcHeights   :: Tree BlockHeight BlockHash
} deriving (Generic, Show)

instance Binary SchemaBtc
instance Value SchemaBtc

emptySchemaBtc :: SchemaBtc
emptySchemaBtc = SchemaBtc B.empty B.empty

schemaBtcFilters :: Lens' SchemaBtc (Tree BlockHash BtcAddrFilter)
schemaBtcFilters = lens _schemaBtcFilters $ \s x -> s { _schemaBtcFilters = x }

schemaBtcHeights :: Lens' SchemaBtc (Tree BlockHeight BlockHash)
schemaBtcHeights = lens _schemaBtcHeights $ \s x -> s { _schemaBtcHeights = x }

instance Value BtcAddrFilter