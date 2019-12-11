module Ergvein.Wallet.Headers.Types(
    Schema(..)
  , schemaBtc
  , module Ergvein.Wallet.Headers.Btc.Types
  ) where

import Control.Lens (Lens', lens)
import Data.Binary (Binary(..))
import Data.BTree.Primitives (Value)
import Database.Haskey.Alloc.Concurrent (Root)
import Ergvein.Wallet.Headers.Btc.Types
import GHC.Generics (Generic)

data Schema = Schema {
  _schemaBtc :: !SchemaBtc
} deriving (Generic, Show)

instance Binary Schema
instance Value Schema
instance Root Schema

schemaBtc :: Lens' Schema SchemaBtc
schemaBtc = lens _schemaBtc $ \s x -> s { _schemaBtc = x }
