module Ergvein.Filters.Btc.TestVectors where

import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Map.Strict (Map)
import Ergvein.Filters.Btc.Index
import Ergvein.Filters.Btc.TestHelpers
import Ergvein.Text
import Network.Haskoin.Block
import Network.Haskoin.Transaction

import qualified Data.Map.Strict as M

data TVecRow = TVecRow {
  tvecHeight     :: !Integer
, tvecBlockHash  :: !BlockHash
, tvecBlock      :: !Block
, tvecScripts    :: ![ByteString]
, tvecPrevHash   :: !FilterHash
, tvecFilter     :: !Text
, tvecFilterHash :: !FilterHash
, tvecNote       :: !Text
}

-- Got from https://github.com/jimpo/bitcoin/blob/c7efb652f3543b001b4dd22186a354605b14f47e/src/test/data/blockfilters.json
testVector :: [TVecRow]
testVector = [
      TVecRow {
      tvecHeight     = 0
    , tvecBlockHash  = loadBlockHash "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"
    , tvecBlock      = loadBlock "0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4adae5494dffff001d1aa4ae180101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff4d04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73ffffffff0100f2052a01000000434104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac00000000"
    , tvecScripts    = []
    , tvecPrevHash   = filterHashFromText "0000000000000000000000000000000000000000000000000000000000000000"
    , tvecFilter     = "019dfca8"
    , tvecFilterHash = filterHashFromText "21584579b7eb08997773e5aeff3a7f932700042d0ed2a6129012b7d7ae81b750"
    , tvecNote       = "Genesis block"
    }
    , TVecRow {
      tvecHeight     = 2
    , tvecBlockHash  = loadBlockHash "000000006c02c8ea6e4ff69651f7fcde348fb9d557a06e6957b65552002a7820"
    , tvecBlock      = loadBlock "0100000006128e87be8b1b4dea47a7247d5528d2702c96826c7a648497e773b800000000e241352e3bec0a95a6217e10c3abb54adfa05abb12c126695595580fb92e222032e7494dffff001d00d235340101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0e0432e7494d010e062f503253482fffffffff0100f2052a010000002321038a7f6ef1c8ca0c588aa53fa860128077c9e6c11e6830f4d7ee4e763a56b7718fac00000000"
    , tvecScripts    = []
    , tvecPrevHash   = filterHashFromText "d7bdac13a59d745b1add0d2ce852f1a0442e8945fc1bf3848d3cbffd88c24fe1"
    , tvecFilter     = "0174a170"
    , tvecFilterHash = filterHashFromText "186afd11ef2b5e7e3504f2e8cbf8df28a1fd251fe53d60dff8b1467d1b386cf0"
    , tvecNote       = ""
    }
  ]

testVecPrevIndex :: TVecRow -> Map OutPoint ByteString
testVecPrevIndex TVecRow{..} = M.fromList $ outs `zip` tvecScripts
  where
    outs = fmap prevOutput $ concat $ fmap txIn $ blockTxns tvecBlock

withPrevScripts :: TVecRow -> (ReaderT (Map OutPoint ByteString) m a) -> m a
withPrevScripts = flip runReaderT . testVecPrevIndex
