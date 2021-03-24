module Ergvein.Index.Server.Types
  (
    BlockMeta(..)
  , BlockInfo (..)
  , TxInfo
  , ScanProgressInfo(..)
  , CacheEntry(..)
  ) where

import Control.DeepSeq
import GHC.Generics
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Word
import Network.Haskoin.Transaction (OutPoint)
import Ergvein.Types

data BlockMeta = BlockMeta
  { blockMetaCurrency      :: !Currency
  , blockMetaBlockHeight   :: !BlockHeight
  , blockMetaHeaderHash    :: !ShortByteString
  , blockMetaPreviousHeaderBlockHash :: !ShortByteString
  , blockMetaAddressFilter :: !ByteString
  } deriving (Show, Generic)

instance NFData BlockMeta

data BlockInfo = BlockInfo
  { blockInfoMeta    :: !BlockMeta
  , spentTxOutputs   :: ![OutPoint]
  , createdTxOutputs :: ![TxInfo]
  } deriving (Show, Generic)
instance NFData BlockInfo

type TxInfo = (ByteString, [(Word32, ByteString)])

type CreatedIds = (ByteString, Word32)

data ScanProgressInfo = ScanProgressInfo
  { nfoCurrency      :: !Currency
  , nfoScannedHeight :: !BlockHeight
  , nfoActualHeight  :: !BlockHeight
  }

data CacheEntry = CacheEntry {
  cacheHeight   :: !BlockHeight
, cacheHash     :: !ShortByteString
, cacheSpent    :: ![OutPoint]
, cacheCreated  :: ![CreatedIds]
} deriving (Show, Generic)
instance NFData CacheEntry
