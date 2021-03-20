module Ergvein.Index.Server.Types
  (
    BlockMeta(..)
  , BlockInfo (..)
  , TxInfo
  ) where

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
  } deriving (Show)

data BlockInfo = BlockInfo
  { blockInfoMeta    :: !BlockMeta
  , spentTxOutputs   :: ![OutPoint]
  , createdTxOutputs :: ![TxInfo]
  } deriving (Show)

type TxInfo = (TxHash, [(Word32, ByteString)])
