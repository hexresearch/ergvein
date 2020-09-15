module Ergvein.Index.Server.BlockchainScanning.Types where

import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import Control.DeepSeq
import Data.ByteString
import Data.ByteString.Short (ShortByteString)
import Data.Word
import GHC.Generics
import Data.Map.Strict (Map)

data BlockMetaInfo = BlockMetaInfo
  { blockMetaCurrency      :: !Currency
  , blockMetaBlockHeight   :: !BlockHeight
  , blockMetaHeaderHash :: !ShortByteString
  , blockMetaPreviousHeaderBlockHash :: !ShortByteString
  , blockMetaAddressFilter :: !ByteString
  }

data TxInfo = TxInfo
  { txHash         :: TxHash
  , txBytes        :: ByteString
  , txOutputsCount :: Word32
  } deriving (Show, Generic)

instance NFData TxInfo

data BlockInfo = BlockInfo
  { blockInfoMeta       :: !BlockMetaInfo
  , spentTxOutputs      :: !(Map TxHash Word32)
  , blockContentTxInfos :: ![TxInfo]
  }

data ScanProgressInfo = ScanProgressInfo
  { nfoCurrency      :: !Currency
  , nfoScannedHeight :: !BlockHeight
  , nfoActualHeight  :: !BlockHeight
  }
