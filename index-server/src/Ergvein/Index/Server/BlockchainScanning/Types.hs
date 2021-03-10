module Ergvein.Index.Server.BlockchainScanning.Types where

import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import Control.DeepSeq
import Data.ByteString
import Data.ByteString.Short (ShortByteString)
import Data.Word
import GHC.Generics
import Data.HashMap.Strict (HashMap)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField

data BlockMetaInfo = BlockMetaInfo
  { blockMetaCurrency                 :: !Currency
  , blockMetaBlockHeight              :: !BlockHeight
  , blockMetaHeaderHash               :: !ShortByteString
  , blockMetaPreviousHeaderBlockHash  :: !ShortByteString
  , blockMetaAddressFilter            :: !ByteString
  }

data TxInfo = TxInfo
  { txHash         :: ByteString
  , txBytes        :: ByteString
  , txOutputsCount :: Word32
  } deriving (Show, Generic)

instance ToRow TxInfo where
  toRow (TxInfo th tr tu) = [toField th, toField tr, toField tu]

instance NFData TxInfo

data BlockInfo = BlockInfo
  { blockInfoMeta       :: !BlockMetaInfo
  , spentTxOutputs      :: !(HashMap ByteString Word32)
  , blockContentTxInfos :: ![TxInfo]
  }

data ScanProgressInfo = ScanProgressInfo
  { nfoCurrency      :: !Currency
  , nfoScannedHeight :: !BlockHeight
  , nfoActualHeight  :: !BlockHeight
  }
