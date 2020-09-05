module Ergvein.Index.Server.BlockchainScanning.Types where

import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Types.Block

import Data.Word

data BlockMetaInfo = BlockMetaInfo
  { blockMetaCurrency                       :: !Currency
  , blockMetaBlockHeight                    :: !BlockHeight
  , blockMetaHeaderHashHexView              :: !BlockHeaderHashHexView
  , blockMetaPreviousHeaderBlockHashHexView :: !BlockHeaderHashHexView
  , blockMetaAddressFilterHexView           :: !AddressFilterHexView
  }

data TxInfo = TxInfo
  { txHash         :: !TxHash
  , txHexView      :: !TxHexView
  , txOutputsCount :: !Word32
  }

data BlockInfo = BlockInfo
  { blockInfoMeta       :: !BlockMetaInfo
  , spentTxsHash        :: ![TxHash]
  , blockContentTxInfos :: ![TxInfo]
  }

data ScanProgressInfo = ScanProgressInfo
  { nfoCurrency      :: !Currency
  , nfoScannedHeight :: !BlockHeight
  , nfoActualHeight  :: !BlockHeight
  }  
