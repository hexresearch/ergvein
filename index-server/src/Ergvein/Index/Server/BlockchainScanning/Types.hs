module Ergvein.Index.Server.BlockchainScanning.Types where

import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Types.Block

data BlockMetaInfo = BlockMetaInfo
  { blockMetaCurrency      :: Currency
  , blockMetaBlockHeight   :: BlockHeight
  , blockMetaHeaderHashHexView :: BlockHeaderHashHexView
  , blockMetaAddressFilterHexView :: AddressFilterHexView
  }

data TxInfo = TxInfo
  { txHash      :: TxHash
  , txHexView     :: TxHexView
  , txOutputsCount :: Word
  }

data BlockInfo = BlockInfo
  { blockInfoMeta    :: BlockMetaInfo
  , spentTxsHash :: [TxHash]
  , blockContentTxInfos :: [TxInfo]
  }