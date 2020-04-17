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
  { txHash2      :: TxHash
  , txHexView2     :: TxHexView
  , txOutputsCount :: Word
  }

data BlockInfo = BlockInfo
  { blockInfoMeta    :: BlockMetaInfo
  , spentTxsHash :: [TxHash]
  , blockContentTxInfos :: [TxInfo]
  }