module Ergvein.Index.Server.BlockchainScanning.Types where

import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Types.Block

data TxInfo = TxInfo
  { txHash        :: TxHash
  , txHexView     :: TxHexView
  , txBlockHeight :: BlockHeight
  , txBlockIndex  :: TxBlockIndex
  }

data TxOutInfo = TxOutInfo
  { txOutTxHash           :: TxHash
  , txOutPubKeyScriptHash :: PubKeyScriptHash
  , txOutIndex            :: TxOutIndex
  , txOutValue            :: MoneyUnit
  }

data TxInInfo = TxInInfo
  { txInTxHash     :: TxHash
  , txInTxOutHash  :: TxHash
  , txInTxOutIndex :: TxOutIndex
  }

data BlockMetaInfo = BlockMetaInfo
  { blockMetaCurrency      :: Currency
  , blockMetaBlockHeight   :: BlockHeight
  , blockMetaHeaderHashHexView :: BlockHeaderHashHexView
  , blockMetaAddressFilterHexView :: AddressFilterHexView
  }

data BlockContentInfo = BlockContentInfo
  { blockContentTxInfos    :: [TxInfo]
  , blockContentTxInInfos  :: [TxInInfo]
  , blockContentTxOutInfos :: [TxOutInfo]
  }

data TxInfo2 = TxInfo2
  { txHash2      :: TxHash
  , txHexView2     :: TxHexView
  , txOutputsCount :: Word
  }

data BlockInfo = BlockInfo
  { blockInfoMeta    :: BlockMetaInfo
  , spentTxsHash :: [TxHash]
  , blockContentTxInfos2 :: [TxInfo2]
  , blockInfoContent :: BlockContentInfo
  }