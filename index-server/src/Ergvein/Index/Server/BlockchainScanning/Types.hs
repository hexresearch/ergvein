module Ergvein.Index.Server.BlockchainScanning.Types where

import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Types.Block

data TxInfo = TxInfo
  { txHash        :: TxHash
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
  , blockMetaHeaderHexView :: BlockHeaderHexView
  }

data BlockContentInfo = BlockContentInfo
  { blockContentTxInfos    :: [TxInfo]
  , blockContentTxInInfos  :: [TxInInfo]
  , blockContentTxOutInfos :: [TxOutInfo]
  }

data BlockInfo = BlockInfo
  { blockInfoMeta    :: BlockMetaInfo
  , blockInfoContent :: BlockContentInfo
  }
