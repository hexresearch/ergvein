{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ergvein.Index.Server.BlockchainScanning.Types where

import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Types.Block

data TxInfo = TxInfo 
  { tx'hash        :: TxHash
  , tx'blockHeight :: BlockHeight
  , tx'blockIndex  :: TxBlockIndex
  }

data TxOutInfo = TxOutInfo 
  { txOut'txHash           :: TxHash
  , txOut'pubKeyScriptHash :: PubKeyScriptHash
  , txOut'index            :: TxOutIndex
  , txOut'value            :: MoneyUnit
  }
  
data TxInInfo = TxInInfo  
  { txIn'txHash     :: TxHash
  , txIn'txOutHash  :: TxHash
  , txIn'txOutIndex :: TxOutIndex
  }

data BlockMetaInfo = BlockMetaInfo
  { blockMeta'currency    :: Currency
  , blockMeta'blockHeight  :: BlockHeight
  , blockMeta'headerHexView :: BlockHeaderHexView
  }

data BlockContentInfo = BlockContentInfo
  { blockContent'TxInfos    :: [TxInfo]
  , blockContent'TxInInfos  :: [TxInInfo]
  , blockContent'TxOutInfos :: [TxOutInfo]
  }

data BlockInfo = BlockInfo
  { blockInfo'meta    :: BlockMetaInfo
  , blockInfo'content :: BlockContentInfo
  }