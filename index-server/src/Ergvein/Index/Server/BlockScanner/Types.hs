module Ergvein.Index.Server.BlockScanner.Types where

import Ergvein.Types.Currency
import Ergvein.Types.Transaction

type BlockScanner = BlockHeight -> IO BlockInfo

data TxInfo = TxInfo 
  { tx'hash :: TxHash
  , tx'blockHeight :: BlockHeight
  , tx'blockIndex :: TxBlockIndex
  } deriving Show

data TxOutInfo = TxOutInfo 
  { txOut'txHash  :: TxHash
  , txOut'pubKeyScriptHash :: PubKeyScriptHash
  , txOut'index   :: TxOutIndex
  , txOut'value   :: MoneyUnit
  } deriving Show
  
data TxInInfo = TxInInfo  
  { txIn'txHash   :: TxHash
  , txIn'txOutHash  :: TxHash
  , txIn'txOutIndex :: TxOutIndex
  } deriving Show

data BlockInfo = BlockInfo
  { block'TxInfos :: [TxInfo]
  , block'TxInInfos :: [TxInInfo]
  , block'TxOutInfos :: [TxOutInfo]
  }