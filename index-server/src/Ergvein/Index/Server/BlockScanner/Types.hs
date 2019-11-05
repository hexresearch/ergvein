module Ergvein.Index.Server.BlockScanner.Types where

import Ergvein.Types.Currency
import Ergvein.Types.Transaction

data TXOInfo = TXOInfo 
  { txo'txHash     :: TxHash
  , txo'scriptHash :: PubKeyScriptHash
  , txo'outIndex   :: TxOutIndex
  , txo'outValue   :: MoneyUnit
  } deriving Show
  
data SpentTXOInfo = SpentTXOInfo  
  { stxo'txHash   :: TxHash
  , stxo'txoHash  :: TxHash
  , stxo'outIndex :: TxOutIndex
  } deriving Show