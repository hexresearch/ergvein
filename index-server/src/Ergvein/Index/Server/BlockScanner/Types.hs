{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ergvein.Index.Server.BlockScanner.Types where

import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import GHC.Generics
import Control.DeepSeq

type BlockScanner = BlockHeight -> IO BlockInfo

data TxInfo = TxInfo 
  { tx'hash        :: TxHash
  , tx'blockHeight :: BlockHeight
  , tx'blockIndex  :: TxBlockIndex
  } deriving (Generic, Show, NFData)

data TxOutInfo = TxOutInfo 
  { txOut'txHash           :: TxHash
  , txOut'pubKeyScriptHash :: PubKeyScriptHash
  , txOut'index            :: TxOutIndex
  , txOut'value            :: MoneyUnit
  } deriving (Generic, Show, NFData)
  
data TxInInfo = TxInInfo  
  { txIn'txHash     :: TxHash
  , txIn'txOutHash  :: TxHash
  , txIn'txOutIndex :: TxOutIndex
  } deriving (Generic, Show, NFData)

data BlockInfo = BlockInfo
  { block'TxInfos    :: [TxInfo]
  , block'TxInInfos  :: [TxInInfo]
  , block'TxOutInfos :: [TxOutInfo]
  } deriving (Generic, Show, NFData)