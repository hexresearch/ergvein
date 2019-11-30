module Ergvein.Index.Server.DB.Schema where

import Database.Persist.TH
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Types.Block
import Ergvein.Index.Server.DB.Drv
import Ergvein.Index.Server.BlockScanner.Types
import Conversion
import Database.Persist.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ScannedHeightRec
  currency Currency
  height BlockHeight
  UniqueCurrency currency
  deriving Show
TxRec
  hash TxHash
  blockHeigh BlockHeight
  blockIndex TxBlockIndex
  deriving Show
TxOutRec
  txHash TxHash
  pubKeyScriptHash PubKeyScriptHash
  index TxOutIndex
  value MoneyUnit
  deriving Show
TxInRec
  txHash TxHash
  txOutHash TxHash
  txOutIndex TxOutIndex
  deriving Show
BlockMetaRec
  currency Currency
  height BlockHeight
  blockHeaderHexView BlockHeaderHexView
  deriving Show
  |]

instance Conversion (Entity TxRec) TxInfo where
  convert entity = let 
    value = entityVal entity 
    in TxInfo (txRecHash value) (txRecBlockHeigh value) (txRecBlockIndex value)

instance Conversion (Entity TxInRec) TxInInfo where
  convert entity = let 
    value = entityVal entity 
    in TxInInfo (txInRecTxHash value) (txInRecTxOutHash value) (txInRecTxOutIndex value)

instance Conversion (Entity TxOutRec) TxOutInfo where
  convert entity = let 
    value = entityVal entity 
    in TxOutInfo (txOutRecTxHash value) (txOutRecPubKeyScriptHash value) (txOutRecIndex value) (txOutRecValue value)

instance Conversion (Entity BlockMetaRec) BlockMetaInfo where
  convert entity = let 
    value = entityVal entity 
    in BlockMetaInfo (blockMetaRecCurrency value) (blockMetaRecHeight value) (blockMetaRecBlockHeaderHexView value)