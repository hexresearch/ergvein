module Ergvein.Index.Server.DB.Schema where

import Database.Persist.TH
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Index.Server.DB.Drv

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
  |]