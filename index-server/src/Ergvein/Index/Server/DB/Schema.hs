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
UtxoRec
  txHash TxHash
  pubKey TxHash
  outIndex TxOutIndex
  outValue MoneyUnit
  UniqueTxHashPubKeyOutIndex txHash pubKey outIndex
StxoRec
  txHash TxHash
  utxoId UtxoRecId
  UniqueTxHashUtxoId txHash utxoId
  |]