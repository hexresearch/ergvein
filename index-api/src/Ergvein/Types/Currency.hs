module Ergvein.Types.Currency where

import Data.Word
import Data.Text

data Currency = BTC | ERGO

-- | SHA256 hash of locking script with big-endian byte order, used to track transfers due inaccessibility
-- of transaction adresess when indexer scans blockchain
type PubKeyScriptHash = Text

type MoneyUnit = Word64