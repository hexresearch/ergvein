module Ergvein.Wallet.Filters.Btc.Types(
    initBtcDbs
  , getBtcFiltersDb
  , getBtcHeightsDb
  , getBtcTotalDb
  ) where

import Database.LMDB.Simple
import Ergvein.Filters.Btc
import Network.Haskoin.Block
import Network.Haskoin.Crypto

import Ergvein.Wallet.Codec()

filtersDbName :: String
filtersDbName = "btcfilters"

heightsDbName :: String
heightsDbName = "btcheights"

totalDbName :: String
totalDbName = "btctotal"

-- | Force creation of datab
initBtcDbs :: Transaction ReadWrite ()
initBtcDbs = do
  fdb <- getBtcFiltersDb
  hdb <- getBtcHeightsDb
  tdb <- getBtcTotalDb
  tdb `seq` hdb `seq` fdb `seq` pure ()

getBtcFiltersDb :: Mode mode => Transaction mode (Database BlockHash BtcAddrFilter)
getBtcFiltersDb = getDatabase $ Just filtersDbName

getBtcHeightsDb :: Mode mode => Transaction mode (Database BlockHeight BlockHash)
getBtcHeightsDb = getDatabase $ Just heightsDbName

getBtcTotalDb :: Mode mode => Transaction mode (Database () BlockHeight)
getBtcTotalDb = getDatabase $ Just totalDbName
