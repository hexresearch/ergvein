module Ergvein.Wallet.Filters.Btc.Types(
    getBtcFiltersDb
  , getBtcHeightsDb
  , getBtcTotalDb
  ) where

import Database.LMDB.Simple
import Ergvein.Filters.Btc
import Ergvein.Wallet.Platform
import Network.Haskoin.Block
import Network.Haskoin.Crypto

import qualified Data.BTree.Impure as B
import qualified Data.Serialize as S

filtersDbName :: String 
filtersDbName = "btcfilters"

heightsDbName :: String 
heightsDbName = "btcheights"

totalDbName :: String 
totalDbName = "btctotal"

getBtcFiltersDb :: Transaction ReadWrite (Database BlockHash BtcAddrFilter)
getBtcFiltersDb = getDatabase $ Just filtersDbName

getBtcHeightsDb :: Transaction ReadWrite (Database BlockHeight BlockHash)
getBtcHeightsDb = getDatabase $ Just heightsDbName

getBtcTotalDb :: Transaction ReadWrite (Database () BlockHeight)
getBtcTotalDb = getDatabase $ Just totalDbName