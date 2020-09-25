{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Filters.Btc.Types(
    initBtcDbs
  , cleanBtcDbs
  , getBtcFiltersDb
  , getBtcHeightsDb
  , getBtcTotalDb
  ) where

import Data.ByteString
import Database.LMDB.Simple
import Ergvein.Types.Transaction

import Ergvein.Types.Currency
import Ergvein.Wallet.Codec()
import Ergvein.Wallet.Platform

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
  mtotal <- get tdb ()
  let h = filterStartingHeight BTC
  case mtotal of
    Nothing -> put tdb () $ Just h
    Just v -> if h > v then put tdb () $ Just h else pure ()

cleanBtcDbs :: Transaction ReadWrite ()
cleanBtcDbs = do
  clear =<< getBtcFiltersDb
  clear =<< getBtcHeightsDb
  clear =<< getBtcTotalDb

getBtcFiltersDb :: Mode mode => Transaction mode (Database BlockHash ByteString)
getBtcFiltersDb = getDatabase $ Just filtersDbName

getBtcHeightsDb :: Mode mode => Transaction mode (Database BlockHeight BlockHash)
getBtcHeightsDb = getDatabase $ Just heightsDbName

getBtcTotalDb :: Mode mode => Transaction mode (Database () BlockHeight)
getBtcTotalDb = getDatabase $ Just totalDbName
