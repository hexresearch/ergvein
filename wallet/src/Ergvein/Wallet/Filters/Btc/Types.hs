{-# OPTIONS_GHC -Wno-orphans #-}
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

import qualified Codec.Serialise as S
import qualified Data.Serialize as Cereal

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

deriving instance S.Serialise BlockHash

instance S.Serialise Hash256 where
  encode = S.encode . Cereal.encode
  {-# INLINE encode #-}
  decode = do
    bs <- S.decode
    either fail pure $ Cereal.decode bs
  {-# INLINE decode #-}

instance S.Serialise BtcAddrFilter where
  encode = S.encode . encodeBtcAddrFilter
  {-# INLINE encode #-}
  decode = do
    bs <- S.decode
    either fail pure $ decodeBtcAddrFilter bs
  {-# INLINE decode #-}
