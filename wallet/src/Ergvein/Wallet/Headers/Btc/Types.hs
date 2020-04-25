{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Headers.Btc.Types(
    initBtcDbs
  , getBtcHeadersDB
  , getBtcBestHeaderDB
  ) where

import Database.LMDB.Simple
import Network.Haskoin.Block
import Network.Haskoin.Crypto

import Ergvein.Wallet.Codec()

btcHeadersDBName :: String
btcHeadersDBName = "btcheaders"

btcBestHeaderDBName :: String
btcBestHeaderDBName = "btcbestheader"

-- | Force creation of datab
initBtcDbs :: Transaction ReadWrite ()
initBtcDbs = do
  fdb <- getBtcHeadersDB
  bdb <- getBtcBestHeaderDB
  bdb `seq` fdb `seq` pure ()

getBtcHeadersDB :: Mode mode => Transaction mode (Database BlockHash BlockNode)
getBtcHeadersDB = getDatabase $ Just btcHeadersDBName

getBtcBestHeaderDB :: Mode mode => Transaction mode (Database () BlockNode)
getBtcBestHeaderDB = getDatabase $ Just btcBestHeaderDBName
