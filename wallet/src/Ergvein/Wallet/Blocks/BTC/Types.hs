{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Blocks.BTC.Types(
    initBtcDbs
  , getBTCBlocksDb
  ) where

import Database.LMDB.Simple
import Ergvein.Filters.Btc
import Network.Haskoin.Block
import Network.Haskoin.Crypto

import Ergvein.Wallet.Codec()

blocksDbName :: String
blocksDbName = "btcblocks"

-- | Force creation of datab
initBtcDbs :: Transaction ReadWrite ()
initBtcDbs = do
  bdb <- getBTCBlocksDb
  bdb `seq` pure ()

getBTCBlocksDb :: Mode mode => Transaction mode (Database BlockHash Block)
getBTCBlocksDb = getDatabase $ Just blocksDbName
