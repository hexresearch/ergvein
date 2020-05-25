{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Blocks.BTC.Types(
    initBtcDbs
  , getBtcBlocksDb
  , getBtcTxsToBlocksDb
  ) where

import Database.LMDB.Simple
import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Network.Haskoin.Transaction

import Ergvein.Wallet.Codec()

blocksDbName :: String
blocksDbName = "btcblocks"

txsToBlocksDbName :: String
txsToBlocksDbName = "btcTxsToBlocks"

-- | Force creation of datab
initBtcDbs :: Transaction ReadWrite ()
initBtcDbs = do
  bdb <- getBtcBlocksDb
  tdb <- getBtcTxsToBlocksDb
  bdb `seq` tdb `seq` pure ()

getBtcBlocksDb :: Mode mode => Transaction mode (Database BlockHash Block)
getBtcBlocksDb = getDatabase $ Just blocksDbName

getBtcTxsToBlocksDb :: Mode mode => Transaction mode (Database TxHash BlockHash)
getBtcTxsToBlocksDb = getDatabase $ Just txsToBlocksDbName
