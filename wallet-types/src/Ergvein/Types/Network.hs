{-# LANGUAGE CPP #-}

module Ergvein.Types.Network (
    BtcNetwork
  , Network(..)
  , EgvNetwork(..)
  , btc
  , btcTest
  , getCurrencyNetwork
  , getCurrencyIndex
  ) where

import Ergvein.Crypto.Keys
import Ergvein.Types.Currency
import Flat
import Network.Haskoin.Constants

type BtcNetwork = Network

data EgvNetwork
  = EgvBtcNetwork {getBtcNetwork :: !BtcNetwork}
  deriving (Eq, Generic)

#ifdef TESTNET
-- | Get network correspondent to a given tag
getCurrencyNetwork :: Currency -> EgvNetwork
getCurrencyNetwork t = case t of
  BTC -> EgvBtcNetwork btcTest

getCurrencyIndex :: Currency -> KeyIndex
getCurrencyIndex = const 1
{-# INLINE getCurrencyIndex #-}

#else
getCurrencyNetwork :: Currency -> EgvNetwork
getCurrencyNetwork t = case t of
  BTC -> EgvBtcNetwork btc

getCurrencyIndex :: Currency -> KeyIndex
getCurrencyIndex t = case t of
  BTC -> getBip44Coin btc
#endif
