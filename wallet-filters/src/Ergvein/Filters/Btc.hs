module Ergvein.Filters.Btc
  ( -- * SegWit address
    SegWitAddress(..)
  , guardSegWit
  , fromSegWit
    -- * Filter
  , BtcAddressFilter
  , makeBtcFilter
  , applyBtcFilter
  )
where

import           GHC.Generics
import           Network.Haskoin.Crypto         ( Hash160
                                                , Hash256
                                                )
import           Network.Haskoin.Address
import           Network.Haskoin.Block


-- | Special wrapper around SegWit address (P2WPKH or P2WSH) to distinct it from other types of addresses.
data SegWitAddress = SegWitPubkey !Hash160 | SegWitScript !Hash256
  deriving (Eq, Show, Generic)

-- | Nothing if the given address is not SegWit.
guardSegWit :: Address -> Maybe SegWitAddress
guardSegWit a = case a of
  WitnessPubKeyAddress v -> Just $ SegWitPubkey v
  WitnessScriptAddress v -> Just $ SegWitScript v
  _                      -> Nothing

-- | Unwrap segwit to generic BTC address.
fromSegWit :: SegWitAddress -> Address
fromSegWit a = case a of
  SegWitPubkey v -> WitnessPubKeyAddress v
  SegWitScript v -> WitnessScriptAddress v

-- | BIP 158 filter that tracks only Bech32 SegWit addresses that are used in specific block.
data BtcAddressFilter = BtcAddressFilter

-- | Add each segwit transaction to filter.
makeBtcFilter :: Block -> BtcAddressFilter
makeBtcFilter = undefined

-- | Check that given address is located in the filter.
applyBtcFilter :: BtcAddressFilter -> SegWitAddress -> Bool
applyBtcFilter = undefined
