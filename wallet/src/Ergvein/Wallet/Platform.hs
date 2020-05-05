{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Platform(
    Platform(..)
  , currentPlatform
  , isDesktop
  , isAndroid
  , isTestnet
  , btcNetwork
  , filterStartingHeight
  , avgTimePerBlock
  ) where

import GHC.Generics (Generic)
import Network.Haskoin.Constants
import Network.Haskoin.Block
import Ergvein.Types.Currency

-- | Platform the wallet is compiled for.
data Platform = DesktopLinux | Android
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | Get current platform of wallet
currentPlatform :: Platform
#ifdef ANDROID
currentPlatform = Android
#else
currentPlatform = DesktopLinux
#endif

-- | Helpers to test current platform
isDesktop, isAndroid :: Bool
isDesktop = currentPlatform == DesktopLinux
isAndroid = currentPlatform == Android

-- | Global flag that indicates that we need to compile for testnet.
-- The value of the function is controlled by `testnet` cabal flag.
isTestnet :: Bool
#ifdef TESTNET
isTestnet = True
#else
isTestnet = False
#endif

-- | Network parameters for BTC blockchain. Depends on `isTestnet` flag.
btcNetwork :: Network
btcNetwork = if isTestnet then btcTest else btc
{-# INLINE btcNetwork #-}

-- | The starting height for filters downloader. Start from SegWit adoption
filterStartingHeight :: Currency -> BlockHeight
filterStartingHeight cur = case cur of
  ERGO -> 0
  BTC -> if isTestnet then 834624 else 481824
{-# INLINE filterStartingHeight #-}

-- | Average time it takes to mine a block. In minutes
avgTimePerBlock :: Currency -> Double
avgTimePerBlock cur = case cur of
  ERGO -> 2
  BTC -> 10
{-# INLINE avgTimePerBlock #-}
