{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Platform(
    Platform(..)
  , currentPlatform
  , isDesktop
  , isAndroid
  , isTestnet
  , btcNetwork
  ) where

import GHC.Generics (Generic)
import Network.Haskoin.Constants

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
