{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Platform(
    Platform(..)
  , currentPlatform
  , isDesktop
  , isAndroid
  ) where

import GHC.Generics (Generic)

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
