{-# LANGUAGE TemplateHaskell #-}
module Ergvein.Core.Status.Types(
    WalletStatus(..)
  , WalletStatusNormal(..)
  , RestoreStage(..)
  , WalletStatusRestore(..)
  , emptyWalletStatus
  , CurrencyStatus(..)
  , calcPercentage
  -- * Lens
  , walletStatus'normal
  , walletStatus'restore
  , walletStatusRestore'stage
  , walletStatusRestore'progress
  ) where

import Data.Default
import Data.Word
import Ergvein.Lens
import Ergvein.Types.Currency

calcPercentage :: Word64 -> Word64 -> Word64 -> Double
calcPercentage from to now = 100 * (fromIntegral now - fromIntegral from) / (fromIntegral to - fromIntegral from)

data WalletStatus = WalletStatus {
    _walletStatus'normal :: WalletStatusNormal
  , _walletStatus'restore :: WalletStatusRestore
} deriving (Show, Eq)

instance Default WalletStatus where
  def = emptyWalletStatus

data WalletStatusNormal =
    WalletStatusNormal'gettingNodeAddresses
  | WalletStatusNormal'connectingToPeers !Currency
  | WalletStatusNormal'gettingHeight !Int             -- Current height for catch up
  | WalletStatusNormal'newFilters !Int
  | WalletStatusNormal'synced
  | WalletStatusNormal'empty
  deriving (Show, Eq, Ord)

data CurrencyStatus = CurrencyStatus !Currency !WalletStatusNormal
  deriving (Show, Eq)

data RestoreStage =
    RestoreStage'connectingToBtcNodes
  | RestoreStage'askingHeight
  | RestoreStage'scanning
  | RestoreStage'empty
  deriving (Show, Eq)

data WalletStatusRestore = WalletStatusRestore {
    _walletStatusRestore'stage :: RestoreStage
  , _walletStatusRestore'progress :: Maybe Double
} deriving (Show, Eq)

instance Default WalletStatusRestore where
  def = emptyRestoreStatus

emptyRestoreStatus :: WalletStatusRestore
emptyRestoreStatus = WalletStatusRestore {
      _walletStatusRestore'stage = RestoreStage'empty
    , _walletStatusRestore'progress = Nothing
  }

emptyWalletStatus :: WalletStatus
emptyWalletStatus = WalletStatus {
      _walletStatus'normal = WalletStatusNormal'empty
    , _walletStatus'restore = emptyRestoreStatus
  }

makeLenses ''WalletStatus
makeLenses ''WalletStatusRestore
