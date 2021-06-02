module Ergvein.Wallet.Settings(
    getSettingsUnitBtc
  , getSettingsUnitErg
  ) where

import Ergvein.Wallet.Monad

getSettingsUnitBtc :: MonadSettings t m => m UnitBTC
getSettingsUnitBtc = do
  settings <- getSettings
  let btcSettings = getBtcSettings settings
  pure $ btcSettings'units btcSettings

getSettingsUnitErg :: MonadSettings t m => m UnitERGO
getSettingsUnitErg = do
  settings <- getSettings
  let ergSettings = getErgoSettings settings
  pure $ ergSettings'units ergSettings
