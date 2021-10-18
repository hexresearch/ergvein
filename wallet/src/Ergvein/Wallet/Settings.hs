module Ergvein.Wallet.Settings(
    getSettingsUnitBtc
  ) where

import Ergvein.Wallet.Monad

getSettingsUnitBtc :: MonadSettings t m => m UnitBTC
getSettingsUnitBtc = do
  settings <- getSettings
  let btcSettings = getBtcSettings settings
  pure $ btcSettings'units btcSettings
