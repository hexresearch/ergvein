{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Index.Server.TCPService.Conversions (
    currencyCodeToCurrency
  , currencyToCurrencyCode
  , scanProgressInfoToScanBlock
  ) where

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.Types
import Ergvein.Index.Server.Config

import qualified Ergvein.Types.Currency as C

currencyCodeToCurrency :: (Monad m, HasServerConfig m) => CurrencyCode -> m C.Currency
currencyCodeToCurrency code = case codeToCurrency code of
  Nothing -> fail $ "Unimplemented currency " <> show code
  Just c -> pure c

currencyToCurrencyCode :: (Monad m, HasServerConfig m) => C.Currency -> m CurrencyCode
currencyToCurrencyCode code = do
  isTestnet <- cfgBTCNodeIsTestnet <$> serverConfig
  pure $ currencyToCode isTestnet code

scanProgressInfoToScanBlock :: (Monad m, HasServerConfig m) => ScanProgressInfo -> m ScanBlock
scanProgressInfoToScanBlock ScanProgressInfo {..} = do
  scanBlockCurrency <- currencyToCurrencyCode nfoCurrency
  let scanBlockVersion    = (1, 0, 0)
      scanBlockScanHeight = nfoScannedHeight
      scanBlockHeight     = nfoActualHeight
  pure $ ScanBlock {..}
