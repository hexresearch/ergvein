{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Index.Server.TCPService.Conversions where

import Conversion

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.BlockchainScanning.Types
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

instance Conversion BlockMetaRec BlockFilter where
  convert BlockMetaRec {..} = BlockFilter
    { blockFilterBlockId = blockMetaRecHeaderHash
    , blockFilterFilter  = blockMetaRecAddressFilter
    }


scanProgressInfoToScanBlock :: (Monad m, HasServerConfig m) => ScanProgressInfo -> m ScanBlock
scanProgressInfoToScanBlock ScanProgressInfo {..} = do
  scanBlockCurrency <- currencyToCurrencyCode nfoCurrency
  let scanBlockVersion    = 1
      scanBlockScanHeight = nfoScannedHeight
      scanBlockHeight     = nfoActualHeight
  pure $ ScanBlock {..}
