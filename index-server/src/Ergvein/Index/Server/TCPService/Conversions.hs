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
currencyCodeToCurrency code = do
  isTestnet <- cfgBTCNodeIsTestnet <$> serverConfig
  pure $ (if isTestnet then testnet else mainnet) code
  where
    testnet = \case
      TBTC   -> C.BTC
      TERGO  -> C.ERGO
    mainnet = \case
      BTC   -> C.BTC
      ERGO  -> C.ERGO

currencyToCurrencyCode :: (Monad m, HasServerConfig m) => C.Currency -> m CurrencyCode
currencyToCurrencyCode code = do
  isTestnet <- cfgBTCNodeIsTestnet <$> serverConfig
  pure $ (if isTestnet then testnet else mainnet) code
  where
    testnet = \case
      C.BTC  -> TBTC
      C.ERGO -> TERGO
    mainnet = \case
      C.BTC   -> BTC
      C.ERGO  -> ERGO

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
