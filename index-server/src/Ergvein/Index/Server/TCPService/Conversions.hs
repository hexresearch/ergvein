module Ergvein.Index.Server.TCPService.Conversions where

import Conversion

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Text
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

instance Conversion C.Currency CurrencyCode where
  convert = \case
    C.BTC  -> TBTC
    C.ERGO -> TERGO

instance Conversion BlockMetaRec BlockFilter where
  convert BlockMetaRec {..} = BlockFilter
    { blockFilterBlockId = blockMetaRecHeaderHash
    , blockFilterFilter  = blockMetaRecAddressFilter
    }

instance Conversion ScanProgressInfo ScanBlock where
  convert ScanProgressInfo {..} = ScanBlock
    { scanBlockCurrency   = convert nfoCurrency
    , scanBlockVersion    = 1
    , scanBlockScanHeight = nfoScannedHeight
    , scanBlockHeight     = nfoActualHeight
    }
