module Ergvein.Index.Server.TCPService.Conversions where

import Conversion

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Text

import qualified Ergvein.Types.Currency as C

instance Conversion CurrencyCode C.Currency where
  convert = \case
    BTC   -> C.BTC
    ERGO  -> C.ERGO

instance Conversion C.Currency CurrencyCode where
  convert = \case
    C.BTC  -> TBTC
    C.ERGO -> TERGO

instance Conversion BlockMetaRec BlockFilter where
  convert BlockMetaRec {..} = BlockFilter
    { blockFilterBlockId = blockMetaRecHeaderHashHexView
    , blockFilterFilter  = blockMetaRecAddressFilterHexView
    }

instance Conversion ScanProgressInfo ScanBlock where
  convert ScanProgressInfo {..} = ScanBlock
    { scanBlockCurrency   = convert nfoCurrency
    , scanBlockVersion    = 1
    , scanBlockScanHeight = nfoScannedHeight
    , scanBlockHeight     = nfoActualHeight
    }
