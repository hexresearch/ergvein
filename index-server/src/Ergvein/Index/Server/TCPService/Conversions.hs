module Ergvein.Index.Server.TCPService.Conversions where

import Conversion

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.DB.Schema
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
    { blockFilterBlockId = hex2bs blockMetaRecHeaderHashHexView
    , blockFilterFilter  = hex2bs blockMetaRecAddressFilterHexView
    }