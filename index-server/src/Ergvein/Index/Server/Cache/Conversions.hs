module Ergvein.Index.Server.Cache.Conversions where

import Conversion
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.Cache.Schema

instance Conversion TxInfo TxCacheRec where
  convert txInfo = TxCacheRec (txHash txInfo) (txHexView txInfo) (txOutputsCount txInfo)