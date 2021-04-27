{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.IP(
    module Ergvein.Core.IP
  ) where

import Sepulcas.Elements.Input.Class
import Network.Socket (SockAddr)
import Ergvein.Text
import Ergvein.Core.IP
import Ergvein.Wallet.Localize

instance (LocalizedPrint l, Wrappable IPStrings l) => Inputable l IP where
  displayInput _ = showt
  parseInput = maybe (Left $ wrap IPParseFailed) Right . parseIP
