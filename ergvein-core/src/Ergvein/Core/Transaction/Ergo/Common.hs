{-# OPTIONS_GHC -Wall #-}

module Ergvein.Core.Transaction.Ergo.Common(
    checkAddrTxErg
) where

import Ergvein.Core.Store.Monad
import Ergvein.Types.Address
import Ergvein.Types.Transaction
import Sepulcas.Native

checkAddrTxErg :: (HasTxStorage m, PlatformNatives) => ErgTxRaw -> ErgAddress -> m Bool
checkAddrTxErg = undefined
