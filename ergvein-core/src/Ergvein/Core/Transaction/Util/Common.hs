{-# OPTIONS_GHC -Wall #-}

module Ergvein.Core.Transaction.Util.Common(
    checkAddr
  , checkAddrTx
  , countConfirmations
) where

import Data.Word

import Ergvein.Core.Store.Monad
import Ergvein.Core.Transaction.Btc
import Ergvein.Core.Transaction.Erg
import Ergvein.Types.Address
import Ergvein.Types.Transaction
import Sepulcas.Native

import qualified Data.List as L

checkAddr :: (HasTxStorage m, PlatformNatives) => [EgvAddress] -> EgvTx -> m Bool
checkAddr addrs tx = do
  bL <- traverse (checkAddrTx tx) addrs
  pure $ L.or bL

-- | Checks given tx if there are some inputs or outputs containing given address.
checkAddrTx :: (HasTxStorage m, PlatformNatives) => EgvTx -> EgvAddress -> m Bool
checkAddrTx (TxBtc (BtcTx tx _)) (BtcAddress addr) = checkAddrTxBtc tx addr
checkAddrTx (TxErg (ErgTx tx _)) (ErgAddress addr) = checkAddrTxErg tx addr
checkAddrTx _ _ = pure False

countConfirmations :: BlockHeight -> Maybe BlockHeight -> Word64
countConfirmations _ Nothing = 0
countConfirmations currentHeight (Just confirmationHeight) = currentHeight - confirmationHeight + 1
