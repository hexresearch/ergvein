{-# OPTIONS_GHC -Wno-orphans #-}

module Ergvein.Wallet.Orphanage where

import Ergvein.Types.Address
import Ergvein.Wallet.Validate
import Sepulcas.Validate

instance Validate BtcAddress where
  validate = validateBtcRecipient
