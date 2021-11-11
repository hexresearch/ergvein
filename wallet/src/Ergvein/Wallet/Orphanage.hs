{-# OPTIONS_GHC -Wno-orphans #-}

module Ergvein.Wallet.Orphanage where

import Ergvein.Core.IP
import Ergvein.Types.Address
import Ergvein.Wallet.Validate
import Sepulcas.Validate

instance Validate BtcAddress where
  validate = validateBtcRecipient

instance Validate Int where
  validate = validateInt

instance Validate IP where
  validate = validateIP
