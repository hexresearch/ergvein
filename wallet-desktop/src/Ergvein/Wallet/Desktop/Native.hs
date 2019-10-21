module Ergvein.Wallet.Desktop.Native(

  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text(Text)

import Ergvein.Wallet.Native

instance PlatformNatives where
  resUrl = undefined

  storeValue = undefined

  retrieveValue = undefined

  readStoredFile = undefined

  appendStoredFile = undefined

  moveStoredFile = undefined

  getStoreFileSize = undefined

  pasteStr = undefined

  copyStr = undefined
