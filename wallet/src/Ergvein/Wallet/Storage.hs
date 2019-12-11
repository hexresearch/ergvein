-- | Here will be widgets that implement interactions with the storage
module Ergvein.Wallet.Storage
  (
    withWallet
  , module Ergvein.Wallet.Storage.Data
  ) where

import Control.Monad.IO.Class
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Storage.Data
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Alert

-- | Requests password, runs a callback against decoded wallet and returns the result
-- The callback is in 'Performable m'. Basically 'MonadIO'
withWallet :: MonadFront t m
  => Event t (PrivateStorage -> Performable m a)    -- Event with a callback
  -> m (Event t a)                              -- results of applying the callback to the wallet
withWallet reqE = do
  eps      <- getEncryptedPrivateStorage
  widgD    <- holdDyn Nothing $ Just <$> reqE
  storageE <- handleDangerMsg . fmap (decryptPrivateStorage eps) =<< requestPasssword (() <$ reqE)
  performEvent $ attachWithMaybe (\mwg wall -> mwg <*> pure wall) (current widgD) storageE
