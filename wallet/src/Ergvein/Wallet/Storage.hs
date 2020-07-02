-- | Here will be widgets that implement interactions with the storage
module Ergvein.Wallet.Storage
  (
    withWallet
  ) where

import Ergvein.Text
import Ergvein.Wallet.Monad
import Ergvein.Types.Storage
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Util

-- | Requests password, runs a callback against decoded wallet and returns the result
-- The callback is in 'Performable m'. Basically 'MonadIO'
withWallet :: MonadFront t m
  => Event t (PrvStorage -> Performable m a) -- ^ Event with a callback
  -> m (Event t a)                               -- ^ results of applying the callback to the wallet
withWallet reqE = do
  walletName <- getWalletName
  eps      <- getEncryptedPrvStorage
  widgD    <- holdDyn Nothing $ Just <$> reqE
  storageE <- handleDangerMsg . fmap (decryptPrvStorage eps) =<< requestPasssword (walletName <$ reqE)
  performEvent $ attachWithMaybe (\mwg wall -> mwg <*> pure wall) (current widgD) storageE
