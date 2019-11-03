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
import Control.Monad.Trans.Class

withWallet :: (MonadFront t m) => Event t (WalletData -> Performable m a) -> m (Event t a)
withWallet reqE = do
  encwal  <- getEncryptedWallet
  widgD   <- holdDyn Nothing $ Just <$> reqE
  wallE   <- handleDangerMsg . fmap (decryptWalletData encwal) =<< requestPasssword (() <$ reqE)
  performEvent $ attachWithMaybe (\mwg wall -> mwg <*> pure wall) (current widgD) wallE
