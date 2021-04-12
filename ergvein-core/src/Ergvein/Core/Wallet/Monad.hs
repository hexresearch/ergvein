module Ergvein.Core.Wallet.Monad(
    MonadWallet(..)
  , getWalletInfoMaybe
  , isAuthorized
  ) where

import Control.Monad.IO.Class
import Control.Monad.Random.Class
import Data.Maybe (isJust)
import Data.Text (Text)
import Reflex
import Reflex.ExternalRef

import Ergvein.Types.WalletInfo
import Ergvein.Types.Storage
import Sepulcas.Native


-- Context for unauthed widgets
-- Only to be used to request password and open the local storage
type MonadWalletConstr t (m :: * -> *) = (
    MonadHold t m
  , MonadIO m
  , Reflex t
  )

class MonadWalletConstr t m => MonadWallet t (m :: * -> *) | m -> t where
  -- | Internal method to get storage of auth info
  getWalletInfoMaybeRef :: m (ExternalRef t (Maybe WalletInfo))
  -- | Manually set authorisation information for context. Used by widgets that
  -- implement actual login/logout. Some implementations may ingore 'Nothing'
  -- values if their semantic require persistent authorisation.
  setWalletInfo :: MonadWallet t m => Event t (Maybe WalletInfo) -> m (Event t ())

-- | Return flag that comes 'True' as soon as user passes authoristion on server
isAuthorized :: MonadWallet t m => m (Dynamic t Bool)
isAuthorized = (fmap . fmap) isJust getWalletInfoMaybe
{-# INLINE isAuthorized #-}

-- | Get authorization information that can be updated if user logs or logouts
getWalletInfoMaybe :: MonadWallet t m => m (Dynamic t (Maybe WalletInfo))
getWalletInfoMaybe = externalRefDynamic =<< getWalletInfoMaybeRef
{-# INLINE getWalletInfoMaybe #-}
