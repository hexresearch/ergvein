module Ergvein.Wallet.Monad.Base
  (
    MonadFrontBase(..)
  , MonadFrontConstr
  , getWalletInfoMaybe
  , isAuthorized
  , loadingWidgetDyn
  , requestPasssword
  , toggleLoadingWidget
  ) where

import Control.Concurrent.Chan
import Control.Monad.IO.Class
import Control.Monad.Random.Class
import Data.Maybe (isJust)
import Data.Text (Text)
import Reflex
import Reflex.Dom.Retractable
import Reflex.ExternalRef

import Ergvein.Types.WalletInfo
import Ergvein.Types.Storage
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Version
import Sepulcas.Monad
import Sepulcas.Native

import qualified Crypto.Random.Types as CR

-- Context for unauthed widgets
-- Only to be used to request password and open the local storage
type MonadFrontConstr t m = (
    PlatformNatives
  , HasVersion
  , MonadSettings t m
  , MonadIndexClient t m
  , Sepulcable t m
  , CR.MonadRandom (Performable m)
  , HasStoreDir (Performable m)
  , HasSepulca t m
  )

class MonadFrontConstr t m => MonadFrontBase t m | m -> t where
  -- | Internal method to get storage of auth info
  getWalletInfoMaybeRef :: m (ExternalRef t (Maybe WalletInfo))
  -- | Manually set authorisation information for context. Used by widgets that
  -- implement actual login/logout. Some implementations may ingore 'Nothing'
  -- values if their semantic require persistent authorisation.
  setWalletInfo :: MonadFrontBase t m => Event t (Maybe WalletInfo) -> m (Event t ())

-- | Return flag that comes 'True' as soon as user passes authoristion on server
isAuthorized :: MonadFrontBase t m => m (Dynamic t Bool)
isAuthorized = (fmap . fmap) isJust getWalletInfoMaybe
{-# INLINE isAuthorized #-}

-- | Get authorization information that can be updated if user logs or logouts
getWalletInfoMaybe :: MonadFrontBase t m => m (Dynamic t (Maybe WalletInfo))
getWalletInfoMaybe = externalRefDynamic =<< getWalletInfoMaybeRef
{-# INLINE getWalletInfoMaybe #-}
