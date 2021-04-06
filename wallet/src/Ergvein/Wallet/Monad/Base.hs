module Ergvein.Wallet.Monad.Base
  (
    MonadFrontBase(..)
  , MonadFrontConstr
  , getAuthInfoMaybe
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

import Ergvein.Types.AuthInfo
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
  , MonadHasSettings t m
  , MonadIndexClient t m
  , Sepulcable t m
  , CR.MonadRandom (Performable m)
  , HasStoreDir (Performable m)
  , HasSepulca t m
  )

class MonadFrontConstr t m => MonadFrontBase t m | m -> t where
  -- | Internal method to get storage of auth info
  getAuthInfoMaybeRef :: m (ExternalRef t (Maybe AuthInfo))
  -- | Manually set authorisation information for context. Used by widgets that
  -- implement actual login/logout. Some implementations may ingore 'Nothing'
  -- values if their semantic require persistent authorisation.
  setAuthInfo :: MonadFrontBase t m => Event t (Maybe AuthInfo) -> m (Event t ())

-- | Return flag that comes 'True' as soon as user passes authoristion on server
isAuthorized :: MonadFrontBase t m => m (Dynamic t Bool)
isAuthorized = (fmap . fmap) isJust getAuthInfoMaybe
{-# INLINE isAuthorized #-}

-- | Get authorization information that can be updated if user logs or logouts
getAuthInfoMaybe :: MonadFrontBase t m => m (Dynamic t (Maybe AuthInfo))
getAuthInfoMaybe = externalRefDynamic =<< getAuthInfoMaybeRef
{-# INLINE getAuthInfoMaybe #-}
