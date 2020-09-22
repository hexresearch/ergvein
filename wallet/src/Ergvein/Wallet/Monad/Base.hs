module Ergvein.Wallet.Monad.Base
  (
    MonadFrontBase(..)
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
import Reflex.ExternalRef

import Ergvein.Types.AuthInfo
import Ergvein.Types.Storage
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad.Prim

class MonadFrontConstr t m => MonadFrontBase t m | m -> t where
  -- | Get loading widget trigger and fire. This is internal stuff
  getLoadingWidgetTF :: m (Event t (Bool, Text), (Bool, Text) -> IO ())
  -- | System back button event
  getBackEventFire :: m (Event t (), IO ())
  -- | Internal method of getting channel where you can post actions that must be
  -- executed in main UI thread.
  getUiChan :: m (Chan (IO ()))
  -- | Get langRef Internal
  getLangRef :: m (ExternalRef t Language)
  -- | Get event and trigger for pasword requesting modal. Int -- id of the request.
  getPasswordModalEF :: m (Event t (Int, Text), (Int, Text) -> IO ())
  -- | Get event and trigger for the event that the password was submitted from modal. Internal
  -- Nothing value means that the modal was dismissed
  getPasswordSetEF :: m (Event t (Int, Maybe Password), (Int, Maybe Password) -> IO ())
  -- | Internal method to get storage of auth info
  getAuthInfoMaybeRef :: m (ExternalRef t (Maybe AuthInfo))
  -- | Manually set authorisation information for context. Used by widgets that
  -- implement actual login/logout. Some implementations may ingore 'Nothing'
  -- values if their semantic require persistent authorisation.
  setAuthInfo :: MonadFrontBase t m => Event t (Maybe AuthInfo) -> m (Event t ())

-- | Proper requester of passwords. Send wallet's name
requestPasssword :: MonadFrontBase t m => Event t Text -> m (Event t Password)
requestPasssword reqE = do
  i <- liftIO getRandom
  modalF <- fmap snd getPasswordModalEF
  setE <- fmap fst getPasswordSetEF
  performEvent_ $ (liftIO . modalF . (i,)) <$> reqE
  pure $ fforMaybe setE $ \(i', mp) -> if i == i' then mp else Nothing

-- | Request displaying the loading widget
toggleLoadingWidget :: MonadFrontBase t m => forall l . LocalizedPrint l => Event t (Bool, l) -> m ()
toggleLoadingWidget reqE = do
  fire <- fmap snd getLoadingWidgetTF
  langRef <- getLangRef
  performEvent_ $ ffor reqE $ \(b,lbl) -> liftIO $ do
    lang <- readExternalRef langRef
    fire (b,localizedShow lang lbl)
{-# INLINE toggleLoadingWidget #-}

-- | Display loading via Dynamic
loadingWidgetDyn :: MonadFrontBase t m => forall l . LocalizedPrint l => Dynamic t (Bool, l) -> m ()
loadingWidgetDyn reqD = do
  fire <- fmap snd getLoadingWidgetTF
  langRef <- getLangRef
  performEvent_ $ ffor (updated reqD) $ \(b,lbl) -> liftIO $ do
    lang <- readExternalRef langRef
    fire (b,localizedShow lang lbl)
{-# INLINE loadingWidgetDyn #-}

-- | Return flag that comes 'True' as soon as user passes authoristion on server
isAuthorized :: MonadFrontBase t m => m (Dynamic t Bool)
isAuthorized = (fmap . fmap) isJust getAuthInfoMaybe
{-# INLINE isAuthorized #-}

-- | Get authorization information that can be updated if user logs or logouts
getAuthInfoMaybe :: MonadFrontBase t m => m (Dynamic t (Maybe AuthInfo))
getAuthInfoMaybe = externalRefDynamic =<< getAuthInfoMaybeRef
{-# INLINE getAuthInfoMaybe #-}
