module Sepulcas.Monad.Class(
    MonadReflex
  , HasStoreDir(..)
  , MonadAlertPoster(..)
  , MonadHasMain(..)
  , MonadLocalized(..)
  , MonadNativeLogger(..)
  , MonadRetract(..)
  , PlatformNatives(..)
  , Prepulcable
  , Sepulcable(..)
  , toggleLoadingWidget
  , loadingWidgetDyn
  ) where

import Control.Monad.IO.Class
import Data.Text
import Reflex
import Reflex.Dom.Retractable
import Reflex.ExternalRef
import Reflex.Localize
import Reflex.Localize.Language
import Sepulcas.Alert.Monad
import Sepulcas.Log
import Sepulcas.Monad.Password
import Sepulcas.Monad.Reflex
import Reflex.Main.Thread
import Sepulcas.Native

type Prepulcable t m = (
    HasStoreDir m
  , HasPassModal t m
  , MonadAlertPoster t m
  , MonadReflex t m
  , MonadHasMain m
  , MonadLocalized t m
  , MonadNativeLogger t m
  , MonadRetract t m
  )

class Prepulcable t m => Sepulcable t (m :: * -> *) | m -> t where
  -- | Get loading widget trigger and fire. This is internal stuff
  getLoadingWidgetTF :: m (Event t (Bool, Text), (Bool, Text) -> IO ())
  -- | Application pause event. Informs that the application goes into the background.
  getPauseEventFire :: m (Event t (), IO ())
  -- | Application resume event. Informs that the application goes into the foreground.
  getResumeEventFire :: m (Event t (), IO ())
  -- | System back button event
  getBackEventFire :: m (Event t (), IO ())
  -- | Get langRef Internal
  getLangRef :: m (ExternalRef t Language)

-- | Request displaying the loading widget
toggleLoadingWidget :: Sepulcable t m => forall l . LocalizedPrint l => Event t (Bool, l) -> m ()
toggleLoadingWidget reqE = do
  fire <- fmap snd getLoadingWidgetTF
  langRef <- getLangRef
  performEvent_ $ ffor reqE $ \(b,lbl) -> liftIO $ do
    lang <- readExternalRef langRef
    fire (b,localizedShow lang lbl)
{-# INLINE toggleLoadingWidget #-}

-- | Display loading via Dynamic
loadingWidgetDyn :: Sepulcable t m => forall l . LocalizedPrint l => Dynamic t (Bool, l) -> m ()
loadingWidgetDyn reqD = do
  fire <- fmap snd getLoadingWidgetTF
  langRef <- getLangRef
  performEvent_ $ ffor (updated reqD) $ \(b,lbl) -> liftIO $ do
    lang <- readExternalRef langRef
    fire (b,localizedShow lang lbl)
{-# INLINE loadingWidgetDyn #-}
