module Sepulcas.Alert.Monad where

import Sepulcas.Monad.Reflex
import Sepulcas.Alert.Types

-- | Allows to delegate alert displaying to another widget without coupling with it
class MonadReflex t m => MonadAlertPoster t m | m -> t where
  -- | Add timed alert to queue of alerts to be displayed with special widget
  postAlert :: Event t AlertInfo -> m ()
  -- | Fires when new alert arrives from 'postAlert'
  newAlertEvent :: m (Event t AlertInfo)
  -- | Get alert's event and trigger. Internal
  getAlertEventFire :: m (Event t AlertInfo, AlertInfo -> IO ())
