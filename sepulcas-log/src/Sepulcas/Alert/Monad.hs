module Sepulcas.Alert.Monad where

import Reflex
import Sepulcas.Alert.Types

type MonadAlertPosterConstr t m = (
    MonadHold t m,
    Reflex t
  )

-- | Allows to delegate alert displaying to another widget without coupling with it
class MonadAlertPosterConstr t m => MonadAlertPoster t m | m -> t where
  -- | Add timed alert to queue of alerts to be displayed with special widget
  postAlert :: Event t AlertInfo -> m ()
  -- | Fires when new alert arrives from 'postAlert'
  newAlertEvent :: m (Event t AlertInfo)
  -- | Get alert's event and trigger. Internal
  getAlertEventFire :: m (Event t AlertInfo, AlertInfo -> IO ())
