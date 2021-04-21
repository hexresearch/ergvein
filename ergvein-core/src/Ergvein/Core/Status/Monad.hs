module Ergvein.Core.Status.Monad(
    MonadStatusConstr
  , MonadStatus(..)
  , getStatusUpdates
  , publishStatusUpdate
  ) where

import Control.Monad.IO.Class
import Ergvein.Core.Status.Types
import Reflex.ExternalRef
import Data.Map (Map)
import Ergvein.Types
import Reflex
import Data.Maybe

import qualified Data.Map.Strict as M

type MonadStatusConstr t m = (
    MonadHold t m
  , MonadIO (Performable m)
  , MonadIO m
  , PerformEvent t m
  , Reflex t
  )

class MonadStatusConstr t m => MonadStatus t m | m -> t where
  -- | Internal method.
  getStatusUpdRef :: m (ExternalRef t (Map Currency StatusUpdate))

-- | Get global status value
getStatusUpdates :: MonadStatus t m => Currency -> m (Dynamic t StatusUpdate)
getStatusUpdates cur = do
  statMapD <- externalRefDynamic =<< getStatusUpdRef
  pure $ fmap (fromMaybe NotActive . M.lookup cur) statMapD
{-# INLINE getStatusUpdates #-}

-- | Set global sync process value each time the event is fired
publishStatusUpdate :: MonadStatus t m => Event t CurrencyStatus -> m (Event t ())
publishStatusUpdate spE = do
  statusUpdRef <- getStatusUpdRef
  performEvent $ ffor spE $ \(CurrencyStatus cur sp) -> do
    modifyExternalRef_ statusUpdRef $ M.insert cur sp
{-# INLINE publishStatusUpdate #-}
