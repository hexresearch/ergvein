module Ergvein.Core.Status.Monad(
    MonadStatusConstr
  , MonadStatus(..)
  , getWalletStatus
  , updateWalletStatusNormal
  , updateWalletStatusRestore
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Maybe
import Ergvein.Core.Status.Types
import Ergvein.Types
import Reflex
import Reflex.ExternalRef

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
  getWalletStatusRef :: m (ExternalRef t (Map Currency WalletStatus))

-- | Get global wallet status value
getWalletStatus :: MonadStatus t m => Currency -> m (Dynamic t WalletStatus)
getWalletStatus cur = do
  statMapD <- externalRefDynamic =<< getWalletStatusRef
  pure $ fmap (fromMaybe emptyWalletStatus . M.lookup cur) statMapD
{-# INLINE getWalletStatus #-}

-- | Updates normal wallet status each time the event is fired
updateWalletStatusNormal :: MonadStatus t m => Currency -> Event t (WalletStatusNormal -> WalletStatusNormal) -> m (Event t ())
updateWalletStatusNormal cur updateE = do
  walletStatusRef <- getWalletStatusRef
  performEvent $ ffor updateE $ \f ->
    modifyExternalRef_ walletStatusRef $ M.insertWith (\_ -> walletStatus'normal %~ f) cur (emptyWalletStatus & walletStatus'normal %~ f)

updateWalletStatusRestore :: MonadStatus t m => Currency -> Event t (WalletStatusRestore -> WalletStatusRestore) -> m (Event t ())
updateWalletStatusRestore cur updateE = do
  walletStatusRef <- getWalletStatusRef
  performEvent $ ffor updateE $ \f ->
    modifyExternalRef_ walletStatusRef $ M.insertWith (\_ -> walletStatus'restore %~ f) cur (emptyWalletStatus & walletStatus'restore %~ f)
