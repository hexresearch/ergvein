module Ergvein.Index.Server.Monad.Class
  (
    HasBitcoinNodeNetwork(..)
  , HasDiscoveryRequisites(..)
  , HasShutdownSignal(..)
  , MonadFees(..)
  , HasBroadcastChannel(..)
  , ServerMonad
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Unlift
import Data.Map.Strict (Map)
import Ergvein.Index.Protocol.Types (CurrencyCode, Message)
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Types.Fees

import qualified Network.Haskoin.Constants  as HK

type ServerMonad m = (
    MonadIO m
  , HasBitcoinNodeNetwork m
  , HasDiscoveryRequisites m
  , HasShutdownSignal m
  , MonadFees m
  , HasBroadcastChannel m
  )

class Monad m => HasBitcoinNodeNetwork m where
  currentBitcoinNetwork :: m HK.Network

class Monad m => HasDiscoveryRequisites m where
  getDiscoveryRequisites  :: m PeerDiscoveryRequisites

class Monad m => HasShutdownSignal m where
  getShutdownFlag  :: m (TVar Bool)
  getShutdownChannel  :: m (TChan Bool)

class Monad m => MonadFees m where
  getFees :: m (Map CurrencyCode FeeBundle)
  setFees :: CurrencyCode -> FeeBundle -> m ()

class MonadUnliftIO m => HasBroadcastChannel m where
  broadcastChannel :: m (TChan Message)
