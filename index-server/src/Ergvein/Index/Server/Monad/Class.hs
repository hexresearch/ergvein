module Ergvein.Index.Server.Monad.Class
  (
    HasBitcoinNodeNetwork(..)
  , HasDiscoveryRequisites(..)
  , HasShutdownSignal(..)
  , MonadFees(..)
  , MonadRates(..)
  , HasBroadcastChannel(..)
  , ServerMonad
  , module Ergvein.Socket.Manager
  ) where

import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Fixed
import Data.Map.Strict (Map)

import Ergvein.Index.Protocol.Types (CurrencyCode, Message)
import Ergvein.Index.Server.Bitcoin.API
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Socket.Manager
import Ergvein.Types.Currency
import Ergvein.Types.Fees

import qualified Ergvein.Index.Protocol.Types as IPT
import qualified Network.Haskoin.Constants  as HK

type ServerMonad m = (
    MonadIO m
  , MonadBaseControl IO m
  , MonadCatch m
  , MonadLogger m
  , BitcoinApiMonad m
  , HasBitcoinNodeNetwork m
  , HasBroadcastChannel m
  , HasDbs m
  , HasDiscoveryRequisites m
  , HasServerConfig m
  , HasShutdownSignal m
  , HasSocketsManagement m
  , HasThreadsManagement m
  , MonadFees m
  , MonadRates m
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

class Monad m => MonadRates m where
  getRatesVar :: m (TVar (Map IPT.CurrencyCode (Map Fiat Centi)))

class MonadUnliftIO m => HasBroadcastChannel m where
  broadcastChannel :: m (TChan Message)
