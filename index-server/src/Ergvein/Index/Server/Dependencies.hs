module Ergvein.Index.Server.Dependencies where

import qualified Network.Haskoin.Constants   as HK
import qualified Network.HTTP.Client as HC
import Control.Monad.IO.Unlift
import Network.HTTP.Client
import Servant.Client.Core
import Ergvein.Index.Server.PeerDiscovery.Types
import Control.Concurrent.STM.TVar

class HasBitcoinNodeNetwork m where
  currentBitcoinNetwork :: m HK.Network

class MonadIO m => HasHttpManager m where
  getHttpManager  :: m Manager

class MonadIO m => HasTlsManager m where
  getTlsManager  :: m Manager

class HasDiscoveryRequisites m where
  getDiscoveryRequisites  :: m PeerDiscoveryRequisites

class HasShutdownFlag m where
  getShutdownFlag  :: m (TVar Bool)