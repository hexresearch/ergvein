module Ergvein.Index.Server.Dependencies where

import Control.Concurrent.STM.TVar
import Ergvein.Index.Protocol.Types
import Control.Monad.IO.Unlift
import Ergvein.Index.Server.PeerDiscovery.Types
import Network.HTTP.Client
import Servant.Client.Core
import qualified Network.HTTP.Client as HC
import qualified Network.Haskoin.Constants   as HK
import qualified Data.Map.Strict     as Map
import Ergvein.Types.Fees

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

class MonadFees m where
  getFees :: m (Map.Map CurrencyCode FeeBundle)
  setFees :: CurrencyCode -> FeeBundle -> m ()