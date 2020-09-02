module Ergvein.Index.Server.Dependencies 
 ( HasBitcoinNodeNetwork
 , HasDiscoveryRequisites
 , HasShutdownFlag
 , MonadFees
 , HasConnectionsManagement
 , HasBroadcastChannel
 , currentBitcoinNetwork
 , getDiscoveryRequisites
 , getShutdownFlag
 , getFees
 , setFees
 , openConnections
 , broadcastChannel
 )where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Unlift
import Data.Map.Strict (Map(..))
import Ergvein.Index.Protocol.Types (CurrencyCode, Message)
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Types.Fees
import Network.HTTP.Client
import Network.Socket
import Servant.Client.Core

import qualified Data.Map.Strict            as Map
import qualified Network.HTTP.Client        as HC
import qualified Network.Haskoin.Constants  as HK

class HasBitcoinNodeNetwork m where
  currentBitcoinNetwork :: m HK.Network

class HasDiscoveryRequisites m where
  getDiscoveryRequisites  :: m PeerDiscoveryRequisites

class HasShutdownFlag m where
  getShutdownFlag  :: m (TVar Bool)

class MonadFees m where
  getFees :: m (Map.Map CurrencyCode FeeBundle)
  setFees :: CurrencyCode -> FeeBundle -> m ()

class MonadUnliftIO m => HasConnectionsManagement m where
  openConnections :: m (TVar (Map SockAddr (ThreadId, Socket)))

class MonadUnliftIO m => HasBroadcastChannel m where
  broadcastChannel :: m (TChan Message)