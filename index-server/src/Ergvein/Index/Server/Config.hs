module Ergvein.Index.Server.Config where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text
import Data.Time.Clock
import Data.Yaml.Config
import GHC.Generics

import Ergvein.Aeson

data CfgPeer = CfgPeer
 { cfgPeerIP   :: !String
 , cfgPeerPort :: !String
 } deriving (Show, Eq)

instance FromJSON CfgPeer where
  parseJSON = withObject "CfgPeer" $ \o -> do
    cfgPeerIP               <- o .: "peerIP"
    cfgPeerPort             <- o .: "peerPort"
    pure CfgPeer{..}

data Config = Config
  { cfgServerPort               :: !Int
  , cfgServerTcpPort            :: !Int
  , cfgFiltersDbPath            :: !String
  , cfgIndexerDbPath            :: !String
  , cfgBlockchainScanDelay      :: !Int
  , cfgBTCNodeIsTestnet         :: !Bool
  , cfgBTCNodeHost              :: !String
  , cfgBTCNodePort              :: !Int
  , cfgBTCNodeUser              :: !Text
  , cfgBTCNodePassword          :: !Text
  , cfgBTCNodeTCPHost           :: !String
  , cfgBTCNodeTCPPort           :: !Int
  , cfgERGONodeHost             :: !String
  , cfgERGONodePort             :: !Int
  , cfgOwnPeerAddress           :: !(Maybe CfgPeer)
  , cfgKnownPeers               :: ![CfgPeer]
  , cfgPeerActualizationDelay   :: !Int
  , cfgPeerActualizationTimeout :: !NominalDiffTime
  , cfgFeeEstimateDelay         :: !Int
  } deriving (Show, Generic)

class HasServerConfig m where
  serverConfig :: m Config

loadConfig :: MonadIO m => FilePath -> m Config
loadConfig path = liftIO $ loadYamlSettings [path] [] useEnv

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    cfgServerPort               <- o .: "serverPort"
    cfgServerTcpPort            <- o .: "serverTcpPort"
    cfgFiltersDbPath            <- o .:? "filtersDbPath" .!= "./ergveinDb"
    cfgIndexerDbPath            <- o .:? "indexerDbPath" .!= "./indexerDb"
    cfgBTCNodeIsTestnet         <- o .: "BTCNodeIsTestnet"
    cfgBTCNodeHost              <- o .: "BTCNodeHost"
    cfgBTCNodePort              <- o .: "BTCNodePort"
    cfgBTCNodeUser              <- o .: "BTCNodeUser"
    cfgBTCNodePassword          <- o .: "BTCNodePassword"
    cfgBTCNodeTCPHost           <- o .: "BTCNodeTCPHost"
    cfgBTCNodeTCPPort           <- o .: "BTCNodeTCPPort"
    cfgERGONodeHost             <- o .: "ERGONodeHost"
    cfgERGONodePort             <- o .: "ERGONodePort"
    cfgOwnPeerAddress           <- o .:? "ownPeerAddress"
    cfgKnownPeers               <- o .:? "knownPeers"               .!= (filterOwnAddressFromDefault cfgOwnPeerAddress)
    cfgBlockchainScanDelay      <- o .:? "blockchainScanDelay"      .!= 1000000
    cfgPeerActualizationDelay   <- o .:? "peerActualizationDelay"   .!= 10000000
    cfgPeerActualizationTimeout <- o .:? "peerActualizationTimeout" .!= 86400
    cfgFeeEstimateDelay         <- o .:? "feeEstimateDelay"         .!= (300 * 1000000) -- 5 min
    pure Config{..}

defaultPeers :: [CfgPeer]
defaultPeers = [
    CfgPeer "https://ergvein-indexer1.hxr.team" "8087"
  , CfgPeer "https://ergvein-indexer2.hxr.team" "8087"
  ]

filterOwnAddressFromDefault :: Maybe CfgPeer -> [CfgPeer]
filterOwnAddressFromDefault mown = case mown of
  Nothing -> defaultPeers
  Just own -> Prelude.filter (own /=) defaultPeers
