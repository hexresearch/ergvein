module Ergvein.Index.Server.Config where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text
import Data.Time.Clock
import Data.Yaml.Config
import GHC.Generics

import Ergvein.Aeson

data Config = Config
  { cfgServerPort               :: !Int
  , cfgDBPath                   :: !String
  , cfgBlockchainScanDelay      :: !Int
  , cfgBTCNodeIsTestnet         :: !Bool
  , cfgBTCNodeHost              :: !String
  , cfgBTCNodePort              :: !Int
  , cfgBTCNodeUser              :: !Text
  , cfgBTCNodePassword          :: !Text
  , cfgERGONodeHost             :: !String
  , cfgERGONodePort             :: !Int
  , cfgOwnPeerAddress           :: !(Maybe String)
  , cfgKnownPeers               :: ![String]
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
    cfgDBPath                   <- o .:? "dbPath" .!= "./ergveinDb"
    cfgBTCNodeIsTestnet         <- o .: "BTCNodeIsTestnet"
    cfgBTCNodeHost              <- o .: "BTCNodeHost"
    cfgBTCNodePort              <- o .: "BTCNodePort"
    cfgBTCNodeUser              <- o .: "BTCNodeUser"
    cfgBTCNodePassword          <- o .: "BTCNodePassword"
    cfgERGONodeHost             <- o .: "ERGONodeHost"
    cfgERGONodePort             <- o .: "ERGONodePort"
    cfgOwnPeerAddress           <- o .:? "ownPeerAddress"
    cfgKnownPeers               <- o .:? "knownPeers"               .!= (filterOwnAddressFromDefault cfgOwnPeerAddress)
    cfgBlockchainScanDelay      <- o .:? "blockchainScanDelay"      .!= 1000000
    cfgPeerActualizationDelay   <- o .:? "peerActualizationDelay"   .!= 10000000
    cfgPeerActualizationTimeout <- o .:? "peerActualizationTimeout" .!= 86400
    cfgFeeEstimateDelay         <- o .:? "feeEstimateDelay"         .!= (300 * 1000000) -- 5 min
    pure Config{..}

defaultPeers :: [String]
defaultPeers = [
    "https://ergvein-indexer1.hxr.team"
  , "https://ergvein-indexer2.hxr.team"
  ]

filterOwnAddressFromDefault :: Maybe String -> [String]
filterOwnAddressFromDefault mown = case mown of
  Nothing -> defaultPeers
  Just own -> Prelude.filter (own /=) defaultPeers
