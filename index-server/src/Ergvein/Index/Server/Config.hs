module Ergvein.Index.Server.Config where

import Control.Monad.IO.Class
import Data.Text
import Data.Yaml.Config
import Ergvein.Aeson
import GHC.Generics
import Data.Time.Clock

data Config = Config
  { cfgServerPort               :: !Int
  , cfgDbHost                   :: !String
  , cfgDbPort                   :: !Int
  , cfgDbUser                   :: !String
  , cfgDbPassword               :: !String
  , cfgDbName                   :: !String
  , cfgCachePath                :: !String
  , cfgBlockchainScanDelay      :: !Int
  , cfgDbLog                    :: !Bool
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
  } deriving (Show, Generic)
deriveJSON (aesonOptionsStripPrefix "cfg") ''Config

class HasServerConfig m where
  serverConfig :: m Config

connectionStringFromConfig :: Config -> String
connectionStringFromConfig cfg = let
  params = [ ("host", cfgDbHost)
           , ("port", show . cfgDbPort)
           , ("user", cfgDbUser)
           , ("password", cfgDbPassword)
           , ("dbname", cfgDbName)
           ]
  in unpack $ intercalate " " $ segment <$> params
  where
    segment (label, accessor) = mconcat [label, "=", pack $ accessor cfg]

loadConfig :: MonadIO m => FilePath -> m Config
loadConfig path = liftIO $ loadYamlSettings [path] [] useEnv
