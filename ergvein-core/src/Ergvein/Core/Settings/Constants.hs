module Ergvein.Core.Settings.Constants(
    defIndexerPort
  , defaultIndexers
  , defaultSeedNodesSource
  , defaultIndexersNum
  , defaultIndexerTimeout
  , defaultActUrlNum
  , defaultDns
  ) where

import Data.Text
import Data.Time
import Ergvein.Core.Platform
import Ergvein.Node.Constants
import Network.DNS.Types
import Network.Socket (HostName, PortNumber)
import Sepulcas.Native

import qualified Data.Set as S

defIndexerPort :: PortNumber
defIndexerPort = defNodePort isTestnet

defaultIndexers :: [Text]
defaultIndexers = defNodes isTestnet

defaultSeedNodesSource :: [Domain]
defaultSeedNodesSource = defSeedNodesSource isTestnet

defaultIndexersNum :: (Int, Int)
defaultIndexersNum = (2, 4)

defaultIndexerTimeout :: NominalDiffTime
defaultIndexerTimeout = 20

defaultActUrlNum :: Int
defaultActUrlNum = 10

defaultDns :: PlatformNatives => S.Set HostName
defaultDns = S.fromList $ defDns isAndroid
