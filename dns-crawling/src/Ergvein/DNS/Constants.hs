module Ergvein.DNS.Constants
  ( ErgveinNodeAddr
  , defTestnetNodes
  , defMainnetNodes
  , defIndexerPort
  , defDns
  , defAndroidDns
  , seedTestnetNodesSource
  , seedMainnetNodesSource
  ) where

import Network.Socket (HostName, PortNumber)
import Data.Text (Text)
import Network.DNS.Types

type ErgveinNodeAddr = Text

defIndexerPort :: PortNumber
defIndexerPort = 8667

defTestnetNodes, defMainnetNodes :: [ErgveinNodeAddr]
defTestnetNodes = ["127.0.0.1"]
defMainnetNodes = ["139.59.142.25", "188.244.4.78"]

defDns, defAndroidDns :: [HostName]
defDns = ["8.8.8.8","8.8.4.4", "1.1.1.1"]
defAndroidDns = mempty

seedTestnetNodesSource, seedMainnetNodesSource :: [Domain]
seedTestnetNodesSource = ["testseed.cypra.io"]
seedMainnetNodesSource = ["seed.cypra.io"]