module Ergvein.DNS.Constants
  ( ErgveinNodeAddr
  , defNodes
  , defNodePort
  , defDns
  , defSeedNodesSource
  ) where

import Data.Text (Text)
import Network.DNS.Types
import Network.Socket (HostName, PortNumber)

type ErgveinNodeAddr = Text

defNodePort :: Bool -> PortNumber
defNodePort isTestnet =  if isTestnet then 8667 else 18667

defNodes :: Bool -> [ErgveinNodeAddr]
defNodes isTestnet = if isTestnet then ["127.0.0.1"] else ["139.59.142.25", "188.244.4.78"]

defDns :: Bool -> [HostName]
defDns isTestnet = if isTestnet then ["8.8.8.8","8.8.4.4", "1.1.1.1"] else mempty 

defSeedNodesSource :: Bool -> [Domain]
defSeedNodesSource isTestnet = if isTestnet then ["testseed.cypra.io"] else ["seed.cypra.io"]
