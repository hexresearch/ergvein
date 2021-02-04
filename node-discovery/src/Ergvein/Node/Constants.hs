module Ergvein.Node.Constants
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
defNodePort isTestnet =  if isTestnet then 18667 else 8667

defNodes :: Bool -> [ErgveinNodeAddr]
defNodes isTestnet = if isTestnet 
  then ["127.0.0.1"]
  else [ "ergvein-indexermainnet1.hxr.team:8667"
       , "ergvein-indexermainnet2.hxr.team:8667"
       , "ergvein-indexermainnet3.hxr.team:8667"
       , "indexer.ergvein.net" -- OwO
       ]

defDns :: Bool -> [HostName]
defDns isTestnet = if isTestnet then ["8.8.8.8","8.8.4.4", "1.1.1.1"] else mempty -- use resolv.conf

defSeedNodesSource :: Bool -> [Domain]
defSeedNodesSource isTestnet = if isTestnet then ["testseed.cypra.io"] else ["seed.cypra.io"]