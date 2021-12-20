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
defNodePort isTestnet = if isTestnet then 18667 else 8667

defNodes :: Bool -> [ErgveinNodeAddr]
defNodes isTestnet = if isTestnet
  then ["64.227.120.8"]
  else [ "ergvein-indexermainnet1.hxr.team"
       , "ergvein-indexermainnet2.hxr.team"
       , "ergvein-indexermainnet3.hxr.team"
       , "indexer.ergvein.net" -- OwO
       ]

defDns :: Bool -> [HostName]
defDns isAndroid = if isAndroid then ["8.8.8.8","8.8.4.4", "1.1.1.1"] else mempty  --use resolv.conf if not android

defSeedNodesSource :: Bool -> [Domain]
defSeedNodesSource isTestnet = if isTestnet then ["testseed.cypra.io"] else ["seed.cypra.io"]
