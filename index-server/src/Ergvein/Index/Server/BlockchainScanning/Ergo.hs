module Ergvein.Index.Server.BlockchainScanning.Ergo where

import           Ergvein.Types.Transaction
import           Ergvein.Index.Server.Config
import           Ergvein.Index.Server.Environment
import           Ergvein.Index.Server.BlockchainScanning.Types

actualHeight :: Config -> IO BlockHeight
actualHeight cfg = undefined

blockInfo :: ServerEnv -> BlockHeight -> IO BlockInfo
blockInfo env blockHeightToScan = undefined