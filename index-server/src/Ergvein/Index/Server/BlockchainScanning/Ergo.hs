module Ergvein.Index.Server.BlockchainScanning.Ergo where

import Control.Monad.Reader

import Ergvein.Types.Transaction
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Interfaces.Ergo.It.Api.NodeApi 
import Network.Ergo.Api.Info
import Data.Maybe

import qualified Data.Text.IO as T
import Data.Text (Text, pack)

actualHeight :: ServerEnv -> IO BlockHeight
actualHeight env = do 
  info <- flip runReaderT (env'ergoNodeClient env) $ getInfo
  T.putStrLn $ pack $ show $ bestBlockHeight $ info
  pure $ fromIntegral $ fromMaybe 0 $ bestBlockHeight $ info

blockInfo :: ServerEnv -> BlockHeight -> IO BlockInfo
blockInfo env blockHeightToScan = undefined