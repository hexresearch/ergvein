module Ergvein.Index.Server.App where

import Control.Immortal
import Control.Monad
import Database.LevelDB.Internal
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Gzip
import Servant
import Servant.API.Generic
import System.Posix.Signals

import Ergvein.Index.API
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Server.V1

indexServerApp :: ServerEnv -> Application
indexServerApp e = gzip def . appCors $ serve indexApi $ hoistServer indexApi (runServerM e) $ toServant indexServer
    where
        appCors = cors $ const $ Just simpleCorsResourcePolicy 
            { corsRequestHeaders = ["Content-Type"]
            , corsMethods = "PUT" : simpleMethods 
            }

onShutdown :: ServerEnv -> [Thread] -> IO ()
onShutdown env workerTreads = do
  sequenceA $ mortalize <$> workerTreads
  sequence_ $ wait <$> workerTreads

appSettings :: IO () -> Settings
appSettings shutdownAction = setInstallShutdownHandler shutdownHandler defaultSettings
  where
    shutdownHandler closeSocket =
      void $ installHandler sigTERM (Catch $ shutdownAction >> closeSocket) Nothing