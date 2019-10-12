module Ergvein.Index.Server.App where

import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Gzip

import Servant
import Servant.API.Generic
import Servant.Server.Generic


import Ergvein.Index.API
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.Server
import Ergvein.Index.Server.Server.V1

indexServerApp :: ServerEnv -> Application
indexServerApp e = gzip def . appCors $ serve indexApi $ hoistServer indexApi (runServerM e) $ toServant indexServer
    where
        appCors = cors $ const $ Just simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"]
        , corsMethods = "PUT" : simpleMethods 
        }