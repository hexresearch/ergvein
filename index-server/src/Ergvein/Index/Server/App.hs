module Ergvein.Index.Server.App where

import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Gzip
import Network.HTTP.Types

import Servant
import Servant.API.Generic

import Ergvein.Index.API
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Server.V1

indexServerApp :: ServerEnv -> Application
indexServerApp e = gzip def . mid . appCors $ serve indexApi $ hoistServer indexApi (runServerM e) $ toServant indexServer
    where
        appCors = cors $ const $ Just simpleCorsResourcePolicy 
            { corsRequestHeaders = ["Content-Type"]
            , corsMethods = "PUT" : simpleMethods 
            }

mid :: Middleware
mid app env sendResponse =sendResponse $ responseLBS status200 [] "Hello World"

app :: Application
app req respond = let 
  in (respond $ responseLBS status200 [] "Hello World")