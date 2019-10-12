module Main where


import Network.Wai.Handler.Warp   (run)
import Servant

import Servant.API.Generic
import Servant.Server.Generic

import Ergvein.Types.Currency
import Ergvein.Index.API
import Ergvein.Index.API.Types
import Ergvein.Index.API.V1
import Ergvein.Index.Server.Server
import Ergvein.Index.Server.Server.V1


app :: Application
app = serve indexApi $ toServant indexServer

main :: IO ()
main = do
    env <- newEnv cfg
    