module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.App
import Ergvein.Index.Server.Config

configurationFilePath :: String
configurationFilePath = "./configuration.yaml"

main :: IO ()
main = do
    cfg <- loadConfig configurationFilePath
    env <- newServerEnv cfg
    let app = logStdoutDev $ indexServerApp env
        warpSettings = setPort (configServerPort cfg) defaultSettings
    runSettings warpSettings app