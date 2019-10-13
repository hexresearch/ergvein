module Main where

import Network.Wai.Handler.Warp   (run)
import Network.Wai.Middleware.RequestLogger
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.App
import Ergvein.Index.Server.Config

main :: IO ()
main = do
    env <- newServerEnv $ Config { configDb = ""}
    let app = logStdoutDev $ indexServerApp env
    run 8000 app