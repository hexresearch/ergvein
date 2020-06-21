module Main where

import Control.Concurrent
import Control.Immortal
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Options.Applicative

import Ergvein.Index.Server.App
import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.PeerDiscovery.Discovery

import qualified Data.Text.IO as T
import Data.Text (Text, pack)

data Options = Options {
  optsCommand :: Command
}

type ServerUrl = Text

data Command =
    CommandListen FilePath

options :: Parser Options
options = Options
  <$> subparser (
       command "listen" (info (listenCmd <**> helper) $ progDesc "Start server")
  )
  where
    listenCmd = CommandListen
      <$> strArgument (
          metavar "CONFIG_PATH"
        )

main :: IO ()
main = do
    startServer =<< execParser opts
    where
      opts = info (options <**> helper)
        ( fullDesc
       <> progDesc "Starts Ergvein index server"
       <> header "ergvein-index-server - cryptocurrency index server for ergvein client" )

onStartup :: ServerEnv -> ServerM [Thread]
onStartup env = do
  scanningThreads <- blockchainScanning
  syncWithDefaultPeers
  refreshKnownPeersCache
  feesScanning
  peerIntroduce
  knownPeersActualization
  pure scanningThreads

startServer :: Options -> IO ()
startServer Options{..} = case optsCommand of
    CommandListen cfgPath ->  do
      cfg <- loadConfig cfgPath
      env <- runStdoutLoggingT $ newServerEnv cfg
      workerThreads <- runServerMIO env $ onStartup env
      T.putStrLn $ pack $ "Server started at " <> cfgDbHost cfg <> ":" <> (show . cfgServerPort $ cfg)

      let settings = appSettings $ onShutdown env workerThreads
          app = logStdoutDev $ indexServerApp env
          warpSettings = setPort (cfgServerPort cfg) settings
      runSettings warpSettings app
