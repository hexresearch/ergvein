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
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.TCPService.Server

import qualified Data.Text.IO as T
import Data.Text (Text, pack)

data Options = Options {
  optsCommand :: Command
}

type ServerUrl = Text

data Command =  CleanKnownPeers FilePath | CommandListen FilePath

options :: Parser Options
options = Options
  <$> subparser (
       command "listen" (info (listenCmd <**> helper) $ progDesc "Start server") <>
       command "clean-known-peers" (info (cleanKnownPeers <**> helper) $ progDesc "resetting peers")
  )
  where
    cleanKnownPeers = CleanKnownPeers
      <$> strArgument (
          metavar "CONFIG_PATH"
        )
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
  scanningWorkers <- blockchainScanning
  syncWithDefaultPeers
  feeWorkers <- feesScanning
  peerIntroduce
  knownPeersActualization
  liftIO $ forkIO tcpSrv
  pure $ scanningWorkers ++ feeWorkers

startServer :: Options -> IO ()
startServer Options{..} = case optsCommand of
    CommandListen cfgPath -> do
      T.putStrLn $ pack "Server starting"
      cfg <- loadConfig cfgPath
      env <- runStdoutLoggingT $ newServerEnv cfg
      workerThreads <- runServerMIO env $ onStartup env
      T.putStrLn $ pack $ "Server started at:" <> (show . cfgServerPort $ cfg)

      let settings = appSettings $ onShutdown env workerThreads
          app = logStdoutDev $ indexServerApp env
          warpSettings = setPort (cfgServerPort cfg) settings
      runSettings warpSettings app
    CleanKnownPeers cfgPath -> do
      cfg <- loadConfig cfgPath
      env <- runStdoutLoggingT $ newServerEnv cfg
      runServerMIO env emptyKnownPeers
      T.putStrLn $ pack "knownPeers cleared"
