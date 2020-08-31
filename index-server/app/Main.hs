module Main where

import Control.Monad.Logger
import Options.Applicative

import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.App

import Data.Text (Text, pack)

import qualified Data.Text.IO as T
import qualified Network.Bitcoin.Api.Client  as BitcoinApi

data Options = Options {
  optsCommand :: Command
, optsNoDropFilters :: Bool
}

type ServerUrl = Text

data Command =  CleanKnownPeers FilePath | CommandListen FilePath

options :: Parser Options
options = Options
  <$> subparser (
       command "listen" (info (listenCmd <**> helper) $ progDesc "Start server") <>
       command "clean-known-peers" (info (cleanKnownPeers <**> helper) $ progDesc "resetting peers")
  ) <*> flag False True (long "no-drop-filters")
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

startServer :: Options -> IO ()
startServer Options{..} = case optsCommand of
    CommandListen cfgPath -> do
      T.putStrLn $ pack "Server starting"
      cfg@Config{..} <- loadConfig cfgPath
      BitcoinApi.withClient cfgBTCNodeHost cfgBTCNodePort cfgBTCNodeUser cfgBTCNodePassword $ \client -> do
        env <- runStdoutLoggingT $ newServerEnv optsNoDropFilters client cfg
        runStdoutLoggingT $ app cfg env
    CleanKnownPeers cfgPath -> do
      cfg@Config{..} <- loadConfig cfgPath
      BitcoinApi.withClient cfgBTCNodeHost cfgBTCNodePort cfgBTCNodeUser cfgBTCNodePassword $ \client -> do
        env <- runStdoutLoggingT $ newServerEnv optsNoDropFilters client cfg
        runServerMIO env emptyKnownPeers
      T.putStrLn $ pack "knownPeers cleared"
