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

startServer :: Options -> IO ()
startServer Options{..} = case optsCommand of
    CommandListen cfgPath -> do
      T.putStrLn $ pack "Server starting"
      cfg <- loadConfig cfgPath
      env <- runStdoutLoggingT $ newServerEnv cfg
      runStdoutLoggingT $ app cfg env
    CleanKnownPeers cfgPath -> do
      cfg <- loadConfig cfgPath
      env <- runStdoutLoggingT $ newServerEnv cfg
      runServerMIO env emptyKnownPeers
      T.putStrLn $ pack "knownPeers cleared"
