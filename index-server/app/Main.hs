module Main where

import Control.Monad.Logger
import Control.Monad.IO.Class
import Data.Text (Text, pack)
import Data.Word
import Options.Applicative
import Text.Read

import Ergvein.Index.Server.App
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.TxIndex
import Ergvein.Index.Server.Monad

import qualified Data.Text.IO as T
import qualified Network.Bitcoin.Api.Client  as BitcoinApi

data Options = Options {
  -- | Which command to execute
  optsCommand           :: Command
  -- |Def: False. Use TCP connection to the node
, optsBtcTcpConn        :: Bool
  -- |Def: False. Start only BC-scanning threads
, optsOnlyScan          :: Bool
  -- | Starting height for btc
, optsBtcStartHeight    :: Word64
}

wordReader :: ReadM Word64
wordReader = eitherReader $ \arg -> case readMaybe arg of
  Nothing -> Left ("Cannot parse word: " ++ arg)
  Just w  -> Right w

type ServerUrl = Text

data Command
  = CleanKnownPeers FilePath
  | CommandListen FilePath
  | BuildBtcIndex FilePath Int

options :: Parser Options
options = Options
  <$> subparser (
       command "listen" (info (listenCmd <**> helper) $ progDesc "Start server") <>
       command "clean-known-peers" (info (cleanKnownPeers <**> helper) $ progDesc "resetting peers") <>
       command "build-index" (info (indexCmd <**> helper) $ progDesc "Build btc index")
  ) <*> flag False True (long "tcp-node" <> help "Use TCP connection to the node instead of RPC" )
    <*> flag False True (long "only-scan" <> help "Start only BC-scanning threads" )
    <*> option wordReader (long "btc-start" <> help "BTC starting height" <> value 0)
  where
    cleanKnownPeers = CleanKnownPeers
      <$> strArgument ( metavar "CONFIG_PATH" )
    listenCmd = CommandListen
      <$> strArgument ( metavar "CONFIG_PATH" )
    indexCmd = BuildBtcIndex
      <$> strArgument ( metavar "CONFIG_PATH" )
      <*> option auto ( metavar "THREAD_COUNT" <> short 'n' <> long "thread-count" <> value 1)

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
        runStdoutLoggingT $ withNewServerEnv optsBtcTcpConn client cfg $ \env -> do
          app optsOnlyScan cfg env
    CleanKnownPeers cfgPath -> do
      cfg@Config{..} <- loadConfig cfgPath
      BitcoinApi.withClient cfgBTCNodeHost cfgBTCNodePort cfgBTCNodeUser cfgBTCNodePassword $ \client -> do
        runStdoutLoggingT $ withNewServerEnv optsBtcTcpConn client cfg $ \env -> do
          liftIO $ runServerMIO env emptyKnownPeers
      T.putStrLn $ pack "knownPeers cleared"
    BuildBtcIndex cfgPath n -> do
      cfg@Config{..} <- loadConfig cfgPath
      BitcoinApi.withClient cfgBTCNodeHost cfgBTCNodePort cfgBTCNodeUser cfgBTCNodePassword $ \client -> do
        runStdoutLoggingT $ withTxIndexEnv optsBtcTcpConn client cfg $ \env -> do
          txIndexApp optsBtcStartHeight n env
      T.putStrLn $ pack "Index builder done"
