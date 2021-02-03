module Main where

import Control.Monad.Logger
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
  -- |Def: False. Drop filters db when versions changed or not
, optsOverrideFilters   :: Bool
  -- |Def: False. Drop indexers db when versions changed or not
, optsOverrideIndexers  :: Bool
  -- |Def: False. Drop utxo db when versions changed or not
, optsOverrideUtxo      :: Bool
  -- |Def: False. Use TCP connection to the node
, optsBtcTcpConn        :: Bool
  -- |Def: False. Start only BC-scanning threads
, optsOnlyScan          :: Bool
  -- |Def: False. Whether to skip BTC hack or not
, optsSkipBtcHack       :: Bool
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
  ) <*> flag False True (long "override-ver-filters" <> help "Override Filters db version" )
    <*> flag False True (long "override-ver-indexer" <> help "Override Indexer's db version" )
    <*> flag False True (long "override-ver-utxo" <> help "Override Utxo's db version" )
    <*> flag False True (long "tcp-node" <> help "Use TCP connection to the node instead of RPC" )
    <*> flag False True (long "only-scan" <> help "Start only BC-scanning threads" )
    <*> flag False True (long "no-btc-hack" <> help "Skip BTC hack which starts the scan one block early each time" )
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
        env <- runStdoutLoggingT $ newServerEnv optsBtcTcpConn optsOverrideFilters optsOverrideIndexers optsOverrideUtxo client cfg
        runStdoutLoggingT $ app optsOnlyScan optsSkipBtcHack cfg env
    CleanKnownPeers cfgPath -> do
      cfg@Config{..} <- loadConfig cfgPath
      BitcoinApi.withClient cfgBTCNodeHost cfgBTCNodePort cfgBTCNodeUser cfgBTCNodePassword $ \client -> do
        env <- runStdoutLoggingT $ newServerEnv optsBtcTcpConn optsOverrideFilters optsOverrideIndexers optsOverrideUtxo client cfg
        runServerMIO env emptyKnownPeers
      T.putStrLn $ pack "knownPeers cleared"
    BuildBtcIndex cfgPath n -> do
      cfg@Config{..} <- loadConfig cfgPath
      BitcoinApi.withClient cfgBTCNodeHost cfgBTCNodePort cfgBTCNodeUser cfgBTCNodePassword $ \client -> do
        env <- runStdoutLoggingT $ newTxIndexEnv optsBtcTcpConn client cfg
        runStdoutLoggingT $ txIndexApp optsBtcStartHeight n env
      T.putStrLn $ pack "Index builder done"
