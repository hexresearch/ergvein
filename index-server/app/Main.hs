module Main where

import Data.Text (Text, pack)
import Options.Applicative
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.App
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Environment
import Control.Immortal
import Control.Monad.IO.Unlift
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.BlockchainScanner 

import qualified Data.Text.IO as T

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

startServer :: Options -> IO ()
startServer Options{..} = case optsCommand of
    CommandListen cfgPath ->  do
        cfg <- loadConfig cfgPath      
        env <- newServerEnv cfg
        t <- liftIO $ startBlockchainScanner env
        T.putStrLn $ pack $ "Server started at " <> configDbHost cfg <> ":" <> (show . configServerPort $ cfg)
        let app = logStdoutDev $ indexServerApp env
            warpSettings = setPort (configServerPort cfg) defaultSettings
        runSettings warpSettings app

  