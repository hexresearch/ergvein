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
import Network.Bitcoin.Api.Client
import Network.Bitcoin.Api.Blockchain

import qualified Data.Text.IO as T

configurationFilePath :: String
configurationFilePath = "./configuration.yaml"

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

scanner :: MonadUnliftIO m => Config -> m Thread
scanner cfg = let 
  f = withClient (configBTCNodeHost cfg) (configBTCNodePort cfg) (configBTCNodeUser cfg) (configBTCNodePassword cfg) getBlockCount
  in create $ \thread -> liftIO $ do
     x <- f
     T.putStrLn $ pack $ show x
     pure ()

startServer :: Options -> IO ()
startServer Options{..} = case optsCommand of
    CommandListen cfgPath ->  do

        cfg <- loadConfig cfgPath
        t <- liftIO $ scanner cfg
        T.putStrLn $ pack "thread"
        T.putStrLn $ pack $ connectionStringFromConfig cfg
        env <- newServerEnv cfg
        
        T.putStrLn $ pack $ "Server started at " <> configDbHost cfg <> ":" <> (show . configServerPort $ cfg)
        let app = logStdoutDev $ indexServerApp env
            warpSettings = setPort (configServerPort cfg) defaultSettings
        runSettings warpSettings app

  