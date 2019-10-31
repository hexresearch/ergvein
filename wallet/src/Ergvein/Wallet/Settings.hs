{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Settings (
    Settings(..)
  , getSettings
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Default
import Data.Text(Text, pack)
import Data.Yaml (encodeFile)
import Ergvein.Aeson
import Ergvein.Lens
import Ergvein.Wallet.Language
import Ergvein.Wallet.Yaml(readYaml')
import System.Directory
#ifdef ANDROID
import Android.HaskellActivity
#endif

data Settings = Settings {
  settingsLang      :: Language
, settingsStoreDir  :: Text
} deriving (Eq, Show)

$(deriveJSON (aesonOptionsStripPrefix "settings") ''Settings)

makeLensesWith humbleFields ''Settings

#ifdef ANDROID
mkDefSettings :: MonadIO m => FilePath -> m Settings
mkDefSettings home = liftIO $ do
  createDirectoryIfMissing True (home <> "/store")
  let cfg = Settings English (pack home <> "/store")
  encodeFile (home <> "/config.yaml") cfg
  pure cfg

getSettings :: MonadIO m => Maybe FilePath -> m Settings
getSettings = const $ liftIO $ do
  mpath <- getFilesDir =<< getHaskellActivity
  case mpath of
    Nothing -> fail "Ergvein panic! No local folder!"
    Just path -> do
      ex <- doesFileExist $ path <> "/config.yaml"
      if not ex
        then mkDefSettings path
        else maybe (mkDefSettings path) pure =<< readYaml' path
#else
mkDefSettings :: MonadIO m => m Settings
mkDefSettings = liftIO $ do
  home <- getHomeDirectory
  putStrLn   "[ WARNING ]: Failed to load config. Reverting to default values: "
  putStrLn $ "Config path: " <> home <> "/.ergvein/config.yaml"
  putStrLn $ "Store  path: " <> home <> "/.ergvein/store"
  putStrLn $ "Language   : English"
  createDirectoryIfMissing True (home <> "/.ergvein/store")
  let cfg = Settings English (pack home <> "/.ergvein/store")
  encodeFile (home <> "/.ergvein/config.yaml") cfg
  pure cfg

getSettings :: MonadIO m => Maybe FilePath -> m Settings
getSettings mpath = liftIO $ case mpath of
  Nothing -> do
    home <- getHomeDirectory
    let path = home <> "/.ergvein/config.yaml"
    putStrLn "[ WARNING ]: No path provided. Trying the default: "
    putStrLn path
    getSettings $ Just path
  Just path -> do
    ex <- doesFileExist path
    if not ex
      then mkDefSettings
      else maybe mkDefSettings pure =<< readYaml' path
#endif
