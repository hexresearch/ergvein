{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Settings (
    Settings(..)
  , loadSettings
  , storeSettings
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson (withText)
import Data.Default
import Data.Text(Text, pack, unpack)
import Data.Time (NominalDiffTime)
import Data.Yaml (encodeFile)
import Ergvein.Aeson
import Ergvein.Lens
import Ergvein.Wallet.Language
import Ergvein.Wallet.Yaml(readYamlEither')
import Servant.Client(BaseUrl(..), Scheme(..), parseBaseUrl, showBaseUrl)
import System.Directory

import qualified Control.Exception   as Exception
import qualified Data.Text as T

#ifdef ANDROID
import Android.HaskellActivity
#endif

data Settings = Settings {
  settingsLang        :: Language
, settingsStoreDir    :: Text
, settingsConfigPath  :: Text
, settingsDefUrls     :: [BaseUrl]
, settingsDefUrlNum   :: (Int, Int)
, settingsReqTimeout  :: NominalDiffTime
} deriving (Eq, Show)

$(deriveJSON (aesonOptionsStripPrefix "settings") ''Settings)

instance ToJSON BaseUrl where
  toJSON = toJSON . showBaseUrl

instance FromJSON BaseUrl where
  parseJSON = withText "BaseUrl" $ maybe (fail "Failed to parse BaseUrl") pure . parseBaseUrl . unpack

makeLensesWith humbleFields ''Settings

-- | TODO: Implement some checks to see if the configPath folder is ok to write to
storeSettings :: MonadIO m => Settings -> m ()
storeSettings s = liftIO $ do
  let configPath = settingsConfigPath s
  createDirectoryIfMissing True $ unpack $ T.dropEnd 1 $ fst $ T.breakOnEnd "/" configPath
  encodeFile (unpack configPath) s

#ifdef ANDROID
mkDefSettings :: MonadIO m => FilePath -> m Settings
mkDefSettings home = liftIO $ do
  let storePath   = home <> "/store"
      configPath  = home <> "/config.yaml"
      cfg = Settings English (pack storePath) (pack configPath) [] (2,3) 5
  createDirectoryIfMissing True storePath
  encodeFile configPath cfg
  pure cfg

loadSettings :: MonadIO m => Maybe FilePath -> m Settings
loadSettings = const $ liftIO $ do
  mpath <- getFilesDir =<< getHaskellActivity
  case mpath of
    Nothing -> fail "Ergvein panic! No local folder!"
    Just path -> do
      ex <- doesFileExist $ path <> "/config.yaml"
      if not ex
        then mkDefSettings path
        else either (const $ mkDefSettings path) pure =<< readYamlEither' path
#else
mkDefSettings :: MonadIO m => m Settings
mkDefSettings = liftIO $ do
  home <- getHomeDirectory
  putStrLn   "[ WARNING ]: Failed to load config. Reverting to default values: "
  putStrLn $ "Config path: " <> home <> "/.ergvein/config.yaml"
  putStrLn $ "Store  path: " <> home <> "/.ergvein/store"
  putStrLn $ "Language   : English"
  let storePath   = home <> "/.ergvein/store"
      configPath  = home <> "/.ergvein/config.yaml"
      cfg = Settings English (pack storePath) (pack configPath) [] (2,3) 5
  createDirectoryIfMissing True storePath
  encodeFile configPath cfg
  pure cfg

loadSettings :: MonadIO m => Maybe FilePath -> m Settings
loadSettings mpath = liftIO $ case mpath of
  Nothing -> do
    home <- getHomeDirectory
    let path = home <> "/.ergvein/config.yaml"
    putStrLn "[ WARNING ]: No path provided. Trying the default: "
    putStrLn path
    loadSettings $ Just path
  Just path -> do
    ex <- doesFileExist path
    if not ex
      then mkDefSettings
      else either (const mkDefSettings) pure =<< readYamlEither' path
#endif
